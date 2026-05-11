"""Command line wrapper for running PETFit in Docker."""

from __future__ import annotations

import argparse
import shlex
import subprocess
import sys
from pathlib import Path
from typing import List, Optional, Sequence

from ._version import __version__

DEFAULT_IMAGE = "mathesong/petfit:latest"
DEFAULT_PORT = 3838
APPS = ("regiondef", "modelling_plasma", "modelling_ref")
MODES = ("automatic", "interactive")
STEPS = ("datadef", "weights", "delay", "reference_tac", "model1", "model2", "model3")


class PETFitHelpFormatter(argparse.ArgumentDefaultsHelpFormatter, argparse.RawDescriptionHelpFormatter):
    """Preserve paragraphs while still showing defaults."""


class PathAction(argparse.Action):
    """Expand user paths while preserving argparse's standard display."""

    def __call__(self, parser, namespace, values, option_string=None):
        if isinstance(values, list):
            setattr(namespace, self.dest, [str(Path(value).expanduser()) for value in values])
        else:
            setattr(namespace, self.dest, str(Path(values).expanduser()))


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="petfit-docker",
        formatter_class=PETFitHelpFormatter,
        description=(
            "The PETFit on Docker wrapper\n\n"
            "This is a lightweight Python wrapper to run PETFit. Docker must be "
            "installed and running. This can be checked running::\n\n"
            "    docker info\n\n"
            "The wrapper accepts a BIDS-App-like command line and translates host "
            "paths into Docker bind mounts before executing the PETFit image."
        ),
    )

    parser.add_argument("bids_dir", nargs="?", action=PathAction)
    parser.add_argument("output_dir", nargs="?", action=PathAction)
    parser.add_argument("analysis_level", nargs="?", choices=("participant",), default="participant")
    parser.add_argument("--version", action="version", version=f"%(prog)s {__version__}")
    parser.add_argument("-i", "--image", default=DEFAULT_IMAGE, help="image name")

    wrapper = parser.add_argument_group(
        "Wrapper options",
        "Standard options that require mapping files into the container; see petfit usage for complete descriptions",
    )
    wrapper.add_argument("--app", choices=APPS, default="regiondef", help="PETFit app to run")
    wrapper.add_argument("--mode", choices=MODES, default="automatic", help="execution mode")
    wrapper.add_argument("--step", choices=STEPS, help="single automatic modelling step to run")
    wrapper.add_argument("--blood-dir", action=PathAction, help="blood data directory for plasma input models")
    wrapper.add_argument("-w", "--work-dir", action=PathAction, help="working directory to mount in the container")
    wrapper.add_argument("--petfit-output-foldername", default="petfit", help="petfit output folder within derivatives")
    wrapper.add_argument("--analysis-foldername", default="Primary_Analysis", help="analysis folder name")
    wrapper.add_argument("--cores", type=int, default=1, help="number of cores for parallel processing")
    wrapper.add_argument(
        "--ancillary-analysis-folder",
        help="sibling analysis folder to inherit delay or k2prime estimates from",
    )
    wrapper.add_argument("--port", type=int, default=DEFAULT_PORT, help="host and container port for interactive Shiny apps")

    developer = parser.add_argument_group("Developer options", "Tools for testing and debugging PETFit")
    developer.add_argument("--shell", action="store_true", help="open shell in image instead of running PETFit")
    developer.add_argument(
        "-e",
        "--env",
        nargs=2,
        action="append",
        metavar=("ENV_VAR", "value"),
        help="set custom environment variables within container",
    )
    developer.add_argument(
        "-u",
        "--user",
        help="run container as a given user/uid. A group/gid can also be assigned, i.e. --user <UID>:<GID>",
    )
    developer.add_argument("--network", help='run container with a different network driver, e.g. "none"')
    developer.add_argument("--no-tty", action="store_true", help="run docker without TTY flag -it")
    developer.add_argument("--dry-run", action="store_true", help="print the Docker command without executing it")
    developer.add_argument(
        "--skip-image-check",
        action="store_true",
        help="do not check whether the Docker image exists before running",
    )

    return parser


def _absolute_path(path: Optional[str], *, create: bool = False) -> Optional[str]:
    if path is None:
        return None

    resolved = Path(path).expanduser().resolve()
    if create:
        resolved.mkdir(parents=True, exist_ok=True)
    return str(resolved)


def _mount_argument(host_path: str, container_path: str, mode: str) -> str:
    return f"{host_path}:{container_path}:{mode}"


def build_docker_command(opts: argparse.Namespace) -> List[str]:
    """Build the Docker command corresponding to parsed options."""

    if opts.mode == "interactive" and (opts.port < 1 or opts.port > 65535):
        raise SystemExit("--port must be between 1 and 65535")
    if opts.cores < 1:
        raise SystemExit("--cores must be at least 1")
    if opts.app == "regiondef" and opts.step:
        raise SystemExit("--step is only valid for modelling_plasma and modelling_ref")

    bids_dir = _absolute_path(opts.bids_dir) if opts.bids_dir else None
    output_dir = _absolute_path(opts.output_dir, create=not opts.shell) if opts.output_dir else None
    blood_dir = _absolute_path(opts.blood_dir) if opts.blood_dir else None
    work_dir = _absolute_path(opts.work_dir, create=True) if opts.work_dir else None

    if not opts.shell:
        missing = []
        if not bids_dir:
            missing.append("bids_dir")
        if not output_dir:
            missing.append("output_dir")
        if missing:
            raise SystemExit("the following arguments are required unless --shell is used: " + ", ".join(missing))

    command = ["docker", "run", "--rm"]
    if not opts.no_tty:
        command.append("-it")

    if opts.user:
        command.extend(["--user", opts.user])
    if opts.network:
        command.extend(["--network", opts.network])

    env = list(opts.env or [])
    if opts.mode == "interactive":
        env.append(("SHINY_PORT", str(opts.port)))
        command.extend(["-p", f"{opts.port}:{opts.port}"])

    for key, value in env:
        command.extend(["-e", f"{key}={value}"])

    if bids_dir:
        command.extend(["-v", _mount_argument(bids_dir, "/data/bids_dir", "ro")])
    if output_dir:
        command.extend(["-v", _mount_argument(output_dir, "/data/derivatives_dir", "rw")])
    if blood_dir:
        command.extend(["-v", _mount_argument(blood_dir, "/data/blood_dir", "ro")])
    if work_dir:
        command.extend(["-v", _mount_argument(work_dir, "/data/work_dir", "rw")])

    if opts.shell:
        command.extend(["--entrypoint", "/bin/bash", opts.image])
        return command

    command.append(opts.image)
    command.extend(
        [
            "--func",
            opts.app,
            "--mode",
            opts.mode,
            "--petfit_output_foldername",
            opts.petfit_output_foldername,
            "--analysis_foldername",
            opts.analysis_foldername,
            "--cores",
            str(opts.cores),
        ]
    )

    if opts.step:
        command.extend(["--step", opts.step])
    if opts.ancillary_analysis_folder:
        command.extend(["--ancillary_analysis_folder", opts.ancillary_analysis_folder])

    return command


def check_docker() -> None:
    """Fail early if Docker is unavailable."""

    try:
        subprocess.run(["docker", "info"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
    except (OSError, subprocess.CalledProcessError):
        raise SystemExit("Docker is not available. Please install Docker and ensure the daemon is running.")


def image_exists(image: str) -> bool:
    """Return whether a Docker image is available locally."""

    try:
        subprocess.run(["docker", "image", "inspect", image], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
    except (OSError, subprocess.CalledProcessError):
        return False
    return True


def maybe_pull_image(image: str) -> None:
    """Offer to pull a missing image, matching the PETPrep wrapper's flow."""

    if image_exists(image):
        return

    answer = input(f"Image '{image}' is missing\nWould you like to download? [Y/n] ")
    if answer.strip().lower() not in ("", "y", "yes"):
        raise SystemExit(f"Image '{image}' is required")

    subprocess.run(["docker", "pull", image], check=True)


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = _parser()
    opts = parser.parse_args(argv)
    command = build_docker_command(opts)

    print("RUNNING: " + shlex.join(command))

    if opts.dry_run:
        return 0

    check_docker()
    if not opts.skip_image_check:
        maybe_pull_image(opts.image)

    return subprocess.call(command)
