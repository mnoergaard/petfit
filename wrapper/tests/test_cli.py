import tempfile
import unittest
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from petfit_docker.cli import _parser, build_docker_command


class DockerCommandTests(unittest.TestCase):
    def test_builds_bids_app_like_modelling_command(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            bids = root / "bids"
            derivatives = root / "derivatives"
            blood = root / "blood"
            bids.mkdir()
            blood.mkdir()

            opts = _parser().parse_args(
                [
                    str(bids),
                    str(derivatives),
                    "participant",
                    "--app",
                    "modelling_plasma",
                    "--blood-dir",
                    str(blood),
                    "--step",
                    "weights",
                    "--no-tty",
                    "--dry-run",
                ]
            )

            command = build_docker_command(opts)

            self.assertIn("docker", command)
            self.assertIn("--func", command)
            self.assertIn("modelling_plasma", command)
            self.assertIn("--step", command)
            self.assertIn("weights", command)
            self.assertIn(f"{bids.resolve()}:/data/bids_dir:ro", command)
            self.assertIn(f"{derivatives.resolve()}:/data/derivatives_dir:rw", command)
            self.assertIn(f"{blood.resolve()}:/data/blood_dir:ro", command)
            self.assertTrue(derivatives.exists())

    def test_interactive_mode_maps_port_and_env(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            bids = root / "bids"
            derivatives = root / "derivatives"
            bids.mkdir()

            opts = _parser().parse_args(
                [
                    str(bids),
                    str(derivatives),
                    "participant",
                    "--mode",
                    "interactive",
                    "--port",
                    "3840",
                    "--no-tty",
                    "--dry-run",
                ]
            )

            command = build_docker_command(opts)

            self.assertIn("-p", command)
            self.assertIn("3840:3840", command)
            self.assertIn("-e", command)
            self.assertIn("SHINY_PORT=3840", command)

    def test_regiondef_rejects_step(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            bids = root / "bids"
            derivatives = root / "derivatives"
            bids.mkdir()

            opts = _parser().parse_args(
                [
                    str(bids),
                    str(derivatives),
                    "participant",
                    "--app",
                    "regiondef",
                    "--step",
                    "weights",
                ]
            )

            with self.assertRaises(SystemExit):
                build_docker_command(opts)


if __name__ == "__main__":
    unittest.main()
