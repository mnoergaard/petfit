# petfit-docker

`petfit-docker` is a lightweight Python wrapper that turns a BIDS-App-like
command line into the matching `docker run` invocation for PETFit.

```bash
petfit-docker /path/to/bids /path/to/derivatives participant \
  --app modelling_plasma \
  --blood-dir /path/to/blood \
  --analysis-foldername Primary_Analysis
```

The command above runs:

```bash
docker run --rm -it \
  -v /path/to/bids:/data/bids_dir:ro \
  -v /path/to/derivatives:/data/derivatives_dir:rw \
  -v /path/to/blood:/data/blood_dir:ro \
  mathesong/petfit:latest \
  --func modelling_plasma --mode automatic
```

## Install for development

```bash
cd wrapper
python -m pip install -e .
```

## Examples

Run region definition automatically:

```bash
petfit-docker /path/to/bids /path/to/derivatives participant --app regiondef
```

Run plasma-input modelling:

```bash
petfit-docker /path/to/bids /path/to/derivatives participant \
  --app modelling_plasma \
  --blood-dir /path/to/blood
```

Launch the reference-tissue Shiny app:

```bash
petfit-docker /path/to/bids /path/to/derivatives participant \
  --app modelling_ref \
  --mode interactive \
  --port 3838
```

Open a shell in the image:

```bash
petfit-docker --shell -i mathesong/petfit:latest
```
