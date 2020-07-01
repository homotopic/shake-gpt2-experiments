# Shake GPT-2 Experiments

These are some sketches of how to put together text generation using shakebook
and gpt-2-simple.

There are several wiki scrapers and parsing tools in the Shakefile. There are
two nix shell environments, one for shakebook and one for gpt-2 with
tensorflow-gpu. These are separate because shakebook uses stack-to-nix and a
pinned version of nixpkgs, where as tensorflow-gpu needs to build using a cuda
version in line with your nvidia drivers, so it needs to be built against the
same nixpkgs used to build your host drivers.

gpt-2-simple has been cargoed directly into this repository because
data_sampler.sample() needs to be set to 256 reduced in order to run within 6GB
of GPU memory, which is how much I have, and there is no command line option
for this.

## Cachix

Cachix is provided by shakebook

```
cachix use shakebook 
```

## Data

First launch the haskell `nix-shell`.

```
nix-shell
```

You can use the phony rules to download one of the manifests in
`manifests/originals`. It will perform the following steps:

* Evaluate the manifest, polling each Category for a list of subcategories and
  writing that to a derived manifest in `manifest/derived`.
* Using the new derived manifest to download all pages from that wiki into the
  `raw` folder
* Processing the downloaded mediawiki into markdown via pandoc filters, into
  the `processed` folder.

For example

```
shake wikipedia startrek\
```

You can also give this more jobs with `-j`

```
shake -jN wikipedia startrek
```

Requests to the mediawiki api are cached in the shake database in `.shake`. You
can safely delete the `raw` folder and shake will use the database to
repopulate it. If you delete the database however, you will have to redownload
everything.

To try new pandoc filtering steps, just delete the processed folder, or delete
a single file. You can test new filters by running the rule for a single file
by specifying it on the command line, which will be quicker than re-evaluating
the manifest or reprocessing everything.

```
rm processed/mediawiki/en.wikipedia.org/Julius\ Caesar.md
shake processed/mediawiki/en.wikipedia.org/Julius\ Caesar.md
```

## Training

To train with gpt-2 drop into the python/cuda shell from within the haskell shell
and run `shake train`. This will take a long time to build.

```
nix-shell cuda-shell.nix
shake train
```

This will pull everything from the `processed` folder and throw it into a training set, then
 run  gpt_2_simple.py with a standard set of options. The model and checkpoint will be stored
in `/out`

## Generating

To generate 20 samples to the `out/` folder.

```
shake generate
```
