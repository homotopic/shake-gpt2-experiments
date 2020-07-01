# Shake GPT-2 Experiments

These are some sketches of how to put together text generation using shakebook and gpt-2-simple.

There are several wiki scrapers and parsing tools in the Shakefile. There are
two nix shell environments, one for shakebook and one for gpt-2 with
tensorflow-gpu. These are separate because shakebook uses stack-to-nix and a pinned version of
nixpkgs, where as tensorflow-gpu needs to build using a cuda version in line with your nvidia
drivers, so it needs to be built against the same nixpkgs used to build your host drivers.
