#!/bin/bash

set -e -x -o pipefail

DC=${DC:-dmd}

# test for successful release build
dub build -b release --compiler=$DC
dub clean --all-packages

# test for successful 32-bit build
if [ "$DC" == "dmd" ]; then
    dub build -b release --compiler=$DC --arch=x86
    dub clean --all-packages
fi

# Run unit tests
dub test --compiler=$DC

# Execute extended_pascal build with time -v
cd pegged/examples/extended_pascal
# Hack to workaround dub bug with preGenerateCommands
dub build -b release --compiler=$DC || true
/usr/bin/time -v dub build -b release --compiler=$DC

