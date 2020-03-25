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
