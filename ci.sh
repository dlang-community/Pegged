#!/bin/bash

set -e -x -o pipefail

DC=${DC:-dmd}

# test for successful release build
dub build -b release --compiler=$DC
dub clean --all-packages -q

# test for successful 32-bit build
if [ "$DC" == "dmd" ]; then
    dub build -b release --compiler=$DC --arch=x86
    dub clean --all-packages -q
fi

# Run unit tests
dub test --compiler=$DC

# Run examples tests
pushd examples/simple_arithmetic
dub test --compiler=$DC -q
popd

dub test :arithmetic --compiler=$DC -q
dub test :numbers --compiler=$DC -q
dub test :strings --compiler=$DC -q
dub test :csv --compiler=$DC -q
dub test :json --compiler=$DC -q

pushd examples/composition
dub test --compiler=$DC -q
popd

# TODO Actually doesn't compiles
# pushd examples/c
# dub test --compiler=$DC -q
# popd

pushd examples/dgrammar
dub test --compiler=$DC -q
popd

pushd examples/markdown
dub test --compiler=$DC -q
popd

pushd examples/oberon2
dub test --compiler=$DC -q
popd

pushd examples/parameterized
dub test --compiler=$DC -q
popd

pushd examples/PEG
dub test --compiler=$DC -q
popd

pushd examples/peggedgrammar
dub test --compiler=$DC -q
popd

pushd examples/xml
dub test --compiler=$DC -q
popd

# Execute extended_pascal build with time -v
pushd examples/extended_pascal
# Hack to workaround dub bug with preGenerateCommands
dub build -b release --compiler=$DC || true
/usr/bin/time -v dub build -b release --compiler=$DC
popd

