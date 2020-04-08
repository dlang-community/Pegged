@echo off
@setlocal
setlocal EnableDelayedExpansion

echo Unit Tests

echo "Test for successful release build"
dub build -b release --compiler=%DC%
dub clean --all-packages -q

echo "Running tests"
dub test --compiler=%DC% -v

echo "Running example tests"
dub test :arithmetic --compiler=%DC% 
dub test :numbers --compiler=%DC% 
dub test :strings --compiler=%DC%
dub test :csv --compiler=%DC%
dub test :json --compiler=%DC%

pushd examples\composition
dub test --compiler=%DC%
popd

rem TODO Actually doesn't compiles
rem pushd examples\c
rem dub test --compiler=%DC%
rem popd

pushd examples\dgrammar
dub test --compiler=%DC%
popd

pushd examples\markdown
dub test --compiler=%DC%
popd

pushd examples\oberon2
dub test --compiler=%DC%
popd

pushd examples\parameterized
dub test --compiler=%DC%
popd

pushd examples\PEG
dub test --compiler=%DC%
popd

pushd examples\peggedgrammar
dub test --compiler=%DC%
popd

pushd examples\xml
dub test --compiler=%DC%
popd

echo Execute extended_pascal build 
pushd examples\extended_pascal
rem Hack to workaround dub bug with preGenerateCommands
dub build -b release --compiler=%DC% || dub build -b release --compiler=%DC%
popd
