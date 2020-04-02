@echo off
@setlocal
setlocal EnableDelayedExpansion

echo Unit Tests
REM cd %~dp0\..

echo "Test for successful release build"
dub build -b release --compiler=%DC%
dub clean --all-packages

echo "Runnin tests"
dub test --compiler=%DC% -v
REM --build=unittest-cov
