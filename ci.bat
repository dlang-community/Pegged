@echo off
@setlocal
setlocal EnableDelayedExpansion

echo Unit Tests
REM cd %~dp0\..

echo ""
dub test --build=unittest-cov
