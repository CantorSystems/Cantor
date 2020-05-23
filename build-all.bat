@echo off

call "%~dp0build-samples.bat" %*
if errorlevel 1 goto exit

call "%~dp0build-tools.bat" %*

:exit