@echo off
call "%~dp0..\..\Tools\build.bat" "%~dp0HostApp" %*
if "%1" == "" (
  call "%~dp0..\..\Tools\build.bat" "%~dp0CoreLib" %*
)