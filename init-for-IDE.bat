@echo off
call "%~dp0\build-all.bat" -inc

if not exist "%~dp0\Tools\Pet\Pet.dof" (
  copy "%~dp0\Tools\Pet.dof" "%~dp0\Tools\Pet\Pet.dof" >nul
  if errorlevel 1 goto exit
)

if not exist "%~dp0\Samples\HostThunk\HostApp.dof" (
  copy "%~dp0\Samples\Sample.dof" "%~dp0\Samples\HostThunk\HostApp.dof" >nul
  if errorlevel 1 goto exit
)

if not exist "%~dp0\Samples\HostThunk\CoreLib.dof" (
  copy "%~dp0\Samples\Sample.dof" "%~dp0\Samples\HostThunk\CoreLib.dof" >nul
  if errorlevel 1 goto exit
)

if not exist "%~dp0\Samples\HostThunk\CoreLib.dof" (
  copy "%~dp0\Samples\Sample.dof" "%~dp0\Samples\HostThunk\CoreLib.dof" >nul
  if errorlevel 1 goto exit
)

if not exist "%~dp0\Samples\LiteConv\LiteConv.dof" (
  copy "%~dp0\Samples\Sample.dof" "%~dp0\Samples\LiteConv\LiteConv.dof" >nul
  if errorlevel 1 goto exit
)

if not exist "%~dp0\Samples\Textest\Textest.dof" (
  copy "%~dp0\Samples\Sample.dof" "%~dp0\Samples\Textest\Textest.dof" >nul
  if errorlevel 1 goto exit
)

:exit