@echo off

call "%~dp0..\build.bat" "%~dp0Pet" %*
if errorlevel 1 goto exit

if "%1" neq "" goto exit

set Pet=%~dp0..\..\Bin\Pet
fasm "%~dp0Win32.asm" "%Pet%\Win32.exe"
copy "%~dp0strip*.bat" "%Pet%" >nul
"%Pet%\Pet.exe" -strip -trunc -aslr -osver 5 -ls -stub "%Pet%\Win32.exe" . -into "%Temp%\Pet.exe"
if errorlevel 1 goto exit
move /y "%Temp%\Pet.exe" "%Pet%" >nul
if exist "%Pet%\Win32.exe" del "%Pet%\Win32.exe" >nul

set Stubs=%Pet%\CustomStubs
if not exist "%Stubs%" mkdir "%Stubs%"
for %%f in (%~dp0CustomStubs\*.asm) do (
  fasm "%%f"
  if errorlevel 1 (
    del "%~dp0CustomStubs\*.exe"
    goto exit
  )
)
move /y "%~dp0CustomStubs\*.exe" "%Stubs%" >nul

:exit