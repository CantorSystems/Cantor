@echo off
call "%~dp0strip" -nologo -ls . -into "%Temp%\Pet.exe"
move /y "%Temp%\Pet.exe" "%~dp0Pet.exe" >nul