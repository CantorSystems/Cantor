@echo off
call "%~dp0strip" -nologo -raw "%~dp0Pet.exe" -into "%Temp%\Pet.exe"
move /y "%Temp%\Pet.exe" "%~dp0Pet.exe"