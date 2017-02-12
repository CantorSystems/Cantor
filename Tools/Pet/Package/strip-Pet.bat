rem @echo off
call "%~dp0strip" -raw "%~dp0Pet.exe" -into "%Temp%\Pet.exe"
echo move /y "%Temp%\Pet.exe" "%~dp0Pet.exe"
move /y "%Temp%\Pet.exe" "%~dp0Pet.exe"