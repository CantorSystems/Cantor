@echo off

for /d %%f in (%~dp0Tools\*) do (
  call "%%f\build.bat" %*
  if errorlevel 1 goto exit
)

:exit