@echo off

for /d %%f in (%~dp0Samples\*) do (
  call "%%f\build.bat" %*
  if errorlevel 1 goto exit
)

:exit