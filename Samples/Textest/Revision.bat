@echo off
for /f "tokens=1,2 delims=:MSP" %%i in ('svnversion -c -n') do (
  if "%%j" == "" (
    set rev=%%i
  ) else (
    set rev=%%j
  )
)
if defined rev (
  if "%rev%" leq "9999999999" (
    echo SVN revision %rev%
    echo const Revision = 'r%rev%'; > revision.inc
  )
)