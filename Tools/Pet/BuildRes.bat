@echo off
for /f "tokens=1,2 delims=:MSP" %%i in ('svnversion -c -n') do (
  if "%%j" == "" (
  set rev=%%i
  ) else (
    set rev=%%j
  )
)
echo SVN revision %rev%
echo #define rev %rev% > revision.inc
echo #define textrev "%rev%" >> revision.inc
if /i "%1" neq "" (
  echo #define %1 >> revision.inc
)
brcc32 Resources.rc -foResources.res -l409