@echo off
setlocal

if "%1" == "" (
  echo Usage: %~n0 [source-file [-inc]]
  echo Option -inc is for generate build.inc and Resource.res files 
  echo to compile sources from the Delphi IDE
  goto exit
)

set Pet=%~dp0..\Bin\Pet\Pet.exe
if "%2" == "" (
  if "%~n1" neq "Pet" (
    if not exist "%Pet%" call "%~dp0Pet\build.bat"
  )
)

set BuildInc=%~dp1build.inc
set Target=%~n1

if not exist "%~dp1Resources.rc" goto sample
if not exist "%~dp0..\.git" goto res

for /f "delims= " %%i in ('git log -1 --pretty^=oneline') do (
  set CommitHash=%%i
)
for /f %%i in ('git rev-parse --abbrev-ref HEAD') do (
  set branch=%%i
)

if "%branch%" == "master" goto master

for /f "tokens=2 delims=-" %%i in ("%branch%") do (
  set source=%%i
)
for /f "tokens=3 delims=-" %%i in ('git describe --tags') do (
  set Build=%%i
)
set Flags=VS_FF_PRERELEASE
goto retrieved

:master
for /f "tokens=1 delims=-" %%i in ('git log -1 --pretty^=%%cd-%%h --date^=format:%%y%%j') do (
  set Build=%%i
)
for /f "tokens=2 delims=-" %%i in ('git describe --tags') do (
  set source=%%i
)

:retrieved
for /f "tokens=1* delims=." %%a in ("%source%") do (
  set Major=%%a
  set Minor=%%b
  if defined %%c (
    set Release=%%c
  )
)

if "%Major%" == "" (
  set Major=0
)
if "%Minor%" == "" (
  set Minor=0
)
if "%Release%" == "" (
  set Release=0
)
if "%Build%" == "" (
  set Build=0
)

echo == %Target% ==
echo         Version  %Major%.%Minor%.%Release%.%Build% (%Flags%)
echo Git commit hash  %CommitHash%

echo #define CommitHash "%CommitHash%" >"%BuildInc%"
echo #define Major %Major% >>"%BuildInc%"
echo #define Minor %Minor% >>"%BuildInc%"
echo #define Release %Release% >>"%BuildInc%"
echo #define Build %Build% >>"%BuildInc%"
echo #define MajorStr "%Major%" >>"%BuildInc%"
echo #define MinorStr "%Minor%" >>"%BuildInc%"
echo #define ReleaseStr "%Release%" >>"%BuildInc%"
echo #define BuildStr "%Build%" >>"%BuildInc%"
if "%Flags%" neq "" (
  echo #define Flags %Flags% >>"%BuildInc%"
)

:res
brcc32 "%~dp1Resources.rc" -fo"%~dp1Resources.res" -l409
if errorlevel 1 goto exit

if "%2" == "-inc" goto exit

set Bin=%~dp0..\Bin
if not exist "%Bin%" mkdir "%Bin%"
if errorlevel 1 goto exit

set Bin=%Bin%\%Target%
if not exist "%Bin%" mkdir "%Bin%"
if errorlevel 1 goto exit

copy "%~dp0..\license.md" "%Bin%" >nul

set TypedAddr=-$T+
set ASLR=-aslr

if exist %~dp0..\..\Tricks (
  set Symbols=Tricks;ForceMMX
  set Units=%~dp0..\..\Tricks\MMX.%DelphiVer%;%~dp0..\..\Tricks
)

goto build

:sample
if exist %~dp0..\..\Tricks (
  set Symbols=Tricks
  set Units=%~dp0..\..\Tricks\i386.%DelphiVer%;%~dp0..\..\Tricks
)

if not exist "%~dp0..\.git" goto env

for /f "tokens=1,2 delims=-" %%i in ('git log -1 --pretty^=%%cd-%%h --date^=format:%%y%%j') do (
  set Build=%%i
  set CommitHash=%%j
)

echo == %Target% ==
echo           Build  %Build%
echo Git commit hash  %CommitHash%

echo const Build = '%Build% (%CommitHash%)'; >"%BuildInc%"

:env
set Bin=%~dp0..\Bin
if not exist "%Bin%" mkdir "%Bin%"
if errorlevel 1 goto exit

set Bin=%Bin%\Samples
if not exist "%Bin%" mkdir "%Bin%"
if errorlevel 1 goto exit

if "%2" == "-inc" goto exit

:build
set Options=-b -$C- -$I- %TypedAddr%
set Target=%Bin%\%Target%
set Units=%~dp0..\CoreLite;%Units%

if exist "%1.cfg" del "%1.cfg" >nul
dcc32 "%1.dpr" -d"Lite;%Symbols%" -u"%Units%" -n"%~dp0..\Bin" -e"%Bin%" %Options%
if errorlevel 1 goto exit

call stripRes "%Target%.exe"
call stripRes "%Target%.dll"

if "%~n1" neq "Pet" (
  "%Pet%" -nologo -strip -trunc -stub -osver 5 -ls %ASLR% "%Target%.exe" "%Target%.dll" -into
)

:exit