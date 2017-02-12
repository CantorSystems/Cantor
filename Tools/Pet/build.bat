@echo off

set System=..\..\..\..\System
set Units=..\..\CoreLite;%System%\MMX.%DelphiVer%;%System%

set Options=-b -gp -$C- -$I- -$T+
set Exe=..\Pet.exe

if /i "%2"=="debug" (
  set Debug=debug
) else if /i "%1"=="debug" (
  set Debug=debug
)

if /i "%2"=="ASLR" (
  set ASLR=-ASLR
) else if /i "%1"=="ASLR" (
  set ASLR=-ASLR
)

call BuildRes.bat %Debug%

del %~dp0Pet.cfg
dcc32 %~dp0Pet.dpr -d"Tricks;ForceMMX;Lite;%1" -e.. %Options% -u"%Units%"
if errorlevel 1 goto exit

call StripRes %~dp0%Exe%
pet -nologo -strip -trunc %Exe% -into %Exe% -osver 5 -stub %ASLR%

:exit
