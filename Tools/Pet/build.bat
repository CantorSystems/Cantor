@echo off

set System=..\..\..\..\System
set Units=..\..\CoreLite;%System%\MMX;%System%

set Options=-b -gp -$C- -$I- -$T+
set Exe=..\Pet.exe

call BuildRes.bat %1

del %~dp0Pet.cfg
dcc32 %~dp0Pet.dpr -d"Tricks;ForceMMX;Lite;%1" -e.. %Options% -u"%Units%"
if errorlevel 1 goto exit

call StripRes %~dp0%Exe%
pet -strip -trunc %Exe% -into %Exe% -osver 5 -stub

:exit