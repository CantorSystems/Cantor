@echo off

set System=..\..\..\..\System
set Units=..\..\CoreLite;%System%\MMX.%DelphiVer%;%System%

set Options=-b -gp -$C- -$I- -$T+
set Exe=..\LiteConv.exe

call Revision.bat

del %~dp0LiteConv.cfg
dcc32 %~dp0LiteConv.dpr -d"Tricks;Lite;%1" -e.. %Options% -u"%Units%"
if errorlevel 1 goto exit

pet -nologo -strip -trunc %Exe% -into %Exe% -osver 5 -dropsect .rsrc -stub

:exit