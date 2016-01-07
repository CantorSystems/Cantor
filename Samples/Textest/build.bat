@echo off

set System=..\..\..\..\System
set Units=..\..\CoreLite;%System%\MMX.%DelphiVer%;%System%

set Options=-b -gp -$C- -$I-
set Exe=..\Textest.exe

call Revision.bat

del %~dp0Textest.cfg
dcc32 %~dp0Textest.dpr -d"CoreLiteVCL;Tricks;ForceMMX;Lite;%1" -e.. %Options% -u"%Units%"
if errorlevel 1 goto exit

call StripRes %~dp0%Exe%
pet -nologo -strip -trunc %Exe% -into %Exe% -osver 5 -dropsect .rsrc -stub

:exit