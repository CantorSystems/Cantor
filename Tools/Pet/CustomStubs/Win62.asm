; FASM syntax

format MZ
  heap 0
  stack 100h
  entry main:start

segment main use16

start:
  PUSH CS
  POP DS
  MOV AH, 9
  MOV DX, Message
  INT 21h
  MOV AX, 4C01h
  INT 21h
  
Message db 'This program requires Windows 8 or Windows Server 2012', 13, 10, '$'
