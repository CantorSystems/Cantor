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
  MOV AX, 4C00h
  INT 21h

MsgLen    db TermChar - Message
Message   db 'This program requires Windows NT'
TermChar  db '$'
