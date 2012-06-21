(*
    Kolibri OS Platform SDK for Delphi

    Copyright (c) 2012 The Unified Environment Laboratory
*)

unit Kolibri;

interface

type
  TBound = packed record
    Origin, Size: Word;
  end;

  TColor = packed record
    Blue, Green, Red, Style: Byte;
  end;

procedure DrawWindow(const X, Y: TBound; const WindowColor, CaptionColor, BorderColor: TColor); overload; // mcall 0;
procedure DrawWindow(const Top, Left, Width, Height: Word; WindowColor, CaptionColor, BorderColor: LongWord); overload;

implementation

procedure DrawWindow(const X, Y: TBound; const WindowColor, CaptionColor, BorderColor: TColor); overload;
asm
        PUSH EBX
        PUSH ESI
        PUSH EDI

        MOV EBX, EAX
        XCHG ECX, EDX
        MOV ESI, CaptionColor
        MOV EDI, BorderColor

        XOR EAX, EAX
        INT $40

        POP EDI
        POP ESI
        POP EBX
end;

procedure DrawWindow(const Top, Left, Width, Height: Word; WindowColor, CaptionColor, BorderColor: LongWord); overload;
asm
        PUSH EBX
        PUSH ESI
        PUSH EDI

        SHL ECX, 16
        ADD EAX, ECX
        MOV EBX, EAX
        MOV CX, Height
        SHL ECX, 16
        ADD ECX, EDX
        MOV EDX, WindowColor
        MOV ESI, CaptionColor
        MOV EDI, BorderColor

        XOR EAX, EAX
        INT $40

        POP EDI
        POP ESI
        POP EBX
end;


end.

