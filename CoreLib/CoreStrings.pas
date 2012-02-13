(*
    The Unified Environment Core Library

    Core string and character set implementation 

    Copyright (c) 2012 The Unified Environment Laboratory
*)

unit CoreStrings;

interface

uses
  Windows, CoreUtils;

type
  TIgnoreOptions = set of (ioCase, ioComposition, ioDiacritics, ioHanzi, ioKana,
    ioNonSpace, ioPunctuation, ioTurkic, ioWidth);

type
  TNormalForm = (NFC, NFD, NFKC, NFKD);

{ Legacy Windows service }

type
  TCPInfoEx = packed record
    MaxCharSize: LongWord;
    DefaultChar: array[0..MAX_DEFAULTCHAR - 1] of LegacyChar;
    LeadByte: array[0..MAX_LEADBYTES - 1] of Byte;
    UnicodeDefaultChar: WideChar;
    CodePage: LongWord;
    CodePageName: array[0..MAX_PATH - 1] of CoreChar;
  end;

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;

implementation

{ Legacy Windows service }

function GetCPInfoEx(CodePage, Flags: LongWord; var CPInfoEx: TCPInfoEx): BOOL; stdcall;
  external kernel32 name 'GetCPInfoExW';

end.

