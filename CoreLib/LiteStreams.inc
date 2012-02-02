(*
    The Unified Environment Core Library

    Lite streams declaration -- no virtual methods, only THandleStream

    Copyright (c) 2012 The Unified Environment Laboratory
*)

type
  THandleStream = class
  private
    FHandle: THandle;
  protected
    function GetPosition: Int64;
    function GetSize: Int64;
    procedure SetPosition(Value: Int64);
    procedure SetSize(Value: Int64);
  public
    constructor Create(FileName: PWideChar; Access, Creation: LongWord;
      Share: LongWord = FILE_SHARE_READ; Attributes: LongWord = FILE_ATTRIBUTE_NORMAL); 
    destructor Destroy; override;
    function Lock(Offset, Count: Int64): Boolean;
    function Open(FileName: PWideChar; Access, Creation: LongWord;
      Share: LongWord = FILE_SHARE_READ; Attributes: LongWord = FILE_ATTRIBUTE_NORMAL): Boolean;
    function Read(var Data; Count: LongWord): Cardinal;
    procedure ReadBuffer(var Data; Count: LongWord);
    function Seek(Offset: Int64; Origin: LongWord): Int64;
    function Unlock(Offset, Count: Int64): Boolean;
    function Write(const Data; Count: LongWord): LongWord; 
    procedure WriteBuffer(const Data; Count: LongWord);
  // properties
    property Handle: THandle read FHandle;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;
  end;

  TReadableStream = THandleStream;
  TWriteableStream = THandleStream;
  
