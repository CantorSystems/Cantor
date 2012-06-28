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
    function GetPosition: QuadWord;
    function GetSize: QuadWord;
    procedure SetPosition(Value: QuadWord);
    procedure SetSize(Value: QuadWord);
  public
    constructor Create(FileName: PWideChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]); overload;
    destructor Destroy; override;

    function Open(FileName: PWideChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]): Boolean; overload;

    function Seek(Offset: QuadWord; Origin: LongWord): QuadWord;
    function Read(var Data; Count: LongWord): Cardinal;
    procedure ReadBuffer(var Data; Count: LongWord);
    function Write(const Data; Count: LongWord): LongWord;
    procedure WriteBuffer(const Data; Count: LongWord);

    function Lock(Offset, Count: QuadWord): Boolean;
    function Unlock(Offset, Count: QuadWord): Boolean;
  // properties
    property Handle: THandle read FHandle;
    property Position: QuadWord read GetPosition write SetPosition;
    property Size: QuadWord read GetSize write SetSize;
  end;

  TReadableStream = THandleStream;
  TWritableStream = THandleStream;

