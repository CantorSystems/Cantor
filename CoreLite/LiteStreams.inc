(*
    Lite Core Library (CoreLite mini)

    Lite streams declaration -- no virtual methods, only THandleStream

    Copyright (c) 2012, 2015 Vladislav Javadov (aka Freeman)
*)

type
  PHandleStream = ^THandleStream;
  THandleStream = object
  private
    FHandle: THandle;
    FDummy: Pointer; // dummy field to padding object to be larger than register size
  protected
    property Dummy: Pointer read FDummy; // publish it to prevent compiler's hint
    function GetPosition: QuadWord;
    function GetSize: QuadWord;
    procedure SetPosition(Value: QuadWord);
    procedure SetSize(Value: QuadWord);
  public
    constructor Create(FileName: PWideChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]); 
    destructor Destroy; 

    function Open(FileName: PWideChar; Access: TFileAccess;
      Attributes: TFileAttributes = [faNormal]): Boolean; overload;

    function Seek(Offset: QuadWord; Origin: TSeekOrigin): QuadWord;

    function Read(var Data; Count: LongWord): LongWord;
    function ReadBOM: TReadableBOM;
    procedure ReadBuffer(var Data; Count: LongWord);

    function Write(const Data; Count: LongWord): LongWord;
    procedure WriteBOM(Value: TWritableBOM);
    procedure WriteBuffer(const Data; Count: LongWord);

    function Lock(Offset, Count: QuadWord): Boolean;
    function Unlock(Offset, Count: QuadWord): Boolean;
  // properties
    property Handle: THandle read FHandle;
    property Position: QuadWord read GetPosition write SetPosition;
    property Size: QuadWord read GetSize write SetSize;
  end;

  PReadableStream = PHandleStream;
  TReadableStream = THandleStream;

  PWritableStream = PHandleStream;
  TWritableStream = THandleStream;

