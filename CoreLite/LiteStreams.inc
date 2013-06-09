(*
    Lite Core Library (CoreLite)

    Lite streams declaration -- no virtual methods, only THandleStream

    Copyright © 2012-2013 Vladislav Javadov (Freeman)
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

    function Seek(Offset: QuadWord; Origin: TSeekOrigin): QuadWord;
    function Read(var Data; Count: LongWord): LongWord;
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

