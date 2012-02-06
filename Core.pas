(*
  Unified Environment by Freeman

  Legacy core implementation:
    - objects
    - interfaces
    - strings
    - exceptions

  Copyright (c) 2007 Freeman

  Custom defines (turned off by default):

    MANAGED    - Use managed object behavior (like .NET)
*)

unit Core;

interface

uses
{$IFDEF MSWINDOWS}
  Windows
{$ELSE}
  // TODO: POSIX
{$ENDIF}
  ;

const
  ECoreError      = 500;
  EMemoryLeak     = ECoreError + 0;
  EIOError        = ECoreError + 1;
  EDuplicateKey   = ECoreError + 2;

  ovtInstanceSize = -8;   // offset of object instance size  // TODO: FPC

type
  MultiAnsiChar     = AnsiChar;
  MultiAnsiString   = AnsiString;
  PMultiAnsiChar    = PAnsiChar;
  PPMultiAnsiChar   = PPAnsiChar;
  PMultiAnsiString  = PAnsiString;

  UnicodeChar       = WideChar;
  UnicodeString     = WideString;
  PUnicodeChar      = PWideChar;
  PPUnicodeChar     = PPWideChar;
  PUnicodeString    = PWideString;

{$IFNDEF MSWINDOWS} // TODO: POSIX
  CoreChar    = UnicodeChar;
  CoreString  = UnicodeString;
  PCoreChar   = PUnicodeChar;
  PPCoreChar  = PPUnicodeChar;
  PCoreString = PUnicodeString;
{$ENDIF}
{$IFDEF UNICODE}
  CoreChar    = UnicodeChar;
  CoreString  = UnicodeString;
  PCoreChar   = PUnicodeChar;
  PPCoreChar  = PPUnicodeChar;
  PCoreString = PUnicodeString;
{$ELSE}
  CoreChar    = Char;
  CoreString  = MultiAnsiString;
  PCoreChar   = PMultiAnsiChar;
  PPCoreChar  = PPMultiAnsiChar;
  PCoreString = PMultiAnsiString;
{$ENDIF}

  IObject = object
  public
    constructor Init;
    destructor Done; virtual; abstract;
    procedure Free;
    class function IsAncestorOf(Obj: Pointer): Boolean;
    function InstanceSize: Integer;
    function VMT: PPointerArray;
  end;

  PObject = ^TObject;

{$IFDEF MANAGED}

  IInterface = object(IObject)
  private
    FInstance: PObject;
  public
    constructor Init(ObjInstance: PObject);
    destructor Done; virtual;
  // properties
    property Instance: PObject read FInstance;
  end;

  TObject = object(IObject)
  private
    FRefCount: Cardinal;
  public
    function Ref: PObject;
    procedure Release;
  // properties
    property RefCount: Cardinal read FRefCount;
  end;

{$ELSE}

  IInterface = object
  end;

  TObject = IObject;

{$ENDIF}

{ Core services }

procedure CoreError(Code: Cardinal);
procedure FreeAndNil(var P: Pointer); overload;
{$IFDEF MANAGED}
procedure FreeAndNil(var P: PObject; SupressMemoryLeaks: Boolean = True); overload;
{$ELSE}
procedure FreeAndNil(var P: PObject); overload;
{$ENDIF}

procedure Exchange(var P1, P2: Pointer); overload;
{$IFNDEF MULTITHREAD}
procedure Exchange(var P1, P2: Int64); overload;
{$ENDIF}

{ Core string support }

{$IFDEF MSWINDOWS}
const
  CP_ACP                      = Windows.CP_ACP;
  CP_OEMCP                    = Windows.CP_OEMCP;
  CP_MACCP                    = Windows.CP_MACCP;
  CP_UTF7                     = Windows.CP_UTF7;
  CP_UTF8                     = Windows.CP_UTF8;
{$ELSE}
  // TODO: POSIX
{$ENDIF}
  CP_UTF16_BE                 = 1201;
  CP_UTF16_LE                 = 1200;  // ISO 10646 

  DefaultCP   = CP_ACP;   // platform-specific
  DefaultBE   = False;    // hardware-specific
{$IFNDEF MSWINDOWS}
  DefaultUTF8 = True; // TODO: POSIX
{$ENDIF}
{$IFDEF UNICODE}
  DefaultUTF8 = True;     // otherwise UTF-16
{$ELSE}
  DefaultUTF8 = False;    // otherwise SBCS code page
{$ENDIF}

{$IFDEF MSWINDOWS}
function IsPlatformUnicode: Boolean;
{$ENDIF}

function DecodeString(const S: MultiAnsiString;
  CodePage: Cardinal = DefaultCP): CoreString; overload;
function DecodeString(const S: UnicodeString;
  BigEndian: Boolean = DefaultBE): CoreString; overload;
function EncodeString(const S: CoreString;
  CodePage: Cardinal = DefaultCP): MultiAnsiString; overload;
function EncodeString(const S: CoreString;
  BigEndian: Boolean = DefaultBE): UnicodeString; overload;

function IsTextUTF7(const Source: MultiAnsiString): Boolean; // TODO
function IsTextUTF8(const Source: MultiAnsiString): Boolean;

implementation

uses
  Unicode;

{ Core services }

procedure CoreError(Code: Cardinal);
begin
  RunError(Code);
end;

procedure FreeAndNil(var P: Pointer);
{$IFDEF MULTITHREAD}
var
  T: Pointer;
begin
  T := P;
  P := nil;
  FreeMem(T);
end;
{$ELSE}
begin
  FreeMem(P);
  P := nil;
end;
{$ENDIF}

{$IFDEF MANAGED}

procedure FreeAndNil(var P: PObject; SupressMemoryLeaks: Boolean); 
{$IFDEF MULTITHREAD}
var
  T: PObject;
begin
  T := P;
  P := nil;
  if (T.RefCount <> 0) and SupressMemoryLeaks then
    CoreError(EMemoryLeak);
  T.Release;
end;
{$ELSE}
begin
  if (P.RefCount <> 0) and SupressMemoryLeaks then
    CoreError(EMemoryLeak);
  P.Release;
  P := nil;
end;
{$ENDIF}

{$ELSE}

procedure FreeAndNil(var P: PObject);
{$IFDEF MULTITHREAD}
var
  T: PObject;
begin
  T := P;
  P := nil;
  T.Free;
end;
{$ELSE}
begin
  P.Free;
  P := nil;
end;
{$ENDIF}

{$ENDIF}

procedure Exchange(var P1, P2: Pointer); overload;
{$IFDEF MULTITHREAD}
{$IFDEF MSWINDOWS}
begin
  InterlockedExchange(LongInt(P1), LongInt(P2));
end;
{$ELSE}
  // TODO: POSIX
{$ENDIF}
{$ELSE}
asm
        MOV   ECX, [EAX]
        XCHG  ECX, [EDX]
        MOV   [EAX], ECX
end;
{$ENDIF}

{$IFNDEF MULTITHREAD}
procedure Exchange(var P1, P2: Int64); overload;
asm
        MOV   ECX, [EAX]
        XCHG  ECX, [EDX]
        MOV   [EAX], ECX
        MOV   ECX, [EAX+4]
        XCHG  ECX, [EDX+4]
        MOV   [EAX+4], ECX
end;
{$ENDIF}

{ TObject }

constructor IObject.Init;
begin
end;

procedure IObject.Free;
var
  P: PObject;
begin
  if @Self <> nil then
  begin
    P := @Self;
    Dispose(P, Done);
  end;
end;

function IObject.InstanceSize: Integer;
asm
       MOV    EAX, [EAX]
       MOV    EAX, [EAX].ovtInstanceSize
end;

class function IObject.IsAncestorOf(Obj: Pointer): Boolean;
asm
        MOV   ECX, [EAX]
        MOV   EAX, EDX
        JMP   @@loop1
@@loop:
        MOV   EAX, [EAX]
@@loop1:
        TEST  EAX, EAX
        JE    @@exit
        CMP   EAX, ECX
        JNE   @@loop
@@success:
        MOV   AL, 1
@@exit:
end;

function IObject.VMT: PPointerArray;
asm
        MOV   EAX, [EAX]
end;

{$IFDEF MANAGED}

{ IInterface }

constructor IInterface.Init(ObjInstance: PObject);
begin
  FInstance := ObjInstance.Ref;
end;

destructor IInterface.Done;
begin
  FInstance.Release;
end;

{ TObject }

function TObject.Ref: PObject;
begin
  if @Self <> nil then
    Inc(FRefCount);
  Result := @Self;
end;

procedure TObject.Release;
begin
  if @Self <> nil then
  begin
    Dec(FRefCount);
    if RefCount = 0 then
      Free;
  end;
end;

{$ENDIF}

{ Core string support }

{$IFDEF MSWINDOWS}
function IsPlatformUnicode: Boolean;
var
  Info: TOSVersionInfo;
begin
  Info.dwOSVersionInfoSize := SizeOf(Info);
  Result := GetVersionEx(Info) and (Info.dwPlatformId = VER_PLATFORM_WIN32_NT);
end;
{$ENDIF}

function DecodeString(const S: MultiAnsiString; CodePage: Cardinal): CoreString;
{$IFDEF MSWINDOWS}
var
  W: UnicodeString;
{$ENDIF}
begin
{$IFNDEF UNICODE}
  if (CodePage = CP_ACP) or (CodePage = GetACP) then
    Result := S
  else if (CodePage = CP_OEMCP) or (CodePage = GetOEMCP) then
  begin
    SetLength(Result, Length(S));
    OemToCharBuff(Pointer(S), Pointer(Result), Length(Result));
  end
  else
{$ENDIF}
{$IFDEF MSWINDOWS}
  begin
    SetLength(W, MultiByteToWideChar(CodePage, 0, Pointer(S), Length(S), nil, 0));
    MultiByteToWideChar(CodePage, 0, Pointer(S), Length(S), Pointer(W), Length(W));
    Result := W;
  end;
{$ELSE}
  Result := S;  // TODO: POSIX
{$ENDIF}
end;

function DecodeString(const S: UnicodeString; BigEndian: Boolean): CoreString;
var
  W: UnicodeString;
begin
  W := S;
  if BigEndian then
  begin
    UniqueString(W);
    StrSwapByteOrder(Pointer(W));
  end;
  Result := W;
end;

function EncodeString(const S: CoreString; CodePage: Cardinal): MultiAnsiString;
{$IFDEF MSWINDOWS}
var
  W: UnicodeString;
{$ENDIF}
begin
{$IFNDEF UNICODE}
  if (CodePage = CP_ACP) or (CodePage = GetACP) then
    Result := S
  else if (CodePage = CP_OEMCP) or (CodePage = GetOEMCP) then
  begin
    SetLength(Result, Length(S));
    CharToOemBuff(Pointer(S), Pointer(Result), Length(Result));
  end
  else
{$ENDIF}
{$IFDEF MSWINDOWS}
  begin
    W := S;
    SetLength(Result, WideCharToMultiByte(CodePage, 0, Pointer(W), Length(W),
      nil, 0, nil, nil));
    WideCharToMultiByte(CodePage, 0, Pointer(W), Length(W), Pointer(Result),
      Length(Result), nil, nil);
  end;
{$ELSE}
  Result := S; // TODO: POSIX
{$ENDIF}
end;

function EncodeString(const S: CoreString; BigEndian: Boolean): UnicodeString;
begin
  Result := S;
  if BigEndian then
  begin
    UniqueString(Result);
    StrSwapByteOrder(Pointer(Result));
  end;
end;

function IsTextUTF7(const Source: MultiAnsiString): Boolean;
begin
  Result := False; // TODO
end;

function IsTextUTF8(const Source: MultiAnsiString): Boolean;
var
  Chr: Byte;
  I, Octets: Integer;
  AllAscii: Boolean;
begin
  if Source <> '' then
  begin
    Octets := 0;
    AllAscii := True;
    for I := 1 to Length(Source) do
    begin
      Chr := Byte(Source[I]);
      if Chr and $80 <> 0 then
        AllAscii := False;
      if Octets = 0 then
      begin
        if Chr >= $80 then
        begin
          repeat
            Chr := Chr shl 1;
            Inc(Octets);
          until Chr and $80 = 0;
          Dec(Octets);
          if Octets = 0 then
          begin
            Result := False;
            Exit;
          end;
        end;
      end
      else
      begin
        if Chr and $C0 <> $80 then
        begin
          Result := False;
          Exit;
        end;
        Dec(Octets);
      end;
    end;
    Result := (Octets <= 0) and not AllAscii;
  end
  else
    Result := False;
end;

end.

