// interface

type
  PLatinStrInfo = ^TLatinStrInfo;
  TLatinStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, Latin1Count, DiacriticCount: Cardinal);
  end;

  PSingleByteStrInfo = ^TSingleByteStrInfo;
  TSingleByteStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, NonLatinCount, DiacriticCount: Cardinal);
  end;

  PDoubleByteStrInfo = ^TDoubleByteStrInfo;
  TDoubleByteStrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, DoubleByteCount, DiacriticCount, NonWidthCount: Cardinal);
  end;

  PUTF8StrInfo = ^TUTF8StrInfo;
  TUTF8StrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, SurrogateCount, DiacriticCount, NonWidthCount,
          NonUnicodeCount, UnicodeCount, ZeroCount: Cardinal;
          Compliance: TUTF8Compliance);
  end;

  PUTF16StrInfo = ^TUTF16StrInfo;
  TUTF16StrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, SurrogateCount, DiacriticCount, NonWidthCount: Cardinal);
  end;

  PUTF32StrInfo = ^TUTF32StrInfo;
  TUTF32StrInfo = record
    Count: Cardinal;
  {$IFNDEF Lite}
    Blocks: TCharBlocks;
  {$ENDIF}
    case Byte of
      0: (InvalidChar: QuadChar);
      1: (InvalidCount, SurrogateCount, DiacriticCount, NonWidthCount, NonUnicodeCount: Cardinal);
  end;


{ Core services }

  TUnicodeResult = (crSuccess, crFail, crSurrogates, crNonUnicode);
  TLegacyResult = crSuccess..crFail;

function TryLatinFromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeLatin = []; SourceOptions: TLatinSource = []): Boolean; overload;
function TryLatinFromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeLatin = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryLatinFromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeLatin = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryLatinFromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeLatin = []; SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF32FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TLatinSource = []): Boolean; overload;
function TryUTF32FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF32FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF32FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF16FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TLatinSource = []): Boolean; overload;
function TryUTF16FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF16FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF16FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeEndian = []; SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF8FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TLatinSource = []): Boolean; overload;
function TryUTF8FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF8FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF8FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TLegacySource = []): Boolean; overload;


function LatinFromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeLatin = [];
  SourceOptions: TLatinSource = []): Cardinal; overload;
function LatinFromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function LatinFromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function LatinFromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeLatin = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF32FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF32FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF32FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF32FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF16FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF16FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF16FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF16FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF8FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLatinSource = []): Cardinal; overload;
function UTF8FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF8FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF8FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;


function TryLatinFromUTF32(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryLatinFromUTF16(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryLatinFromUTF8(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF32FromLatin(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Boolean; overload;
function TryUTF32FromUTF16(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF32FromUTF8(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF16FromLatin(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Boolean; overload;
function TryUTF16FromUTF32(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF16FromUTF8(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Boolean; overload;

function TryUTF8FromLatin(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLegacySource = []): Boolean; overload;
function TryUTF8FromUTF32(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Boolean; overload;
function TryUTF8FromUTF16(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Boolean; overload;


function TryLatinFromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): TLatinStrInfo; overload;
function TryLatinFromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): TLatinStrInfo; overload;
function TryLatinFromUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TLegacySource = []): TLatinStrInfo; overload;

function TryUTF32FromLatin(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): TUTF32StrInfo; overload;
function TryUTF32FromUTF16(Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): TUTF32StrInfo; overload;
function TryUTF32FromUTF8(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): TUTF32StrInfo; overload;

function TryUTF16FromLatin(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): TUTF16StrInfo; overload;
function TryUTF16FromUTF32(Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): TUTF16StrInfo; overload;
function TryUTF16FromUTF8(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): TUTF16StrInfo; overload;

function TryUTF8FromLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLegacySource = []): TUTF8StrInfo; overload;
function TryUTF8FromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): TUTF8StrInfo; overload;
function TryUTF8FromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): TUTF8StrInfo; overload;


function LatinFromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function LatinFromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function LatinFromUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF32FromLatin(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF32FromUTF16(Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF32FromUTF8(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF16FromLatin(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF16FromUTF32(Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF16FromUTF8(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;

function UTF8FromLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TLegacySource = []): Cardinal; overload;
function UTF8FromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;
function UTF8FromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8 = [];
  SourceOptions: TEndianSource = []): Cardinal; overload;


{function TryLatinFromLatin(Dest, Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeLatin = []; SourceOptions: TLatinSource = []): TStringInfo; overload;
function TryLatinFromUTF32(Dest: PLegacyChar; Source: PQuadChar; SourceCount: Cardinal;
  DestOptions: TEncodeLatin = []; SourceOptions: TEndianSource = []): TStringInfo; overload;
function TryLatinFromUTF16(Dest: PLegacyChar; Source: PWideChar; SourceCount: Cardinal;
  DestOptions: TEncodeLatin = []; SourceOptions: TEndianSource = []): TStringInfo; overload;
function TryLatinFromUTF8(Dest, Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeLatin = []; SourceOptions: TLegacySource = []): TStringInfo; overload;

function TryUTF32FromLatin(Dest: PQuadChar; Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian = []; SourceOptions: TLatinSource = []): TStringInfo; overload;
function TryUTF32FromUTF32(Dest, Source: PQuadChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian = [];  SourceOptions: TEndianSource = []): TStringInfo; overload;
function TryUTF32FromUTF16(Dest: PQuadChar; Source: PWideChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): TStringInfo; overload;
function TryUTF32FromUTF8(Dest: PQuadChar; Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian = [];  SourceOptions: TLegacySource = []): TStringInfo; overload;

function TryUTF16FromLatin(Dest: PWideChar; Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian = []; SourceOptions: TLatinSource = []): TStringInfo; overload;
function TryUTF16FromUTF32(Dest: PWideChar; Source: PQuadChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): TStringInfo; overload;
function TryUTF16FromUTF16(Dest, Source: PWideChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian = []; SourceOptions: TEndianSource = []): TStringInfo; overload;
function TryUTF16FromUTF8(Dest: PWideChar; Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian = []; SourceOptions: TLegacySource = []): TStringInfo; overload;

function TryUTF8FromLatin(Dest, Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TLatinSource = []): TStringInfo; overload;
function TryUTF8FromUTF32(Dest: PLegacyChar; Source: PQuadChar; SourceCount: Cardinal;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TEndianSource = []): TStringInfo; overload;
function TryUTF8FromUTF16(Dest: PLegacyChar; Source: PWideChar; SourceCount: Cardinal;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TEndianSource = []): TStringInfo; overload;
function TryUTF8FromUTF8(Dest, Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeUTF8 = []; SourceOptions: TLegacySource = []): TStringInfo; overload;}


// implementation

{ Core services }

function TryLatinFromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeLatin; SourceOptions: TLatinSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeLatin; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeLatin; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeLatin; SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF32FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TLatinSource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF16FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TLatinSource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeEndian; SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF8FromLatin(var Dest: Cardinal; const Source: TLatinStrInfo;
  DestOptions: TEncodeUTF8; SourceOptions: TLatinSource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF32(var Dest: Cardinal; const Source: TUTF32StrInfo;
  DestOptions: TEncodeUTF8; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF16(var Dest: Cardinal; const Source: TUTF16StrInfo;
  DestOptions: TEncodeUTF8; SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF8(var Dest: Cardinal; const Source: TUTF8StrInfo;
  DestOptions: TEncodeUTF8; SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function LatinFromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeLatin;
  SourceOptions: TLatinSource): Cardinal;
begin
  if not TryLatinFromLatin(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function LatinFromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryLatinFromUTF32(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function LatinFromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryLatinFromUTF16(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function LatinFromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeLatin;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryLatinFromUTF8(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF32FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF32FromLatin(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF32FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF32FromUTF32(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF32FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF32FromUTF16(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF32FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF32FromUTF8(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF16FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF16FromLatin(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF16FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF16FromUTF32(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF16FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF16FromUTF16(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF16FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF16FromUTF8(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF8FromLatin(const Source: TLatinStrInfo; DestOptions: TEncodeUTF8;
  SourceOptions: TLatinSource): Cardinal;
begin
  if not TryUTF8FromLatin(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF8FromUTF32(const Source: TUTF32StrInfo; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF8FromUTF32(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF8FromUTF16(const Source: TUTF16StrInfo; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Cardinal;
begin
  if not TryUTF8FromUTF16(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function UTF8FromUTF8(const Source: TUTF8StrInfo; DestOptions: TEncodeUTF8;
  SourceOptions: TLegacySource): Cardinal;
begin
  if not TryUTF8FromUTF8(Result, Source, DestOptions, SourceOptions) then
    raise EString.Create // TODO
end;

function TryLatinFromUTF32(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF16(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF8(var Info: TLatinStrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF32FromLatin(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF16(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF32FromUTF8(var Info: TUTF32StrInfo; Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF16FromLatin(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF32(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF16FromUTF8(var Info: TUTF16StrInfo; Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF8FromLatin(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TLegacySource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF32(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryUTF8FromUTF16(var Info: TUTF8StrInfo; Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Boolean;
begin
  // TODO
end;

function TryLatinFromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): TLatinStrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF32(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryLatinFromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): TLatinStrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF16(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryLatinFromUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TLegacySource): TLatinStrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF8(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromLatin(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): TUTF32StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromLatin(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromUTF16(Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): TUTF32StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromUTF16(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromUTF8(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): TUTF32StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromUTF8(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromLatin(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): TUTF16StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromLatin(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromUTF32(Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): TUTF16StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromUTF32(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromUTF8(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): TUTF16StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromUTF8(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TLegacySource): TUTF8StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromLatin(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): TUTF8StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromUTF32(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): TUTF8StrInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromUTF16(Result, Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
end;

function LatinFromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TLatinStrInfo;
begin
  Info := TryLatinFromUTF32(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function LatinFromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TLatinStrInfo;
begin
  Info := TryLatinFromUTF16(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function LatinFromUTF8(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeLatin;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TLatinStrInfo;
begin
  Info := TryLatinFromUTF8(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF32FromLatin(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  Info := TryUTF32FromLatin(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF32FromUTF16(Dest: PQuadChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  Info := TryUTF32FromUTF16(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF32FromUTF8(Dest: PQuadChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF32StrInfo;
begin
  Info := TryUTF32FromUTF8(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF16FromLatin(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  Info := TryUTF16FromLatin(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF16FromUTF32(Dest: PWideChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  Info := TryUTF16FromUTF32(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF16FromUTF8(Dest: PWideChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeEndian;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF16StrInfo;
begin
  Info := TryUTF16FromUTF8(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF8FromLatin(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PLegacyChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TLegacySource): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  Info := TryUTF8FromLatin(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF8FromUTF32(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PQuadChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  Info := TryUTF8FromUTF32(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;

function UTF8FromUTF16(Dest: PLegacyChar; DestCount: Cardinal;
  Source: PWideChar; SourceCount: Cardinal; DestOptions: TEncodeUTF8;
  SourceOptions: TEndianSource): Cardinal;
var
  Info: TUTF8StrInfo;
begin
  Info := TryUTF8FromUTF16(Dest, DestCount, Source, SourceCount, DestOptions, SourceOptions);
  if (Info.InvalidChar <> 0) and not (coForceInvalid in DestOptions) then
    raise EString.Create; // TODO
  Result := Info.Count;
end;


{function TryLatinFromLatin(Dest, Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeLatin; SourceOptions: TLatinSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromLatin(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryLatinFromUTF32(Dest: PLegacyChar; Source: PQuadChar; SourceCount: Cardinal;
  DestOptions: TEncodeLatin; SourceOptions: TEndianSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF32(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryLatinFromUTF16(Dest: PLegacyChar; Source: PWideChar; SourceCount: Cardinal;
  DestOptions: TEncodeLatin; SourceOptions: TEndianSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF16(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryLatinFromUTF8(Dest, Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeLatin; SourceOptions: TLegacySource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryLatinFromUTF8(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromLatin(Dest: PQuadChar; Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian; SourceOptions: TLatinSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromLatin(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromUTF32(Dest, Source: PQuadChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromUTF32(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromUTF16(Dest: PQuadChar; Source: PWideChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromUTF16(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF32FromUTF8(Dest: PQuadChar; Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian; SourceOptions: TLegacySource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF32FromUTF8(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromLatin(Dest: PWideChar; Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian; SourceOptions: TLatinSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromLatin(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromUTF32(Dest: PWideChar; Source: PQuadChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromUTF32(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromUTF16(Dest, Source: PWideChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian; SourceOptions: TEndianSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromUTF16(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF16FromUTF8(Dest: PWideChar; Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeEndian; SourceOptions: TLegacySource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF16FromUTF8(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromLatin(Dest, Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeUTF8; SourceOptions: TLatinSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromLatin(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromUTF32(Dest: PLegacyChar; Source: PQuadChar; SourceCount: Cardinal;
  DestOptions: TEncodeUTF8; SourceOptions: TEndianSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromUTF32(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromUTF16(Dest: PLegacyChar; Source: PWideChar; SourceCount: Cardinal;
  DestOptions: TEncodeUTF8; SourceOptions: TEndianSource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromUTF16(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;

function TryUTF8FromUTF8(Dest, Source: PLegacyChar; SourceCount: Cardinal;
  DestOptions: TEncodeUTF8; SourceOptions: TLegacySource): TStringInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  TryUTF8FromUTF8(Result, Dest, Source, SourceCount, DestOptions, SourceOptions);
end;}