(*
    Lite Core Library (CoreLite)

    Wiki engine client

    Copyright (c) 2013 Vladislav Javadov (Freeman)
*)

unit CoreWiki;

interface

uses
  CoreUtils;

type
  TTextType = (ttNormal, ttNoBreak, ttCode, ttKeyboard, ttSample,
    ttStrong, ttEmphasis, ttInsert, ttDelete, ttSmall, ttMark, ttSuperscript,
    ttSubscript, ttVariable, ttDefinition, ttCite, ttEmotion); // ttQuote, ttSpelling // Ruby?

type
  TParagraphType = (ptNormal, ptCode, ptHeading, ptLeft, ptCenter, ptRight, ptJustify);

  TGroupType = (gtIndent, gtQuote, gtBullet, gtSquare, gtDisc, gtDecimal,
    gtLeadingZero, gtLowerLatin, gtUpperLatin, gtLowerRoman, gtUpperRoman,
    gtTable, gtRow, gtHeadingRow);

{function CharIndex(Source: Char): Integer;
function ExtCharIndex(Source: WideChar): Integer;}

//function HTMLEncoding(CodePage: Word = CP_ACP): CoreString; overload;

implementation

uses
  Windows;

(*function FindCharIndex(Source: WideChar; Chars: PCharCodes; Count: Integer): Integer;
var
  Min, Max: Integer;
begin
  Min := 0;
  Max := Count - 1;
  while Min <= Max do
  begin
    Result := (Max + Min) div 2;
    if Chars[Result].Source <> Source then
      if Chars[Result].Source > Source then
        Max := Result - 1
      else
        Min := Result + 1
    else
      Exit;
  end;
  Result := -1;
end;

function CharIndex(Source: Char): Integer;
begin
  Result := FindCharIndex(WideChar(Source), @Chars, Length(Chars));
end;

function ExtCharIndex(Source: WideChar): Integer;
begin
  Result := FindCharIndex(Source, @ExtChars, Length(ExtChars));
end;*)

(*function HTMLEncoding(CodePage: Word): CoreString;

var
  Key: HKEY;

procedure QueryKey(const Ident: CoreString);
var
  ValType, Len: DWORD;
begin
  if ( {$IFDEF UNICODE} RegQueryValueExW {$ELSE} RegQueryValueExA {$ENDIF}
    (Key, Pointer(Ident), nil, @ValType, nil, @Len) = ERROR_SUCCESS) and
    (ValType = REG_SZ) then
  begin
    SetLength(Result, Len div SizeOf(CoreChar) - 1);
    {$IFDEF UNICODE} RegQueryValueExW {$ELSE} RegQueryValueExA {$ENDIF}
      (Key, Pointer(Ident), nil, @ValType, Pointer(Result), @Len);
  end;
end;

var
  Branch: CoreString;
begin
  case CodePage of
    CP_ACP:
      CodePage := GetACP;
    CP_OEMCP:
      CodePage := GetOEMCP;
  end;
  Branch := 'MIME\Database\Codepage\' + IntToStr(CodePage);
  if {$IFDEF UNICODE} RegOpenKeyExW {$ELSE} RegOpenKeyExA {$ENDIF}
    (HKEY_CLASSES_ROOT, Pointer(Branch), 0, KEY_QUERY_VALUE, Key) = ERROR_SUCCESS then
  begin
    QueryKey('WebCharset');
    if Result = '' then
      QueryKey('BodyCharset');
  end
  else
    Result := '';
end;*)


end.
