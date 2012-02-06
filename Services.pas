unit Services;

interface

procedure LineColFromOffset(Source, Pos: PChar; var Line, Col: Integer);

function FromOem(const Source: string): string;
function ToOem(const Source: string): string;

function DisplayName(const Name: string): string;

implementation

uses
  Windows, SysUtils, Model;

procedure LineColFromOffset(Source, Pos: PChar; var Line, Col: Integer);
var
  LineStart: PChar;
begin
  Line := 1;
  LineStart := Source;
  while (Source^ <> #0) and (Source < Pos) do
    if Source^ in [#13, #10] then
    begin
      Inc(Line);
      if Source^ = #13 then
      begin
        Inc(Source);
        if Source^ = #10 then
          Inc(Source);
      end
      else
        Inc(Source);
      LineStart := Source;
    end
    else
      Inc(Source);
  Col := Pos - LineStart + 1;
end;

function ToOem(const Source: string): string;
begin
  SetLength(Result, Length(Source));
  CharToOem(Pointer(Source), Pointer(Result));
end;

function FromOem(const Source: string): string;
begin
  SetLength(Result, Length(Source));
  OemToChar(Pointer(Source), Pointer(Result));
end;

//class function TPersistent.ReadSize(S: TStream): Cardinal;
//var
//  B: Byte;
//  P: PByte;
//  Size: Cardinal;
//begin
//  Size := 0;
//  P := @Size;
//  repeat
//    S.Read(B, SizeOf(B));
//    if B and $80 <> 0 then
//    begin
//      P^ := B;
//      Inc(P);
//    end
//    else
//      Break;
//  until False;
//  Result := B;
//  while P <> @Size do
//  begin
//    Dec(P);
//    Result := (Result shl 7) or (P^ and $7F);
//  end;
//end;

//class procedure TPersistent.WriteSize(S: TStream; Size: Cardinal);
//var
//  B: Byte;
//begin
//  repeat
//    if Size > $7F then
//    begin
//      B := (Byte(Size) and $7F) or $80;
//      S.Write(B, SizeOf(B));
//      Size := Size shr 7;
//    end
//    else
//    begin
//      S.Write(Size, 1);
//      Break;
//    end;
//  until False;
//end;

function DisplayName(const Name: string): string;
begin
  if Pos(' ', Name) <> 0 then
    Result := QuotedStr(Name)
  else
    Result := Name;
end;

end.

