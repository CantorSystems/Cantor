unit TestLib;

interface

uses
  CoreUtils, CoreStrings;

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  var SourceIndex: Integer; Dest: PWideChar; DestOptions: TEncodeUTF16 = []): Integer;
function IsUTF8(Source: PLegacyChar; Count: Integer; DestOptions: TEncodeUTF16 = [];
  Threshold: Integer = 1): Boolean;

implementation

function FromUTF8(var Info: TStringInfo; Source: PLegacyChar; Count: Integer;
  var SourceIndex: Integer; Dest: PWideChar; DestOptions: TEncodeUTF16): Integer;

function GetChar: QuadChar; // Fast core: closure
var
  FirstByte, Bytes, B, C: Byte;
begin
  FirstByte := Byte(Source[SourceIndex]);
  Inc(SourceIndex);

  if FirstByte and $80 <> 0 then
  begin
    if FirstByte and $40 <> 0 then
    begin
      if FirstByte and $20 = 0 then
      begin
        Result := FirstByte and $1F;
        Bytes := 1;
      end
      else if FirstByte and $10 = 0 then
      begin
        Result := FirstByte and $0F;
        Bytes := 2;
      end
      else if FirstByte and $08 = 0 then
      begin
        Result := FirstByte and $07;
        Bytes := 3;
      end
      else
      begin
        Result := 0;
        Bytes := 0;
      end;

      B := Bytes;

      if Result <> 0 then
        while (B <> 0) and (SourceIndex < Count) do
        begin
          C := Byte(Source[SourceIndex]);
          if C and $C0 = $80 then
          begin
            Result := (Result shl 6) or (C and $3F);
            Inc(SourceIndex);
            Dec(B);
          end
          else
            Break; // broken sequence
        end;

      if (B <> 0) or (Result = 0) then // broken sequence or unexpected end of string
        if (SourceIndex = Count) and (coContinuous in DestOptions) then
        begin
          Dec(SourceIndex, Bytes - B + 1);
          Result := InvalidUTFMask;
        end
        else
          Result := (Bytes + 1) or (Byte(Bytes - B + 2) shl 8) or InvalidUTFMask; // Fast core

      Exit;
    end;

    Result := FirstByte or InvalidUTFMask; // Fast core
  end
  else
    Result := QuadChar(FirstByte);
end;

var
  Q, T: QuadChar;
  W: Word;
{$IFNDEF Lite}
  Block: TCharBlock;
{$ENDIF}
begin
  Result := 0;
  T := 0;
{$IFNDEF Lite}
  Block := cbNonUnicode;
{$ENDIF}

  while SourceIndex < Count do
  begin
    if T <> 0 then
      Q := T
    else
      Q := GetChar;

    if Q and InvalidUTFMask = 0 then
      case Q of
        Low(THighSurrogates)..High(THighSurrogates):
          if coSurrogates in DestOptions then
          begin
            T := GetChar;
            if T and InvalidUTFMask = 0 then
              case T of
                Low(TLowSurrogates)..High(TLowSurrogates): // CESU-8
                  begin
                    if coBigEndian in DestOptions then // Fast core
                      PLongWord(Dest)^ := Swap(Q) or (Swap(T) shl 16)
                    else
                      PLongWord(Dest)^ := Q or (T shl 16);

                    Inc(Dest, 2);
                    Inc(Result, 2);

                    with Info do
                    begin
                      Inc(CharCount);
                      Inc(SequenceCount);
                      Inc(SurrogatePairCount);
                    {$IFNDEF Lite}
                      if coRangeBlocks in DestOptions then
                      begin
                        Block := FindCharBlock(
                          (Q - Low(THighSurrogates)) shl 10 + Low(TUnicodeSMP) +
                          T - Low(TLowSurrogates), Block);
                        Include(Blocks, Block);
                      end;
                    {$ENDIF}
                    end;

                    T := 0; // surrogate pair flushed
                    Continue;
                  end;
              else
                Q := Q or (Byte(ivUTF8) shl 16) or InvalidUTFMask; // Fast core
              end
            else if T and not InvalidUTFMask = 0 then // coContinuous
              Break;
          end;

        Low(TLowSurrogates)..High(TLowSurrogates):
          Q := Q or (Byte(ivUTF8) shl 16) or InvalidUTFMask; // Fast core

        Low(TUnicodeSMP)..High(TUnicodePUA):
          if coSurrogates in DestOptions then
          begin
            W := Q - Low(TUnicodeSMP);

            if coBigEndian in DestOptions then // Fast core
              PLongWord(Dest)^ :=
                Word(Swap(Low(THighSurrogates) + W shr 10)) or
                Word(Swap((Low(TLowSurrogates) + W and $3FF) shl 16))
            else
              PLongWord(Dest)^ :=
                Word(Low(THighSurrogates) + W shr 10) or
                Word((Low(TLowSurrogates) + W and $3FF) shl 16);

            Inc(Dest, 2);
            Inc(Result, 2);
            with Info do
            begin
              Inc(CharCount);
              Inc(SequenceCount);
              Inc(SurrogatePairCount);
            end;
          {$IFNDEF Lite}
            if coRangeBlocks in DestOptions then
            begin
              Block := FindCharBlock(Q, Block);
              Include(Info.Blocks, Block);
            end;
          {$ENDIF}

            Continue;
          end;
      else                                  
      {$IFNDEF Lite}
        if coRangeBlocks in DestOptions then
        begin
          Block := FindCharBlock(Q, Block);
          Include(Info.Blocks, Block);
        end;
      {$ENDIF}
      end
    else if Q and not InvalidUTFMask = 0 then // coContinuous
      Break;

    if Q > High(TUnicodeBMP) then // both InvalidUTFMask and higher Unicode
      if coForceInvalid in DestOptions then
      begin
        Q := QuadChar(Unknown_UTF16);
        Inc(Info.InvalidCount);
      {$IFNDEF Lite}
        if coRangeBlocks in DestOptions then
          Include(Info.Blocks, cbSpecials); // Fast core
      {$ENDIF}
      end
      else
      begin
        with Info do
        begin
          InvalidChar := Q;
          Inc(Count, Result);
        end;

        Result := 0;
        Exit;
      end;

    if coBigEndian in DestOptions then
      Dest^ := WideChar(Swap(Q))
    else
      Dest^ := WideChar(Q);

    Inc(Dest);
    Inc(Result);
    Inc(Info.CharCount);
    if Q > $7F then
      Inc(Info.SequenceCount);
  end;

  Inc(Info.Count, Result);
{  if not (coContinuous in DestOptions) then
    Info.CodePage := nil;}
end;

function IsUTF8(Source: PLegacyChar; Count: Integer; DestOptions: TEncodeUTF16;
  Threshold: Integer): Boolean;
var
  Info: TStringInfo;
  Buf: array[0..$3FF] of WideChar;
  Cnt, Idx: Integer;
  Opt: TEncodeUTF16;
begin
  FillChar(Info, SizeOf(Info), 0);

  if coSurrogates in DestOptions then
    Cnt := Length(Buf) div 2
  else
    Cnt := Length(Buf);

  Opt := DestOptions + [coContinuous]; 

  repeat
    if Count < Cnt then
    begin
      Cnt := Count;
      Opt := DestOptions;
    end;

    Idx := 0;
    FromUTF8(Info, Source, Cnt, Idx, Buf, DestOptions);
    if (Idx = 0) or (Info.InvalidChar <> 0) then
    begin
      Result := False;
      Exit;
    end;

    Inc(Source, Idx);
    Dec(Count, Idx);
  until Count = 0;

  Result := Info.SequenceCount > Threshold;
end;

end.
