(*
    Pythia -- Delphi to FASM preprocessor for Kolibri OS

    Core and command line interface implementation

    Copyright © 2013 Vladislav Javadov (Freeman)
*)

unit PythiaCore;

interface

uses
  CoreUtils, CoreWrappers, CoreClasses, CoreStrings, CoreExceptions;

type
  TInputFile = (ifInt, ifInto, ifDef, ifMap, ifSource);
  TInputFileNames = array[TInputFile] of PCoreChar;

  TApplication = class
  private
    FConsole: TStreamConsole;
    FAppName: PCoreChar;
    FFileNames: TInputFileNames;
    FPause: Boolean;
    procedure Warning(Msg: PLegacyChar); overload;
    procedure Warning(Msg: PLegacyChar; const Args: array of const); overload;
  public
    constructor Create(CommandLine: PCoreChar);
    destructor Destroy; override;
    procedure Run;
  end;

  TFuncNames = class;

  TFuncName = class(TRedBlackTreeItem)
  private
  { hold } FOwner: TFuncNames;
  { hold } FLeft, FRight, FParent: TFuncName;
  { hold } FRed: Boolean;
    FValue: TLegacyString;
  public
    constructor Create(Value: PLegacyChar; Count: Integer);
    destructor Destroy; override;
    function Compare(Item: TBalancedTreeItem): Integer; override;
    procedure Save(Dest: TWritableStream; Delimiter: LegacyChar = #10);
    function TextLength: Integer;

    property Left: TFuncName read FLeft;
    property Owner: TFuncNames read FOwner;
    property Parent: TFuncName read FParent;
    property Right: TFuncName read FRight;
    property Red: Boolean read FRed;
    property Value: TLegacyString read FValue;
  end;

  TFuncNames = class(TRedBlackTree)
  private
  { hold } FRoot: TFuncName;
  public
    function Exists(Value: PLegacyChar; Count: Integer): Boolean;
    function Load(FileName: PCoreChar): Integer;
    procedure Save(Dest: TWritableStream; Delimiter: LegacyChar = #10); overload;
    procedure Save(FileName: PCoreChar; Delimiter: LegacyChar = #10); overload;
    function TextLength: Integer;

    property Root: TFuncName read FRoot;
  end;

  TEngine = class
  private
    FInt, FSourceText: TLegacyString;
    FFuncNames: TFuncNames;
    FSource: TLegacyStrings;
  public
    constructor Create(const FileNames: TInputFileNames);
    destructor Destroy; override;
    function Execute(Into: TLegacyStrings; App: TApplication;
      var FuncCount: Integer): Integer;
  end;

  ECore = class(Exception);

implementation

uses
  Windows, PythiaConsts;

const
  CSTR_EQUAL = 2;

type
  TLineBuf = array[0..80] of LegacyChar; // including #0

  TToken = record
    Token: PLegacyChar;
    Count: Integer;
    Next: PLegacyChar;
  end;

{ TApplication }

constructor TApplication.Create(CommandLine: PCoreChar);

procedure AssignFileName(InputFile: TInputFile; const Source: TWideParamRec);
begin
  with Source do
  begin
    Param[Length] := CoreChar(0); // unsafe
    FFileNames[InputFile] := Param;
  end;
end;

function SameKey(const P: TWideParamRec; Key: PWideChar): Boolean;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Key, WideStrLen(Key),
    P.Param + 1, P.Length - 1) = CSTR_EQUAL;
end;

const
  Keys: array[ifInto..ifSource] of PCoreChar = (sInto, sDef, sMap, sSrc);
var
  Dot: PCoreChar;
  P: TWideParamRec;
  I: TInputFile;
begin
  FConsole := TStreamConsole.Create;
  with FConsole do
  begin
    CodePage := GetACP;
    WriteLn;
    WriteLn(sCopyright, [sTitle, sVersion], 2);
  end;

  with WideParamStr(CommandLine) do
  begin
    FAppName := WideStrRScan(Param, PathDelimiter, Length);
    if FAppName <> nil then
      Inc(FAppName)
    else
      FAppName := Param;
    Dot := WideStrScan(FAppName, '.', Length - (PLegacyChar(FAppName) - PLegacyChar(Param)) div SizeOf(CoreChar));
    if Dot <> nil then
      Dot^ := CoreChar(0) // unsafe
    else
      Param[Length] := CoreChar(0); // unsafe
    CommandLine := NextParam;
  end;

  repeat
    P := WideParamStr(CommandLine);
    if P.Length = 0 then
      Break;
    if (not P.Quoted) and ((P.Param^ = CoreChar('/')) or (P.Param^ = CoreChar('-'))) then
    begin
      for I := Low(Keys) to High(Keys) do
      begin
        if SameKey(P, Keys[I]) then
        begin
          P := WideParamStr(P.NextParam);
          if P.Length <> 0 then
          begin
            AssignFileName(I, P);    
            P.Param := nil;
          end;
          Break;
        end;
      end;
      if (P.Param <> nil) and SameKey(P, sPause) then
      begin
        FPause := True;
        P.Param := nil;
      end;
    end;
    if P.Param <> nil then
      AssignFileName(ifInt, P);
    CommandLine := P.NextParam;
  until False;
end;

destructor TApplication.Destroy;
begin
  if FPause then // placed here to show exceptions properly
    FConsole.ReadLn(sPressEnterToExit);
  FConsole.Free;
end;

procedure TApplication.Run;
var
  Engine: TEngine;
  Into: TLegacyStrings;
  LineCount, FuncCount: Integer;
  DefFile: PCoreChar;
begin
  if (FFileNames[ifInt] = nil) or (FFileNames[ifInto] = nil) then
  begin
    FConsole.WriteLn(sUsage, [FAppName]);
    Exit;
  end;

  Engine := TEngine.Create(FFileNames);
  try
    Into := TLegacyStrings.Create(Engine.FInt.Count div 32, -4);
    try
      FuncCount := 0;
      LineCount := Engine.Execute(Into, Self, FuncCount);
      with FConsole do
      begin
        WriteLn(sFuncCount, [FuncCount]);
        WriteLn(sLineCount, [LineCount]);
      end;
      if LineCount <> 0 then
      begin
        Into.Save(FFileNames[ifInto]);
        if (FuncCount <> 0) and (FFileNames[ifMap] <> nil) then
        begin
          DefFile := FFileNames[ifDef];
          if DefFile <> nil then
            Engine.FFuncNames.Save(DefFile);
        end;
      end
      else
        FConsole.WriteLn(sNothingToSave);
    finally
      Into.Free;
    end;
  finally
    Engine.Free;
  end;
end;

procedure TApplication.Warning(Msg: PLegacyChar);
begin
  FConsole.WriteLn(sWarning, [Msg]);
end;

procedure TApplication.Warning(Msg: PLegacyChar; const Args: array of const);
var
  Buf: TLineBuf;
begin
  FormatBuf(Msg, Args, Buf);
  Warning(Buf);
end;

{ TFuncName }

constructor TFuncName.Create(Value: PLegacyChar; Count: Integer);
begin
  FValue := TLegacyString.Create;
  FValue.Insert(Value, Count);
end;

destructor TFuncName.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TFuncName.Compare(Item: TBalancedTreeItem): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    TFuncName(Item).Value.Data, TFuncName(Item).Value.Count, Value.Data, Value.Count) - 2;
end;

function TFuncName.TextLength: Integer;
begin
  if FValue <> nil then
    Result := FValue.Count
  else
    Result := 0;

  if FLeft <> nil then
    Inc(Result, FLeft.TextLength);

  if FRight <> nil then
    Inc(Result, FRight.TextLength);
end;

procedure TFuncName.Save(Dest: TWritableStream; Delimiter: LegacyChar);
begin
  if FLeft <> nil then
    FLeft.Save(Dest, Delimiter);

  if FValue <> nil then
    with Dest do
    begin
      with FValue do
        WriteBuffer(Data^, Count);
      WriteBuffer(Delimiter, SizeOf(Delimiter));
    end;

  if FRight <> nil then
    FRight.Save(Dest, Delimiter);
end;

{ TFuncNames }

function TFuncNames.Exists(Value: PLegacyChar; Count: Integer): Boolean;
var
  FuncName: TFuncName;
  Cmp: Integer;
begin
  FuncName := FRoot;
  while FuncName <> nil do
  begin
    Cmp := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Value, Count,
      FuncName.Value.Data, FuncName.Value.Count) - 2;
    if Cmp < 0 then
      FuncName := FuncName.Left
    else if Cmp > 0 then
      FuncName := FuncName.Right
    else
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TFuncNames.Load(FileName: PCoreChar): Integer;

var
  Limit: PLegacyChar;

function NextToken(S: PLegacyChar): TToken;
var
  T: PLegacyChar;
begin
  while S < Limit do
  begin
    while (S < Limit) and not (S^ in ['0'..'9', ':', 'A'..'Z', 'a'..'z', '_', '@']) do
      Inc(S);
    T := S;
    while (T < Limit) and (T^ in ['0'..'9', ':', 'A'..'Z', 'a'..'z', '_', '@']) do
      Inc(T);
    with Result do
    begin
      Token := S;
      Count := T - S;
    end;
    Inc(T);
    while T^ in [#9, #32] do
      Inc(T);
    Result.Next := T;
    Exit;
  end;
  with Result do
  begin
    Token := nil;
    Next := S + 1;
  end;
end;

function InsertToken(const T: TToken): Boolean;
begin
  with T do
    if (Token <> nil) and ((Next^ in [#13, #10]) or (Next >= Limit)) then
    begin
      Insert(TFuncName.Create(Token, Count));
      Result := True;
    end
    else
      Result := False;
end;

var
  P: PLegacyChar;
  T: TToken;
begin
  Result := 0;
  with TLegacyString.Create do
  try
    with TFileStream.Create(FileName, faRead) do
    try
      Count := Size;
      ReadBuffer(Data^, Count);
    finally
      Free;
    end;

    P := Data;
    Limit := Data + Count;

    while P < Limit do
    begin
      T := NextToken(P);
      if InsertToken(T) then
        Inc(Result)
      else
      begin
        T := NextToken(T.Next);
        if InsertToken(T) then
          Inc(Result);
      end;
      P := T.Next;
      while (P < Limit) and not (T.Next^ in [#13, #10]) do
        Inc(P);
    end;
  finally
    Free;
  end;
end;

procedure TFuncNames.Save(Dest: TWritableStream; Delimiter: LegacyChar);
begin
  if FRoot <> nil then
  begin
    with Dest do
      Size := Position + FRoot.TextLength + Count * SizeOf(Delimiter);
    FRoot.Save(Dest, Delimiter);
  end;
end;

procedure TFuncNames.Save(FileName: PCoreChar; Delimiter: LegacyChar);
var
  F: TWritableStream;
begin
  F := TFileStream.Create(FileName, faRewrite);
  try
    Save(F, Delimiter);
  finally
    F.Free;
  end;
end;

function TFuncNames.TextLength: Integer;
begin
  if FRoot <> nil then
    Result := FRoot.TextLength
  else
    Result := 0;
end;

{ TEngine }

constructor TEngine.Create(const FileNames: TInputFileNames);
var
  FileName: PCoreChar;
begin
  FInt := TLegacyString.Create;
  FInt.Load(FileNames[ifInt]);

  FFuncNames := TFuncNames.Create;
  FileName := FileNames[ifMap];
  if FileName <> nil then
    FFuncNames.Load(FileName);

  FSourceText := TLegacyString.Create;
  FileName := FileNames[ifSource];
  if FileName <> nil then
    FSourceText.Load(FileName);

  FSource := TLegacyStrings.Create(4096, -2, True);
  FSource.Load(FSourceText, [soAttachBuffer]);
end;

destructor TEngine.Destroy;
begin
  FSource.Free;
  FSourceText.Free;

  FFuncNames.Free;

  FInt.Free;
//  inherited;
end;

function TEngine.Execute(Into: TLegacyStrings; App: TApplication;
  var FuncCount: Integer): Integer;

var
  Limit: PLegacyChar;

function CustomStrToInt(var S: PLegacyChar): Integer;
begin
  Result := 0;
  while (S < Limit) and (S^ in ['0'..'9']) do
  begin
    Result := Result * 10 + Byte(S^) - Byte('0');
    Inc(S);
  end;
end;

function SameToken(S: PLegacyChar; Count: Integer; const Token: ShortString): Boolean;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S, Count,
    @Token[1], Length(Token)) = CSTR_EQUAL;
end;

function NextToken(S, Limit: PLegacyChar; AllowSlashSlash: Boolean = False): TToken;
var
  T: PLegacyChar;
begin
  while S < Limit do
  begin
    while (S < Limit) and not (S^ in ['A'..'Z', 'a'..'z', '_', '''', '{', '(', '/']) do
      Inc(S);
    case S^ of
      '''':
        begin
          Inc(S);
          while (S < Limit) and not (S^ in ['''', #13, #10]) do // skip 'strings'
            Inc(S);
        end;
      '{':
        while (S < Limit) and (S^ <> '}') do // skip {comments}
          Inc(S);
      '(':
        begin
          Inc(S);
          if S^ = '*' then // skip (*comments*)
          begin
            Inc(S);
            while S < Limit do
            begin
              while (S < Limit) and (S^ <> '*') do
                Inc(S);
              if S[1] <> ')' then
                Inc(S);
            end;
          end;
        end;
      '/':
        begin
          T := S + 1;
          if T^ = '/' then // skip //comments
          begin
            Inc(T);
            if AllowSlashSlash then
            begin
              with Result do
              begin
                Token := S;
                Count := T - S;
                Next := T + 1;
              end;
              Exit;
            end;
            S := T;
            while (S < Limit) and not (S^ in [#13, #10]) do // manual NextLine inline
              Inc(S);
          end
          else
            S := T;
        end;
    else
      T := S;
      while (T < Limit) and (T^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) do
        Inc(T);
      with Result do
      begin
        Token := S;
        Count := T - S;
        Next := T + 1;
      end;
      Exit;
    end;
  end;
  with Result do
  begin
    Token := nil;
    Next := S + 1;
  end;
end;

type
  TKeyword = record
    Index: Integer;
    Next: PLegacyChar;
  end;

function NextKeyword(S: PLegacyChar; const Keywords: array of PShortString;
  AllowSlashSlash: Boolean = False): TKeyword;
var
  I: Integer;
begin
  while S < Limit do
    with NextToken(S, Limit, AllowSlashSlash) do
    begin
      if Token <> nil then
        for I := Low(Keywords) to High(Keywords) do
        begin
          if SameToken(Token, Count, Keywords[I]^) then
          begin
            Result.Index := I;
            Result.Next := Next;
            Exit;
          end;
        end;
      S := Next;
    end;
  with Result do
  begin
    Index := -1;
    Next := S + 1;
  end;
end;

function NextLine(Source: PLegacyChar): PLegacyChar;
begin
  Result := Source;
  while (Result < Limit) and not (Result^ in [#13, #10]) do
    Inc(Result);
  Inc(Result);
  if (Result < Limit) and (Result^ = #10) then
    Inc(Result);
end;

function AppendPasLine(Source: PLegacyChar): PLegacyChar;
var
  Idx: Integer;
  PasLine: PLegacyChar;
  T: TLegacyString;
begin
  Result := Source;
  Idx := CustomStrToInt(Result) - 1;
  if (Idx >= 0) and (Idx < FSource.Count) then
  begin
    PasLine := FSource.Items[Idx].Data;
    PasLine[FSource.Items[Idx].Count] := #0; // unsafe
  end
  else
  begin
    App.Warning(sNoSourceLine, [Idx + 1, FSource.Count]);
    PasLine := sNotFound;
  end;
  T := TLegacyString.Create;
  T.Format(sSourceCodeLine, [Idx + 1, PasLine]);
  Into.Append(T);
  Result := NextLine(Result);
end;

function AppendAsmLines(Source: PLegacyChar; var LineCount: Integer): PLegacyChar;

function MoveComments(Source: PLegacyChar; Count: Integer): TLegacyString;
type
  PComment = ^TComment;
  TComment = record
    Text: PLegacyChar;
    Length: Integer;
  end;
  PComments = ^TComments;
  TComments = array[0..MaxInt div SizeOf(TComment) - 1] of TComment;
var
  P, R, Limit, Rslt: PLegacyChar;
  Comments: PComments;
  L, C, I: Integer;
begin
  P := StrScan(Source, '{', Count);
  if P <> nil then
  begin
    Result := TLegacyString.Create;
    Result.Count := Count + Count div 4 + 3; // {} by ','
    GetMem(Comments, (Count div 2) * SizeOf(TComment));
    try
      C := 0;
      Rslt := Result.Data;
      Limit := Source + Count;
      while P < Limit  do
      begin
        L := P - Source;
        Move(Source^, Rslt^, L);
        Source := P + 1;
        Dec(Count, L + 1);
        Inc(Rslt, L);
        R := StrScan(Source, '}', Count);
        if R <> nil then
        begin
          with Comments[C] do
          begin
            Text := P;
            Length := R - Text + 1;
          end;
          Inc(C);
          Inc(R);
          Dec(Count, R - Source);
          Source := R;
        end
        else
          App.Warning(sUnclosedComment);
        P := StrScan(Source, '{', Count);
        if P = nil then
        begin
          Move(Source^, Rslt^, Count);
          Inc(Rslt, Count);
          Break;
        end;
      end;
      if C <> 0 then
      begin
        PLongWord(Rslt)^ := $203B09; // fast core: #9'; '
        Inc(Rslt, 3);
        for I := 0 to C - 1 do
          with Comments[I] do
          begin
            Move(Text^, Rslt^, Length);
            Inc(Rslt, Length);
            if I < C - 1 then
            begin
              Rslt^ := ',';
              Inc(Rslt);
            end;
          end;
      end;
      Result.Count := Rslt - Result.Data;
    finally
      FreeMem(Comments);
    end;
  end
  else
    Result := nil;
end;

procedure StripTokens(Line: TLegacyString; const Tokens: array of PShortString);
var
  I, NewCount: Integer;
  P, Limit: PLegacyChar;
begin
  P := Line.Data;
  NewCount := Line.Count;
  Limit := P + NewCount;
  while P < Limit do
  begin
    with NextToken(P, Limit) do
    begin
      if Token <> nil then
        for I := Low(Tokens) to High(Tokens) do
          if SameToken(Token, Count, Tokens[I]^) then
          begin
            P := Token + Count;
            if P^ in [#9, #32, ','] then
            begin
              Inc(P);
              Inc(Count);
            end;
            Move(P^, Token^, Limit - P);
            Dec(NewCount, Count);
            Break;
          end;
      P := Next;
    end;
  end;
  Line.Count := NewCount;
end;

procedure FixJumpSyntax(Line: TLegacyString);
var
  Limit, P, Z: PLegacyChar;
  T: TToken;
begin
  with Line do
    Limit := Data + Count;
  T := NextToken(Line.Data, Limit);
  if (T.Token <> nil) and (T.Token^ in ['J', 'j']) then
  begin
    P := T.Next;
    while (P < Limit) and not (P^ in ['+', '-']) do
      Inc(P);
    Z := P + 1;
    while (Z < Limit) and (Z^ in ['0'..'9']) do
      Inc(Z);
    Inc(Z);
    Move(P^, P[1], Z - P);
    P^ := '$';
    with Line do
      Count := Z - Data;
  end;
end;

var
  Z, AsmLine: PLegacyChar;
  T: TLegacyString;
  L: Integer;
begin
  Result := Source;
  while Source < Limit do
  begin
    while (Source < Limit) and not (Source^ in [#13, #10]) do
      Inc(Source);
    AsmLine := StrRScan(Result, '|', Source - Result);
    if AsmLine <> nil then
    begin
      Inc(AsmLine); 
      AsmLine^ := #9; // unsafe

      Z := Source;
      while Z^ in [#9, #32] do
        Dec(Z);

      L := Z - AsmLine;
      T := MoveComments(AsmLine, L);
      if T = nil then
      begin
        T := TLegacyString.Create;
        T.Insert(AsmLine, L, [soAttachBuffer]);
      end;
      StripTokens(T, [@sPtr, @sNear]);
      FixJumpSyntax(T);
      Into.Append(T);
      Inc(LineCount);

      Source := NextLine(Source);
      Result := Source;
    end
    else
      Break;
  end;
end;

function ParseBody(Source: PLegacyChar; var Ident: TToken; var LineCount: Integer): PLegacyChar;

procedure Replace(Where: TLegacyString; What: LegacyChar);
var
  P: PLegacyChar; 
begin
  repeat
    P := StrScan(Where.Data, What, Where.Count);
    if P <> nil then
      P^ := '_'
    else
      Break;
  until False;
end;

var
  LineNo: TToken;
  T: TLegacyString;
  Keyword: TKeyword;
begin
  Result := Source;
  while Source < Limit do
  begin
    LineNo := NextToken(Source, Limit);
    if SameToken(LineNo.Token, LineNo.Count, sLine) then
    begin
      with LineNo do
        Source := Token + Count;
      while (Source < Limit) and (Source^ in [#9, #32]) do
        Inc(Source);
      if Source^ = '#' then
      begin
        Inc(Source);
        if Ident.Token <> nil then
        begin
          with Ident do
            Token[Count] := #0; // unsafe
          T := TLegacyString.Create;
          T.Format('%s:', [Ident.Token]); // asm label
          Replace(T, '.');
          //Replace(T, '@');
          Into.Append(nil);
          Into.Append(T);
          Inc(FuncCount);
          Ident.Token := nil;
        end;
        Result := AppendPasLine(Source);
        Inc(LineCount);
      end;
    end
    else
      Result := NextLine(LineNo.Next);

    Result := AppendAsmLines(Result, LineCount);

    Keyword := NextKeyword(Result, [@sSlashSlash, @sEnd], True);
    if Keyword.Index = 0 then
      Source := Keyword.Next
    else
      Exit;
  end;
end;

function ParseFunc(S: PLegacyChar; var LineCount: Integer): PLegacyChar;
var
  Keyword: TKeyword;
  Ident: TToken;
begin
  Ident := NextToken(S, Limit);
  if Ident.Token <> nil then
  begin
    if (FFuncNames.Count = 0) or FFuncNames.Exists(Ident.Token, Ident.Count) then
    begin
      Result := Ident.Next;
      while Result < Limit do
      begin
        Keyword := NextKeyword(Result, [@sSlashSlash, @sFunction, @sProcedure, @sEnd], True);
        case Keyword.Index of
          0:
            Result := ParseBody(Keyword.Next, Ident, LineCount);
          1..2:
            Result := ParseFunc(Keyword.Next, LineCount);
        else
          Result := Keyword.Next;
          Exit;
        end;
      end;
    end;
  end
  else
    App.Warning(sIdentExpected);

  Result := Ident.Next;
end;

procedure ParseImpl(S: PLegacyChar);
var
  Func: TKeyword;
begin
  while S < Limit do
  begin
    Func := NextKeyword(S, [@sFunction, @sProcedure, @sConstructor, @sDestructor, @sEnd]);
    if Func.Index in [0..3] { function..destructor } then
      S := ParseFunc(Func.Next, Result)
    else
      S := Func.Next;
  end;
end;

var
  T: TLegacyString;
begin
  Result := 0;
  if FInt.Data <> nil then
  begin
    with FInt do
      Limit := Data + Count;
    with NextKeyword(FInt.Data, [@sImplementation]) do
    begin
      if Index <> 0 then
        raise ECore.Create(sNoImplementation);
      ParseImpl(Next);
    end;
  end;

  Inc(Result, FuncCount * 2);
  if Result <> 0 then
  begin
    T := TLegacyString.Create;
    T.Format(sDoNotEdit, [sTitle]);
    Into.Insert(0, T);
    Inc(Result, 2);
  end;
end;

end.

