unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, CoreWrappers;

type
  TMainForm = class(TForm)
    Panel: TPanel;
    Memo: TMemo;
    StatusBar: TStatusBar;
    btLoad: TButton;
    btANSI: TButton;
    btOEM: TButton;
    btUTF7: TButton;
    btUTF16LE: TButton;
    btUTF8: TButton;
    btUTF16BE: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    cbOEM: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure LoadFile(Sender: TObject);
    procedure SaveLegacyChar(Sender: TObject);
    procedure SaveUTF16(Sender: TObject);
    procedure cbOEMClick(Sender: TObject);
  private
    FEncodingOEM: Boolean;
    procedure BytesRead(Source: PWritableStream);
    procedure BytesWritten(Dest: PWritableStream);
    function FileChosen(Button: TButton): Boolean;
  end;

var
  MainForm: TMainForm;

implementation

uses
  CoreUtils, CoreStrings;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  Constraints.MinWidth := btLoad.Left + btUTF16LE.Left + btUTF16LE.Width;
  Constraints.MinHeight := Panel.Height * 2 + StatusBar.Height;
  StatusBar.Panels[0].Text := SysUtils.Format('ACP = %d', [GetACP]);
  StatusBar.Panels[1].Text := SysUtils.Format('OEMCP = %d', [GetOEMCP]);
end;

procedure TMainForm.BytesRead(Source: PWritableStream);
begin
  StatusBar.Panels[2].Text := SysUtils.Format('%u bytes', [Source.Size])
end;

procedure TMainForm.BytesWritten(Dest: PWritableStream);
begin
  Application.MessageBox(Pointer(SysUtils.Format('%u bytes written.', [Dest.Size])),
    Pointer(Caption), MB_ICONINFORMATION);
end;

function TMainForm.FileChosen(Button: TButton): Boolean;
var
  Ident: string;
begin
  if Memo.Text <> '' then
  begin
    Ident := Button.Caption;
    Delete(Ident, Length(Ident) - 3 + 1, 3);
    SaveDialog.Title := Ident;
    SaveDialog.FileName := ChangeFileExt(OpenDialog.FileName, '') + '-' +
      Copy(Ident, 6, MaxInt) + SaveDialog.DefaultExt;
    Result := SaveDialog.Execute
  end
  else
    Result := False;
end;

procedure TMainForm.LoadFile(Sender: TObject);
var
  FileName: UnicodeString;
  CP: TCodePage;
  S: TLegacyString;
begin
  if OpenDialog.Execute then
  begin
    S.Create;
    try
      CP.Create(Byte(cbOEM.Checked)); // 0 = CP_ACP, 1 = CP_OEMCP
      S.CodePage := @CP;
      FileName := OpenDialog.FileName;
      CoreWrappers.LoadFile(S.Load, Pointer(FileName), faSequentialRead, nil, BytesRead);
      Memo.Text := S.AsRawByteString;
    finally
      S.Destroy;
    end;
  end;
end;

procedure TMainForm.SaveLegacyChar(Sender: TObject);
var
  FileName: UnicodeString;
  CP: TCodePage;
  S: TLegacyString;
begin
  if FileChosen(TButton(Sender)) then
  begin
    S.Create;
    try
      S.AsRawByteString := Memo.Text;
      CP.Create(TButton(Sender).Tag);
      S.CodePage := @CP;
      FileName := SaveDialog.FileName;
      SaveFile(S.Save, Pointer(FileName), S.Count, faSequentialRewrite, nil, BytesWritten);
    finally
      S.Destroy;
    end;
  end;
end;

procedure TMainForm.SaveUTF16(Sender: TObject);
var
  FileName: UnicodeString;
  W: TWideString;
begin
  if FileChosen(TButton(Sender)) then
  begin
    W.Create;
    try
      W.AsRawByteString := Memo.Text;
      if Boolean(TButton(Sender).Tag) then
        W.SwapByteOrder;
      FileName := SaveDialog.FileName;
      SaveFile(W.Save, Pointer(FileName), W.Count * SizeOf(WideChar),
        faSequentialRewrite, nil, BytesWritten);
    finally
      W.Destroy;
    end;
  end;
end;

procedure TMainForm.cbOEMClick(Sender: TObject);
var
  S: TLegacyString;
begin
  if not FEncodingOEM then
  begin
    S.Create;
    try
      S.AsRawByteString := Memo.Text;
      DefaultUnicodeCodePage := Byte(cbOEM.Checked); // 0 = CP_ACP, 1 = CP_OEMCP
      FEncodingOEM := True;
      try
        try
          Memo.Text := S.AsRawByteString;
        except
          cbOEM.Checked := not cbOEM.Checked;
          DefaultUnicodeCodePage := Byte(cbOEM.Checked);
          raise;
        end;
      finally
        FEncodingOEM := False;
      end;
    finally
      S.Destroy;
    end;
  end;
end;

initialization
  DefFontData.Name := 'Tahoma';

end.
