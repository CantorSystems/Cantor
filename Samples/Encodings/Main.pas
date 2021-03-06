unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, CoreWrappers, CoreStrings;

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
    FACP, FOEMCP: TCodePage;
    function FileChosen(Button: TButton): Boolean;
    procedure FileSaved(Size: Int64);
  end;

var
  MainForm: TMainForm;

implementation

uses
  CoreUtils;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  Constraints.MinWidth := btLoad.Left + btUTF16LE.Left + btUTF16LE.Width;
  Constraints.MinHeight := Panel.Height * 2 + StatusBar.Height;
  StatusBar.Panels[0].Text := SysUtils.Format('ACP = %d', [GetACP]);
  StatusBar.Panels[1].Text := SysUtils.Format('OEMCP = %d', [GetOEMCP]);
  FACP.Create(CP_ACP);
  FOEMCP.Create(CP_OEMCP);
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
    Result := SaveDialog.Execute;
  end
  else
    Result := False;
end;

procedure TMainForm.FileSaved(Size: Int64);
begin
  Application.MessageBox(Pointer(SysUtils.Format('%u bytes written.', [Size])),
    Pointer(Caption), MB_ICONINFORMATION);
end;

procedure TMainForm.LoadFile(Sender: TObject);
var
  FileName: UnicodeString;
  W: TWideString;
begin
  if OpenDialog.Execute then
  begin
    W.Create;
    try
      FileName := OpenDialog.FileName;
      StatusBar.Panels[2].Text := SysUtils.Format('%u bytes',
        [CoreWrappers.LoadFile(W.Load, Pointer(FileName)).BytesRead]);
      DefaultSystemCodePage := Byte(cbOEM.Checked);
      if soBigEndian in W.Options then
        W.SwapByteOrder;
      Memo.Text := W.AsRawByteString;
    finally
      W.Destroy;
    end;
  end;
end;

procedure TMainForm.SaveLegacyChar(Sender: TObject);
var
  FileName: UnicodeString;
  FileCP: TCodePage;
  S: TLegacyString;
begin
  if FileChosen(TButton(Sender)) then
  begin
    S.Create;
    try
      S.CodePage := @FACP;
      S.AsRawByteString := Memo.Text;
      FileCP.Create(TButton(Sender).Tag);
      S.CodePage := @FileCP;
      FileName := SaveDialog.FileName;
      FileSaved(SaveFile(S.Save, Pointer(FileName), S.Count).BytesWritten);
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
      FileSaved(SaveFile(W.Save, Pointer(FileName), W.Count * SizeOf(WideChar)).BytesWritten);
    finally
      W.Destroy;
    end;
  end;
end;

procedure TMainForm.cbOEMClick(Sender: TObject);
var
  S: TLegacyString;
begin
  if FEncodingOEM then
    Exit;

  FEncodingOEM := True;
  try
    S.Create;
    try
      try
        if cbOEM.Checked then
          S.CodePage := @FOEMCP
        else
          S.CodePage := @FACP;
        S.AsRawByteString := Memo.Text;
        S.CodePage := @FACP;
        Memo.Text := S.AsRawByteString;
      except
        cbOEM.Checked := not cbOEM.Checked;
        raise;
      end;
    finally
      S.Destroy;
    end;
  finally
    FEncodingOEM := False;
  end;
end;

initialization
  DefFontData.Name := 'Tahoma';

end.
