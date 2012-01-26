unit Registry;

interface

uses
  Windows, Core;

const
  HKEY_CLASSES_ROOT     = Windows.HKEY_CLASSES_ROOT;
  HKEY_CURRENT_USER     = Windows.HKEY_CURRENT_USER;
  HKEY_LOCAL_MACHINE    = Windows.HKEY_LOCAL_MACHINE;
  HKEY_USERS            = Windows.HKEY_USERS;
  HKEY_PERFORMANCE_DATA = Windows.HKEY_PERFORMANCE_DATA;
  HKEY_CURRENT_CONFIG   = Windows.HKEY_CURRENT_CONFIG;
  HKEY_DYN_DATA         = Windows.HKEY_DYN_DATA;

  REG_NONE                        = Windows.REG_NONE;
  REG_SZ                          = Windows.REG_SZ;
  REG_EXPAND_SZ                   = Windows.REG_EXPAND_SZ;
  REG_BINARY                      = Windows.REG_BINARY;
  REG_DWORD                       = Windows.REG_DWORD;
  REG_DWORD_LITTLE_ENDIAN         = Windows.REG_DWORD_LITTLE_ENDIAN;
  REG_DWORD_BIG_ENDIAN            = Windows.REG_DWORD_BIG_ENDIAN;
  REG_LINK                        = Windows.REG_LINK;
  REG_MULTI_SZ                    = Windows.REG_MULTI_SZ;
  REG_RESOURCE_LIST               = Windows.REG_RESOURCE_LIST;
  REG_FULL_RESOURCE_DESCRIPTOR    = Windows.REG_FULL_RESOURCE_DESCRIPTOR;
  REG_RESOURCE_REQUIREMENTS_LIST  = Windows.REG_RESOURCE_REQUIREMENTS_LIST;

type
  TRegistry = class(TObject) // platform, non-core class
  private
    FKey: THandle;
  public
    constructor Create(Key: THandle = HKEY_CURRENT_USER);
    function QueryValue(Ident: PCoreChar; DataType: Cardinal; var Data;
      DataSize: Cardinal): Boolean; overload;
    function QueryValue(Ident: PCoreChar; var Data;
      DataSize: Cardinal): Boolean; overload;
    function QueryValueInfo(Ident: PCoreChar; var DataType,
      DataSize: Cardinal): Boolean;
    function SetValue(Ident: PCoreChar; DataType: Cardinal; const Data;
      DataSize: Cardinal): Boolean;
  // properties
    property Key: THandle read FKey;
  end;

implementation

uses
  Exceptions;

{ TRegistry }

constructor TRegistry.Create(Key: THandle);
begin
  FKey := Key;
end;

function TRegistry.QueryValue(Ident: PCoreChar; DataType: Cardinal; var Data;
  DataSize: Cardinal): Boolean;
var
  ActualType: Cardinal;
begin
  Result := QueryValueInfo(Ident, ActualType, DataSize) and (ActualType = DataType) and
    ({$IFDEF UNICODE} RegQueryValueExW {$ELSE} RegQueryValueExA {$ENDIF}
    (FKey, Ident, nil, nil, @Data, @DataSize) = ERROR_SUCCESS);
end;

function TRegistry.QueryValue(Ident: PCoreChar; var Data;
  DataSize: Cardinal): Boolean;
begin
  Result := {$IFDEF UNICODE} RegQueryValueExW {$ELSE} RegQueryValueExA {$ENDIF}
    (FKey, Ident, nil, nil, @Data, @DataSize) = ERROR_SUCCESS;
end;

function TRegistry.QueryValueInfo(Ident: PCoreChar; var DataType,
  DataSize: Cardinal): Boolean;
begin
  Result := {$IFDEF UNICODE} RegQueryValueExW {$ELSE} RegQueryValueExA {$ENDIF}
    (FKey, Ident, nil, @DataType, nil, @DataSize) = ERROR_SUCCESS;
end;

function TRegistry.SetValue(Ident: PCoreChar; DataType: Cardinal;
  const Data; DataSize: Cardinal): Boolean;
begin
  Result := {$IFDEF UNICODE} RegSetValueExW {$ELSE} RegSetValueExA {$ENDIF}
    (FKey, Ident, 0, DataType, @Data, DataSize) = ERROR_SUCCESS;
end;

end.
