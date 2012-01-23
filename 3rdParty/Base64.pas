(*
  CoreLib base64 legacy unit
  Based on code by Daniel Wischnewski, gate(n)etwork GmbH

  Copyright (c) 2009 The Unified Environment Laboratory
*)

unit Base64;

interface

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// !! THE COMPILER SWITCH MAY BE USED TO ADJUST THE BEHAVIOR !!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

// enable "SpeedDecode"
//     the switch to gain speed while decoding the message, however, the codec
//     will raise different exceptions/access violations or invalid output if
//     the incoming data is invalid or missized.

// disable "SpeedDecode"
//     the switch to enable a data check, that will scan the data to decode to
//     be valid. This method is to be used if you cannot guarantee to validity
//     of the data to be decoded.

{.$DEFINE SpeedDecode}

// codiert einen String in die zugehörige Base64-Darstellung
function Base64Encode(const Source: AnsiString): AnsiString; overload;
// decodiert die Base64-Darstellung eines Strings in den zugehörigen String
function Base64Decode(const Source: AnsiString): AnsiString; overload;
{$IFNDEF SpeedDecode}
function Base64Decode(const Source: AnsiString; var Dest: AnsiString): Boolean; overload;
{$ENDIF}

// bestimmt die Größe der Base64-Darstellung
function Base64EncodeSize(Count: Integer): Integer;
// bestimmt die Größe der binären Darstellung
function Base64DecodeSize(const Source; Count: Integer): Integer;

// codiert einen Buffer in die zugehörige Base64-Darstellung
procedure Base64Encode(const Source; Count: Integer; var Dest); overload;
// decodiert die Base64-Darstellung in einen Buffer
{$IFDEF SpeedDecode}
procedure Base64Decode(const Source; Count: Integer; var Dest); overload;
{$ELSE}
function Base64Decode(const Source; Count: Integer; var Dest): Boolean; overload;
{$ENDIF}

implementation

uses
  SysUtils;

{$I Base64.inc}

end.
