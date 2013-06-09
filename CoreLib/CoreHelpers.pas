(*
    The Unified Environment Core Library

    Object helpers using core wrappers, core classes and core strings

    Copyright © 2012 The Unified Environment Laboratory
*)

unit CoreHelpers;

interface

uses
  CoreUtils, CoreWrappers, CoreClasses, CoreStrings;

type  
  PSharedSubstringArray = ^TSharedStringArray;
  TSharedStringArray = array[0..MaxInt div SizeOf(TSharedString) - 1] of TSharedString;

  TCommandLineParams = class(TInnerObjects) // TODO: Strings?
  private
    FItems: PSharedSubstringArray;  { hold }
  public
    constructor Create(Source: PCoreChar);
    destructor Destroy; override;
  // properties
    property Items: PSharedSubstringArray read FItems;
  end;

implementation

uses
  Windows;

{ TCommandLine }

constructor TCommandLineParams.Create(Source: PCoreChar);
begin

end;

destructor TCommandLineParams.Destroy;
begin

  inherited;
end;

end.

