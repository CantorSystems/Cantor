(*
    Lite Core Library (CoreLite mini)

    CoreLite XML parser

    Copyright (c) 2015 Vladislav Javadov (aka Freeman)

    Conditional defines:
      * Debug -- diagnostic exceptions
*)

unit CoreXML;

interface

uses
  CoreUtils, CoreExceptions, CoreWrappers, CoreClasses, CoreStrings;

type
  PXMLName = ^TXMLName;
  TXMLName = object(TLegacyString)
  private
    FPrefix, FLocalName: TLegacyString;
  public
    property LocalName: TLegacyString read FLocalName;
    property Prefix: TLegacyString read FPrefix;
  end;

  PXMLEscapedString = ^TXMLEscapedString;
  TXMLEscapedString = object(TLegacyString)
  public
//    procedure AsXML;
  end;

  PXMLObject = ^TXMLObject;
  TXMLObject = object
  private
    FName: TXMLName;
    FText: TXMLEscapedString;
    FXML: TLegacyString;
  public
    property LocalName: TLegacyString read FName.LocalName;
    property Name: TXMLName read FName;
    property Prefix: TLegacyString read FName.Prefix;
    property Text: TXMLEscapedString read FText;
    property XML: TLegacyString read FXML;
  end;

  PXMLAttribute = ^TXMLAttribute;
  TXMLAttribute = object(TXMLObject)
  end;

  PXMLAttributeArray = ^TXMLAttributeArray;
  TXMLAttributeArray = array[0..MaxInt div SizeOf(TXMLAttribute) - 1] of TXMLAttribute;

  PXMLAttributes = ^TXMLAttributes;
  TXMLAttributes = object(TCollection)
  private
  { hold } FItems: PXMLAttributeArray;
  public
    property Items: PXMLAttributeArray read FItems;
  end;

  PXMLNodeArray = ^TXMLNodeArray;

  TXMLNodes = object(TCollection)
  private
  { hold } FItems: PXMLNodeArray;
  public
    property Items: PXMLNodeArray read FItems;
  end;

  TXMLNodeType = (ntText, ntCData, ntInstruction, ntComment);

  PXMLNode = ^TXMLNode;
  TXMLNode = object(TXMLObject)
  private
    FAttributes: TXMLAttributes;
    FChildNodes: TXMLNodes;
    FNodeType: TXMLNodeType;
  public
    property Attributes: TXMLAttributes read FAttributes;
    property ChildNodes: TXMLNodes read FChildNodes;
    property NodeType: TXMLNodeType read FNodeType;
  end;

  TXMLNodeArray = array[0..MaxInt div SizeOf(TXMLNode) - 1] of TXMLNode;

{ Delphi XML compatibility }

type
  TNodeType = TXMLNodeType;

const
  ntProcessingInst = ntInstruction;

implementation

uses
  CoreConsts;

end.

