(*******************************************************************************
* The contents of this file are subject to the Mozilla Public License Version  *
* 1.1 (the "License"); you may not use this file except in compliance with the *
* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *
*                                                                              *
* Software distributed under the License is distributed on an "AS IS" basis,   *
* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *
* the specific language governing rights and limitations under the License.    *
*                                                                              *
* The Original Code is mr_Storage_XML.pas                                      *
*                                                                              *
* The Initial Developer of the Original Code is Miha Remec,                    *
*   http://www.MihaRemec.com/                                                  *
*                                                                              *
* Contributor(s):                                                              *
*   Miha Vrhovnik, Primoz Gabrijelcic, John                                    *
*******************************************************************************)
unit OmniXMLPersistent;

interface

{$I OmniXML.inc}

{$IFDEF OmniXML_HasZeroBasedStrings}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

// if you want to use MS XML parser, uncomment (in your program!!!)
// the following compiler directive {.$DEFINE USE_MSXML}

uses
  Classes, SysUtils,
{$IFDEF VCL} Controls, {$ENDIF}
  TypInfo,
{$IFDEF HAS_UNIT_VARIANTS} Variants, {$ENDIF}
  OEncoding, OmniXML, OmniXML_Types,
{$IFDEF USE_MSXML} OmniXML_MSXML, {$ENDIF}
  System.Rtti,
  System.Generics.Collections,
  Spring.Collections,
  Spring.Collections.Lists,
  PropFormatAttributeUnit,
  ElementNameAttributeUnit,
  OmniXMLUtils;

type
  TPropsFormat = (pfAuto, pfAttributes, pfNodes);
  EOmniXMLPersistent = class(Exception);

{$IFDEF VisualCLX}
  TTime = type TDateTime;
  TDate = type TDateTime;
{$ENDIF}

type TOmnyXMLUtil = class
  private var ctx: TRttiContext;

  public constructor Create();

  public function GetElementPropFormat(Instance: TObject; PropInfo: PPropInfo): TPropsFormat;
  public function GetElementCustomName(Instance: TObject; PropInfo: PPropInfo): string;
  public function GetRootName(Instance: TObject): string;
  public function GetSubTypeItemClassFromList(ObjectList: TObject): TClass;
  public function GetSubInstanceTypeItemClassFromList(objectList: IList<IInterface>): TClass;
end;


type
  TOmniXMLWriter = class
  protected
    Doc: IXMLDocument;
    procedure WriteProperty(Instance: TPersistent; PropInfo: PPropInfo;
      Element: IXMLElement; WriteDefaultValues: Boolean; const PropFormat: TPropsFormat = pfNodes);
    procedure InternalWriteText(Root: IXMLElement; Name, Value: XmlString; const PropFormat: TPropsFormat = pfNodes);
    procedure WriteCollection(Collection: TCollection; Root: IXMLElement);
  public
    constructor Create(Doc: IXMLDocument);
    class procedure SaveToFile(const Instance: TPersistent; const FileName: string; const OutputFormat: TOutputFormat = ofNone);
    class procedure SaveXML(const Instance: TPersistent; var XML: XmlString; const OutputFormat: TOutputFormat = ofNone);
    procedure Write(Instance: TPersistent; Root: IXMLElement;
      const WriteRoot: Boolean = True; const CheckIfEmpty: Boolean = True;
      const WriteDefaultValues: Boolean = False);
  end;

  TOmniXMLReader = class
  protected
    function FindElement(const Root: IXMLElement; const TagName: XmlString): IXMLElement;
    function FindAllElements(const Root: IXMLElement; const TagName: XmlString): TList<IXMLElement>;
    procedure ReadProperty(Instance: TPersistent; PropInfo: Pointer; Element: IXMLElement;
    const PropFormat: TPropsFormat = pfNodes);
    function InternalReadText(Root: IXMLElement; Name: XmlString; var Value: XmlString;
      const PropFormat: TPropsFormat = pfNodes): Boolean;
    procedure ReadCollection(Collection: TCollection; Root: IXMLElement);
  public
    constructor Create();
    class procedure LoadFromFile(Instance: TPersistent; FileName: string); overload;
    class procedure LoadFromFile(Collection: TCollection; FileName: string); overload;
    class procedure LoadXML(Instance: TPersistent; const XML: XmlString); overload;
    procedure Read(Instance: TPersistent; Root: IXMLElement; const ReadRoot: Boolean = False);
  end;

var
  DefaultPropFormat: TPropsFormat = pfNodes;

implementation

{$IFDEF UNICODE}
const
  CP_ACP = 0;  // default to ANSI code page
  CP_RawByteString = $FFFF;  // codepage of RawByteString string type
{$ENDIF}  // UNICODE

const
  COLLECTIONITEM_NODENAME = 'o';  // do not change!
  PROP_FORMAT = 'PropFormat';  // do not change!
  StringS_COUNT_NODENAME = 'Count';  // do not change!
  StringS_PREFIX = 'l';  // do not change!

var
  PropFormatValues: array[TPropsFormat] of string = ('auto', 'attr', 'node');

function IsElementEmpty(Element: IXMLElement): Boolean;
begin
  Result := (Element.Attributes.Length = 0) and (Element.ChildNodes.Length = 0);
end;

procedure CreateDocument(var XMLDoc: IXMLDocument; var Root: IXMLElement; RootNodeName: XmlString);
begin
  XMLDoc := CreateXMLDoc;
  XMLDoc.AppendChild(XMLDoc.CreateProcessingInstruction('xml', 'version="1.0" encoding="utf-8"'));
  if RootNodeName<>'' then
  begin
    Root := XMLDoc.CreateElement(RootNodeName);
    XMLDoc.DocumentElement := Root;
  end
  else
  begin
    Root:= XMLDoc.DocumentElement;
  end;
end;

procedure Load(var XMLDoc: IXMLDocument; var XMLRoot: IXMLElement);
var
  i: TPropsFormat;
  PropFormatValue: XmlString;
begin
  // set root element
  XMLRoot := XMLDoc.documentElement;

  if XMLRoot = nil then
    Exit;
end;

procedure LoadDocument(const FileName: string; var XMLDoc: IXMLDocument; var XMLRoot: IXMLElement);
begin
  XMLDoc := CreateXMLDoc;
  { TODO : implement and test preserveWhiteSpace }
  XMLDoc.preserveWhiteSpace := True;
  XMLDoc.Load(FileName);

  Load(XMLDoc, XMLRoot);
end;

{ TOmniXMLWriter }

class procedure TOmniXMLWriter.SaveToFile(const Instance: TPersistent;
  const FileName: string; const OutputFormat: TOutputFormat = ofNone);
var
  XMLDoc: IXMLDocument;
  Root: IXMLElement;
  Writer: TOmniXMLWriter;
begin
  if Instance is TCollection then
  begin
    CreateDocument(XMLDoc, Root, Instance.ClassName);
  end
  else
  begin
    CreateDocument(XMLDoc, Root, '');
  end;

  Writer := TOmniXMLWriter.Create(XMLDoc);
  try
    if Instance is TCollection then
    begin
      Writer.WriteCollection(TCollection(Instance), Root);
    end
    else
    begin
      Writer.Write(Instance, Root);
    end;
  finally
    Writer.Free;
  end;

{$IFNDEF USE_MSXML}
  XMLDoc.Save(FileName, OutputFormat);
{$ELSE}
  XMLDoc.Save(FileName);
{$ENDIF}
end;

class procedure TOmniXMLWriter.SaveXML(const Instance: TPersistent; var XML: XmlString; const OutputFormat: TOutputFormat);
var
  XMLDoc: IXMLDocument;
  Root: IXMLElement;
  Writer: TOmniXMLWriter;
begin
  if Instance is TCollection then
    CreateDocument(XMLDoc, Root, Instance.ClassName)
  else
    CreateDocument(XMLDoc, Root, 'data');

  Writer := TOmniXMLWriter.Create(XMLDoc);
  try
    if Instance is TCollection then
      Writer.WriteCollection(TCollection(Instance), Root)
    else
      Writer.Write(Instance, Root);
  finally
    Writer.Free;
  end;

  XML := XMLDoc.XML;
end;

constructor TOmniXMLWriter.Create(Doc: IXMLDocument);
begin
  Self.Doc := Doc;
end;

procedure TOmniXMLWriter.InternalWriteText(Root: IXMLElement; Name, Value: XmlString; const PropFormat: TPropsFormat = pfNodes);
var
  PropNode: IXMLElement;
begin
  case PropFormat of
    pfAttributes: Root.SetAttribute(Name, Value);
    pfNodes:
      begin
        PropNode := Doc.CreateElement(Name);
        PropNode.Text := Value;
        Root.appendChild(PropNode);
      end;
  end;
end;


constructor TOmnyXMLUtil.Create();
begin
  self.ctx := TRttiContext.Create();
end;

function TOmnyXMLUtil.GetElementPropFormat(Instance: TObject; PropInfo: PPropInfo): TPropsFormat;
begin
  result:= pfNodes;

  var objType: TRttiType:= ctx.GetType(Instance.ClassInfo);
  var prop: TRttiProperty:= objType.GetProperty(PropInfo.Name);

  for var LAttr: TCustomAttribute in prop.GetAttributes() do
  begin
    if LAttr.ClassInfo = TypeInfo(PropFormatAttribute) then
    begin
      var attr: PropFormatAttribute:= PropFormatAttribute(LAttr);
      if attr.IsAttribute() then
      begin
        result:= pfAttributes;
      end;
      break;
    end;
  end;
end;

function TOmnyXMLUtil.GetElementCustomName(Instance: TObject; PropInfo: PPropInfo): string;
begin
  result:= XmlString(PPropInfo(PropInfo)^.Name);

  var objType: TRttiType:= ctx.GetType(Instance.ClassInfo);
  var prop: TRttiProperty:= objType.GetProperty(PropInfo.Name);

  for var LAttr: TCustomAttribute in prop.GetAttributes() do
  begin
    if LAttr.ClassInfo = TypeInfo(ElementNameAttribute) then
    begin
      var attr: ElementNameAttribute:= ElementNameAttribute(LAttr);
      result:= attr.GetName();
      break;
    end;
  end;


end;

function TOmnyXMLUtil.GetRootName(Instance: TObject): string;
begin
  result:= Instance.ClassName;

  var objType: TRttiType:= ctx.GetType(Instance.ClassInfo);
  var LAttr: TCustomAttribute;
  for LAttr in objType.GetAttributes() do
  begin
    if Lattr.ClassInfo = TypeInfo(ElementNameAttribute) then
    begin
      result:= ElementNameAttribute(LAttr).GetName();
      break;
    end;
  end;

end;

function TOmnyXMLUtil.GetSubTypeItemClassFromList(ObjectList: TObject): TClass;
var
  typeRtti : TRttiType;
  atrbRtti : TCustomAttribute;
  methodRtti: TRttiMethod;
  parameterRtti: TRttiParameter;
begin
  result := nil;

  typeRtti := ctx.GetType( ObjectList.ClassType );
  methodRtti := typeRtti.GetMethod('Add');
  for parameterRtti in methodRtti.GetParameters do
  begin
    if SameText(parameterRtti.Name,'Value') then
    begin
      if parameterRtti.ParamType.IsInstance then
        result := parameterRtti.ParamType.AsInstance.MetaclassType;
      break;
    end;
  end;
end;


function TOmnyXMLUtil.GetSubInstanceTypeItemClassFromList(objectList: IList<IInterface>): TClass;
var
  ctxRtti  : TRttiContext;
  typeRtti : TRttiType;
  atrbRtti : TCustomAttribute;
  methodRtti: TRttiMethod;
  parameterRtti: TRttiParameter;
begin
  result := nil;

  ctxRtti  := TRttiContext.Create;
  typeRtti := ctxRtti.GetType( ObjectList.AsObject.ClassType );
  methodRtti := typeRtti.GetMethod('Add');
  for parameterRtti in methodRtti.GetParameters do
  begin
    if SameText(parameterRtti.Name,'item') then
    begin
      if parameterRtti.ParamType.IsInstance then
      begin
        result := parameterRtti.ParamType.AsInstance.MetaclassType;
      end;
      break;
    end;
  end;
  ctxRtti.Free;
end;


procedure TOmniXMLWriter.WriteCollection(Collection: TCollection; Root: IXMLElement);
var
  i: Integer;
begin
  for i := 0 to Collection.Count - 1 do
    Write(Collection.Items[i], Root, True, False);
end;

procedure TOmniXMLWriter.WriteProperty(Instance: TPersistent; PropInfo: PPropInfo;
  Element: IXMLElement; WriteDefaultValues: Boolean; const PropFormat: TPropsFormat = pfNodes);
var
  PropType: PTypeInfo;
  Name: XmlString;

  procedure WriteStrProp;
  var
    Value: XmlString;
  begin
    if PropType^.Kind = tkWString then
      Value := GetWideStrProp(Instance, PropInfo)
    else
      Value := GetStrProp(Instance, PropInfo);

    if (Value <> EmptyStr) or (WriteDefaultValues) then
      InternalWriteText(Element, Name, Value, PropFormat);
  end;

  procedure WriteOrdProp;
  var
    Value: Longint;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    if (WriteDefaultValues) or (Value <> PPropInfo(PropInfo)^.Default) then begin
      case PropType^.Kind of
        tkInteger: InternalWriteText(Element, Name, XMLIntToStr(Value), PropFormat);
        tkChar:
          begin
            {$IFDEF UNICODE}
            InternalWriteText(Element, Name, Char(Value), PropFormat);
            {$ELSE}
            InternalWriteText(Element, XmlString(PPropInfo(PropInfo)^.Name), UTF8Decode(AnsiToUtf8(Char(Value))), PropFormat);
            {$ENDIF}  // UNICODE
          end;
        tkWChar: InternalWriteText(Element, Name, WideChar(Value), PropFormat);
        tkSet: InternalWriteText(Element, Name, GetSetProp(Instance, PPropInfo(PropInfo), True), PropFormat);
        tkEnumeration:
          begin
            if PropType = System.TypeInfo(Boolean) then
              InternalWriteText(Element, Name, XMLBoolToStr(Boolean(Value)), PropFormat)
            else if PropType^.Kind = tkInteger then
              InternalWriteText(Element, Name, XMLIntToStr(Value), PropFormat)
            // 2003-05-27 (mr): added tkEnumeration processing
            else if PropType^.Kind = tkEnumeration then
              InternalWriteText(Element, Name, GetEnumName(PropType, Value), PropFormat);
          end;
      end;
    end;
  end;

  procedure WriteFloatProp;
  var
    Value: Real;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    InternalWriteText(Element, Name, XMLRealToStr(Value), PropFormat);
  end;

  procedure WriteDateTimeProp;
  var
    Value: TDateTime;
  begin
    Value := VarAsType(GetFloatProp(Instance, PropInfo), varDate);
    if (Value <> 0) or (WriteDefaultValues) then
      InternalWriteText(Element, Name, XMLDateTimeToStrEx(Value), PropFormat);
  end;

  procedure WriteInt64Prop;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    InternalWriteText(Element, Name, XMLInt64ToStr(Value), PropFormat);
  end;

  procedure WriteObjectProp;
  var
    Value: TObject;
    PropNode: IXMLElement;

    procedure WriteStrings(const Strings: TStrings);
    var
      i: Integer;
    begin
      SetNodeAttrInt(PropNode, StringS_COUNT_NODENAME, Strings.Count);
      for i := 0 to Strings.Count - 1 do begin
        if Strings[i] <> '' then
          InternalWriteText(PropNode, StringS_PREFIX + IntToStr(i), Strings[i], PropFormat);
      end;
    end;
  begin
    Value := TObject(GetOrdProp(Instance, PropInfo));
    if (Value <> nil) then
    begin
      if (Value is TPersistent) then
      begin
        PropNode := Doc.CreateElement(Name);

        // write object's properties
        Write(TPersistent(Value), PropNode, False, True, WriteDefaultValues);
        if Value is TCollection then begin
          WriteCollection(TCollection(Value), PropNode);
          if not IsElementEmpty(PropNode) then
            Element.AppendChild(PropNode);
        end
        else if Value is TStrings then begin
          WriteStrings(TStrings(Value));
          Element.AppendChild(PropNode);
        end
        else if not IsElementEmpty(PropNode) then
          Element.AppendChild(PropNode);
      end
      else if (Value.ClassName.StartsWith('TObjectList<')) then // TODO ugly
      begin
        // write a xml tag for every occurrence
        var ValueList: TObjectList<TPersistent> := TObjectList<TPersistent>(Value) ;
        for var i:integer := 0 to ValueList.Count-1 do
        begin
          var PropNodeX: IXMLElement := Doc.CreateElement(Name);
          Write(TPersistent(ValueList[i]), PropNodeX, False, True, WriteDefaultValues);
          Element.AppendChild(PropNodeX);
        end;
      end;

    end;




  end;

  procedure WriteInterfaceProp;
  var
    Value: TObject;
    ValueIntf: IInterface;
    PropNode: IXMLElement;

    procedure WriteStrings(const Strings: TStrings);
    var
      i: Integer;
    begin
      SetNodeAttrInt(PropNode, StringS_COUNT_NODENAME, Strings.Count);
      for i := 0 to Strings.Count - 1 do begin
        if Strings[i] <> '' then
          InternalWriteText(PropNode, StringS_PREFIX + IntToStr(i), Strings[i], PropFormat);
      end;
    end;

  begin
    var ctx: TRttiContext := TRttiContext.Create();
    var objType: TRttiType:= ctx.GetType(Instance.ClassInfo);
    var p:TRttiProperty:= objType.GetProperty(PropInfo.Name);
    var tval: TValue:= p.GetValue(Instance);
    ValueIntf := tval.AsInterface;
    Value:= TObject(ValueIntf);

    if (Value <> nil) then
    begin
        if (Value is TInterfacedPersistent) then
        begin
          PropNode := Doc.CreateElement(Name);

          // write object's properties
          Write(TPersistent(Value), PropNode, False, True, WriteDefaultValues);
          if Value is TCollection then begin
            WriteCollection(TCollection(Value), PropNode);
            if not IsElementEmpty(PropNode) then
              Element.AppendChild(PropNode);
          end
          else if Value is TStrings then begin
            WriteStrings(TStrings(Value));
            Element.AppendChild(PropNode);
          end
          else if not IsElementEmpty(PropNode) then
            Element.AppendChild(PropNode);
        end
        else if (Value.ClassName.StartsWith('TFoldedList<System.IInterface>')) then      // IList -> TFoldedList TODO ugly
        begin
          // write a xml tag for every occurrence
          var ValueList:= TFoldedList<System.IInterface>(valueIntf);
          for var i:integer := 0 to ValueList.Count-1 do
          begin
            var PropNodeX: IXMLElement := Doc.CreateElement(Name);
            Write(TInterfacedPersistent(ValueList.Items[i]), PropNodeX, False, True, WriteDefaultValues);
            Element.AppendChild(PropNodeX);
          end;

        end;
      end;




  end;


begin
  if (PPropInfo(PropInfo)^.GetProc <> nil) then begin
    PropType := {$IFDEF FPC}@{$ENDIF}PPropInfo(PropInfo).PropType{$IFNDEF FPC}^{$ENDIF};

    var util:TOmnyXMLUtil := TOmnyXMLUtil.Create();
    Name:= util.GetElementCustomName(Instance, PropInfo);

    case PropType^.Kind of
      tkInteger, tkChar, tkWChar, tkEnumeration, tkSet: WriteOrdProp;
      tkString, tkLString, tkWString {$IFDEF UNICODE}, tkUString {$ENDIF}: WriteStrProp;
      tkFloat:
        if (PropType = System.TypeInfo(TDateTime)) or (PropType = System.TypeInfo(TTime))
          or (PropType = System.TypeInfo(TDate)) then
            WriteDateTimeProp
        else
          WriteFloatProp;
      tkInt64: WriteInt64Prop;
      tkClass: WriteObjectProp;
      tkInterface: WriteInterfaceProp;
    end;
    util.Free();
  end;
end;

procedure TOmniXMLWriter.Write(Instance: TPersistent; Root: IXMLElement;
  const WriteRoot: Boolean; const CheckIfEmpty: boolean; const WriteDefaultValues: Boolean);
var
  PropCount: Integer;
  PropList: PPropList;
  i: Integer;
  PropInfo: PPropInfo;
  Element: IXMLElement;
begin
  PropCount := GetTypeData(Instance.ClassInfo)^.PropCount;
  if PropCount = 0 then
    Exit;

  var util: TOmnyXMLUtil:= TOmnyXMLUtil.Create();

  if Instance is TCollectionItem then
  begin
    Element := Doc.CreateElement(COLLECTIONITEM_NODENAME);
  end
  else if WriteRoot then
  begin
    var RootName: string := util.GetRootName(Instance);
    Element := Doc.CreateElement(RootName);
  end
  else
  begin
    Element := Root;
  end;

  GetMem(PropList, PropCount * SizeOf(Pointer));



  try
    GetPropInfos(Instance.ClassInfo, PropList);
    for i := 0 to PropCount - 1 do begin
      PropInfo := PropList^[I];
      if PropInfo = nil then
        Break;
      if IsStoredProp(Instance, PropInfo) then
      begin
        var PropFormat: TPropsFormat := util.GetElementPropFormat(Instance, PropInfo);
        WriteProperty(Instance, PropInfo, Element, WriteDefaultValues, PropFormat);
      end;
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;

  util.Free();

  if WriteRoot then begin
    if CheckIfEmpty and IsElementEmpty(Element) then
      Exit
    else begin
      if Root <> nil then
        Root.appendChild(Element)
      else
        Doc.documentElement := Element;
    end;
  end;
end;

{ TOmniXMLReader }

class procedure TOmniXMLReader.LoadXML(Instance: TPersistent; const XML: XmlString);
var
  XMLDoc: IXMLDocument;
  XMLRoot: IXMLElement;
  Reader: TOmniXMLReader;
begin
  XMLDoc := CreateXMLDoc;
  { TODO : implement and test preserveWhiteSpace }
  XMLDoc.preserveWhiteSpace := True;
  XMLDoc.LoadXML(XML);

  Load(XMLDoc, XMLRoot);

  Reader := TOmniXMLReader.Create();
  try
    if Instance is TCollection then
      Reader.ReadCollection(TCollection(Instance), XMLRoot)
    else
      Reader.Read(Instance, XMLRoot, True);
  finally
    Reader.Free;
  end;
end;

class procedure TOmniXMLReader.LoadFromFile(Instance: TPersistent; FileName: string);
var
  XMLDoc: IXMLDocument;
  XMLRoot: IXMLElement;
  Reader: TOmniXMLReader;
begin
  // read document
  LoadDocument(FileName, XMLDoc, XMLRoot);

  Reader := TOmniXMLReader.Create();
  try
    if Instance is TCollection then
      Reader.ReadCollection(TCollection(Instance), XMLRoot)
    else
      Reader.Read(Instance, XMLRoot, False);
  finally
    Reader.Free;
  end;
end;

class procedure TOmniXMLReader.LoadFromFile(Collection: TCollection; FileName: string);
var
  XMLDoc: IXMLDocument;
  XMLRoot: IXMLElement;
  Reader: TOmniXMLReader;
begin
  // read document
  LoadDocument(FileName, XMLDoc, XMLRoot);

  Reader := TOmniXMLReader.Create();
  try
    Reader.ReadCollection(Collection, XMLRoot);
  finally
    Reader.Free;
  end;
end;

constructor TOmniXMLReader.Create();
begin

end;

function TOmniXMLReader.FindElement(const Root: IXMLElement; const TagName: XmlString): IXMLElement;
var
  i: Integer;
begin
  Result := nil;
  if Root = nil then
    Exit;
  i := 0;
  while (Result = nil) and (i < Root.ChildNodes.Length) do begin
    if (Root.ChildNodes.Item[i].NodeType = ELEMENT_NODE)
      and (CompareText(Root.ChildNodes.Item[i].NodeName, TagName) = 0) then
        Result := Root.ChildNodes.Item[i] as IXMLElement
    else
      Inc(i);
  end;
end;

function TOmniXMLReader.FindAllElements(const Root: IXMLElement; const TagName: XmlString): TList<IXMLElement>;
begin
  Result := nil;
  if Root = nil then
    Exit();
  Result := TList<IXMLElement>.Create();
  for var i:integer:=0 to Root.ChildNodes.Length-1 do
  begin
    if (Root.ChildNodes.Item[i].NodeType = ELEMENT_NODE)
      and (CompareText(Root.ChildNodes.Item[i].NodeName, TagName) = 0) then
        Result.Add(Root.ChildNodes.Item[i] as IXMLElement)
  end;
end;

function TOmniXMLReader.InternalReadText(Root: IXMLElement; Name: XmlString; var Value: XmlString;
  const PropFormat: TPropsFormat = pfNodes): Boolean;
var
  PropNode: IXMLElement;
  AttrNode: IXMLNode;
begin
  case PropFormat of
    pfAttributes:
      begin
        AttrNode := Root.Attributes.GetNamedItem(Name);
        Result := AttrNode <> nil;
        if Result then
          Value := AttrNode.NodeValue;
      end;
    pfNodes:
      begin
        PropNode := FindElement(Root, Name);
        Result := PropNode <> nil;
        if Result then
          Value := PropNode.Text;
      end;
    else
      Result := False;
  end;
end;

procedure TOmniXMLReader.ReadCollection(Collection: TCollection; Root: IXMLElement);
var
  i: Integer;
  Item: TCollectionItem;
begin
  Collection.Clear;
  if Root = nil then
    Exit;
  for i := 0 to Root.ChildNodes.Length - 1 do begin
    if Root.ChildNodes.Item[i].NodeType = ELEMENT_NODE then begin
      if Root.ChildNodes.Item[i].NodeName = COLLECTIONITEM_NODENAME then begin
        Item := Collection.Add;
        Read(Item, Root.ChildNodes.Item[i] as IXMLElement, False);
      end;
    end;
  end;
end;

procedure TOmniXMLReader.ReadProperty(Instance: TPersistent; PropInfo: Pointer;
  Element: IXMLElement; const PropFormat: TPropsFormat);
var
  PropType: PTypeInfo;
  Name: XmlString;

  procedure ReadFloatProp;
  var
    Value: Extended;
    Text: XmlString;
  begin
    if InternalReadText(Element, Name, Text, PropFormat) then
      Value := XMLStrToRealDef(Text, 0)
    else
      Value := 0;
    SetFloatProp(Instance, PropInfo, Value)
  end;

  procedure ReadDateTimeProp;
  var
    Value: TDateTime;
    Text: XmlString;
  begin
    if InternalReadText(Element, Name, Text, PropFormat) then begin
      if XMLStrToDateTime(Text, Value) then
        SetFloatProp(Instance, PropInfo, Value)
      else
        raise EOmniXMLPersistent.CreateFmt('Error in datetime property %s', [PPropInfo(PropInfo)^.Name]);
    end
    else
      SetFloatProp(Instance, PropInfo, 0);  // 2004-02-02
  end;

  procedure ReadStrProp;
  var
    Value: XmlString;
    {$IFDEF UNICODE}
    TypeData: PTypeData;
    CP: Word;
    RBS: RawByteString;
    {$ENDIF}  // UNICODE
  begin
    if not InternalReadText(Element, Name, Value, PropFormat) then
      Value := '';

    case PropType^.Kind of
      tkWString: SetWideStrProp(Instance, PropInfo, Value);
      {$IFDEF UNICODE}
      tkLString:
        begin
          TypeData := GetTypeData(PropType);
          CP := TypeData^.CodePage;

          case CP of
            CP_ACP: SetAnsiStrProp(Instance, PropInfo, AnsiString(Value));  // default code page
            CP_UNICODE: SetUnicodeStrProp(Instance, PropInfo, Value);  // Unicode code page
          else
            // convert to valid codepage using RawByteString
            RBS := UTF8Encode(Value);
            if CP <> CP_RawByteString then
              SetCodePage(RBS, CP, True);
            SetAnsiStrProp(Instance, PropInfo, RBS);
          end;
        end;
      {$ENDIF}  // UNICODE
    else
      SetStrProp(Instance, PropInfo, Value);
    end;
  end;

  procedure ReadOrdProp;
  var
    Value: XmlString;
    IntValue: Integer;
    BoolValue: Boolean;
  begin
    if InternalReadText(Element, Name, Value, PropFormat) then begin
      case PropType^.Kind of
        tkInteger:
          if XMLStrToInt(Value, IntValue) then
            SetOrdProp(Instance, PropInfo, XMLStrToIntDef(Value, 0))
          else
            raise EOmniXMLPersistent.CreateFmt('Invalid integer value (%s).', [Value]);
        tkChar:
          begin
            {$IFDEF UNICODE}
            SetOrdProp(Instance, PropInfo, Ord(Char(Value[1])));
            {$ELSE}
            SetOrdProp(Instance, PropInfo, Ord(Char(Utf8ToAnsi(UTF8Encode(Value))[1])));
            {$ENDIF}  // UNICODE
          end;
        tkWChar: SetOrdProp(Instance, PropInfo, Cardinal(Value[1]));
        tkSet: SetSetProp(Instance, PropInfo, Value);
        tkEnumeration:
          begin
            if PropType = System.TypeInfo(Boolean) then begin
              if XMLStrToBool(LowerCase(Value), BoolValue) then
                SetOrdProp(Instance, PropInfo, Ord(BoolValue))
              else
                raise EOmniXMLPersistent.CreateFmt('Invalid boolean value (%s).', [Value]);
            end
            else if PropType^.Kind = tkInteger then begin
              if XMLStrToInt(Value, IntValue) then
                SetOrdProp(Instance, PropInfo, IntValue)
              else
                raise EOmniXMLPersistent.CreateFmt('Invalid enum value (%s).', [Value]);
            end
            // 2003-05-27 (mr): added tkEnumeration processing
            else if PropType^.Kind = tkEnumeration then
              SetEnumProp(Instance, PropInfo, Value);
          end;
      end;
    end
    else
      SetOrdProp(Instance, PropInfo, PPropInfo(PropInfo)^.Default)
  end;

  procedure ReadInt64Prop;
  var
    Value: XmlString;
    IntValue: Int64;
  begin
    if InternalReadText(Element, Name, Value, PropFormat) then begin
      if XMLStrToInt64(Value, IntValue) then
        SetInt64Prop(Instance, PropInfo, IntValue)
      else
        raise EOmniXMLPersistent.CreateFmt('Invalid int64 value (%s).', [Value]);
    end
    else
      SetFloatProp(Instance, PropInfo, 0)
  end;

  procedure ReadObjectProp;
  var
    Value: TObject;
    PropNode: IXMLElement;

    procedure ReadStrings(const Strings: TStrings);
    var
      i: Integer;
      Count: Integer;
      Value: XmlString;
    begin
      Strings.Clear;

      Count := GetNodeAttrInt(PropNode, StringS_COUNT_NODENAME, 0);
      for i := 0 to Count - 1 do
        Strings.Add('');
        
      for i := 0 to Strings.Count - 1 do begin
        if InternalReadText(PropNode, StringS_PREFIX + IntToStr(i), Value, PropFormat) then
          Strings[i] := Value;
      end;
    end;
  begin
    Value := TObject(GetOrdProp(Instance, PropInfo));
    if (Value <> nil) then
    begin
      if (Value is TPersistent) then
      begin
        PropNode := FindElement(Element, Name);
        Read(TPersistent(Value), PropNode);
        if Value is TCollection then
          ReadCollection(TCollection(Value), PropNode)
        else if Value is TStrings then
          ReadStrings(TStrings(Value));
      end
      else
      if (Value.ClassName.StartsWith('TObjectList<')) then // TODO ugly
      begin
        var ValueList: TObjectList<TPersistent> := TObjectList<TPersistent>(Value);
        var PropNodeList: TList<IXMLElement> := FindAllElements(Element, Name);

        var util: TOmnyXMLUtil:= TOmnyXMLUtil.Create();

        for var i:integer := 0 to PropNodeList.Count-1 do
        begin
          PropNode:= PropNodeList[i];

          ValueList.Add(util.GetSubTypeItemClassFromList(ValueList).Create() as TPersistent);
          Read(TPersistent(ValueList[i]), PropNode);
        end;
        util.Free();
      end;


    end;
  end;

  procedure ReadInterfaceProp;
  var
    Value: TObject;
    ValueIntf: IInterface;
    PropNode: IXMLElement;

    procedure ReadStrings(const Strings: TStrings);
    var
      i: Integer;
      Count: Integer;
      Value: XmlString;
    begin
      Strings.Clear;

      Count := GetNodeAttrInt(PropNode, StringS_COUNT_NODENAME, 0);
      for i := 0 to Count - 1 do
        Strings.Add('');

      for i := 0 to Strings.Count - 1 do begin
        if InternalReadText(PropNode, StringS_PREFIX + IntToStr(i), Value, PropFormat) then
          Strings[i] := Value;
      end;
    end;
  begin
    var ctx: TRttiContext := TRttiContext.Create();
    var objType: TRttiType:= ctx.GetType(Instance.ClassInfo);
    var p:TRttiProperty:= objType.GetProperty(PPropInfo(PropInfo)^.Name);
    var tval: TValue:= p.GetValue(Instance);
    ValueIntf := tval.AsInterface;
    Value:= TObject(ValueIntf);
    if (Value <> nil) then
    begin
		// even if it is an interface I can deserialize because I've instantitated concrete type before reading xml
      if (Value is TInterfacedPersistent) then
      begin
        PropNode := FindElement(Element, Name);
        Read(TInterfacedPersistent(Value), PropNode);
        if Value is TCollection then
          ReadCollection(TCollection(Value), PropNode)
        else if Value is TStrings then
          ReadStrings(TStrings(Value));
      end
      else
      if (Value.ClassName.StartsWith('TFoldedList<System.IInterface>')) then // TODO ugly
      begin
        var ValueList:= TFoldedList<System.IInterface>(valueIntf);
        var PropNodeList: TList<IXMLElement> := FindAllElements(Element, Name);

        var util: TOmnyXMLUtil:= TOmnyXMLUtil.Create();

        for var i:integer := 0 to PropNodeList.Count-1 do
        begin
          PropNode:= PropNodeList[i];
			// TODO cannot deserialize due to missing info about concrete class	
        end;
        util.Free();
      end;

    end;
  end;


begin
  PropType := PPropInfo(PropInfo)^.PropType^;
  if (PPropInfo(PropInfo)^.GetProc <> nil) and
     ((PropType^.Kind = tkClass) or (PPropInfo(PropInfo)^.SetProc <> nil)) and
     (IsStoredProp(Instance, PPropInfo(PropInfo))) then
  begin

    var util:TOmnyXMLUtil := TOmnyXMLUtil.Create();
    Name:= util.GetElementCustomName(Instance, PropInfo);

    case PropType^.Kind of
      tkInteger, tkChar, tkWChar, tkEnumeration, tkSet: ReadOrdProp;
      tkString, tkLString, tkWString {$IFDEF UNICODE}, tkUString {$ENDIF}: ReadStrProp;
      tkFloat:
        if (PropType = System.TypeInfo(TDateTime)) or (PropType = System.TypeInfo(TTime))
          or (PropType = System.TypeInfo(TDate)) then
            ReadDateTimeProp
        else
          ReadFloatProp;
      tkInt64: ReadInt64Prop;
      tkClass: ReadObjectProp;
      tkInterface: ReadInterfaceProp;
    end;
  end;
end;

procedure TOmniXMLReader.Read(Instance: TPersistent; Root: IXMLElement; const ReadRoot: Boolean);
var
  PropCount: Integer;
  PropList: PPropList;
  i: Integer;
  PropInfo: PPropInfo;
begin
  var util: TOmnyXMLUtil:= TOmnyXMLUtil.Create();
  if ReadRoot then
    Root := FindElement(Root, util.GetRootName(Instance));

  if Root = nil then
  begin
    util.Free();
    Exit();
  end;
  PropCount := GetTypeData(Instance.ClassInfo)^.PropCount;
  if PropCount > 0 then begin
    GetMem(PropList, PropCount * SizeOf(Pointer));
    try
      GetPropInfos(Instance.ClassInfo, PropList);
      for i := 0 to PropCount - 1 do begin
        PropInfo := PropList^[I];
        if PropInfo = nil then
          Break;

        var PropFormat: TPropsFormat := util.GetElementPropFormat(Instance, PropInfo);
        ReadProperty(Instance, PropInfo, Root, PropFormat);
      end;

    finally
      FreeMem(PropList, PropCount * SizeOf(Pointer));
    end;
  end;
  util.Free();
end;

end.
