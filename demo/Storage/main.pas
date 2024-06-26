﻿unit main;

interface

// if you want to use MS XML parser, create a global compiler define: 'USE_MSXML'

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  OmniXML, OmniXML_Types,
  PropFormatAttributeUnit,
  ElementNameAttributeUnit,
  System.Generics.Collections,
  Spring.Collections,
{$IFDEF USE_MSXML}
  OmniXML_MSXML,
{$ENDIF}
  OmniXMLPersistent;

type
  {$IFDEF UNICODE}
  TSlovenianString = type AnsiString(1250);  // 1250 - Slovenian
  TChineseString = type AnsiString(950);  // 950 - Traditional Chinese Big5
  TJapaneseString = type AnsiString(932);  // 932 - Japanese
  {$ENDIF}  // UNICODE
  TSomeValues = (svOne, svTwo, svThree, svFour, svFive);
  TMySet = set of TSomeValues;

  TStandaloneClass = class(TPersistent)
  private
    FpropFloat: Double;
    FpropFloatAttr: Double;
  published
    property propFloat: Double read FpropFloat write FpropFloat;
    [PropFormat(true)]
    property propFloatAttr: Double read FpropFloatAttr write FpropFloatAttr;
  end;

  TChildClass = class(TCollectionItem)
  private
    FpropFloat: Double;
  published
    property propFloat: Double read FpropFloat write FpropFloat;
  end;

  TPropList = class(TCollection)
  private
    FCurDate: TDateTime;
  published
    property curDate: TDateTime read FCurDate write FCurDate;
  end;

  TWrongClass = class
  private
    FpropFloat: Double;
  published
    property propFloat: Double read FpropFloat write FpropFloat;
  end;

  ISomeInterface = interface
    function getFloat(): double;
    procedure setFloat(theFloat: double);
  end;

  TInterfacedClass = class(TInterfacedPersistent, ISomeInterface)
    private Ffloat: double;

    public function getFloat(): double;
    public procedure setFloat(theFloat: double);

    published property profFloat: double read getFloat write setFloat;
  end;


  [ElementName('myRoot')]
  TMyXML = class(TPersistent)
  private
    FpropNamespace01: string;
    FpropNamespace02: string;
    FpropString: string;
    FpropStringFoo: string;
    FpropAnsiString: AnsiString;
    FpropShortString: ShortString;
    FpropUTF8String: UTF8String;
    {$IFDEF UNICODE}
    FpropChineseString: TChineseString;
    FpropRawByteString: RawByteString;
    {$ENDIF}  // UNICODE
    FpropWideString: WideString;
    FpropWideChar: WideChar;
    FpropBoolean: Boolean;
    FpropInteger: Integer;
    FpropChar: Char;
    FpropCharDefault: Char;
    FpropByte: Byte;
    FpropWord: Word;
    FpropSmallInt: SmallInt;
    FpropEnum: TSomeValues;
    FpropSet: TMySet;
    FpropClass: TStandaloneClass;
    FpropClass_ReadOnly: TStandaloneClass;
    FpropFloat: Double;
    FpropList: TPropList;
    FpropDate: TDate;
    FpropTime: TTime;
    FpropDateTime: TDateTime;
    FpropEmptyDateTime: TDateTime;
    FpropStringList: TStringList;
    FpropClassList: TObjectList<TStandaloneClass>;
    FpropWrongClassList: TObjectList<TWrongClass>;
    FpropSomeInterface: ISomeInterface;
    FpropSomeInterfaceList: IList<ISomeInterface>;
    // TODO
    FpropClassMatrix: TObjectList<TObjectList<TStandaloneClass>>;
  public
    constructor Create;
    destructor Destroy; override;
  published
    [PropFormat(true)]
    [ElementName('xmlns:xsl')]
    property propNamespace01: string read FpropNamespace01 write FpropNamespace01;
    [PropFormat(true)]
    [ElementName('xmlns:xxx')]
    property propNamespace02: string read FpropNamespace02 write FpropNamespace02;
    [ElementName('xxx:baz')]
    property propStringFoo: string read FpropStringFoo write FpropStringFoo;
    [PropFormat(true)]
    property propString: string read FpropString write FpropString;
    property propAnsiString: AnsiString read FpropAnsiString write FpropAnsiString;
    property propShortString: ShortString read FpropShortString write FpropShortString;
    property propUTF8String: UTF8String read FpropUTF8String write FpropUTF8String;
    {$IFDEF UNICODE}
    property propChineseString: TChineseString read FpropChineseString write FpropChineseString;
    property propRawByteString: RawByteString read FpropRawByteString write FpropRawByteString;
    {$ENDIF}  // UNICODE
    property propWideString: WideString read FpropWideString write FpropWideString;
    property propWideChar: WideChar read FpropWideChar write FpropWideChar;
    property propChar: Char read FpropChar write FpropChar;
    property propCharDefault: Char read FpropCharDefault write FpropCharDefault default 'B';
    property propBoolean: Boolean read FpropBoolean write FpropBoolean default False;
    property propInteger: Integer read FpropInteger write FpropInteger;
    property propByte: Byte read FpropByte write FpropByte;
    property propWord: Word read FpropWord write FpropWord;
    property propSmallInt: SmallInt read FpropSmallInt write FpropSmallInt;
    property propEnum: TSomeValues read FpropEnum write FpropEnum;
    property propSet: TMySet read FpropSet write FpropSet;
    property propClass: TStandaloneClass read FpropClass write FpropClass;
    property propClass_ReadOnly: TStandaloneClass read FpropClass_ReadOnly;
    property propFloat: Double read FpropFloat write FpropFloat;
    property propList: TPropList read FpropList write FpropList;
    property propDate: TDate read FpropDate write FpropDate;
    property propTime: TTime read FpropTime write FpropTime;
    property propDateTime: TDateTime read FpropDateTime write FpropDateTime;
    property propEmptyDateTime: TDateTime read FpropEmptyDateTime write FpropEmptyDateTime;
    property propStringList: TStringList read FpropStringList write FpropStringList;
    property propClassList: TObjectList<TStandaloneClass> read FpropClassList write FpropClassList;
    property propClassMatrix: TObjectList<TObjectList<TStandaloneClass>> read FpropClassMatrix write FpropClassMatrix;
    property propSomeInterface: ISomeInterface read FpropSomeInterface write FpropSomeInterface;
    property propSomeInterfaceList: IList<ISomeInterface> read FpropSomeInterfaceList write FpropSomeInterfaceList;
    // not serialized because TWrongClass does not extend TPersistent
    property propWrongClassList: TObjectList<TWrongClass> read FpropWrongClassList write FpropWrongClassList;
  end;

  TfMain = class(TForm)
    bWriteToFile: TButton;
    bLoadFromFile: TButton;
    mDescription: TMemo;
    Label10: TLabel;
    Bevel1: TBevel;
    Label1: TLabel;
    rgOutputFormat: TRadioGroup;
    Bevel2: TBevel;
    Label11: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bWriteToFileClick(Sender: TObject);
    procedure bLoadFromFileClick(Sender: TObject);
  private
    DocPath: string;
    PX: TmyXML;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.DFM}

type
  TExampleTextStringType = XmlString;
  TExampleTextCharType = XmlChar;

  TExampleText = record
    CodePage: Word;
    SampleString: TExampleTextStringType;
    SampleCharIndex: Integer;
    function SampleChar: TExampleTextCharType;
  end;

const
  // Unicode text examples copied from http://www.i18nguy.com/unicode-example.html
  CDefaultString = 'Brad Pitt';  // English (default): Brad Pitt (actor)
  CSloveneString = 'Frane Milčinski - Ježek';  // Slovene: Frane "Jezek" Milcinski (actor, singer)
  CChineseString = '章子怡';  // Traditional Chinese Big5: ZHANG Ziyi (actress)
  CJapaneseString = '久保田    利伸';  // Japanese: KUBOTA Toshinobu (singer)
  CUnicodeString = '€ ß ¤';  // some symbols

const
  ExampleTextList: array[1..5] of TExampleText =
  (
    ( CodePage: 1252; SampleString: CDefaultString; SampleCharIndex: 1 ),
    ( CodePage: 932; SampleString: CJapaneseString; SampleCharIndex: 1 ),
    ( CodePage: 950; SampleString: CChineseString; SampleCharIndex: 1 ),
    ( CodePage: 1250; SampleString: CSloveneString; SampleCharIndex: 21 ),
    ( CodePage: 1200; SampleString: CUnicodeString; SampleCharIndex: 1 )  
  );

{ TExampleText }

function TExampleText.SampleChar: TExampleTextCharType;
begin
  Result := SampleString[SampleCharIndex];
end;

{ TMyXML }

constructor TMyXML.Create;
begin
  inherited;
  propSomeInterfaceList := TCollections.CreateList<ISomeInterface>();
  propSomeInterface := TInterfacedClass.create();
  propList := TPropList.Create(TChildClass);
  propClass := TStandaloneClass.Create;
  FpropClass_ReadOnly := TStandaloneClass.Create;
  propStringList := TStringList.Create;
  propClassList := TObjectList<TStandaloneClass>.Create();
  propClassMatrix := TObjectList<TObjectList<TStandaloneClass>>.Create();
end;

destructor TMyXML.Destroy;
begin
  propClassMatrix.Free;
  propClassList.Free;
  propStringList.Free;
  FreeAndNil(FpropClass_ReadOnly);
  propClass.Free;
  propList.Free;
  inherited;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  UseCodePage: Word;
  ExampleTextIndex: Integer;
  ExampleText: TExampleText;
begin
  // try to find most appropriate codepage based on system default code page
  UseCodePage := GetACP;

  ExampleTextIndex := Low(ExampleTextList);
  while ExampleTextIndex <= High(ExampleTextList) do
  begin
    if ExampleTextList[ExampleTextIndex].CodePage = UseCodePage then
      Break
    else
      Inc(ExampleTextIndex);
  end;
  if ExampleTextIndex > High(ExampleTextList) then
    ExampleTextIndex := Low(ExampleTextList);  // use default
  ExampleText := ExampleTextList[ExampleTextIndex];

  OmniXMLPersistent.DefaultPropFormat := pfNodes;

  DocPath := ExtractFilePath(ExpandFileName(ExtractFilePath(Application.ExeName) + '..\doc\dummy.xml'));
  PX := TMyXML.Create;
  PX.propClass.propFloat := 32/11;
  PX.propClass.propFloatAttr := 32/15;
  PX.propClass_ReadOnly.propFloat := 22/7;
  PX.propString := ExampleText.SampleString;
  PX.propNamespace01 := 'http://www.w3.org/1999/XSL/Transform';
  PX.propNamespace02 := 'http://www.sample.org/2024';
  PX.propStringFoo := 'Bar';
  PX.propAnsiString := AnsiString(ExampleText.SampleString);
  PX.propShortString := ShortString(ExampleText.SampleString);
  PX.propUTF8String := UTF8String(ExampleText.SampleString);
  {$IFDEF UNICODE}
  PX.propChineseString := CChineseString;  // this is hardcoded to constant
  PX.propRawByteString := UTF8Encode(ExampleText.SampleString);
  {$ENDIF}  // UNICODE
  PX.propWideString := WideString(ExampleText.SampleString);
  PX.propWideChar := WideChar(ExampleText.SampleChar);
  PX.propChar := Utf8ToAnsi(UTF8Encode(ExampleText.SampleChar))[1];
  PX.propCharDefault := 'B';

  PX.propBoolean := True;
  PX.propInteger := 128934;
  PX.propEnum := svTwo;
  PX.propSet := [svTwo, svFive, svFour];
  PX.propFloat := 22/7;
  PX.propDate := Trunc(Now);
  PX.propTime := Frac(Now);
  PX.propDateTime := Now;
//  PX.propEmptyDateTime := 0;
  PX.propStringList.Add('line'#3'1');
  PX.propStringList.Add('');
  PX.propStringList.Add('line 3');
  PX.propStringList.Delimiter := ';';

  var cl01: TStandaloneClass := TStandaloneClass.Create();
  cl01.FpropFloat:= 50;
  var cl02: TStandaloneClass := TStandaloneClass.Create();
  cl02.FpropFloatAttr:= 25.6;

  PX.propClassList.Add(cl01);
  PX.propClassList.Add(cl02);

  PX.FpropSomeInterface.setFloat(2.59);

  var intf01: ISomeInterface:= TInterfacedClass.Create();
  intf01.setFloat(7.56);
  var intf02: ISomeInterface:= TInterfacedClass.Create();
  intf02.setFloat(1.9);
  // TODO can serialize but cannot deserialize due to missing info about concrete class
  PX.propSomeInterfaceList.add(intf01);
  PX.propSomeInterfaceList.add(intf02);


//  for var j:integer := 0 to 3 do
//  begin
//    var listJ: TObjectList<TStandaloneClass> := TObjectList<TStandaloneClass>.Create();
//    for var k:integer := 0 to 4 do
//    begin
//        var classjk: TStandaloneClass := TStandaloneClass.Create();
//        classjk.FpropFloat:= 50*j*k;
//        classjk.FpropFloatAttr:= 25.6*j*k;
//        listJ.Add(classjk);
//    end;
//    PX.propClassMatrix.Add(listJ);
//  end;


  PX.propList.curDate := Now;
  TChildClass(PX.propList.Add).propFloat := 23/7;
  TChildClass(PX.propList.Add).propFloat := 12/8;
  TChildClass(PX.propList.Add).propFloat := 1/3;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  PX.Free;
end;

procedure TfMain.bWriteToFileClick(Sender: TObject);
begin
  // first we save PX (custom TPersistent class)
  TOmniXMLWriter.SaveToFile(PX, DocPath + 'storage_PX.xml', TOutputFormat(rgOutputFormat.ItemIndex));
  // then, we save Self (TForm class)
  TOmniXMLWriter.SaveToFile(Self, DocPath + 'storage_form.xml', TOutputFormat(rgOutputFormat.ItemIndex));
end;

procedure TfMain.bLoadFromFileClick(Sender: TObject);
begin
  FreeAndNil(PX);
  // You *must* instantiate concrete types in order to read xml data to put into interfaces
  PX := TMyXML.Create;

  TOmniXMLReader.LoadFromFile(PX, DocPath + 'storage_PX.xml');
  TOmniXMLWriter.SaveToFile(PX, DocPath + 'storage_PX_resaved.xml', TOutputFormat(rgOutputFormat.ItemIndex));
end;


function TInterfacedClass.getFloat(): double;
begin
  result:= self.Ffloat;
end;
procedure TInterfacedClass.setFloat(theFloat: double);
begin
  self.Ffloat:= theFloat;
end;


initialization
  RegisterClass(TChildClass);

end.

