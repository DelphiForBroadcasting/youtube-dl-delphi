unit UJSExec;

interface

uses
  System.Win.ComObj, System.Variants, Winapi.ActiveX, MSHTML, ShDocVw;

type
  TJSExec = class(TObject)
  private
    FHTMLDOcument2    : IHTMLDocument2;
    function GetDocWindow: IHTMLWindow2;
    function GetElementById(const ID: string): IHTMLElement;
    function GetRetValContainer: IHTMLElement;
    function CreateRetValContainer: IHTMLElement;
  public
    constructor Create();
    procedure DocumentWrite(const ADocument: string);
    procedure RunJSProc(const Fn: string);
    function RunJSFn(const Fn: string): string;
  end;

implementation

uses
  SysUtils;

const
  // unique id that should clash with anything in the doc
  cRetValElemId = 'id58A3A2A46539468A943D00FDD6A4FF08';

{ TJSExec }

constructor TJSExec.Create();
begin
  inherited Create;
  //CoInitialize(nil);
  FHTMLDOcument2 := coHTMLDocument.Create as IHTMLDocument2;
end;


procedure TJSExec.DocumentWrite(const ADocument: string);
var
  v             : OleVariant;
begin
  v := VarArrayCreate([0, 0], VarVariant);
  try
    v[0]:='<!DOCTYPE html><html><head>'+ADocument+'</head><body></body></html> ';
  finally
    FHTMLDOcument2.Write(PSafeArray(System.TVarData(v).VArray));
  end;
end;

function TJSExec.CreateRetValContainer: IHTMLElement;
begin
  Result := FHTMLDOcument2.createElement('input');
  Result.id := cRetValElemId;
  Result.setAttribute('name', cRetValElemId, 0);
  Result.setAttribute('type', 'hidden', 0);
  Result.setAttribute('value', '', 0);
end;

function TJSExec.GetDocWindow: IHTMLWindow2;
begin
  Result := FHTMLDOcument2.parentWindow;
  if not Assigned(Result) then
    raise Exception.Create('No document window');
end;

function TJSExec.GetElementById(const ID: string): IHTMLElement;
var
  Doc: IHTMLDocument3;
begin
  if not Supports(FHTMLDOcument2, IHTMLDocument3, Doc) then
    raise Exception.Create('Invalid document');
  Result := Doc.getElementById(ID);
end;

function TJSExec.GetRetValContainer: IHTMLElement;
var
  NewNode: IHTMLDOMNode;
  BodyNode: IHTMLDOMNode;
  LHTMLElement : IHTMLElement;
begin
  Result := GetElementById(cRetValElemId);
  if not Assigned(Result) then
  begin
    if not Supports(FHTMLDocument2.body, IHTMLDOMNode, BodyNode) then
      raise Exception.Create('Invalid body node');
    Result := CreateRetValContainer;
    if not Supports(Result, IHTMLDOMNode, NewNode) then
      raise Exception.Create('Invalid new node');
    BodyNode.appendChild(NewNode);
  end;
end;

function TJSExec.RunJSFn(const Fn: string): string;
var
  EmbedFn: string;
  WrapperFn: string;
  HiddenElem: IHTMLElement;
const
  WrapperFnTplt = 'eval("'
    + 'id = document.getElementById(''' + cRetValElemId + '''); '
    + 'id.value = %s;'
    + '")';
begin
  EmbedFn := StringReplace(Fn, '"', '\"', [rfReplaceAll]);
  EmbedFn := StringReplace(EmbedFn, '''', '\''', [rfReplaceAll]);
  HiddenElem := GetRetValContainer;
  WrapperFn := Format(WrapperFnTplt, [EmbedFn]);
  RunJSProc(WrapperFn);
  Result := HiddenElem.getAttribute('value', 0);
end;

procedure TJSExec.RunJSProc(const Fn: string);
var
  Wdw: IHTMLWindow2;
begin
  try
    Wdw := GetDocWindow;
    Wdw.execScript(Fn, 'JavaScript'); // execute function
  except
    // swallow exception to prevent JS error dialog
  end;
end;

end.