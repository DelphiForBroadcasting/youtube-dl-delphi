
(*

function js2php($f) {
  $f = preg_replace('/\$/', '_', $f);
  $f = preg_replace('/\}/', ';}', $f);
  $f = preg_replace('/var\s+/', '', $f);
  $f = preg_replace('/(\w+).join\(""\)/', 'implode(${1})', $f);
  $f = preg_replace('/(\w+).length/', 'count(${1})', $f);
  $f = preg_replace('/(\w+).reverse\(\)/', 'array_reverse(${1})', $f);
  $f = preg_replace('/(\w+).slice\((\d+)\)/', 'array_slice(\$${1},${2})', $f);
  $f = preg_replace('/(\w+).split\(""\)/', 'str_split(${1})', $f);
  $f = preg_replace('/\((\w+)\)/', '(\$${1})', $f);
  $f = preg_replace('/\[(\w+)/', '[\$${1}', $f);
  $f = preg_replace('/\((\w+,\d+)\)/', '(\$${1})', $f);
  $f = preg_replace('/\((\w+),(\w+)\)/', '(\$${1},\$${2})', $f);
  $f = preg_replace('/(\w+)([=\[;])/', '\$${1}${2}', $f);
  $f = preg_replace('/\$(\d+)/', '${1}', $f);
  #echo $f . "\n";
  return $f;
}
$f1='function WD(a){a=a.split("");a=a.reverse();a=a.slice(1);a=a.reverse();a=a.slice(3);a=XD(a,19);a=a.reverse();a=XD(a,35);a=XD(a,61);a=a.slice(2);return a.join("")}';
$f2='function XD(a,b){var c=a[0];a[0]=a[b%a.length];a[b]=c;return a}';
$s='558D58CA687B9BD6B4C7E22FDB1C07395679C758489.0203EF29F3E3BD1E7A32567C9F02FE09035C837676';
$code = '$a= "' . $s . '";' . js2php($f1) . js2php($f2) . '$sig=WD($a); return $sig;';
$signature = eval($code);
echo 'decipered:' . $signature;

*)


//https://github.com/rylio/ytdl/blob/master/signature.go
//http://stackoverflow.com/questions/27981222/youtube-signature-decipher-with-an-working-example-for-vb-net
//https://github.com/rg3/youtube-dl/blob/master/youtube_dl/extractor/youtube.py

unit FH.YouTube.Decipher;

{$Define SpiderMonkey}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
{$IfDef SpiderMonkey}
  jsDbgServer, jsintf, js15decl,
{$EndIf}
  System.RegularExpressions,
  System.RegularExpressionsCore;

type
  TYouTubeSig = class
  private
    FFunctionName   : string;
    FFunctionCode   : TArray<string>;
    FFunctionArgs   : TArray<string>;
    FHelperObjects  : TArray<string>;

    function GetHelperObject(const AJScript: string; const AObjectName: string): string;
    function GetFunctionName(const AJScript: string): string;
    function BuildFunction(const AJScript: string): string;
  public
    constructor Create(const AJScript: string); overload;
    function Decipher(const ACipher: string): string;  overload;
    class function Decipher(const AJScript: string; const ACipher: string): string;  overload; static;
    function ToString: string; override;

    property FunctionName: string read FFunctionName;
  end;

implementation

constructor TYouTubeSig.Create(const AJScript: string);
begin
  inherited Create;
  BuildFunction(AJScript);
end;

function TYouTubeSig.Decipher(const ACipher: string): string;
var
{$IfDef SpiderMonkey}
  LJSEngine       : TJSEngine;
  LJSValSignature : jsval;
  LVp             : jsval;
  LReturnValue    : jsval;
{$EndIf}
begin
  result := '';

(*
var
  LJSExec : TJSExec;
begin
  LJSExec := TJSExec.Create;
  try
    LJSExec.DocumentWrite('<script>'+memo1.Text+'</script>');
    showmessage(LJSExec.RunJSFn('xq("558D58CA687B9BD6B4C7E22FDB1C07395679C758489.0203EF29F3E3BD1E7A32567C9F02FE09035C837676")'));
  finally
    FreeAndNil(LJSExec);
  end;
*)

{$IfDef SpiderMonkey}
  LJSEngine := TJSEngine.Create;
  try
    LJSEngine.Evaluate(Self.ToString, ':ApplicationInitScript:');
    LJSValSignature:= StringToJSVal(LJSEngine.Context, ACipher);
    if JS_LookupProperty(LJSEngine.Context, LJSEngine.Global.JSObject, PAnsiChar(ansistring(Self.FunctionName)), @LVp) = js_true then
    begin
      if (not JSValIsVoid(LVp)) and (JSValIsObject(LVp)) then
      begin
        if JS_CallFunctionValue(LJSEngine.Context, LJSEngine.Global.JSObject, LVp, 1,@LJSValSignature, @LReturnValue) > 0 then
          result := JSValToString(LJSEngine.Context, LReturnValue);
      end;
    end;
  finally
    FreeAndNil(LJSEngine);
  end;
{$EndIf}
end;

class function TYouTubeSig.Decipher(const AJScript: string; const ACipher: string): string;
var
  LYouTubeSig : TYouTubeSig;
begin
  LYouTubeSig:= TYouTubeSig.Create(AJScript);
  try
    result:= LYouTubeSig.Decipher(ACipher);
  finally
    FreeAndNil(LYouTubeSig);
  end;
end;

function TYouTubeSig.ToString(): string;
begin
  result := format('%s function %s(%s){%s};', [string.Join(' ', FHelperObjects), FFunctionName, string.Join(',', FFunctionArgs), string.Join(';', FFunctionCode)]);
end;

function TYouTubeSig.GetHelperObject(const AJScript: string; const AObjectName: string): string;
const
  cOBJECT_RE = 'var\s+#NAME#={((?:.|\n)*?)};';
var
  LRegEx        : TRegEx;
  LJScript       : string;
begin
  LJScript := AJScript.Replace(#10,' ',[rfReplaceAll]).Replace(#13,' ',[rfReplaceAll]);

  LRegEx := TRegEx.Create(cOBJECT_RE.Replace('#NAME#', AObjectName), [roNotEmpty, roMultiLine]);
  if LRegEx.IsMatch(LJScript) then
  begin
    result := LRegEx.Match(LJScript).Value;
  end;
end;

function TYouTubeSig.GetFunctionName(const AJScript: string): string;
const
  cSIGNATURE_RE = '(["\''])signature\1\s*,\s*(?P<sig>[a-zA-Z0-9$]+)\(';
  cSIG_RE = '\.sig\|\|(?P<sig>[a-zA-Z0-9$]+)\(';
  CSIG_RE1 = 'yt\.akamaized\.net/\)\s*\|\|\s*.*?\s*c\s*&&\s*d\.set\([^,]+\s*,\s*(?P<sig>[a-zA-Z0-9$]+)\(';
  //CSIG_RE2 = '\bc\s*&&\s*d\.set\([^,]+\s*,\s*(?P<sig>[a-zA-Z0-9$]+)\(';
var
  LRegEx        : TRegEx;
begin
  result := '';
  LRegEx := TRegEx.Create(cSIGNATURE_RE);
  if LRegEx.IsMatch(AJScript) then
  begin
    result := LRegEx.Match(AJScript).Groups['sig'].Value;
    if not result.IsEmpty then
      exit;
  end;

  LRegEx := TRegEx.Create(cSIG_RE);
  if LRegEx.IsMatch(AJScript) then
  begin
    result := LRegEx.Match(AJScript).Groups['sig'].Value;
    if not result.IsEmpty then
      exit;
  end;

  LRegEx := TRegEx.Create(cSIG_RE1);
  if LRegEx.IsMatch(AJScript) then
  begin
    result := LRegEx.Match(AJScript).Groups['sig'].Value;
  end;
end;


function TYouTubeSig.BuildFunction(const AJScript: string): string;
const
  cOBJECTNAME_RE = ';(?<ObjectName>[A-Za-z0-9]+)\.';
  cFUNCTION_RE = '(?x)(?:function\s+#NAME#|[{;,]\s*#NAME#\s*=\s*function|var\s+#NAME#\s*=\s*function)\s*\((?P<args>[^)]*)\)\s*\{(?P<code>[^}]+)\}';
  cJSVAR_RE   = '[a-zA-Z_\$][a-zA-Z_0-9]*';
	cREVERSE_RE = ':function\(a\)\{' +
		'(?:return )?a\.reverse\(\)' +
		'\}';
	cSLICE_RE = ':function\(a,b\)\{' +
		'return a\.slice\(b\)' +
		'\}';
	cSPLICE_RE = ':function\(a,b\)\{' +
		'a\.splice\(0,b\)' +
		'\}';
	cSWAP_RE = ':function\(a,b\)\{' +
		'var c=a\[0\];a\[0\]=a\[b%a\.length\];a\[b\]=c(?:;return a)?' +
		'\}';

var
  LRegEx        : TRegEx;
  LJScript      : string;
  LFunction     : string;
  LObjectName   : string;
  LObjectCode   : string;
begin
  LJScript := AJScript.Replace(#10,' ',[rfReplaceAll]).Replace(#13,' ',[rfReplaceAll]);

  FFunctionName := GetFunctionName(LJScript);
  if FFunctionName.IsEmpty then
  begin
    // Function without name
    LRegEx := TRegEx.Create(Format('function(?: %s)?\(a\)\{'+
      'a=a\.split\(\"\"\);\s*'+
      '((?:(?:a=)?%s\.%s\(a,\d+\);)+)'+
      'return a\.join\(\"\"\)\}', [cJSVAR_RE, cJSVAR_RE, cJSVAR_RE]));
    if  LRegEx.IsMatch(LJScript)  then
    begin

    end;
  end;

  // Get Function from script
  LRegEx := TRegEx.Create(cFUNCTION_RE.Replace('#NAME#', TRegEx.Escape(FFunctionName)));
  if LRegEx.IsMatch(LJScript) then
  begin
    FFunctionArgs := LRegEx.Match(LJScript).Groups['args'].Value.Split([',']);
    FFunctionCode :=  LRegEx.Match(LJScript).Groups['code'].Value.Split([';']);
    LFunction := LRegEx.Match(LJScript).Value;
  end;

  // Get Object code from function
  LRegEx := TRegEx.Create(Format(
		'a=a\.split\(\"\"\);\s*'+
		'(?P<ObjectCode>(?:(?:a=)?%s\.%s\(a,\d+\);)+)'+
		'return a\.join\(\"\"\)', [cJSVAR_RE, cJSVAR_RE]));
  if LRegEx.IsMatch(LFunction) then
  begin
    LObjectCode := LRegEx.Match(LFunction).Groups['ObjectCode'].Value;
  end;

  // Get Object name from Object code
  LRegEx := TRegEx.Create(cOBJECTNAME_RE);
  if LRegEx.IsMatch(LObjectCode) then
  begin
    LObjectName := LRegEx.Match(LObjectCode).Groups['ObjectName'].Value;
  end;

  SetLength(FHelperObjects, 1);
  FHelperObjects[0] := GetHelperObject(LJScript, LObjectName);
end;


end.