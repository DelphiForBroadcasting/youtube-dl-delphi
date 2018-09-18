unit FH.YouTube;

interface

uses
  System.Classes,
  System.Json,
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.SyncObjs,
  System.RegularExpressions,
  System.NetEncoding,
  System.Net.HttpClient,
  System.Net.Socket,
  System.Net.URLClient,
  FH.YouTube.Utils,
  FH.YouTube.Decipher;

type     
  ICDNMediaTag = interface
  ['{9F340235-2699-4181-B826-4BB9A068AE15}']
    function GetTag(): Integer; stdcall;
  end;

  ICDNMediaStream = interface   
  ['{47A45292-DA57-4360-8E12-56BD6127777F}']
    function GetURL(): WideString; stdcall;
    function GetMediaType():WideString; stdcall;
    function GetITag(): ICDNMediaTag; stdcall;
  end;

  ICDNMediaThumbnail = interface
  ['{5CC05A46-F708-4940-BF25-12BBAFC9E70C}']
    function GetURL(): WideString; stdcall;
  end;

  ICDNMediaStreams = interface
  ['{8FDBE905-FE88-4536-B13D-D82BF9D5C65E}']
    
  end;

  ICDNMediaInfo = interface
  ['{EE23D72D-B8F3-4655-9234-6CA7262EBCF8}']
    function GetMediaStreams(): ICDNMediaStreams; stdcall;
    function GetTitle(): WideString; stdcall;
    function GetThumbnail(): ICDNMediaThumbnail; stdcall;
    function GetAuthor(): WideString; stdcall;
    function GetVideoID(): WideString; stdcall;
  end;

  ICDNMediaDownloader = interface
  ['{8015076A-F5C7-4E55-B8F1-079D589F7D4D}']
    procedure Download(AFileName: string; AOnReceiveData: TDownloadReceive;  AOnComplete: TDownloadComplete;  AOnBeforeStart: TDownloadBeforeStart);
  end;

  IVideoHostingService = interface
  ['{7AA18828-84F0-4FDD-98B7-9863D38F2AAF}']
  end;

type
  TYouTubeClient = class;

  TYouTubeStream = class
  private
    FOwner              : TYouTubeClient;
    FStreamProperty     : string;
    FURL                : TURI;
    FITag               : TYouTubeITag;
    FType               : string;
    FSIG                : string;
    FS                  : string;
    procedure StreamPropertyParse(AStreamProperty: string);
    function GetURL: TURI;
  public
    constructor Create(const AOwner: TYouTubeClient; const AStreamProperty: string); overload;
    destructor Destroy; override;
    property URL : TURI read GetURL;
    property ITag : TYouTubeITag read FITag;
    property MediaType : string read FType;
    property SIG : string read FSIG;
    property S : string read FS;
    function Download(AFileName: string; AOnReceiveData: TDownloadReceive;  AOnComplete: TDownloadComplete;  AOnBeforeStart: TDownloadBeforeStart): TDownloadHTTP;
  end;

  TYouTubeMainStream = class(TYouTubeStream)
  private
    FQuality      : string;
    FFallbackHost : string;
    procedure StreamPropertyParse();
  public
    constructor Create(const AOwner: TYouTubeClient; const AStreamProperty: string); overload;
    property FallbackHost : string read FFallbackHost;
    property StreamType : string read FType;
    property Quality : string read FQuality;
  end;

  TYouTubeAdaptiveStream = class(TYouTubeStream)
  private
    FXTags          : string;
    FLmt            : integer;
    FFPS            : double;
    FInit           : string;
    FClen           : integer;
    FIndex          : string;
    FProjectionType : integer;
    FSize           : string;
    FBitrate        : integer;
    FQualityLabel   : string;
    procedure StreamPropertyParse();
  public
    constructor Create(const AOwner: TYouTubeClient; const AStreamProperty: string); overload;
    property QualityLabel : string read FQualityLabel;
  end;

  TYouTubeStreams = class(TObjectList<TYouTubeStream>)
  private

  public
    constructor Create(); overload;
    destructor Destroy; override;
  end;

  TYouTubeURL = record
  private
    FURL      : string;
    FProtocol : string;
    FSubdomain: string;
    FDomain   : string;
    FPath     : string;
    FVideoID  : string;
    function GetURL: string;
    function GetEmbedUrl: string;
  public
    class function ValidateUrl(const AUrl: string): boolean; overload; static;
    class function ValidateUrl(const AUrl: string; out AYouTubeURL: TYouTubeURL): boolean; overload; static;
    class function Create(const AUrl: string): TYouTubeURL; overload; static;
    function isEmbed    : boolean;
    property ToString   : string read GetURL;
    property Url        : string read GetURL;
    property EmbedUrl   : string read GetEmbedUrl;
    property Protocol   : string read FProtocol;
    property Subdomain  : string read FSubdomain;
    property Domain     : string read FDomain;
    property Path       : string read FPath;
    property VideoID    : string read FVideoID;
  end;

  TYouTubeClient = class
  private const

  private
    FURI                : TYouTubeURL;
    FEncoding           : TEncoding;
    FVideInfo           : TJSONObject;

    FYouTubeStreams     : TYouTubeStreams;
    FYouTubeThumbnail   : TYouTubeThumbnail;
    function GetTitle: string;

    function GetAuthor: string;
    function GetFmtStreamMap: string;
    function GetAdaptiveFmts: string;
    function GetThumbnailUrl: TURI;
    function GetLengthSeconds: integer;
    function GetKeywords: TArray<string>;
    function GetVideoID: string;

    procedure ParseStreams;
  public
    constructor Create(AURI: string); overload;
    destructor Destroy; override;
    property URI: TYouTubeURL read FURI;

    property VideInfo: TJSONObject read FVideInfo write FVideInfo;
    property Streams: TYouTubeStreams read FYouTubeStreams;
    property Title: string read GetTitle;
    property Thumbnail: TYouTubeThumbnail read FYouTubeThumbnail;
    property Duration: integer read GetLengthSeconds;
    property Author: string read GetAuthor;
    property VideoId: string read GetVideoID;
    property Keywords: TArray<string> read GetKeywords;

    class function ParseEmbeded(const AUrl: string): TJSONObject; static;
    class function ParseYTPlayer(AResponseData: string): TJSONObject;  static;
    class function DownloadWebPage(const AURL: string): string; static;
    class function GetVideoInfo(const AUrl: string): TJSONObject; static;
    class function GetVideoTitle(const AUrl: string): string; static;
    class function GetDesriptionHTML(const AWebPage: string): string; static;
    class function GetTitleHTML(const AWebPage: string): string; static;
    class function GetJSPlayerUrlHTML(const AWebPage: string): string; static; deprecated 'not use';
    class function BuildDecipherScript(const AJSPlayerUrl: string): string; static;
    class function GetDatePublishedHTML(const AWebPage: string): string; static;
  end;

implementation

class function TYouTubeURL.Create(const AUrl: string): TYouTubeURL;
const
  //cYouTubeUrlRegExp1 = '(?:.+?)?(?:\/v\/|watch\/|\?v=|\&v=|youtu\.be\/|\/v=|^youtu\.be\/)(?<VideoID>[a-zA-Z0-9_-]{11})+';
  cYouTubeUrlRegExp = '(?P<YouTubeURL>(?P<Protocol>(?:https?:)?\/\/)?(?P<Subdomain>(?:www|m)\.)?(?P<Domain>(?:youtube\.com|youtu.be))(?P<Path>\/(?:[\w\-]+\?v=|embed\/|v\/)?)(?P<VideoID>[a-zA-Z0-9_-]{11})+)';
var
  LRegEx      : TRegEx;
begin
  try
    if AUrl.IsEmpty then
      exit;

    FillChar(result, SizeOf(TYouTubeURL), #0);

    LRegEx := TRegEx.Create(cYouTubeUrlRegExp, [roIgnoreCase, roMultiLine]);
    if LRegEx.IsMatch(AUrl) then
    begin
      Result.FURL := LRegEx.Match(AUrl).Groups['YouTubeURL'].Value;
      Result.FProtocol := LRegEx.Match(AUrl).Groups['Protocol'].Value;
      Result.FSubdomain := LRegEx.Match(AUrl).Groups['Subdomain'].Value;
      Result.FDomain := LRegEx.Match(AUrl).Groups['Domain'].Value;
      Result.FPath := LRegEx.Match(AUrl).Groups['Path'].Value;
      Result.FVideoID := LRegEx.Match(AUrl).Groups['VideoID'].Value;
    end;

  except
    ;
  end;
end;


class function TYouTubeURL.ValidateUrl(const AUrl: string): boolean;
begin
  result := not TYouTubeURL.Create(AUrl).ToString.IsEmpty
end;

class function TYouTubeURL.ValidateUrl(const AUrl: string; out AYouTubeURL: TYouTubeURL): boolean;
begin
  AYouTubeURL := TYouTubeURL.Create(AUrl);
  result := not AYouTubeURL.ToString.IsEmpty;
end;

function TYouTubeURL.GetURL: string;
begin
  result := FURL;
end;

function TYouTubeURL.GetEmbedUrl: string;
begin
  result := Format('%swww.youtube.com/embed/%s',[Self.FProtocol, Self.VideoID]);
end;


function TYouTubeURL.isEmbed: boolean;
begin
  result := false;
  if pos('embed', Self.FPath) > 0  then
    result := true;
end;

(* TYouTubeAdaptiveStream *)
constructor TYouTubeAdaptiveStream.Create(const AOwner: TYouTubeClient; const AStreamProperty: string);
begin
  inherited Create(AOwner, AStreamProperty);
  StreamPropertyParse;
end;

procedure TYouTubeAdaptiveStream.StreamPropertyParse;
var
  LStreamMapArr : TArray<string>;
  i             : integer;
begin
  LStreamMapArr := FStreamProperty.Split(['&']);

  for I := Low(LStreamMapArr) to High(LStreamMapArr) do
  begin
    if SameText(LStreamMapArr[i].Split(['='])[0], 'quality_label') then
    begin
      Self.FQualityLabel := LStreamMapArr[i].Split(['='])[1];
    end;
  end;

end;

(*  *)
constructor TYouTubeMainStream.Create(const AOwner: TYouTubeClient; const AStreamProperty: string);
begin
  inherited Create(AOwner, AStreamProperty);
  StreamPropertyParse;
end;

procedure TYouTubeMainStream.StreamPropertyParse;
var
  LStreamMapArr : TArray<string>;
  i             : integer;
  LUnknownStr   : string;
begin
  LStreamMapArr := FStreamProperty.Split(['&']);

  for I := Low(LStreamMapArr) to High(LStreamMapArr) do
  begin
    if SameText(LStreamMapArr[i].Split(['='])[0], 'quality') then
    begin
      FQuality := LStreamMapArr[i].Split(['='])[1];
    end else
    if SameText(LStreamMapArr[i].Split(['='])[0], 'fallback_host') then
    begin
      FFallbackHost := TNetEncoding.URL.Decode((LStreamMapArr[i].Split(['='])[1]));
    end else
    begin
      LUnknownStr := LStreamMapArr[i].Split(['='])[1];
    end;
  end;
      // entry["url"].first << "&signature=#{entry["sig"].first}"
end;

(*  *)
constructor TYouTubeStream.Create(const AOwner: TYouTubeClient; const AStreamProperty: string);
begin
  inherited Create;
  FOwner := AOwner;
  StreamPropertyParse(AStreamProperty);
end;

destructor TYouTubeStream.Destroy;
begin
  inherited Destroy;
end;

function TYouTubeStream.GetURL: TURI;
var
  i             : integer;
  LJSONValue    : TJSONvalue;
  LPlayerPage   : string;
  LSignature    : string;
  LParamIndex   : integer;
begin
  try
    // Try Decipher
    if not Self.FS.IsEmpty then
    begin
      if Self.FOwner.VideInfo.TryGetValue<TJSONValue>('player_url', LJSONValue) then
      begin
        if not LJSONValue.Value.IsEmpty then
        begin
          LPlayerPage := Self.FOwner.DownloadWebPage(LJSONValue.Value);
          if not LPlayerPage.IsEmpty then
          begin
            Self.FSIG := TYouTubeSig.Decipher(LPlayerPage, Self.FS);
          end;
        end;
      end;

      // remove old signature from url if present
      for I := 0 to Length(Self.FURL.Params) - 1 do
      begin
        if SameText(Self.FURL.Params[i].Name, 'signature') then
        begin
          LSignature := Self.FURL.Parameter[i].Value;
          Self.FURL.DeleteParameter(i);
          break;
        end;
      end;

      // add  decipher signature to url
      Self.FURL.AddParameter('signature', Self.FSig);

    end;
  finally
    result := Self.FURL
  end;
end;

procedure TYouTubeStream.StreamPropertyParse(AStreamProperty: string);

function ExistsParameter(const Params: TURIParameters; const AName: string): boolean;
var
  I: Integer;
  LName: string;
begin
  Result := false;
  LName := TNetEncoding.URL.EncodeQuery(AName);
  for I := 0 to Length(Params) - 1 do
    if Params[I].Name = LName then
      Exit(true);
end;

var
  LStreamMapArr : TArray<string>;
  i             : integer;
  LDebugStr     : string;
begin
  FStreamProperty := AStreamProperty;
  LStreamMapArr := FStreamProperty.Split(['&']);

  for I := Low(LStreamMapArr) to High(LStreamMapArr) do
  begin
    if SameText(LStreamMapArr[i].Split(['='])[0], 'url') then
    begin
      FURL := TURI.Create(TNetEncoding.URL.Decode(LStreamMapArr[i].Split(['='])[1]));

      if ExistsParameter(FURL.Params, 'ratebypass') then
      begin
        FURL.ParameterByName['ratebypass'] := 'yes';
      end else
      begin
        FURL.AddParameter('ratebypass', 'yes');
      end;

    end else
    if SameText(LStreamMapArr[i].Split(['='])[0], 'sig') then
    begin
      FSIG := LStreamMapArr[i].Split(['='])[1];
    end else
    if SameText(LStreamMapArr[i].Split(['='])[0], 's') then
    begin
      FS := LStreamMapArr[i].Split(['='])[1];
    end else
    if SameText(LStreamMapArr[i].Split(['='])[0], 'type') then
    begin
      FType := TNetEncoding.URL.Decode(LStreamMapArr[i].Split(['='])[1]);
    end else
    if SameText(LStreamMapArr[i].Split(['='])[0], 'itag') then
    begin
      FITAG := TYouTubeITag.Create(StrToInt(LStreamMapArr[i].Split(['='])[1]));
    end;
  end;
end;

function TYouTubeStream.Download(AFileName: string; AOnReceiveData: TDownloadReceive; AOnComplete: TDownloadComplete; AOnBeforeStart: TDownloadBeforeStart): TDownloadHTTP;
var
  LDownloadThread : TDownloadHTTP;
begin
  LDownloadThread := TDownloadHTTP.Create(FURL.ToString);
  try
    LDownloadThread.DownloadStream := TFileStream.Create(AFileName, fmCreate);
    LDownloadThread.DownloadStream.Position := 0;
    LDownloadThread.OnReceiveData := AOnReceiveData;
    LDownloadThread.OnComplete := AOnComplete;
    //LDownloadThread.OnError :=
    LDownloadThread.OnBeforeStart := AOnBeforeStart;
    LDownloadThread.Start;
  finally
    result := LDownloadThread;
  end;
end;


(*  *)
constructor TYouTubeStreams.Create();
begin
  inherited Create;
end;

destructor TYouTubeStreams.Destroy;
begin
  inherited Destroy;
end;

(*  *)
constructor TYouTubeClient.Create(AURI: string);
begin
  if not TYouTubeURL.ValidateUrl(AURI, FURI) then
    raise Exception.Create('YouTube URL not find');

  inherited Create;
  FEncoding := TEncoding.UTF8;

  FYouTubeStreams := TYouTubeStreams.Create;
  FVideInfo := TYouTubeClient.GetVideoInfo(FURI.ToString);
  ParseStreams;
  FYouTubeThumbnail := TYouTubeThumbnail.Create(GetThumbnailUrl);
end;

destructor TYouTubeClient.Destroy;
begin
  if assigned(FYouTubeThumbnail) then
    FreeAndNil(FYouTubeThumbnail);
    
  if assigned(FYouTubeStreams) then
    FreeAndNil(FYouTubeStreams);

  if assigned(FVideInfo) then
    FreeAndNil(FVideInfo);
  inherited Destroy;
end;

procedure TYouTubeClient.ParseStreams;
  var
  LStreamsPropertyArr: TArray<string>;
  i : integer;
begin
  setLength(LStreamsPropertyArr, 0);
  LStreamsPropertyArr := Self.GetFmtStreamMap.Split([',']);
  for I := Low(LStreamsPropertyArr) to High(LStreamsPropertyArr) do
  begin
    FYouTubeStreams.Add(TYouTubeMainStream.Create(Self, LStreamsPropertyArr[i]));
  end;

  setLength(LStreamsPropertyArr, 0);
  LStreamsPropertyArr :=  Self.GetAdaptiveFmts.Split([',']);
  for I := Low(LStreamsPropertyArr) to High(LStreamsPropertyArr) do
  begin
    FYouTubeStreams.Add(TYouTubeAdaptiveStream.Create(Self, LStreamsPropertyArr[i]));
  end;
end;

function TYouTubeClient.GetTitle: string;
var
  LJSONValue : TJSONValue;
begin
  if FVideInfo.TryGetValue<TJSONValue>('title', LJSONValue) then
    result := LJSONValue.Value;
end;

function TYouTubeClient.GetKeywords: TArray<string>;
var
  LJSONValue : TJSONValue;
begin
  if FVideInfo.TryGetValue<TJSONValue>('tags', LJSONValue) then
    result := LJSONValue.Value.Split([',']);
end;

function TYouTubeClient.GetVideoID: string;
var
  LJSONValue : TJSONValue;
begin
  if FVideInfo.TryGetValue<TJSONValue>('video_id', LJSONValue) then
    result := LJSONValue.Value;
end;

function TYouTubeClient.GetAuthor: string;
var
  LJSONValue : TJSONValue;
begin
  if FVideInfo.TryGetValue<TJSONValue>('author', LJSONValue) then
    result := LJSONValue.Value;
end;

function TYouTubeClient.GetFmtStreamMap: string;
var
  LJSONValue : TJSONValue;
begin
  if FVideInfo.TryGetValue<TJSONValue>('url_encoded_fmt_stream_map', LJSONValue) then
    result := LJSONValue.Value;
end;

function TYouTubeClient.GetAdaptiveFmts: string;
var
  LJSONValue : TJSONValue;
begin
  if FVideInfo.TryGetValue<TJSONValue>('adaptive_fmts', LJSONValue) then
    result := LJSONValue.Value;
end;

function TYouTubeClient.GetThumbnailUrl: TURI;
var
  LJSONValue : TJSONValue;
begin
  if FVideInfo.TryGetValue<TJSONValue>('thumbnails.default', LJSONValue) then
    result := TURI.Create(TNetEncoding.URL.Decode(LJSONValue.Value));
end;

function TYouTubeClient.GetLengthSeconds: integer;
var
  LJSONValue : TJSONValue;
begin
  if FVideInfo.TryGetValue<TJSONValue>('duration', LJSONValue) then
    result := TJSONNumber(LJSONValue).AsInt;
end;

class function TYouTubeClient.DownloadWebPage(const AURL: string): string;
var
  LHTTPClient       : THTTPClient;
  LResponse         : IHTTPResponse;
  LRequest          : IHTTPRequest;
  LResponseContent  : TStringStream;
begin
  LResponseContent := TStringStream.Create('', TEncoding.UTF8);
  try
    LHTTPClient:=THTTPClient.Create;
    try
      LHTTPClient.ContentType:='application/timestamp-query';
      LRequest := LHTTPClient.GetRequest('GET', TURI.Create(AURL));
      LRequest.AddHeader('Cookie', 'PREF=f1=50000000&hl=en');
      LResponse := IHTTPResponse(LHTTPClient.Execute(LRequest, LResponseContent));
      if (((LResponse.StatusCode >= 200) and (LResponse.StatusCode < 300)) or (LResponse.StatusCode = 304))  then
      begin
        result := LResponseContent.DataString;
      end else
        raise Exception.CreateFmt('Error Request Content: %s [%d - %s]',[AURL, LResponse.StatusCode, LResponse.StatusText]);
    finally
      FreeAndNil(LHTTPClient);
    end;
  finally
    FreeAndNil(LResponseContent);
  end;
end;

class function TYouTubeClient.ParseYTPlayer(AResponseData: string): TJSONObject;
const
    cYTPLAYER_CONFIG_RE = ';ytplayer\.config\s*=\s*({.+?});ytplayer';
    cYTPLAYER_CONFIG1_RE = ';ytplayer\.config\s*=\s*({.+?});';
var
    LConfigObject : TJSONObject;
    LMatch        : TMatch;
    LConfigStr    : string;
begin
    try
      LConfigObject := TJSONObject.Create;
      try
        LMatch := TRegEx.Match(AResponseData, cYTPLAYER_CONFIG1_RE, [roIgnoreCase]);
        if LMatch.Groups.Count >= 2 then
        begin
          LConfigStr := LMatch.Groups.Item[1].Value;
          LConfigStr := System.NetEncoding.TNetEncoding.HTML.Decode(LConfigStr.Trim);
        end else
        begin
          LMatch := TRegEx.Match(AResponseData, cYTPLAYER_CONFIG_RE, [roIgnoreCase]);
          if LMatch.Groups.Count >= 2 then
          begin
            LConfigStr := LMatch.Groups.Item[1].Value;
            LConfigStr := System.NetEncoding.TNetEncoding.HTML.Decode(LConfigStr.Trim);
          end;
        end;
        LConfigObject.Parse(TEncoding.UTF8.GetBytes(LConfigStr), 0, true);
      finally
        result := LConfigObject.Clone as TJSONObject;
        FreeAndNil(LConfigObject);
      end;
    except  end;
end;

class function TYouTubeClient.ParseEmbeded(const AUrl: string): TJSONObject;
const
  cASSETS_RE  = '"assets":.+?"js":\s*("(?P<url>[^"]+)")';
  cPLAYER_RE =  '"url"\s*:\s*("(?P<url>[^"]+)")';
var
  LYouTebeUrl : TYouTubeUrl;
  LVideoInfo  : TJSONObject;
  LJSONObject : TJSONObject;
  LRegEx  : TRegEx;
  LEmbedUrl : string;
  LEmbedPage : string;
  LVideInfoUrl : TURI;
  LVideoInfoPage : string;
  LStsValue : string;
  LVideoInfoArr : TArray<string>;
  I: Integer;
  LPair : TPair<string, string>;
  LAssetsUrl : string;
  LPlayerUrl : string;
  LPlayerConfig : string;
begin
  LYouTebeUrl := TYouTubeUrl.Create(AUrl);
  LEmbedUrl := LYouTebeUrl.GetEmbedUrl;
  LEmbedPage := TYouTubeClient.DownloadWebPage(LEmbedUrl);

  // GET PLAYER CONFIG
  LRegEx := TRegEx.Create('yt.setConfig\(\{\s*''PLAYER_CONFIG'':\s*(?P<PLAYER_CONFIG>{.*?})\s*}\);');
  if LRegEx.IsMatch(LEmbedPage) then
  begin
    LPlayerConfig := LRegEx.Match(LEmbedPage).Groups['PLAYER_CONFIG'].Value;
    LRegEx := TRegEx.Create(cASSETS_RE);
    if LRegEx.isMatch(LPlayerConfig) then
    begin
      LAssetsUrl := TNetEncoding.URL.Decode(LRegEx.Match(LPlayerConfig).Groups['url'].Value);
    end;
    LRegEx := TRegEx.Create(cPLAYER_RE);
    if LRegEx.isMatch(LPlayerConfig) then
    begin
      LPlayerUrl :=  TNetEncoding.URL.Decode(LRegEx.Match(LPlayerConfig).Groups['url'].Value);
    end;
  end;

  // Get Sts Value
  LStsValue := '';
  LRegEx := TRegEx.Create('"sts"\s*:\s*(?P<value>\d+)');
  if LRegEx.IsMatch(LEmbedPage) then
  begin
    LStsValue := LRegEx.Match(LEmbedPage).Groups['value'].Value;
  end;

  // Create Request
  LVideInfoUrl := TURI.Create(Format('%swww.youtube.com/get_video_info',[LYouTebeUrl.FProtocol]));
  LVideInfoUrl.AddParameter('video_id', LYouTebeUrl.VideoID);
  LVideInfoUrl.AddParameter('eurl', 'https://youtube.googleapis.com/v/' + LYouTebeUrl.VideoID);
  LVideInfoUrl.AddParameter('sts', LStsValue);

  // Get  get_video_info
  LVideoInfoPage := DownloadWebPage(LVideInfoUrl.ToString);

  // Split Response Values
  LVideoInfoArr := LVideoInfoPage.Split(['&']);

  LJSONObject  := TJSONObject.Create;
  try
    LVideoInfo  := TJSONObject.Create;
    try
      for I := 0 to Length(LVideoInfoArr) - 1 do
      begin
        try
          LPair := TPair<string, string>.Create(LVideoInfoArr[i].Split(['='])[0], System.NetEncoding.TNetEncoding.URL.Decode(LVideoInfoArr[i].Split(['='])[1]));
          if SameText(LPair.Value, 'true') then
            LVideoInfo.AddPair(TJSONPair.Create(LPair.Key, TJSONTrue.Create()))
          else if SameText(LPair.Value, 'false') then
             LVideoInfo.AddPair(TJSONPair.Create(LPair.Key, TJSONFalse.Create()))
          else
            LVideoInfo.AddPair(TJSONPair.Create(LPair.Key, TJSONString.Create(LPair.Value)));
        except
          ;
        end;
      end;
    finally
      LJSONObject.AddPair('args', LVideoInfo)
    end;

    if not LAssetsUrl.IsEmpty then
     LJSONObject.AddPair('assets', TJSONObject.Create(TJSONpair.Create('js', LAssetsUrl)));
    if not LPlayerUrl.IsEmpty then
     LJSONObject.AddPair('url', TJSONString.Create(LPlayerUrl));

    result := LJSONObject.Clone as TJSONObject;
  finally
    FreeAndNil(LJSONObject);
  end;
end;

class function TYouTubeClient.GetVideoTitle(const AUrl: string): string;
var
  LYouTubeClient : TYouTubeClient;
begin
  LYouTubeClient := TYouTubeClient.Create(AUrl);
  try
    result := LYouTubeClient.Title;
  finally
    FreeAndNil(LYouTubeClient);
  end;
end;

class function TYouTubeClient.GetDesriptionHTML(const AWebPage: string): string;
var
   LRegEx : TRegEx;
   LDescription : string;
begin

  if AWebPage.Contains('eow-description') then
  begin
    LDescription := THtmlHelper.GetElementById('eow-description', AWebPage);
     (*

        video_description = get_element_by_id("eow-description", video_webpage)
        if video_description:
            video_description = re.sub(r'''(?x)
                <a\s+
                    (?:[a-zA-Z-]+="[^"]*"\s+)*?
                    (?:title|href)="([^"]+)"\s+
                    (?:[a-zA-Z-]+="[^"]*"\s+)*?
                    class="[^"]*"[^>]*>
                [^<]+\.{3}\s*
                </a>
            ''', r'\1', video_description)
            video_description = clean_html(video_description)

     *)
  end else
  begin
    LRegEx := TRegEx.Create('<meta name="description" content="(?P<description>[^"]+)"');
    if LRegEx.isMatch(AWebPage) then
    begin
      result :=  System.NetEncoding.THTMLEncoding.HTML.Decode(LRegEx.Match(AWebPage).Groups['description'].Value);
    end;
  end;
end;

class function TYouTubeClient.GetDatePublishedHTML(const AWebPage: string): string;
var
   LRegEx : TRegEx;
begin
  LRegEx := TRegEx.Create('<meta itemprop="datePublished" content="(?P<datePublished>[^"]+)"');
  if LRegEx.isMatch(AWebPage) then
  begin
    result := LRegEx.Match(AWebPage).Groups['datePublished'].Value;
  end;
end;

class function TYouTubeClient.GetTitleHTML(const AWebPage: string): string;
var
   LRegEx : TRegEx;
   LTitle : string;
begin
  if AWebPage.Contains('eow-title') then
  begin
    LTitle := THtmlHelper.GetElementById('eow-title', AWebPage);
  end else
  begin
    LRegEx := TRegEx.Create('<meta name="name" content="(?P<title>[^"]+)"');
    if LRegEx.isMatch(AWebPage) then
    begin
      result := System.NetEncoding.THTMLEncoding.HTML.Decode(LRegEx.Match(AWebPage).Groups['title'].Value);
    end;
  end;
end;

class function TYouTubeClient.GetJSPlayerUrlHTML(const AWebPage: string): string;
const
  cASSETS_RE  = '"assets":.+?"js":\s*("(?P<assets_url>[^"]+)")';
  cPLAYER_RE =  'ytplayer\.config.*?"url"\s*:\s*("(?P<ytplayer_url>[^"]*)")';

  cPLAYER_URL_RE= '(?P<URL>(?:https|http):(?:\/|\\)+([a-zA-Z0-9\-\.\/\\]+)((?:player-([^/]+)(?:\/|\\)watch_as3)\.swf|(?:player-([^/]+)(?:\/|\\)base)\.js))';
var
   LRegEx     : TRegEx;
   LPlayerUrl : string;
   LAssetsUrl : string;
begin
  result := '';

  LRegEx := TRegEx.Create(cASSETS_RE);
  if LRegEx.isMatch(AWebPage) then
  begin
    LAssetsUrl := TNetEncoding.URL.Decode(LRegEx.Match(AWebPage).Groups['assets_url'].Value);
  end;

  LRegEx := TRegEx.Create(cPLAYER_RE);
  if LRegEx.isMatch(AWebPage) then
  begin
    LPlayerUrl :=  TNetEncoding.URL.Decode(LRegEx.Match(AWebPage).Groups['ytplayer_url'].Value);
  end;

  if LPlayerUrl.IsEmpty then
  begin
    LRegEx := TRegEx.Create(cPLAYER_URL_RE);
    if LRegEx.isMatch(AWebPage) then
    begin
      LPlayerUrl :=  TNetEncoding.URL.Decode(LRegEx.Match(AWebPage).Groups['URL'].Value);
    end;
  end;
  LAssetsUrl := StringReplace(LAssetsUrl, '\/', '/', [rfReplaceAll, rfIgnoreCase]);
  LPlayerUrl := StringReplace(LPlayerUrl, '\/', '/', [rfReplaceAll, rfIgnoreCase]);

  if not LAssetsUrl.IsEmpty and not LPlayerUrl.IsEmpty then
    result := TURI.PathRelativeToAbs(LAssetsUrl, TURI.Create(LPlayerUrl));
end;


class function TYouTubeClient.BuildDecipherScript(const AJSPlayerUrl: string): string;
const
  cPLAYERPARSE_RE= '.*?-(?P<id>[a-zA-Z0-9_-]+)(?:/watch_as3|/html5player(?:-new)?|/base)?\.(?P<ext>[a-z]+)$"';
  cJSPLAYER1_RE = 'html5player-([^/]+?)(?:/html5player(?:-new)?)?\.js';
  cJSPLAYER_RE = '(?:www|player)-([^/]+)/base\.js';
  cSWFPLAYER_RE = '-(.+?)(?:/watch_as3)?\.swf';
var
  LRegEx      : TRegEx;
  LPlayerID   : string;
  LPlayerExt  : string;
begin
    LRegEx := TRegEx.Create(cPLAYERPARSE_RE);
    if LRegEx.isMatch(result) then
    begin
      //LPlayerUrl := LRegEx.Match(result).Groups['id'].Value;
      //LPlayerUrl := LRegEx.Match(result).Groups['ext'].Value;
    end;
end;

class function TYouTubeClient.GetVideoInfo(const AUrl: string): TJSONObject;
var
  LJSONObject         : TJSONObject;
  LVideoInfo          : TJSONObject;
  LThumbnails         : TJSONObject;
  LJSONValue          : TJSONValue;
  LWebPage            : string;
  LValueStr           : string;
  LUrlValueStr        : string;
  LAgeGate            : boolean;
begin
  LVideoInfo := nil;
  LAgeGate := false;
  LValueStr := '';
  LWebPage := '';

  try
    LWebPage := TYouTubeClient.DownloadWebPage(AURL);
  except end;

  if TYouTubeURL.Create(AURL).isEmbed or LWebPage.IsEmpty then
  begin
    LVideoInfo := ParseEmbeded(AURL);
  end else
  if LWebPage.Contains('player-age-gate-content">') then
   begin
      LAgeGate := true;
      LVideoInfo := ParseEmbeded(AURL);
   end else
      LVideoInfo := ParseYTPlayer(LWebPage);
  try
    LJSONObject := TJSONObject.Create;
    try
      // maps LVideoInfo to result JSONObject

      // player_url
      if LVideoInfo.TryGetValue<TJSONValue>('url', LJSONValue) then
      begin
        LUrlValueStr := LJSONValue.Value;
      end;
      if LUrlValueStr.IsEmpty then
        LUrlValueStr:= 'https://www.youtube.com/';
      if LVideoInfo.TryGetValue<TJSONValue>('assets.js', LJSONValue) then
      begin
        if not LJSONValue.Value.IsEmpty and not LUrlValueStr.IsEmpty then
        begin
          try
            LValueStr := TURI.PathRelativeToAbs(LJSONValue.Value.Replace('\/', '/'), TURI.Create(LUrlValueStr.Replace('\/', '/')));
          except    end;
          if not LValueStr.IsEmpty then
            LJSONObject.AddPair(TJSONPair.Create('player_url', TJSONString.Create(LValueStr)));
        end;
      end;

      // url_encoded_fmt_stream_map
      if LVideoInfo.TryGetValue<TJSONValue>('args.url_encoded_fmt_stream_map', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('url_encoded_fmt_stream_map', TJSONString.Create(LJSONValue.Value)));
      // adaptive_fmts
      if LVideoInfo.TryGetValue<TJSONValue>('args.adaptive_fmts', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('adaptive_fmts', TJSONString.Create(LJSONValue.Value)));      
      // title
      if LVideoInfo.TryGetValue<TJSONValue>('args.title', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('title', TJSONString.Create(LJSONValue.Value)));
      // video_id
      if LVideoInfo.TryGetValue<TJSONValue>('args.video_id', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('video_id', TJSONString.Create(LJSONValue.Value)));
      // view_count
      if LVideoInfo.TryGetValue<TJSONValue>('args.view_count', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('view_count', TJSONNumber.Create(StrToInt(LJSONValue.Value))));
      // tags
      if LVideoInfo.TryGetValue<TJSONValue>('args.keywords', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('tags', TJSONString.Create(LJSONValue.Value)));
      // author & author_url
      if LVideoInfo.TryGetValue<TJSONValue>('args.author', LJSONValue) then
      begin
        LJSONObject.AddPair(TJSONPair.Create('author', TJSONString.Create(LJSONValue.Value)));
        LJSONObject.AddPair(TJSONPair.Create('author_url', TJSONString.Create(Format('https://www.youtube.com/user/%s', [LJSONValue.Value]))));
      end;
      // duration
      if LVideoInfo.TryGetValue<TJSONValue>('args.length_seconds', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('duration', TJSONNumber.Create(StrToInt(LJSONValue.Value))));
      // url
      if LVideoInfo.TryGetValue<TJSONValue>('args.loaderUrl', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('url', TJSONString.Create(LJSONValue.Value)));
      // thumbnails
      LThumbnails := TJSONObject.Create;
      try
        if LVideoInfo.TryGetValue<TJSONValue>('args.iurlsd', LJSONValue) then
          LThumbnails.AddPair(TJSONPair.Create('sd', TJSONString.Create(LJSONValue.Value)));
        if LVideoInfo.TryGetValue<TJSONValue>('args.thumbnail_url', LJSONValue) then
          LThumbnails.AddPair(TJSONPair.Create('default', TJSONString.Create(LJSONValue.Value)));
        if LVideoInfo.TryGetValue<TJSONValue>('args.iurlhq', LJSONValue) then
          LThumbnails.AddPair(TJSONPair.Create('hq', TJSONString.Create(LJSONValue.Value)));
        if not LThumbnails.TryGetValue<TJSONValue>('hq', LJSONValue) then
        begin
          if LVideoInfo.TryGetValue<TJSONValue>('args.iurl', LJSONValue) then
            LThumbnails.AddPair(TJSONPair.Create('hq', TJSONString.Create(LJSONValue.Value)));
        end;
        if LVideoInfo.TryGetValue<TJSONValue>('args.iurlmaxres', LJSONValue) then
          LThumbnails.AddPair(TJSONPair.Create('max_res', TJSONString.Create(LJSONValue.Value)));
      finally
        LJSONObject.AddPair(TJSONPair.Create('thumbnails', LThumbnails));
      end;

      // provider_name
      LJSONObject.AddPair(TJSONPair.Create('provider_name', TJSONString.Create('YouTube')));
      // provider_url
      LJSONObject.AddPair(TJSONPair.Create('provider_url', TJSONString.Create('https://www.youtube.com')));
      // uri
      if not LJSONObject.TryGetValue<TJSONValue>('url', LJSONValue) then
        LJSONObject.AddPair(TJSONPair.Create('url', TJSONString.Create(TYouTubeURL.Create(AURL).ToString)));
      // title
      if not LJSONObject.TryGetValue<TJSONValue>('title', LJSONValue) then
      begin
        LValueStr := TYouTubeClient.GetTitleHTML(LWebPage);
        LJSONObject.AddPair(TJSONPair.Create('title', TJSONString.Create(LValueStr)));
      end;
      // player_url
      if not LJSONObject.TryGetValue<TJSONValue>('player_url', LJSONValue) then
      begin
        LValueStr := TYouTubeClient.GetJSPlayerUrlHTML(LWebPage);
        LJSONObject.AddPair(TJSONPair.Create('player_url', TJSONString.Create(LValueStr)));
      end;
      // upload_date
      LValueStr := TYouTubeClient.GetDatePublishedHTML(LWebPage);
      LJSONObject.AddPair(TJSONPair.Create('upload_date', TJSONString.Create(LValueStr)));
      // description
      //LValueStr := TYouTubeClient.GetDesriptionHTML(LWebPage);
      LValueStr := '';
      LJSONObject.AddPair(TJSONPair.Create('description', TJSONString.Create(LValueStr)));
      // license
      LJSONObject.AddPair(TJSONPair.Create('license', TJSONString.Create('')));
      // age_gate
      if LAgeGate then
        LJSONObject.AddPair(TJSONPair.Create('age_gate', TJSONTrue.Create()))
      else LJSONObject.AddPair(TJSONPair.Create('age_gate', TJSONFalse.Create()));


      result := LJSONObject.Clone as TJSONObject;
    finally
      FreeAndNil(LJSONObject);
    end;

            //use_cipher_signature
  finally
    if assigned(LVideoInfo) then
      FreeAndNil(LVideoInfo);
  end;
end;






end.
