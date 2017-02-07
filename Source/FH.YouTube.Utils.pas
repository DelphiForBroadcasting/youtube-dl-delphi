unit FH.YouTube.Utils;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Json,
  System.Generics.Collections,
  System.RegularExpressions,
  System.NetEncoding,
  System.Net.HttpClient,
  System.Net.Socket,
  System.Net.URLClient;


type
  THtmlHelper = record
  private

  public
    class function GetElementById(const AID: string; const AHtml: string): string; static;
    class function GetElementByClass(const AClassName: string; const AHtml: string): string; static;
    class function GetElementByAttribute(const AAttribute: string; const AValue: string; AHtml: string): string; static;
  end;


type
  TYouTubeMediaType = record
  private
    FMediaType  : string;
    FExt        : string;
    FName       : string;
  public
    class function Create(AMediaType: string): TYouTubeMediaType; static;
    property Value: string read FMediaType;
    property Name: string read FName;
    property Ext: string read FExt;
  end;


type
  TYouTubeITag = record
  private
    FITag       : integer;
    FName       : string;
    FExt        : string;

    FVCodec     : string;
    FWidth      : integer;
    FHeight     : integer;
    FFPS        : integer;

    FACodec     : string;
    FABitrate   : integer;
  public
    class function Create(AITag: integer): TYouTubeITag; static;
    property Value: integer read FITag;
  end;

type
  TDownloadReceive = reference to procedure(const Sender: TObject; ASpeed: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
  TDownloadError = reference to procedure(const Sender: TObject; AMessage: string);
  TDownloadComplete = reference to procedure(const Sender: TObject);
  TDownloadBeforeStart = reference to procedure(const Sender: TObject);

  TDownloadHTTP = class(TThread)
  private
    FOnReceiveData    : TDownloadReceive;
    FOnError          : TDownloadError;
    FOnComplete       : TDownloadComplete;
    FOnBeforeStart    : TDownloadBeforeStart;
    FAbort            : boolean;

    FURL              : string;

    [weak]FDownloadStream   : TStream;
    FContentSize      : int64;

    FStartTime        : Cardinal;
    FEndTime          : Cardinal;
    FAverageSpeed     : Cardinal;
  protected
    procedure DoReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure DoDownload;
    procedure DoComplete;
    procedure DoError(AMessage: string);
    procedure DoBeforeStart();
    procedure TerminatedSet; override;
    procedure Execute; override;
  public
    constructor Create(const AURL: string);
    destructor Destroy; override;
    procedure Terminate;
    property DownloadStream   : TStream read FDownloadStream write FDownloadStream;
    procedure Abort;
    property OnReceiveData    : TDownloadReceive write FOnReceiveData;
    property OnError          : TDownloadError write FOnError;
    property OnComplete       : TDownloadComplete write FOnComplete;
    property OnBeforeStart    : TDownloadBeforeStart write FOnBeforeStart;
    property StartTime        : Cardinal read FStartTime;
    property EndTime          : Cardinal read FEndTime;
    property AverageSpeed     : Cardinal read FAverageSpeed;
    property ContentSize      : Int64 read FContentSize;
  end;

type
  TYouTubeThumbnail = class
  private
    FURL   : TURI;
  public
    constructor Create(AThumbnailUrl : TURI); overload;
    destructor Destroy; override;
    property URL: TURI read FURL; 
  end;


implementation

class function THtmlHelper.GetElementById(const AID: string; const AHtml: string): string;
begin
  result :=  THtmlHelper.GetElementByAttribute('id', AID, AHtml);
end;


class function THtmlHelper.GetElementByClass(const AClassName: string; const AHtml: string): string;
begin
  result := THtmlHelper.GetElementByAttribute('class', Format('[^\''"]*\b%s\b[^\''"]*' , [TRegEx.Escape(AClassName)]), AHtml);
end;

class function THtmlHelper.GetElementByAttribute(const AAttribute: string; const AValue: string; AHtml: string): string;
var
  LRegEx : TRegEx;
  LContent  : string;
  LSearchPattern : string;
begin
  LSearchPattern := Format('(?xs)'+
        '<([a-zA-Z0-9:._-]+)'+
         '(?:\s+[a-zA-Z0-9:._-]+(?:=[a-zA-Z0-9:._-]*|="[^"]*"|=''[^'']*''))*?'+
         '\s+%s=[''"]?%s[''"]?'+
         '(?:\s+[a-zA-Z0-9:._-]+(?:=[a-zA-Z0-9:._-]*|="[^"]*"|=''[^'']*''))*?'+
        '\s*>'+
        '(?P<content>.*?)'+
        '</\1>', [LRegEx.Escape(AAttribute), LRegEx.Escape(AValue)]);
  LRegEx := TRegEx.Create(LSearchPattern);
  if LRegEx.IsMatch(AHtml) then
  begin
    LContent := LRegEx.Match(AHtml).Groups['content'].Value;
    LContent := LContent.Trim(['"','''', ' ']);
    LContent := TNetEncoding.HTML.Decode(LContent);
  end;
  result := LContent;
end;

(* --- *)
class function TYouTubeMediaType.Create(AMediaType: String): TYouTubeMediaType;
begin
  result.FMediaType := AMediaType;
  {$REGION 'MediaType'}
  if Pos('video/mp4', AMediaType) > 0 then
  begin
    result.FName := 'MPEG-4';
    result.FExt := '.mp4';
  end else
  if Pos('video/x-flv', AMediaType) > 0 then
  begin
    result.FName := 'Flash';
    result.FExt := '.flv';
  end else
  if Pos('application/x-mpegURL', AMediaType) > 0 then
  begin
    result.FName := 'iPhone Index';
    result.FExt := '.m3u8';
  end else
  if Pos('video/MP2T', AMediaType) > 0 then
  begin
    result.FName := 'iPhone Segment';
    result.FExt := '.ts';
  end else
  if Pos('video/3gpp', AMediaType) > 0 then
  begin
    result.FName := '3GP Mobile';
    result.FExt := '.3gp';
  end else
  if Pos('video/quicktime', AMediaType) > 0 then
  begin
    result.FName := 'QuickTime';
    result.FExt := '.mov';
  end else
  if Pos('video/x-msvideo', AMediaType) > 0 then
  begin
    result.FName := 'A/V Interleave';
    result.FExt := '.avi';
  end else
  if Pos('video/x-ms-wmv', AMediaType) > 0 then
  begin
    result.FName := 'Windows Media';
    result.FExt := '.wmv';
  end else
  if Pos('video/webm', AMediaType) > 0 then
  begin
    result.FName := 'Google WebM';
    result.FExt := '.webm';
  end else
  if Pos('video/mpeg', AMediaType) > 0 then
  begin
    result.FName := 'MPEG-1';
    result.FExt := '.mpg';
  end else
  if Pos('video/ogg', AMediaType) > 0 then
  begin
    result.FName := 'Ogg Theora';
    result.FExt := '.ogg';
  end else
  if Pos('video/3gpp2', AMediaType) > 0 then
  begin
    result.FName := '3GP2 Mobile';
    result.FExt := '.3g2';
  end;
  {$ENDREGION}
end;

(*  *)
class function TYouTubeITag.Create(AITag: integer): TYouTubeITag;
begin
  result.FITag := AITag;
  {$REGION 'ITAG'}
(*

_formats = {
        '5': {'ext': 'flv', 'width': 400, 'height': 240, 'acodec': 'mp3', 'abr': 64, 'vcodec': 'h263'},
        '6': {'ext': 'flv', 'width': 450, 'height': 270, 'acodec': 'mp3', 'abr': 64, 'vcodec': 'h263'},
        '13': {'ext': '3gp', 'acodec': 'aac', 'vcodec': 'mp4v'},
        '17': {'ext': '3gp', 'width': 176, 'height': 144, 'acodec': 'aac', 'abr': 24, 'vcodec': 'mp4v'},
        '18': {'ext': 'mp4', 'width': 640, 'height': 360, 'acodec': 'aac', 'abr': 96, 'vcodec': 'h264'},
        '22': {'ext': 'mp4', 'width': 1280, 'height': 720, 'acodec': 'aac', 'abr': 192, 'vcodec': 'h264'},
        '34': {'ext': 'flv', 'width': 640, 'height': 360, 'acodec': 'aac', 'abr': 128, 'vcodec': 'h264'},
        '35': {'ext': 'flv', 'width': 854, 'height': 480, 'acodec': 'aac', 'abr': 128, 'vcodec': 'h264'},
        # itag 36 videos are either 320x180 (BaW_jenozKc) or 320x240 (__2ABJjxzNo), abr varies as well
        '36': {'ext': '3gp', 'width': 320, 'acodec': 'aac', 'vcodec': 'mp4v'},
        '37': {'ext': 'mp4', 'width': 1920, 'height': 1080, 'acodec': 'aac', 'abr': 192, 'vcodec': 'h264'},
        '38': {'ext': 'mp4', 'width': 4096, 'height': 3072, 'acodec': 'aac', 'abr': 192, 'vcodec': 'h264'},
        '43': {'ext': 'webm', 'width': 640, 'height': 360, 'acodec': 'vorbis', 'abr': 128, 'vcodec': 'vp8'},
        '44': {'ext': 'webm', 'width': 854, 'height': 480, 'acodec': 'vorbis', 'abr': 128, 'vcodec': 'vp8'},
        '45': {'ext': 'webm', 'width': 1280, 'height': 720, 'acodec': 'vorbis', 'abr': 192, 'vcodec': 'vp8'},
        '46': {'ext': 'webm', 'width': 1920, 'height': 1080, 'acodec': 'vorbis', 'abr': 192, 'vcodec': 'vp8'},
        '59': {'ext': 'mp4', 'width': 854, 'height': 480, 'acodec': 'aac', 'abr': 128, 'vcodec': 'h264'},
        '78': {'ext': 'mp4', 'width': 854, 'height': 480, 'acodec': 'aac', 'abr': 128, 'vcodec': 'h264'},


        # 3D videos
        '82': {'ext': 'mp4', 'height': 360, 'format_note': '3D', 'acodec': 'aac', 'abr': 128, 'vcodec': 'h264', 'preference': -20},
        '83': {'ext': 'mp4', 'height': 480, 'format_note': '3D', 'acodec': 'aac', 'abr': 128, 'vcodec': 'h264', 'preference': -20},
        '84': {'ext': 'mp4', 'height': 720, 'format_note': '3D', 'acodec': 'aac', 'abr': 192, 'vcodec': 'h264', 'preference': -20},
        '85': {'ext': 'mp4', 'height': 1080, 'format_note': '3D', 'acodec': 'aac', 'abr': 192, 'vcodec': 'h264', 'preference': -20},
        '100': {'ext': 'webm', 'height': 360, 'format_note': '3D', 'acodec': 'vorbis', 'abr': 128, 'vcodec': 'vp8', 'preference': -20},
        '101': {'ext': 'webm', 'height': 480, 'format_note': '3D', 'acodec': 'vorbis', 'abr': 192, 'vcodec': 'vp8', 'preference': -20},
        '102': {'ext': 'webm', 'height': 720, 'format_note': '3D', 'acodec': 'vorbis', 'abr': 192, 'vcodec': 'vp8', 'preference': -20},

        # Apple HTTP Live Streaming
        '91': {'ext': 'mp4', 'height': 144, 'format_note': 'HLS', 'acodec': 'aac', 'abr': 48, 'vcodec': 'h264', 'preference': -10},
        '92': {'ext': 'mp4', 'height': 240, 'format_note': 'HLS', 'acodec': 'aac', 'abr': 48, 'vcodec': 'h264', 'preference': -10},
        '93': {'ext': 'mp4', 'height': 360, 'format_note': 'HLS', 'acodec': 'aac', 'abr': 128, 'vcodec': 'h264', 'preference': -10},
        '94': {'ext': 'mp4', 'height': 480, 'format_note': 'HLS', 'acodec': 'aac', 'abr': 128, 'vcodec': 'h264', 'preference': -10},
        '95': {'ext': 'mp4', 'height': 720, 'format_note': 'HLS', 'acodec': 'aac', 'abr': 256, 'vcodec': 'h264', 'preference': -10},
        '96': {'ext': 'mp4', 'height': 1080, 'format_note': 'HLS', 'acodec': 'aac', 'abr': 256, 'vcodec': 'h264', 'preference': -10},
        '132': {'ext': 'mp4', 'height': 240, 'format_note': 'HLS', 'acodec': 'aac', 'abr': 48, 'vcodec': 'h264', 'preference': -10},
        '151': {'ext': 'mp4', 'height': 72, 'format_note': 'HLS', 'acodec': 'aac', 'abr': 24, 'vcodec': 'h264', 'preference': -10},

        # DASH mp4 video
        '133': {'ext': 'mp4', 'height': 240, 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},
        '134': {'ext': 'mp4', 'height': 360, 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},
        '135': {'ext': 'mp4', 'height': 480, 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},
        '136': {'ext': 'mp4', 'height': 720, 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},
        '137': {'ext': 'mp4', 'height': 1080, 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},
        '138': {'ext': 'mp4', 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},  # Height can vary (https://github.com/rg3/youtube-dl/issues/4559)
        '160': {'ext': 'mp4', 'height': 144, 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},
        '264': {'ext': 'mp4', 'height': 1440, 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},
        '298': {'ext': 'mp4', 'height': 720, 'format_note': 'DASH video', 'vcodec': 'h264', 'fps': 60, 'preference': -40},
        '299': {'ext': 'mp4', 'height': 1080, 'format_note': 'DASH video', 'vcodec': 'h264', 'fps': 60, 'preference': -40},
        '266': {'ext': 'mp4', 'height': 2160, 'format_note': 'DASH video', 'vcodec': 'h264', 'preference': -40},

        # Dash mp4 audio
        '139': {'ext': 'm4a', 'format_note': 'DASH audio', 'acodec': 'aac', 'abr': 48, 'preference': -50, 'container': 'm4a_dash'},
        '140': {'ext': 'm4a', 'format_note': 'DASH audio', 'acodec': 'aac', 'abr': 128, 'preference': -50, 'container': 'm4a_dash'},
        '141': {'ext': 'm4a', 'format_note': 'DASH audio', 'acodec': 'aac', 'abr': 256, 'preference': -50, 'container': 'm4a_dash'},
        '256': {'ext': 'm4a', 'format_note': 'DASH audio', 'acodec': 'aac', 'preference': -50, 'container': 'm4a_dash'},
        '258': {'ext': 'm4a', 'format_note': 'DASH audio', 'acodec': 'aac', 'preference': -50, 'container': 'm4a_dash'},

        # Dash webm
        '167': {'ext': 'webm', 'height': 360, 'width': 640, 'format_note': 'DASH video', 'container': 'webm', 'vcodec': 'vp8', 'preference': -40},
        '168': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'container': 'webm', 'vcodec': 'vp8', 'preference': -40},
        '169': {'ext': 'webm', 'height': 720, 'width': 1280, 'format_note': 'DASH video', 'container': 'webm', 'vcodec': 'vp8', 'preference': -40},
        '170': {'ext': 'webm', 'height': 1080, 'width': 1920, 'format_note': 'DASH video', 'container': 'webm', 'vcodec': 'vp8', 'preference': -40},
        '218': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'container': 'webm', 'vcodec': 'vp8', 'preference': -40},
        '219': {'ext': 'webm', 'height': 480, 'width': 854, 'format_note': 'DASH video', 'container': 'webm', 'vcodec': 'vp8', 'preference': -40},
        '278': {'ext': 'webm', 'height': 144, 'format_note': 'DASH video', 'container': 'webm', 'vcodec': 'vp9', 'preference': -40},
        '242': {'ext': 'webm', 'height': 240, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '243': {'ext': 'webm', 'height': 360, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '244': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '245': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '246': {'ext': 'webm', 'height': 480, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '247': {'ext': 'webm', 'height': 720, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '248': {'ext': 'webm', 'height': 1080, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '271': {'ext': 'webm', 'height': 1440, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        # itag 272 videos are either 3840x2160 (e.g. RtoitU2A-3E) or 7680x4320 (sLprVF6d7Ug)
        '272': {'ext': 'webm', 'height': 2160, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '302': {'ext': 'webm', 'height': 720, 'format_note': 'DASH video', 'vcodec': 'vp9', 'fps': 60, 'preference': -40},
        '303': {'ext': 'webm', 'height': 1080, 'format_note': 'DASH video', 'vcodec': 'vp9', 'fps': 60, 'preference': -40},
        '308': {'ext': 'webm', 'height': 1440, 'format_note': 'DASH video', 'vcodec': 'vp9', 'fps': 60, 'preference': -40},
        '313': {'ext': 'webm', 'height': 2160, 'format_note': 'DASH video', 'vcodec': 'vp9', 'preference': -40},
        '315': {'ext': 'webm', 'height': 2160, 'format_note': 'DASH video', 'vcodec': 'vp9', 'fps': 60, 'preference': -40},

        # Dash webm audio
        '171': {'ext': 'webm', 'acodec': 'vorbis', 'format_note': 'DASH audio', 'abr': 128, 'preference': -50},
        '172': {'ext': 'webm', 'acodec': 'vorbis', 'format_note': 'DASH audio', 'abr': 256, 'preference': -50},

        # Dash webm audio with opus inside
        '249': {'ext': 'webm', 'format_note': 'DASH audio', 'acodec': 'opus', 'abr': 50, 'preference': -50},
        '250': {'ext': 'webm', 'format_note': 'DASH audio', 'acodec': 'opus', 'abr': 70, 'preference': -50},
        '251': {'ext': 'webm', 'format_note': 'DASH audio', 'acodec': 'opus', 'abr': 160, 'preference': -50},

        # RTMP (unnamed)
        '_rtmp': {'protocol': 'rtmp'},

Format 5: (Basic Youtube Default)
Download Video Format: flv
Video Resolution: 400 x 240
Video Frame Rate: 25 fps
Video Output Format: FLV1 H.263
Audio: Stereo, 22.05 KHz 64.0 Kbps
Audio Format: MP3 (MPEG Audio)

Format 6:
Download Video Format: flv
Video Resolution: 450 x 270
Format 13: (Mobile phones, iPod friendly)
Download Video Format: 3gp

Format 17:
Download Video Format: 3gp
Video Resolution: 176 x 144
Video Frame Rate: 12 fps
Video Output Format: MPEG-4
Audio: Mono 22.05 KHz 24.0 - 25.6 Kbps
Audio Format: AAC

Format 18: Medium Quality [360p]
Download Video Format: mp4
Video Resolution: 640 x 360
Video Frame Rate: 25 fps
Video Output Format: AVC (MPEG4 H.264)
Audio: Stereo, 44.1 KHz 96.0 - 100 Kbps
Audio Format: AAC

Format 22: HD High Quality [720p]
Download Video Format: mp4
Video Resolution: 1280 x 720
Video Frame Rate: 25 fps
Video Output Format: AVC (MPEG4 H.264)
Audio: Stereo, 44.1 KHz 192.0 - 201 Kbps
Audio Format: AAC

Format 34: [360p]
Download Video Format: flv
Video Resolution: 640 x 360
Format 35: [480p]
Download Video Format: flv
Video Resolution: 854 x 480

Format 36: [240p]
Download Video Format: 3gp
Video Resolution: 320 x 240
Video Frame Rate: 25 fps
Video Output Format: MPEG4 H.263
Audio: Mono, 22.05 KHz 32.0 - 34.2 Kbps
Audio Format: AAC

Format 37: HD High Quality [1080p]
Download Video Format: mp4
Video Resolution: 1920 x 1080
Format 38: HD High Quality [3072p]
Download Video Format: mp4
Video Resolution: 4096 x 3072

Format 43: Medium Quality [360p]
Download Video Format: webm
Video Resolution: 640 x 360
Video Frame Rate: 24.194 fps
Video Output Format: VP8
Audio: Stereo, 44.1 KHz 128 Kbps
Audio Format: Vorbis

Format 44: [480p]
Download Video Format: webm
Video Resolution: 854 x 480

Format 45: [720p]
Download Video Format: webm
Video Resolution: 1280 x 720

Format 46: [1080p]
Download Video Format: webm
Video Resolution: 1920 x 1080
3d Videos
These videos requires special medium to enjoy the depth of the video. These are also available in different flavours

Format 82:
Download Video Format: mp4
Video Resolution: 640 x 360 [3D]
Video Frame Rate: 25 fps
Video Output Format: AVC (MPEG4 H.264)
Audio: Stereo, 44.1 KHz 128 - 134 Kbps
Audio Format: AAC

Format 83:
Download Video Format: mp4
Video Resolution: 854 x 480 [3D]

Format 84:
Download Video Format: mp4
Video Resolution: 1280 x 720 [3D]
Video Frame Rate: 25 fps
Video Output Format: AVC (MPEG4 H.264)
Audio: Stereo, 44.1 KHz 192 - 201 Kbps
Audio Format: AAC

Format 85:
Download Video Format: mp4
Video Resolution: 1920 x 1080p [3D]

Format 100:
Download Video Format: webm
Video Resolution: 640 x 360 [3D]
Video Frame Rate: 24.194 fps
Video Output Format: VP8
Audio: Stereo, 44.1 KHz 128 Kbps
Audio Format: AAC

Format 101:
Download Video Format: webm
Video Resolution: 854 x 480 [3D]

Format 102:
Download Video Format: webm
Video Resolution: 1280 x 720 [3D]
Apple HTTP Live Streaming (HLS)

Format 92:
Download Video Format: mp4
Video Resolution: 320 x 240
Streaming Protocol: HLS

Format 93:
Download Video Format: mp4
Video Resolution: 640 x 360
Streaming Protocol: HLS

Format 94:
Download Video Format: mp4
Video Resolution: 854 x 480
Streaming Protocol: HLS
Format 95:

Download Video Format: mp4
Video Resolution: 1280 x 720
Streaming Protocol: HLS
Format 96:

Download Video Format: mp4
Video Resolution: 1920 x 1080
Streaming Protocol: HLS
Format 132:

Download Video Format: mp4
Video Resolution: 320 x 240
Streaming Protocol: HLS
Format 151:

Download Video Format: mp4
Video Resolution: * x 72
Streaming Protocol: HLS
DASH MP4 video

Format 133:

Download Video Format: mp4
Video Resolution: 320 x 240
Video Format: DASH video
Audio: none
Format 134:

Download Video Format: mp4
Video Resolution: 640 x 360
Video Format: DASH video
Audio: none
Format 135:

Download Video Format: mp4
Video Resolution: 854 x 480
Video Format: DASH video
Audio: none
Format 136:

Download Video Format: mp4
Video Resolution: 1280 x 720
Video Format: DASH video
Audio: none
Format 137:

Download Video Format: mp4
Video Resolution: 1920 x 1080
Video Format: DASH video
Audio: none
Format 138:

Download Video Format: mp4
Video Resolution: * x 2160 (not fix)
Video Format: DASH video
Audio: none
Format 160:

Download Video Format: mp4
Video Resolution: 176 x 144
Video Format: DASH video
Audio: none
Format 264:

Download Video Format: mp4
Video Resolution: 176 x 1440
Video Format: DASH video
Audio: none
Format 298:

Download Video Format: mp4
Video Resolution: 1280 x 720
Video Frame Rate: 60 fps
Video Format: DASH video H.264
Audio: none
Format 299:

Download Video Format: mp4
Video Resolution: 1920 x 1080
Video Frame Rate: 60 fps
Video Format: DASH video H.264
Audio: none
Format 266:

Download Video Format: mp4
Video Resolution: * x 2160
Video Frame Rate: 60 fps
Video Format: DASH video H.264
Audio: none
Dash MP4 Audio

Format 139:

Download Audio Format: m4a
Audio: Stereo, 44.1 KHz 48 Kbps
Audio Format: AAC [DASH audio]
Video: none
Format 140:

Download Audio Format: m4a
Audio: Stereo, 44.1 KHz 128 Kbps
Audio Format: AAC [DASH audio]
Video: none
Format 141:

Download Audio Format: m4a
Audio: Stereo, 44.1 KHz 256 Kbps
Audio Format: AAC [DASH audio]
Video: none
Dash WEBM Video

Format 167:

Download Video Format: webm
Video Resolution: 640 x 360
Video Format: VP8 DASH video
Audio: none
Format 168:

Download Video Format: webm
Video Resolution: 854 x 480
Video Format: VP8 DASH video
Audio: none
Format 169:

Download Video Format: webm
Video Resolution: 1280 x 720
Video Format: VP8 DASH video
Audio: none
Format 170:

Download Video Format: webm
Video Resolution: 1920 x 1080
Video Format: VP8 DASH video
Audio: none
Format 218:

Download Video Format: webm
Video Resolution: 854 x 480
Video Format: VP8 DASH video
Audio: none
Format 219:

Download Video Format: webm
Video Resolution: 854 x 480
Video Format: VP8 DASH video
Audio: none
Format 219:

Download Video Format: webm
Video Resolution: * x 144
Video Format: VP9 DASH video
Audio: none
Format 242:

Download Video Format: webm
Video Resolution: 320 x 240
Video Format: VP8 DASH video
Audio: none
Format 243:

Download Video Format: webm
Video Resolution: 640 x 360
Video Format: VP8 DASH video
Audio: none
Format 244:

Download Video Format: webm
Video Resolution: 854 x 480
Video Format: VP8 DASH video
Audio: none
Format 245:

Download Video Format: webm
Video Resolution: 854 x 480
Video Format: VP8 DASH video
Audio: none
Format 246:

Download Video Format: webm
Video Resolution: 854 x 480
Video Format: VP8 DASH video
Audio: none
Format 247:

Download Video Format: webm
Video Resolution: 1280 x 720
Video Format: VP8 DASH video
Audio: none
Format 248:

Download Video Format: webm
Video Resolution: 1920 x 1080
Video Format: VP8 DASH video
Audio: none
Format 271:

Download Video Format: webm
Video Resolution: 176 x 1440
Video Format: VP8 DASH video
Audio: none
Format 272:

Download Video Format: webm
Video Resolution: * x 2160
Video Format: VP8 DASH video
Audio: none
Format 302:

Download Video Format: webm
Video Resolution: * x 2160
Video Frame Rate: 60 fps
Video Format: VP9 DASH video
Audio: none
Format 303:

Download Video Format: webm
Video Resolution: 1920 x 1080
Video Frame Rate: 60 fps
Video Format: VP9 DASH video
Audio: none
Format 308:

Download Video Format: webm
Video Resolution: 176 x 1440
Video Frame Rate: 60 fps
Video Format: VP9 DASH video
Audio: none
Format 313:

Download Video Format: webm
Video Resolution: * x 2160
Video Frame Rate: 60 fps
Video Format: VP9 DASH video
Audio: none
Format 315:

Download Video Format: webm
Video Resolution: * x 2160
Video Frame Rate: 60 fps
Video Format: VP9 DASH video
Audio: none
Dash WEBM Audio

Format 171:

Download Audio Format: webm
Audio: Stereo, 44.1 KHz 128 Kbps
Audio Format: AAC [DASH audio]
Video: none
Format 172:

Download Audio Format: webm
Audio: Stereo, 44.1 KHz 256 Kbps
Audio Format: AAC [DASH audio]
Video: none
*)

  {$ENDREGION}
end;

{ TDownloadThread }

constructor TDownloadHTTP.Create(const AURL: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FURL := AURL;
  FAbort := false;
end;

destructor TDownloadHTTP.Destroy;
begin
  inherited;
end;

procedure TDownloadHTTP.Terminate;
begin
  inherited Terminate;
end;

procedure TDownloadHTTP.TerminatedSet;
begin
  FAbort := true;
end;

procedure TDownloadHTTP.Abort;
begin
  Self.Terminate;
end;

procedure TDownloadHTTP.DoDownload;
var
  LHttpClient: THTTPClient;
  LResponse: IHTTPResponse;
begin
  LHttpClient := THTTPClient.Create;
  try
    LResponse := LHttpClient.Head(FURL);
    FContentSize := LResponse.ContentLength;
    LResponse := nil;

    DoBeforeStart();
    LHttpClient.OnReceiveData := DoReceiveData;
    // Start the download process
    FStartTime := TThread.GetTickCount;
    LResponse := LHttpClient.Get(FURL, FDownloadStream);
    FEndTime := TThread.GetTickCount - FStartTime;
    FAverageSpeed := ((FContentSize*1000) div FEndTime);
    if (((LResponse.StatusCode >= 200) and (LResponse.StatusCode < 300)) or (LResponse.StatusCode = 304))  then
    begin
      DoComplete();
    end else
      raise Exception.CreateFmt('Error download file: %d - %s',[LResponse.StatusCode, LResponse.StatusText]);
  finally
    LHttpClient.Free;
  end;
end;


procedure TDownloadHTTP.DoComplete();
begin
  if Assigned(FOnComplete) then
    FOnComplete(Self);
end;

procedure TDownloadHTTP.DoError(AMessage: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, AMessage);
end;

procedure TDownloadHTTP.DoBeforeStart();
begin
  if Assigned(FOnBeforeStart) then
    FOnBeforeStart(Self);
end;

procedure TDownloadHTTP.Execute;
begin
  try
    if not assigned(FDownloadStream)then
      FDownloadStream := TMemoryStream.Create;
    try
      DoDownload();
    finally
      if assigned(FDownloadStream) then
        FreeAndNil(FDownloadStream);
    end;
  except
    on E:Exception do
      DoError(E.Message);
  end;
end;

procedure TDownloadHTTP.DoReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64;
  var Abort: Boolean);
var
  LTime: Cardinal;
  LSpeed: Integer;
begin
  if Assigned(FOnReceiveData) then
  begin
    LTime := GetTickCount - FStartTime;
    if AReadCount = 0 then
      LSpeed := 0
    else
      LSpeed := (AReadCount * 1000) div LTime;
    Abort := FAbort;
    FOnReceiveData(Self, LSpeed, AContentLength, AReadCount, Abort);
  end;
end;


(* TYouTubeThumbnail *)
constructor TYouTubeThumbnail.Create(AThumbnailUrl: TURI);
begin
  inherited Create;
  FUrl := AThumbnailUrl;
end;

destructor TYouTubeThumbnail.Destroy;
begin
  inherited Destroy;
end;

end.
