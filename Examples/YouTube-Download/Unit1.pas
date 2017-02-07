unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FH.YouTube, FH.YouTube.Utils, System.Net.URLClient, FMX.Layouts,
  FMX.TabControl, FMX.Menus;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    TabControl1: TTabControl;
    TabItem3: TTabItem;
    Memo1: TMemo;
    Layout2: TLayout;
    Button9: TButton;
    TabItem2: TTabItem;
    Memo2: TMemo;
    Layout1: TLayout;
    Button3: TButton;
    Button1: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Edit1: TEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    Layout3: TLayout;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  i : integer;
  LDownloadThread : TDownloadHTTP;
  FYouTubeClient : TYouTubeClient;
begin
  FYouTubeClient := TYouTubeClient.Create(edit1.Text);
  try
    Memo1.Lines.Add(Format('Title: %s', [FYouTubeClient.Title]));

    Memo1.Lines.Add(Format('VideoID: %s', [FYouTubeClient.VideoId]));
    Memo1.Lines.Add(Format('Keywords url: %s', [string.Join(';', FYouTubeClient.Keywords)]));
    Memo1.Lines.Add(Format('Duration: %d sec', [FYouTubeClient.Duration]));
    Memo1.Lines.Add(Format('Thumbnail url: %s', [FYouTubeClient.Thumbnail.URL.ToString]));
    for I := 0 to FYouTubeClient.Streams.Count - 1 do
    begin
      Memo1.Lines.Add(Format('Stream[%d]:', [i]));
      Memo1.Lines.Add(Format('   Direct URL: %s', [FYouTubeClient.Streams.Items[i].URL.ToString]));
      Memo1.Lines.Add(Format('   ITag: %d', [FYouTubeClient.Streams.Items[i].ITag.Value]));
      Memo1.Lines.Add(Format('   MediaType: %s', [FYouTubeClient.Streams.Items[i].MediaType]));
      Memo1.Lines.Add(Format('   SIG: %s', [FYouTubeClient.Streams.Items[i].SIG]));
      Memo1.Lines.Add(Format('   S: %s', [FYouTubeClient.Streams.Items[i].S]));

      if FYouTubeClient.Streams.Items[i] is TYouTubeMainStream then
      begin
        Memo1.Lines.Add(Format('   Quality: %s', [TYouTubeMainStream(FYouTubeClient.Streams.Items[i]).Quality]));
      end else
      if FYouTubeClient.Streams.Items[i] is TYouTubeAdaptiveStream then
      begin
        Memo1.Lines.Add(Format('   QualityLabel: %s', [TYouTubeAdaptiveStream(FYouTubeClient.Streams.Items[i]).QualityLabel]));
      end;
    end;


    if FYouTubeClient.Streams.Items[0] is TYouTubeMainStream then
    begin
      if not TYouTubeMainStream(FYouTubeClient.Streams.Items[0]).S.IsEmpty then
      begin
        FYouTubeClient.Streams.Items[0].URL.AddParameter('signature', FYouTubeClient.Streams.Items[0].Sig);
      end;
    end;

    LDownloadThread := TDownloadHTTP.Create(FYouTubeClient.Streams.Items[0].URL.ToString);
    LDownloadThread.DownloadStream := TFileStream.Create('test.mp4', fmCreate);
    LDownloadThread.DownloadStream.Position := 0;
    LDownloadThread.OnReceiveData := procedure(const ASender: TObject; ASpeed: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean)
        begin
          TThread.Synchronize(nil,
          procedure
          begin
             label1.Text := Format('%d KB/s', [ASpeed div 1024]);
             if AContentLength > 0 then
              progressbar1.Value := (AReadCount / AContentLength) * 100;//AReadCount;
          end);
        end;

    LDownloadThread.OnComplete :=  procedure(const Sender: TObject)
        begin
          TThread.Synchronize(nil,
          procedure
          begin
            memo1.Lines.Add('Finish');
            memo1.Lines.Add(Format('Global Speed: %d KB/s', [TDownloadHTTP(Sender).AverageSpeed div 1024]));
          end);
        end;

    LDownloadThread.OnError := procedure(const Sender: TObject; AMessage: string)
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        showmessage(AMessage);
      end);
    end;
    LDownloadThread.OnBeforeStart :=  procedure(const Sender: TObject)
        begin
          TThread.Synchronize(nil,
          procedure
          begin
            progressbar1.Max := 100;//TDownloadHTTP(Sender).ContentSize;
            progressbar1.Min := 0;
            progressbar1.Value := 0;
          end);
        end;

    LDownloadThread.Start;

  finally
    FreeAndNil(FYouTubeClient);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i : integer;
begin
  for I := 0 to memo2.Lines.Count - 1 do
  begin
    memo2.Lines.Strings[i] :=  booltostr(TYouTubeURL.ValidateUrl(memo2.Lines.Strings[i]), true) + ': '+memo2.Lines.Strings[i];
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  edit1.Text := 'https://www.youtube.com/watch?v=IZ8RiSZBCaM';
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  edit1.Text := 'https://www.youtube.com/watch?v=gBAfejjUQoA';
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  edit1.Text := 'https://www.youtube.com/watch?v=iNJdPyoqt8U';
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  edit1.Text := 'https://www.youtube.com/watch?v=zbBYpZwAQvU';
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  edit1.Text := 'https://youtube.com/embed/DFYRQ_zQ-gk';

end;

procedure TForm1.Button9Click(Sender: TObject);
var
  i : integer;
  FYouTubeClient : TYouTubeClient;
begin
  FYouTubeClient := TYouTubeClient.Create(edit1.Text);
  try
    Memo1.Lines.Add(Format('Title: %s', [FYouTubeClient.Title]));
    Memo1.Lines.Add(Format('VideoID: %s', [FYouTubeClient.VideoId]));
    Memo1.Lines.Add(Format('Keywords url: %s', [string.Join(';', FYouTubeClient.Keywords)]));
    Memo1.Lines.Add(Format('Duration: %d sec', [FYouTubeClient.Duration]));
    Memo1.Lines.Add(Format('Thumbnail url: %s', [FYouTubeClient.Thumbnail.URL.ToString]));
    for I := 0 to FYouTubeClient.Streams.Count - 1 do
    begin
      Memo1.Lines.Add(Format('Stream[%d]:', [i]));
      Memo1.Lines.Add(Format('   Direct URL: %s', [FYouTubeClient.Streams.Items[i].URL.ToString]));
      Memo1.Lines.Add(Format('   ITag: %d', [FYouTubeClient.Streams.Items[i].ITag.Value]));
      Memo1.Lines.Add(Format('   MediaType: %s', [FYouTubeClient.Streams.Items[i].MediaType]));
      Memo1.Lines.Add(Format('   SIG: %s', [FYouTubeClient.Streams.Items[i].SIG]));
      Memo1.Lines.Add(Format('   S: %s', [FYouTubeClient.Streams.Items[i].S]));
      if FYouTubeClient.Streams.Items[i] is TYouTubeMainStream then
      begin
        Memo1.Lines.Add(Format('   Quality: %s', [TYouTubeMainStream(FYouTubeClient.Streams.Items[i]).Quality]));
      end else
      if FYouTubeClient.Streams.Items[i] is TYouTubeAdaptiveStream then
      begin
        Memo1.Lines.Add(Format('   QualityLabel: %s', [TYouTubeAdaptiveStream(FYouTubeClient.Streams.Items[i]).QualityLabel]));
      end;
    end;
  finally
    FreeAndNil(FYouTubeClient);
  end;
end;

end.
