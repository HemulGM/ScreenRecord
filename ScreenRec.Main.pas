unit ScreenRec.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, AviWriter, IPPeerClient, IPPeerServer,
  System.Tether.Manager, HGM.Screenshot, System.Tether.AppProfile;

type
  TForm14 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    TetheringAppProfile1: TTetheringAppProfile;
    DesktopTetherManager: TTetheringManager;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure DesktopTetherManagerRequestManagerPassword(const Sender: TObject; const ARemoteIdentifier: string; var Password: string);
  private
    FShareStream: TStream;
    FScreenRecorder: TScreenRecorder;
    FAviWriter: TAviWriter;
    procedure FOnReadyFrame(Sender: TObject; Stream: TStream);
  public
    { Public declarations }
  end;

var
  Form14: TForm14;

implementation

uses
  FMX.Surfaces, Math;

{$R *.fmx}

procedure TForm14.Button1Click(Sender: TObject);
begin
  //Открыть аудиопоток
  //FAviWriter.Start;
  //Начать запись экрана
  FScreenRecorder.Start;
end;

procedure TForm14.Button2Click(Sender: TObject);
begin
  //Закончить запись экрана (и подождать завершения)
  FScreenRecorder.Stop(True);
  //Закончить видеопоток и сохранить
  //FAviWriter.Save;
end;

procedure TForm14.DesktopTetherManagerRequestManagerPassword(const Sender: TObject; const ARemoteIdentifier: string; var Password: string);
begin
  Password := 'Desktop';
end;

procedure TForm14.FOnReadyFrame(Sender: TObject; Stream: TStream);
var
  Codec: TBitmapCodecSaveParams;
  Surface: TBitmapSurface;
begin
  Image1.Bitmap.LoadFromStream(Stream);
  Stream.Position := 0;

  //Добавление в видео поток
  //FAviWriter.AddVideo(Stream);
  //
  //Стриминг кадров напрямую
  if FShareStream = nil then
    FShareStream := TMemoryStream.Create;
  FShareStream.Size := 0;

  //Конвертируем в JPEG
  Codec.Quality := 70;
  Surface := TBitmapSurface.Create;
  Surface.Assign(Image1.Bitmap);
  TBitmapCodecManager.SaveToStream(FShareStream, Surface, '.jpg', @Codec);
  Surface.Free;

  //Отправляем кадр
  FShareStream.Position := 0;
  TetheringAppProfile1.Resources.Items[0].Value := FShareStream;
end;

procedure TForm14.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FScreenRecorder.Stop(True);
end;

procedure TForm14.FormCreate(Sender: TObject);
begin
  //Видеофайл
  FAviWriter := TAviWriter.Create(nil);
  FAviWriter.Width := Screen.Width;
  FAviWriter.Height := Screen.Height;
  FAviWriter.FileName := 'test.avi';
  FAviWriter.FrameTime := 80;

  //Запись экрана
  FScreenRecorder := TScreenRecorder.Create;
  FScreenRecorder.OnReadyFrame := FOnReadyFrame;
end;

procedure TForm14.FormDestroy(Sender: TObject);
begin
  FScreenRecorder.Free;
  FAviWriter.Free;
  if Assigned(FShareStream) then
    FShareStream.Free;
end;

end.

