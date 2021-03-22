program ScreenRecord;

uses
  System.StartUpCopy,
  FMX.Forms,
  ScreenRec.Main in 'ScreenRec.Main.pas' {Form14},
  AviWriter in 'AviWriter.pas',
  HGM.Screenshot in '..\Components\HGM.Screenshot.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm14, Form14);
  Application.Run;
end.
