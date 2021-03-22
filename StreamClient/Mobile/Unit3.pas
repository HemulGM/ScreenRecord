unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, IPPeerClient, IPPeerServer, System.Tether.Manager, System.Tether.AppProfile, FMX.ListBox,
  FMX.Layouts, FMX.StdCtrls, System.Tether.TCPProtocol, System.Actions, FMX.ActnList, FMX.Objects,
  FMX.Controls.Presentation;

type
  TForm3 = class(TForm)
    CommandManager: TTetheringManager;
    CommandApp: TTetheringAppProfile;
    Image1: TImage;
    ToolBar1: TToolBar;
    lbStatus: TLabel;
    ToolBar2: TToolBar;
    Label1: TLabel;
    Button2: TButton;
    tmCheckConnection: TTimer;
    procedure Button2Click(Sender: TObject);
    procedure CommandAppResources0ResourceReceived(const Sender: TObject; const AResource: TRemoteResource);
    procedure CommandManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
    procedure tmCheckConnectionTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FIsConnected: Boolean;
    procedure CheckRemoteProfiles;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  System.DateUtils;

{$R *.fmx}

procedure TForm3.Button2Click(Sender: TObject);
begin
  CommandManager.AutoConnect;
  tmCheckConnection.Enabled := true;
end;

procedure TForm3.CommandAppResources0ResourceReceived(const Sender: TObject; const AResource: TRemoteResource);
begin
  TThread.Synchronize(nil,
    procedure
    var
      LSize: Int64;
    begin
      try
        LSize := AResource.Value.AsStream.Size;
        lbStatus.Text := 'Resource received at ' + DateTimeToStr(now) + ' size ' + InttoStr(LSize);
        AResource.Value.AsStream.Position := 0;
        Image1.Bitmap.LoadFromStream(AResource.Value.AsStream);
        Image1.Repaint;
      except
      end;
    end);
end;

procedure TForm3.CommandManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
begin
  Password := 'Desktop';
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  FIsConnected := false;
end;

procedure TForm3.CheckRemoteProfiles;
var
  I: Integer;
  ConnectedProfiles: string;
begin
  if CommandManager.RemoteProfiles.Count > 0 then
  begin
    for I := 0 to CommandManager.RemoteProfiles.Count - 1 do
    begin
      ConnectedProfiles := ConnectedProfiles + ' - ' + CommandManager.RemoteProfiles.Items[I].ProfileText;
    end;
    Label1.Text := 'Working with :' + ConnectedProfiles;
    FIsConnected := true;
  end
  else
  begin
    Label1.Text := 'You are not connected';
    FIsConnected := false;
  end;
end;

procedure TForm3.tmCheckConnectionTimer(Sender: TObject);
begin
  CheckRemoteProfiles;
end;

end.

