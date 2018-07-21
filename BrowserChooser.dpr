program BrowserChooser;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmOpenURL in 'frmOpenURL.pas' {frmBrowserChooser},
  frmMain in 'frmMain.pas' {frmBrowserChooseDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBrowserChooseDemo, frmBrowserChooseDemo);
  Application.Run;
end.
