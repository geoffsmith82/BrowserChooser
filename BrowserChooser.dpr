program BrowserChooser;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmOpenURL in 'frmOpenURL.pas' {frmBrowserChooser},
  frmMain in 'frmMain.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
