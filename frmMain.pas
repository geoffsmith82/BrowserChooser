unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TfrmBrowserChooseDemo = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBrowserChooseDemo: TfrmBrowserChooseDemo;

implementation

{$R *.fmx}

uses frmOpenURL;

procedure TfrmBrowserChooseDemo.Button1Click(Sender: TObject);
begin
  OpenUrl('https://www.tysontechnology.com.au');
end;

end.
