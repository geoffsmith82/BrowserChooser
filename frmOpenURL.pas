unit frmOpenURL;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, registry, windows, FMX.Controls.Presentation, FMX.StdCtrls,
  System.ImageList, FMX.ImgList;

type
  TfrmBrowserChooser = class(TForm)
    FBrowserListBox: TListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FBrowserListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
    FURL : String;
    FProgramPathList : TStringList;
    procedure LoadListBox(inKey: HKEY; inListbox: TListBox);
    procedure AddEdge(inListbox: TListBox);
  public
    { Public declarations }
    procedure OpenUrl(url:String);
  end;

procedure OpenURL(inURL: string);

implementation

{$R *.fmx}

uses Winapi.ShellAPI;

procedure OpenURL(inURL: string);
var
  frmBrowserChooser : TfrmBrowserChooser;
begin
  frmBrowserChooser := TfrmBrowserChooser.Create(nil);
  try
    frmBrowserChooser.OpenUrl('https://www.tysontechnology.com.au');
  finally
    FreeAndNil(frmBrowserChooser);
  end;
end;

procedure TfrmBrowserChooser.AddEdge(inListbox:TListBox);
var
  listboxItem : TListboxItem;
  programPath : String;
begin
  listboxItem := TListBoxItem.Create(inListbox);
  listboxItem.Text := 'Microsoft Edge';
  programPath := 'shell:Appsfolder\Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge';

  listboxItem.TagString := programPath;
  inListbox.AddObject(listboxItem);
  FProgramPathList.Add(programPath);
end;

procedure TfrmBrowserChooser.FormCreate(Sender: TObject);
begin
  FProgramPathList := TStringList.Create;
  LoadListBox(HKEY_CURRENT_USER,FBrowserListBox);
  LoadListBox(HKEY_LOCAL_MACHINE,FBrowserListBox);
  OutputDebugString(PChar(TOSVersion.ToString));
  if TOSVersion.Major >= 10 then  // Add Microsoft Edge if Windows 10
    AddEdge(FBrowserListBox);
end;

procedure TfrmBrowserChooser.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProgramPathList);
end;

procedure TfrmBrowserChooser.LoadListBox(inKey : HKEY;inListbox:TListBox);
var
  names : TStringList;
  i : Integer;
  reg : TRegistry;
  regName : TRegistry;
  regOpen : TRegistry;
  listboxItem : TListboxItem;
  programPath : string;
begin
  reg := nil;
  names := nil;
  try
    reg := TRegistry.Create;
    names := TStringList.Create;
    reg.RootKey := inKey;
    if reg.OpenKeyReadOnly('SOFTWARE\Clients\StartMenuInternet') then
    begin
      reg.GetKeyNames(names);
      for i := 0 to names.Count - 1 do
      begin
        regName := nil;
        regOpen := nil;
        try
          regName := TRegistry.Create;
          regName.RootKey := inKey;
          regOpen := TRegistry.Create;
          regOpen.RootKey := inKey;

          if regName.OpenKeyReadOnly('SOFTWARE\Clients\StartMenuInternet\'+names[i]) and
             regOpen.OpenKeyReadOnly('SOFTWARE\Clients\StartMenuInternet\'+names[i]+'\Shell\Open\Command') then
          begin
            programPath := regOpen.ReadString('');
            if FProgramPathList.IndexOf(programPath)<0 then
            begin
              listboxItem := TListBoxItem.Create(inListbox);
              listboxItem.Text := regName.ReadString('');
              listboxItem.TagString := programPath;
              inListbox.AddObject(listboxItem);
              FProgramPathList.Add(programPath);
            end;
          end;
        finally
          FreeAndNil(regName);
          FreeAndNil(regOpen);
        end;
      end;
    end;
  finally
    FreeAndNil(reg);
    FreeAndNil(names);
  end;
end;

procedure TfrmBrowserChooser.FBrowserListBoxDblClick(Sender: TObject);
var
  programPath : String;
begin
  programPath := FBrowserListBox.Selected.TagString;
  ShellExecute(0,'open',PChar(programPath), PChar(FURL),'',SW_SHOWNORMAL);
  Close;
end;

procedure TfrmBrowserChooser.OpenUrl(url: String);
begin
  FURL := url;
  ShowModal;
end;

end.
