unit frmOpenURL;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.ImageList,
  System.Win.ComObj,
  System.Win.Registry,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.MultiResBitmap,
  FMX.ImgList,
  FMX.Objects,
  FMX.ListView.Types,
  FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base,
  FMX.ListView,
  Winapi.ShellAPI,
  Winapi.Windows
  ;

type
  TfrmBrowserChooser = class(TForm)
    Label1: TLabel;
    ImageList1: TImageList;
    FBrowserListBox: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FBrowserListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
    FURL : String;
    FProgramPathList : TStringList;
    procedure LoadListView(inKey: HKEY; inListView: TListView);
    procedure AddEdge(inListView: TListView);
    function ExtractIconFromFile(const FileName: string): FMX.Graphics.TBitmap;
    function GetExecutablePath(const CommandLine: string): string;
  public
    { Public declarations }
    procedure OpenUrl(url:String);
  end;

procedure OpenURL(inURL: string);

implementation

{$R *.fmx}

procedure OpenURL(inURL: string);
var
  frmBrowserChooser : TfrmBrowserChooser;
begin
  frmBrowserChooser := TfrmBrowserChooser.Create(nil);
  try
    frmBrowserChooser.OpenUrl(inURL);
  finally
    FreeAndNil(frmBrowserChooser);
  end;
end;

function TfrmBrowserChooser.GetExecutablePath(const CommandLine: string): string;
var
  Temp: string;
  QuotePos: Integer;
begin
  Result := '';
  Temp := Trim(CommandLine);

  if Temp = '' then Exit;

  // Handle quoted paths
  if Temp[1] = '"' then
  begin
    QuotePos := Pos('"', Copy(Temp, 2, Length(Temp)));
    if QuotePos > 0 then
      Result := Copy(Temp, 2, QuotePos - 1)
    else
      Result := Copy(Temp, 2, Length(Temp));
  end
  else
  begin
    // Handle unquoted paths - take everything before first space
    if Temp.StartsWith('C:\Program Files\') then
      Result := Temp
    else
    begin
      QuotePos := Pos(' ', Temp);
      if QuotePos > 0 then
        Result := Copy(Temp, 1, QuotePos - 1)
      else
        Result := Temp;
    end;
  end;

  // Remove any trailing parameters
  Result := Trim(Result);
end;

function TfrmBrowserChooser.ExtractIconFromFile(const FileName: string): FMX.Graphics.TBitmap;
var
  Icon: HICON;
  IconInfo: TIconInfo;
  DeviceContext: HDC;
  hBmp: HBITMAP;
  BmpInfo: TBitmapInfo;
  BmpData: Pointer;
  BitmapData: TBitmapData;
  I, J: Integer;
  SrcPtr: PByte;
  DstPtr: PAlphaColorRec;
  B, G, R, A: Byte;
begin
  Result := nil;

  // Extract large icon (32x32)
  Icon := ExtractIcon(HInstance, PChar(FileName), 0);
  if Icon = 0 then
    Exit;

  try
    if GetIconInfo(Icon, IconInfo) then
    begin
      try
        Result := FMX.Graphics.TBitmap.Create;
        Result.SetSize(32, 32);

        // Create a device context
        DeviceContext := CreateCompatibleDC(0);
        try
          // Create bitmap info structure
          FillChar(BmpInfo, SizeOf(BmpInfo), 0);
          BmpInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
          BmpInfo.bmiHeader.biWidth := 32;
          BmpInfo.bmiHeader.biHeight := -32; // Top-down DIB
          BmpInfo.bmiHeader.biPlanes := 1;
          BmpInfo.bmiHeader.biBitCount := 32;
          BmpInfo.bmiHeader.biCompression := BI_RGB;

          // Create DIB section - BmpData will be allocated by CreateDIBSection
          hBmp := CreateDIBSection(DeviceContext, BmpInfo, DIB_RGB_COLORS, BmpData, 0, 0);
          if hBmp <> 0 then
          begin
            try
              SelectObject(DeviceContext, hBmp);

              // Draw icon on bitmap
              DrawIconEx(DeviceContext, 0, 0, Icon, 32, 32, 0, 0, DI_NORMAL);

              // Convert to FMX bitmap using proper Map access
              if Result.Map(TMapAccess.Write, BitmapData) then
              try
                SrcPtr := BmpData;
                for I := 0 to 31 do
                begin
                  DstPtr := BitmapData.GetScanline(I);
                  for J := 0 to 31 do
                  begin
                    // Read BGRA from Windows bitmap
                    B := SrcPtr^; Inc(SrcPtr);
                    G := SrcPtr^; Inc(SrcPtr);
                    R := SrcPtr^; Inc(SrcPtr);
                    A := SrcPtr^; Inc(SrcPtr);

                    // Set RGBA in FMX bitmap
                    DstPtr^.R := R;
                    DstPtr^.G := G;
                    DstPtr^.B := B;
                    DstPtr^.A := A;

                    Inc(DstPtr);
                  end;
                end;
              finally
                Result.Unmap(BitmapData);
              end;
            finally
              DeleteObject(hBmp); // This automatically frees the DIB section memory
            end;
          end;
        finally
          DeleteDC(DeviceContext);
        end;
      finally
        if IconInfo.hbmColor <> 0 then DeleteObject(IconInfo.hbmColor);
        if IconInfo.hbmMask <> 0 then DeleteObject(IconInfo.hbmMask);
      end;
    end;
  finally
    DestroyIcon(Icon);
  end;

  // If extraction failed, free the bitmap and return nil
  if (Result <> nil) and (Result.IsEmpty) then
  begin
    FreeAndNil(Result);
  end;
end;

procedure TfrmBrowserChooser.AddEdge(inListView: TListView);
var
  listItem : TListViewItem;
  programPath : String;
  IconBmp: FMX.Graphics.TBitmap;
  vSource: TCustomSourceItem;
  vBitmapItem: TCustomBitmapItem;
  vDest: TCustomDestinationItem;
  vLayer: TLayer;
  ImageIndex: Integer;
begin
  listItem := inListView.Items.Add;
  listItem.Text := 'Microsoft Edge';
  programPath := 'shell:Appsfolder\Microsoft.MicrosoftEdge_8wekyb3d8bbwe!MicrosoftEdge';

  // Try to get Edge icon from system - Edge uses different paths in different Windows versions
  IconBmp := ExtractIconFromFile('C:\Windows\SystemApps\Microsoft.MicrosoftEdge_8wekyb3d8bbwe\MicrosoftEdge.exe');
  if IconBmp = nil then
    IconBmp := ExtractIconFromFile('C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe');
  if IconBmp = nil then
    IconBmp := ExtractIconFromFile('C:\Program Files\Microsoft\Edge\Application\msedge.exe');

  if IconBmp <> nil then
  begin
    // Create source item
    vSource := ImageList1.Source.Add;
    vSource.MultiResBitmap.TransparentColor := TColorRec.Fuchsia;
    vSource.MultiResBitmap.SizeKind := TSizeKind.Source;
    vSource.MultiResBitmap.Width := IconBmp.Width;
    vSource.MultiResBitmap.Height := IconBmp.Height;

    // Add bitmap to source
    vBitmapItem := vSource.MultiResBitmap.ItemByScale(1.0, True, True);
    if vBitmapItem = nil then
    begin
      vBitmapItem := vSource.MultiResBitmap.Add;
      vBitmapItem.Scale := 1.0;
    end;
    vBitmapItem.Bitmap.Assign(IconBmp);

    // Create destination item - this is the key part!
    vDest := ImageList1.Destination.Add;
    vLayer := vDest.Layers.Add;
    vLayer.Name := 'EdgeLayer';
    vLayer.SourceRect.Rect := TRectF.Create(0, 0, IconBmp.Width, IconBmp.Height);

    // CRITICAL: Link the layer to the source item by index
    vLayer.SourceRect.Left := 0;
    vLayer.SourceRect.Top := 0;
    vLayer.SourceRect.Right := IconBmp.Width;
    vLayer.SourceRect.Bottom := IconBmp.Height;

    ImageIndex := ImageList1.Destination.Count - 1;  // Use Destination index, not Source index
    listItem.ImageIndex := ImageIndex;
    IconBmp.Free;
  end;

  listItem.TagString := programPath;
  FProgramPathList.Add(programPath);
end;

procedure TfrmBrowserChooser.FormCreate(Sender: TObject);
begin
  FProgramPathList := TStringList.Create;

  // Remove custom style that might be interfering
  FBrowserListBox.StyleLookup := '';


  // The ListView is already configured in the form with TImageListItemAppearance
  // Just ensure the Images property is set (already done in form)
  FBrowserListBox.Images := ImageList1;
  FBrowserListBox.ItemAppearance.ItemHeight := 48;

  // Create a simple test bitmap and add it to ImageList

  LoadListView(HKEY_CURRENT_USER, FBrowserListBox);
  LoadListView(HKEY_LOCAL_MACHINE, FBrowserListBox);
  OutputDebugString(PChar(TOSVersion.ToString));
  if TOSVersion.Major >= 10 then  // Add Microsoft Edge if Windows 10
    AddEdge(FBrowserListBox);
end;

procedure TfrmBrowserChooser.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProgramPathList);
end;

procedure TfrmBrowserChooser.LoadListView(inKey: HKEY; inListView: TListView);
var
  names: TStringList;
  i: Integer;
  reg, regName, regOpen: TRegistry;
  listItem: TListViewItem;
  programPath, ExePath: string;
  IconBmp: FMX.Graphics.TBitmap;
  ImageIndex: Integer;
  vSource: TCustomSourceItem;
  vBitmapItem: TCustomBitmapItem;
  vDest: TCustomDestinationItem;
  vLayer: TLayer;
begin
  reg := nil;
  names := nil;
  IconBmp := nil;
  try
    reg := TRegistry.Create;
    names := TStringList.Create;
    IconBmp := nil;
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
          regOpen := TRegistry.Create;
          regName.RootKey := inKey;
          regOpen.RootKey := inKey;

          if regName.OpenKeyReadOnly('SOFTWARE\Clients\StartMenuInternet\' + names[i]) and
             regOpen.OpenKeyReadOnly('SOFTWARE\Clients\StartMenuInternet\' + names[i] + '\Shell\Open\Command') then
          begin
            programPath := regOpen.ReadString('');
            if FProgramPathList.IndexOf(programPath) < 0 then
            begin
              listItem := inListView.Items.Add;
              listItem.Text := regName.ReadString('');
              listItem.TagString := programPath;

              ExePath := GetExecutablePath(programPath);
              OutputDebugString(PChar('Trying to extract icon from: ' + ExePath));
              if FileExists(ExePath) then
              begin
                IconBmp.Free;
                IconBmp := ExtractIconFromFile(ExePath);
                if IconBmp <> nil then
                begin
                  OutputDebugString(PChar('Icon extracted successfully, size: ' + IconBmp.Width.ToString + 'x' + IconBmp.Height.ToString));

                  // Add to Source
                  vSource := ImageList1.Source.Add;
                  vSource.MultiResBitmap.TransparentColor := TColorRec.Fuchsia;
                  vSource.MultiResBitmap.SizeKind := TSizeKind.Source;
                  vSource.MultiResBitmap.Width := IconBmp.Width;
                  vSource.MultiResBitmap.Height := IconBmp.Height;


                  vBitmapItem := vSource.MultiResBitmap.ItemByScale(1.0, True, True);
                  if vBitmapItem = nil then
                    vBitmapItem := vSource.MultiResBitmap.Add;

                  vBitmapItem.Scale := 1.0;
                  vBitmapItem.Bitmap.Assign(IconBmp);

                  // Add matching Destination with Layer
                  vDest := ImageList1.Destination.Add;
                  vDest.DisplayName := 'Dest' + (ImageList1.Source.Count - 1).ToString;

                  vLayer := vDest.Layers.Add;
                  vLayer.Name := vSource.Name;//'Layer' + (ImageList1.Source.Count - 1).ToString;
                  vLayer.SourceRect.Rect := TRectF.Create(0, 0, IconBmp.Width, IconBmp.Height);


                  // Use current index
                  ImageIndex := ImageList1.Source.Count - 1;
                  listItem.ImageIndex := ImageIndex;

                  OutputDebugString(PChar('Added icon at ImageIndex: ' + ImageIndex.ToString));
                  FreeAndNil(IconBmp);
                end
                else
                  OutputDebugString(PChar('Failed to extract icon from: ' + ExePath));
              end
              else
                OutputDebugString(PChar('File does not exist: ' + ExePath));

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
    FreeAndNil(IconBmp);
  end;
end;

procedure TfrmBrowserChooser.FBrowserListBoxDblClick(Sender: TObject);
var
  programPath : String;
begin
  if FBrowserListBox.Selected <> nil then
  begin
    programPath := FBrowserListBox.Selected.TagString;
    ShellExecute(0, 'open', PChar(programPath), PChar(FURL), '', SW_SHOWNORMAL);
    Close;
  end;
end;

procedure TfrmBrowserChooser.OpenUrl(url: String);
begin
  FURL := url;
  ShowModal;
end;

end.
