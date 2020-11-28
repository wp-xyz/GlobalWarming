unit gwMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Grids, Buttons, Types, TAGraph, TASeries, TACustomSeries, TASources,
  TACustomSource, TAChartListbox, TATools, TAStyles, TAFuncSeries, TADataTools,
  TADrawUtils, TAIntervalSources, TATransformations,
  gwStations, gwDeutscherWetterDienst, gwNASA, gwPages2k;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    btnShowDataFile: TButton;
    cgDWD_PlotData: TCheckGroup;
    Chart: TChart;
    Pages2K_TreeView: TTreeView;
    pgPages2k: TTabSheet;
    TemperatureTransformations: TChartAxisTransformations;
    TemperatureTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    SunshineHoursTransformations: TChartAxisTransformations;
    SunshineHoursTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    PrecipitationsTransformations: TChartAxisTransformations;
    PrecipitationsTransformationsAutoScaleAxisTransform1: TAutoScaleAxisTransform;
    cmbDWD_Search: TComboBox;
    cmbGISS_Search: TComboBox;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    DWD_SearchPanel1: TPanel;
    Label7: TLabel;
    lblStationName: TLabel;
    lblGISS_Search: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MeasurementTool: TDataPointDistanceTool;
    ChartLineSeries1: TLineSeries;
    cbLocalFilesOnly: TCheckBox;
    cbOverlayCurves: TCheckBox;
    ChartListbox: TChartListbox;
    cgGISS_PlotData: TCheckGroup;
    ChartToolset: TChartToolset;
    CrosshairTool: TDataPointCrosshairTool;
    GISSv4_TreeView: TTreeView;
    PanDragTool: TPanDragTool;
    Panel2: TPanel;
    Panel4: TPanel;
    DWD_SearchPanel: TPanel;
    rgGISS_Version: TRadioGroup;
    pgDWD: TTabSheet;
    DWD_TreeView: TTreeView;
    SpeedButton1: TSpeedButton;
    ZoomDragTool: TZoomDragTool;
    ImageList: TImageList;
    Panel3: TPanel;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    lblStationInfo: TLabel;
    DataSource_PageControl: TPageControl;
    Display_PageControl: TPageControl;
    Panel1: TPanel;
    LeftPanel: TPanel;
    TableCaptionPanel: TPanel;
    pgGISS: TTabSheet;
    GISSv2_TreeView: TTreeView;
    pgChart: TTabSheet;
    pgTable: TTabSheet;
    Splitter1: TSplitter;
    DataGrid: TStringGrid;
    procedure btnShowDataFileClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbLocalFilesOnlyChange(Sender: TObject);
    procedure ChartListboxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CrosshairToolDraw(ASender: TDataPointDrawTool);
    procedure DataGridPrepareCanvas(Sender: TObject; ACol, ARow: Integer; AState: TGridDrawState);
    procedure DataSource_PageControlChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MeasurementToolAfterMouseUp(ATool: TChartTool; APoint: TPoint);
    procedure MeasurementToolGetDistanceText(ASender: TDataPointDistanceTool;
      var AText: String);
    procedure rgGISS_VersionClick(Sender: TObject);
    procedure SearchComboEditingDone(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);

    // Treeview clicks
    procedure DWD_TreeViewSelectionChanged(Sender: TObject);
    procedure GISS_TreeViewSelectionChanged(Sender: TObject);
    procedure Pages2K_TreeViewSelectionChanged(Sender: TObject);

  private
    DWD_Stations: TDWD_StationList;
    GISSv2_Stations: TGISSv2_StationList;
    GISSv4_Stations: TGISSv4_StationList;
    PAGES2k_Stations: TPAGES2k_StationList;
    FFitSeries: TFitSeries;
    procedure CreateFitSeries;

    procedure LoadDWD;
    procedure LoadGISSv2;
    procedure LoadGISSv4;
    procedure LoadPAGES2k;
    procedure SetGISSVersion(AVersionIndex: Integer);

    function CurrentTree: TTreeView;
    function FindStationNode(ATree: TTreeView; AStationName: String): TTreeNode;

    procedure CloseFileViewerHandler(Sender: TObject; var CloseAction: TCloseAction);
    procedure ShowDataFile(AStation: TStation);
    procedure UpdateFileViewer(AStation: TStation);

    procedure ReadIni;
    procedure WriteIni;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  {$IF FPC_FullVersion >= 30200}
  opensslsockets,
  {$ENDIF}
  IniFiles,
  TAMath,
  gwGlobal, gwUtils, gwAbout, gwFileViewer;

type
  TChartSeriesAccess = class(TChartSeries);


{ TMainForm }

procedure TMainForm.btnShowDataFileClick(Sender: TObject);
var
  station: TStation;
begin
  if (FileViewerForm = nil) or not FileViewerForm.Showing then
  begin
    if CurrentTree.Selected <> nil then
      station := TStation(CurrentTree.Selected.Data)
    else
      station := nil;
    ShowDataFile(station);
    btnShowDataFile.Caption := 'Close file viewer';
  end else
  if FileViewerForm <> nil then
  begin
    FileViewerForm.Hide;
    btnShowDataFile.Caption := 'Show data file';
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Chart.ClearSeries;
end;

procedure TMainForm.cbLocalFilesOnlyChange(Sender: TObject);
begin
  DWD_TreeView.Items.Clear;
  GISSv2_Treeview.Items.Clear;
  GISSv4_TreeView.Items.Clear;
  Datasource_PageControlChange(nil);
end;

procedure TMainForm.ChartListboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MeasurementTool.Enabled := ChartListBox.ItemIndex > -1;
end;

procedure TMainForm.CloseFileViewerHandler(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  btnShowDataFile.Caption := 'Show data file';
  if WindowState = wsNormal then
  begin
    FileViewerLeft := FileViewerForm.Left;
    FileViewerTop := FileViewerForm.Top;
    FileViewerWidth := FileViewerForm.Width;
    FileViewerHeight := FileViewerForm.Height;
  end;
end;

procedure TMainForm.CreateFitSeries;
begin
  FFitSeries := TFitSeries.Create(self);
  FFitSeries.ParamCount := 2;   // linear fit --> 2 fit parameters
  FFitSeries.Pen.Color := clLime;
  FFitSeries.Pen.Width := 3;
  FFitSeries.Title := 'Fit';
  FFitSeries.ZPosition := 9999;  // Make fit series appear above all other series
  Chart.AddSeries(FFitSeries);
end;

procedure TMainForm.CrosshairToolDraw(ASender: TDataPointDrawTool);
var
  idx: Integer;
  x, y: Double;
  ser: TChartSeriesAccess;
  title: String;
  dateStr: String;
begin
  if not (ASender.Series is TChartSeries) then
  begin
    Statusbar.Simpletext := '';
    exit;
  end;
  ser := TChartSeriesAccess(ASender.Series);
  idx := ASender.PointIndex;
  x := ser.XValue[idx];
  if PI_MONTH and TStation(ser.Tag).SeriesItems <> 0 then
    dateStr := FormatDateTime('mmm/yyyy', x)
  else
    dateStr := Format('%.0f', [x]);
  if (ASender.YIndex > -1) then
  begin
    if ser.Styles <> nil then
      title := ser.Styles.Styles[ASender.YIndex].Text
    else
      title := ser.Title;
    y := ser.YValues[idx, ASender.YIndex]
  end else
  begin
    y := ser.YValue[idx];
    title := ser.Title;
  end;
  Statusbar.SimpleText := Format('%s: %.1f°C in %s', [title, y, dateStr]);
end;

function TMainForm.CurrentTree: TTreeView;
begin
  if DataSource_PageControl.ActivePage = pgGISS then
  begin
    case rgGISS_Version.ItemIndex of
      0: Result := GISSv2_TreeView;
      1: Result := GISSv4_TreeView;
      else raise Exception.Create('GISS version not handled.');
    end;
  end else
  if Datasource_PageControl.ActivePage = pgDWD then
    Result := DWD_TreeView
  else
  if Datasource_PageControl.ActivePage = pgPages2k then
    Result := Pages2K_TreeView
  else
    raise Exception.Create('DataSource not handled.');
end;

procedure TMainForm.DataGridPrepareCanvas(Sender: TObject; ACol, ARow: Integer;
  AState: TGridDrawState);
var
  ts: TTextStyle;
begin
  ts := (Sender as TStringGrid).Canvas.TextStyle;
  ts.Alignment := taCenter;
  ts.WordBreak := true;
  ts.SingleLine := false;
  (Sender as TStringGrid).Canvas.TextStyle := ts;
end;

procedure TMainForm.DataSource_PageControlChange(Sender: TObject);
begin
  if DataSource_PageControl.ActivePage = pgGISS then
    SetGISSVersion(rgGISS_Version.ItemIndex)
  else
  if DataSource_PageControl.ActivePage = pgDWD then
  begin
    if DWD_TreeView.Items.Count = 0 then begin
      LoadDWD;
      DWD_Stations.PopulateTreeView(DWD_TreeView, cbLocalFilesOnly.Checked);
    end;
  end else
  if DataSource_PageControl.ActivePage = pgPAGES2k then
  begin
    if PAGES2k_TreeView.Items.Count = 0 then
    begin
      LoadPAGES2k;
      PAGES2k_Stations.PopulateTreeView(PAGES2k_TreeView, cbLocalFilesOnly.Checked);
    end;
  end else
    raise Exception.Create('Data source ' + DataSource_PageControl.ActivePage.Caption + ' not handled.');

end;

procedure TMainForm.DWD_TreeViewSelectionChanged(Sender: TObject);
var
  station: TDWD_Station;
  i, items: Integer;
  tree: TTreeView;
  crs: TCursor;
begin
  tree := Sender as TTreeView;

  if tree.Selected = nil then
    exit;
  if tree.Selected.Parent = nil then
    exit;

  items := 0;
  for i := 0 to cgDWD_PlotData.Items.Count-1 do
    if cgDWD_PlotData.Checked[i] then items := items or (1 shl i);
  if items = 0 then
  begin
    MessageDlg('Please select at least one item.', mtError, [mbOK], 0);
    exit;
  end;

  crs := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    station := TDWD_Station(tree.Selected.Data);
    if station.Data.Count = 0 then
    begin
      station.LoadData;
      if station.Data.Count > 0 then
      begin
        tree.Selected.ImageIndex := IMG_LOCAL_FILE;
        tree.Selected.SelectedIndex := tree.Selected.ImageIndex;
      end;
    end;
    if station.Data.Count > 0 then
    begin
      station.PopulateGrid(DataGrid);
      station.CreateSeries(Chart, cbOverlayCurves.Checked, items);
      lblStationName.Caption := station.NiceStationName;
      lblStationInfo.Caption := station.Info;
      UpdateFileViewer(station);
    end;

    MeasurementTool.Enabled := false;
  finally
    Screen.Cursor := crs;
  end;
end;

function TMainForm.FindStationNode(ATree: TTreeView; AStationName: String): TTreeNode;
var
  countryNode, stationNode: TTreeNode;
  station: TStation;
begin
  AStationName := Uppercase(AStationName);
  countryNode := ATree.Items[0];
  while countryNode <> nil do begin
    stationNode := countryNode.GetFirstChild;
    while stationNode <> nil do begin
      station := TStation(stationNode.Data);
      if pos(AStationName, Uppercase(station.name)) = 1 then
      begin
        Result := stationNode;
        exit;
      end;
      stationNode := stationNode.GetNextSibling;
    end;
    countryNode := countryNode.GetNextSibling;
  end;
  Result := nil;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReadIni;

  lblStationName.Caption := '';
  lblStationInfo.Caption := '';

  rgGISS_Version.Controls[rgGISS_Version.ControlCount-1].BorderSpacing.Bottom := 6;
  Chart.ClearSeries;
  GISSV2_TreeView.Align := alClient;
  GISSv4_TreeView.Align := alClient;
  SetGISSVersion(rgGISS_Version.ItemIndex);

  DataSource_PageControlChange(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteIni;
  DWD_Stations.Free;
  GISSv2_Stations.Free;
  GISSv4_Stations.Free;
  PAGES2k_Stations.Free;
end;

procedure TMainForm.GISS_TreeViewSelectionChanged(Sender: TObject);
var
  station: TGISS_Station;
  i, items: Integer;
  tree: TTreeView;
  crs: TCursor;
begin
  tree := Sender as TTreeView;

  if tree.Selected = nil then
    exit;
  if tree.Selected.Parent = nil then
    exit;

  items := 0;
  for i := 0 to cgGISS_PlotData.Items.Count-1 do
    if cgGISS_PlotData.Checked[i] then items := items or (1 shl i);
  if items = 0 then
  begin
    MessageDlg('Please select at least one item.', mtError, [mbOK], 0);
    exit;
  end;

  crs := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    station := TGISS_Station(tree.Selected.Data);
    if station.Data.Count = 0 then
    begin
      station.LoadData;
      if station.Data.Count > 0 then
      begin
        tree.Selected.ImageIndex := IMG_LOCAL_FILE;
        tree.Selected.SelectedIndex := tree.Selected.ImageIndex;
      end;
    end;
    if station.Data.Count > 0 then
    begin
      station.PopulateGrid(DataGrid);
      station.CreateSeries(Chart, cbOverlayCurves.Checked, items);
      if station.Country <> '(other)' then
        lblStationName.Caption := station.NiceStationName + ' (' + station.Country + ')'
      else
        lblStationName.Caption := station.NiceStationName;
      lblStationInfo.Caption := station.Info;
      UpdateFileViewer(station);
    end;

    MeasurementTool.Enabled := false;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.LoadDWD;
var
  crs: TCursor;
begin
  crs := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    if DWD_Stations = nil then
      DWD_Stations := TDWD_StationList.Create;
    DWD_Stations.Load;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.LoadGISSv2;
var
  crs: TCursor;
begin
  crs := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    if GISSv2_Stations = nil then
      GISSv2_Stations := TGISSv2_StationList.Create;
    GISSv2_Stations.Load;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.LoadGISSv4;
var
  crs: TCursor;
begin
  crs := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    if GISSv4_Stations = nil then
      GISSv4_Stations := TGISSv4_StationList.Create;
    GISSv4_Stations.Load;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.LoadPAGES2k;
var
  crs: TCursor;
begin
  crs := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    PAGES2k_Stations := TPAGES2k_StationList.Create;
    PAGES2k_Stations.Load;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.MeasurementToolAfterMouseUp(ATool: TChartTool;
  APoint: TPoint);
begin
  FreeAndNil(FFitSeries);
end;

procedure TMainForm.MeasurementToolGetDistanceText(
  ASender: TDataPointDistanceTool; var AText: String);
var
  xmin, xmax: Double;
begin
  if ChartListbox.ItemIndex = -1 then
    exit;

  xmin := ASender.PointStart.AxisPos.X;
  xmax := ASender.PointEnd.AxisPos.X;
  EnsureOrder(xmin, xmax);

  if FFitSeries = nil then
    CreateFitSeries;
  with FFitSeries.FitRange do begin
    Min := xmin;
    Max := xmax;
    if xmin < xmax then begin
      UseMax := true;
      UseMin := true;
    end else begin
      UseMin := true;
      UseMax := true;
    end;
  end;

  FFitSeries.Source := (ChartListbox.Series[ChartListbox.ItemIndex] as TChartSeries).Source;
  FFitSeries.Active := true;
  FFitSeries.ExecFit;
  if FFitSeries.ErrorMsg <> '' then
    AText := FFitSeries.ErrorMsg
  else
    AText := Format('Slope: %.5f °C/a', [FFitSeries.Param[1]]);
end;

procedure TMainForm.Pages2K_TreeViewSelectionChanged(Sender: TObject);
const
  IMG_INDEX: array[boolean] of Integer = (1, 0);
var
  station: TPages2k_Station;
  i: Integer;
  tree: TTreeView;
  crs: TCursor;
begin
  tree := Sender as TTreeView;

  if tree.Selected = nil then
    exit;
  if tree.Selected.Parent = nil then
    exit;

  crs := Screen.Cursor;
  try
    Screen.Cursor := crHourglass;
    station := TPAGES2k_Station(tree.Selected.Data);
    if station.Data.Count = 0 then
    begin
      station.LoadData;
      if station.Data.Count > 0 then
      begin
        tree.Selected.ImageIndex := station.GetImageIndex(true);
        tree.Selected.SelectedIndex := tree.Selected.ImageIndex;
      end;
    end;
    if station.Data.Count > 0 then
    begin
      station.PopulateGrid(DataGrid);
      station.CreateSeries(Chart, cbOverlayCurves.Checked, 1);
      if station.Country <> '(other)' then
        lblStationName.Caption := station.NiceStationName + ' (' + station.Country + ')'
      else
        lblStationName.Caption := station.NiceStationName;
      lblStationInfo.Caption := station.Info;
      UpdateFileViewer(station);
    end;

    MeasurementTool.Enabled := false;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
begin
  ini := CreateIni;
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Screen.WorkareaRect;
    if W > R.Right - R.Left then W := R.Right - R.Left;
    if H > R.Bottom - R.Top then H := R.Bottom - R.Top;
    if L + W > R.Right then L := R.Right - W;
    if L < R.Left then L := R.Left;
    if T + H > R.Bottom then T := R.Bottom - H;
    if T < R.Top then T := R.Top;
    SetBounds(L, T, W, H);

    LeftPanel.Width := ini.ReadInteger('MainForm', 'LeftPanelWidth', LeftPanel.Width);
    ChartListbox.Width := ini.ReadInteger('MainForm', 'ChartListBoxWidth', ChartListBox.Width);

    DataSource_PageControl.ActivePageIndex := ini.ReadInteger('MainForm', 'DataSource', DataSource_PageControl.ActivePageIndex);
    rgGISS_Version.ItemIndex := ini.ReadInteger('MainForm', 'GISS_Version', rgGISS_Version.ItemIndex);

    DataDir := ini.ReadString('Settings', 'DataDir', DataDir);
    cbLocalFilesOnly.Checked := ini.ReadBool('Settings', 'LocalFilesOnly', cbLocalFilesOnly.Checked);

    L := ini.ReadInteger('FileViewer', 'Left', -1);
    T := ini.ReadInteger('FileViewer', 'Top', -1);
    W := ini.ReadInteger('FileViewer', 'Width', -1);
    H := ini.ReadInteger('FileViewer', 'Height', -1);
    R := Screen.WorkAreaRect;
    if W > R.Right - R.Left then W := R.Right - R.Left;
    if H > R.Bottom - R.Top then H := R.Bottom - R.Top;
    if L + W > R.Right then L := R.Right - W;
    if L < R.Left then L := R.Left;
    if T + H > R.Bottom then T := R.Bottom - H;
    if T < R.Top then T := R.Top;
    FileViewerLeft := L;
    FileViewerTop := T;
    FileViewerWidth := W;
    FileViewerHeight := H;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.rgGISS_VersionClick(Sender: TObject);
begin
  SetGISSVersion(rgGISS_Version.ItemIndex);
end;

procedure TMainForm.SearchComboEditingDone(Sender: TObject);
var
  node: TTreeNode;
  idx: Integer;
  stationName: String;
  combo: TCombobox;
  tree: TTreeView;
  wasSelected: Boolean;
begin
  if Sender = cmbDWD_Search then
  begin
    combo := cmbDWD_Search;
    tree := DWD_TreeView;
  end else
  if Sender = cmbGISS_Search then
  begin
    combo := cmbGISS_Search;
    if rgGISS_Version.ItemIndex = 0 then
      tree := GISSv2_TreeView
    else
      tree := GISSv4_TreeView;
  end else
    raise Exception.Create('Unhandled combobox for searching.');

  node := FindStationNode(tree, combo.Text);
  if node <> nil then begin
    node.Expand(true);
    node.MakeVisible;
    wasSelected := (node = tree.Selected);
    tree.Selected := node;
    stationName := TStation(node.Data).Name;
    idx := combo.Items.IndexOf(stationName);
    if idx = -1 then begin
      idx := 0;
      combo.Items.Insert(0, stationName);
    end;
    if combo.Items.Count > 25 then
      combo.Items.Delete(combo.Items.Count-1);
    combo.Text := combo.Items[idx];
    combo.SelectAll;
    {
    if not wasSelected then
    begin
      if tree = DWD_TreeView then
        DWD_TreeViewSelectionChanged(tree)
      else
        GISS_TreeViewSelectionChanged(tree);
    end;
    }
    exit;
  end;
end;

procedure TMainForm.SetGISSVersion(AVersionIndex: Integer);
begin
  GISSv2_TreeView.Visible := AVersionIndex = 0;
  GISSv4_TreeView.Visible := AVersionIndex = 1;
  if DataSource_PageControl.ActivePage = pgGISS then
    case AVersionIndex of
      0: if GISSv2_TreeView.Items.Count = 0 then
         begin
           LoadGISSv2;
           GISSv2_Stations.PopulateTreeView(GISSv2_TreeView, cbLocalFilesOnly.Checked);
         end;
      1: if GISSv4_TreeView.Items.Count = 0 then
         begin
           LoadGISSv4;
           GISSv4_Stations.PopulateTreeView(GISSv4_TreeView, cbLocalFilesOnly.Checked);
         end;
    end;
end;

procedure TMainForm.ShowDataFile(AStation: TStation);
begin
  if FileViewerForm = nil then begin
    FileViewerForm := TFileViewerForm.Create(Application);
    FileViewerForm.OnClose := @CloseFileViewerHandler;
  end;

  if (FileViewerLeft = -1) or (FileViewerTop = -1) or (FileViewerWidth = -1) or (FileViewerHeight = -1) then
    FileViewerForm.Position := poScreenCenter
  else
  begin
    FileViewerForm.Position := poDesigned;
    FileViewerForm.SetBounds(FileViewerLeft, FileViewerTop, FileViewerWidth, FileViewerHeight);
  end;
  FileViewerForm.Station := AStation;
  FileViewerForm.Show;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
var
  F: TAboutForm;
begin
  F := TAboutForm.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.UpdateFileViewer(AStation: TStation);
begin
  if (FileViewerForm = nil) or (not FileViewerForm.Showing) then exit;
  FileViewerForm.Station := AStation;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    if WindowState = wsNormal then
    begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;

    ini.WriteInteger('MainForm', 'LeftPanelWidth', LeftPanel.Width);
    ini.WriteInteger('MainForm', 'ChartListBoxWidth', ChartListBox.Width);

    ini.WriteInteger('MainForm', 'DataSource', DataSource_PageControl.ActivePageIndex);
    ini.WriteInteger('MainForm', 'GISS_Version', rgGISS_Version.ItemIndex);

    ini.WriteBool('Settings', 'LocalFilesOnly', cbLocalFilesOnly.Checked);
    ini.WriteString('Settings', 'DataDir', DataDir);

    if (FileViewerForm <> nil) and (FileViewerForm.WindowState = wsNormal) then
    begin
      FileViewerLeft := FileViewerForm.Left;
      FileViewerTop := FileViewerForm.Top;
      FileViewerWidth := FileViewerForm.Width;
      FileViewerHeight := FileViewerForm.Height;
    end;
    ini.WriteInteger('FileViewer', 'Left', FileViewerLeft);
    ini.WriteInteger('FileViewer', 'Top', FileViewerTop);
    ini.WriteInteger('FileViewer', 'Width', FileViewerWidth);
    ini.WriteInteger('FileViewer', 'Height', FileViewerHeight);
  finally
    ini.Free;
  end;
end;

end.

