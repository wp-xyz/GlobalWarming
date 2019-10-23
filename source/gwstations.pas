unit gwStations;

{$mode objfpc}{$H+}

interface

uses
  Graphics, Classes, SysUtils, Contnrs, ComCtrls, Grids,
  TACustomSource, TASources, TAGraph,
  gwData;

type
  TStation = class
  private
    FData: TDataList;
  protected
    FName: String;
    FID: String;
    FDataClass: TDataItemClass;
    FSeriesItems: Integer;
    FLatitude: Double;
    FLongitude: Double;
    FElevation: Double;
    procedure DownloadData; virtual;
    procedure GetChartDataItemHandler(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    function GetCountry: String; virtual;
    function GetDataFileName: String; virtual;
    function GetDataURL: String; virtual;
    function GetLegendTitle: String; virtual;
    procedure LoadDataFromCache(AFileName: String);
    procedure ProcessData(AStream: TStream); virtual;
  public
    constructor Create(const AName, AID: String); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TStation); virtual;
    procedure CreateSeries(AChart: TChart; AOverlaySeries: Boolean; AItems: Integer);
    function Info: String;
    procedure LoadData;
    function NiceStationName: String; virtual;
    procedure PopulateGrid(AGrid: TStringGrid); virtual;
    property Country: String read GetCountry;
    property Data: TDataList read FData;
    property DataFileName: String read GetDataFileName;
    property DataURL: String read GetDataURL;
    property Elevation: Double read FElevation;
    property Name: String read FName;
    property ID: String read FID;
    property Latitude: Double read FLatitude;
    property Longitude: Double read FLongitude;
    property SeriesItems: Integer read FSeriesItems;
  end;

  TStationClass = class of TStation;

  TStationList = class(TObjectList)
  private
    function GetItem(AIndex: Integer): TStation;
    procedure SetItem(AIndex: Integer; const AValue: TStation);
  protected
    function CreateStationFromLine(const ALine: String): TStation; virtual;
    procedure Download; virtual;
    function GetStationListFileName: String; virtual;
    function GetStationListURL: String; virtual;
    procedure LoadFromCache(const AFileName: String); virtual;
    procedure Process(AStream: TStream; Downloaded: Boolean); virtual;
  public
    procedure Load;
    procedure PopulateTreeView(ATreeView: TTreeView; LocalOnly: Boolean);
    property Items[AIndex: Integer]: TStation read GetItem write SetItem; default;
    property StationListFileName: String read GetStationListFileName;
    property StationListURL: String read GetStationListURL;
  end;


implementation

uses
  StrUtils, Math, DateUtils, Controls, Forms,
  TATypes, TAStyles, TALegend, TASeries,
  gwGlobal, gwUtils;

constructor TStation.Create(const AName, AID: String);
begin
  FName := AName;
  FID := AID;
  FData := TDataList.Create;
  FDataClass := TDataItem;

  FLatitude := -999;
  FLongitude := -999;
  FElevation := -999;
end;

destructor TStation.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TStation.Assign(ASource: TStation);
var
  item: TDataItem;
  i: Integer;
begin
  FName := ASource.Name;
  FID := ASource.ID;
  FLatitude := ASource.Latitude;
  FLongitude := ASource.Longitude;
  FElevation := ASource.Elevation;
  FData.Clear;
  FDataClass := ASource.FDataClass;
  for i:=0 to ASource.Data.Count-1 do
  begin
    item := FDataClass.Create;
    item.Assign(ASource.Data[i]);
    FData.Add(item);
  end;
end;

procedure TStation.CreateSeries(AChart: TChart; AOverlaySeries: Boolean;
  AItems: Integer);
var
  ser: TLineSeries;
  uds: TUserDefinedChartSource;
  styles: TChartStyles;
  i: Integer;
  n: Integer;
begin
  // Single series only --> erase existing series and their chart sources.
  if not AOverlaySeries then
  begin
    for i:=0 to AChart.SeriesCount-1 do
    begin
      if AChart.Series[i] is TLineSeries then
      begin
        ser := TLineSeries(AChart.Series[i]);
        if ser.Source is TUserDefinedChartSource then
        begin
          uds := ser.Source as TUserDefinedChartSource;
          uds.Free;
        end;
        ser.Styles.Free;
      end;
    end;
    AChart.ClearSeries;
    AChart.Legend.Visible := false;
  end;

  // Determine how many "stack levels" are required
  n := 0;
  if AItems and PI_MET_ANNUAL <> 0 then inc(n); // Meteorological annual average
  if AItems and PI_ANNUAL <> 0 then inc(n);     // Annual average
  if AItems and PI_WINTER <> 0 then inc(n);     // Winter average
  if AItems and PI_SPRING <> 0 then inc(n);     // Spring average
  if AItems and PI_SUMMER <> 0 then inc(n);     // Sommer average
  if AItems and PI_FALL <> 0 then inc(n);       // Fall average
  if AItems and PI_MONTH <> 0 then inc(n);      // Monthly average
  if n = 0 then n := 1;
  FSeriesItems := Max(AItems, 1);

  // Create new series and a chart source for it.
  uds := TUserDefinedChartSource.Create(AChart);
  uds.YCount := n;
  if AItems and PI_MONTH <> 0 then
    uds.PointsNumber := Data.Count * 12    // 12 values per record!
  else
    uds.PointsNumber := Data.Count;
  uds.OnGetChartDataItem := @GetChartDataItemHandler;
  uds.Tag := PtrInt(self);
  ser := TLineSeries.Create(AChart);
  ser.Source := uds;
  ser.Title := GetLegendTitle;
  ser.LinePen.Color := GetSeriesColor(AChart);
  ser.ShowPoints := true;
  ser.Pointer.Brush.Color := ser.LinePen.Color;
  ser.Pointer.Style := psCircle;
  ser.Tag := PtrInt(self);

  if n > 1 then begin
    AChart.Legend.Visible := true;
    ser.Legend.Multiplicity := lmStyle;
    styles := TChartStyles.Create(AChart);
    ser.Styles := styles;
    if FSeriesItems and PI_MET_ANNUAL <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (annual mean/Dec-Nov)';
        Pen.Style := psSolid;
        Pen.Color := ser.LinePen.Color;
        Pen.Width := 2;
        Brush.Color := ser.Pointer.Brush.Color;
      end;
    if FSeriesItems and PI_ANNUAL <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (annual mean/Jan-Dec)';
        Pen.Style := psSolid;
        Pen.Color := ser.LinePen.Color;
        Brush.Color := ser.Pointer.Brush.Color;
      end;
    if FSeriesItems and PI_WINTER <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (winter mean)';
        Pen.Style := psDash;
        Pen.Color := ser.LinePen.Color;
        Brush.Color := ser.Pointer.Brush.Color;
      end;
    if FSeriesItems and PI_SPRING <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (spring mean)';
        Pen.Style := psDot;
        Pen.Color := ser.LinePen.Color;
        Brush.Color := ser.Pointer.Brush.Color;
      end;
    if FSeriesItems and PI_SUMMER <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (sommer mean)';
        Pen.Style := psDashDot;
        Pen.Color := ser.LinePen.Color;
        Brush.Color := ser.Pointer.Brush.Color;
      end;
    if FSeriesItems and PI_FALL <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (fall mean)';
        Pen.Style := psDashDotDot;
        Pen.Color := ser.LinePen.Color;
        Brush.Color := ser.Pointer.Brush.Color;
      end;
    if FSeriesItems and PI_MONTH <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (monthly mean)';
        Pen.Style := psSolid;
        Pen.Color := ser.LinePen.Color;
        ser.Pointer.Style := psNone;
      end;
  end;

  // Add series to chart.
  AChart.AddSeries(ser);
end;

procedure TStation.DownloadData;
begin
  // to be overridden
  // Must read download the data file, process it (ProcessData) and store
  // the datafile to a cache file with name GetDataFileName.
end;

procedure TStation.GetChartDataItemHandler(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  idx: Integer;
  y: Double;
  dataItem: TDataItem;
begin
  if FSeriesItems and PI_MONTH <> 0 then
    dataItem := Data[AIndex div 12]
  else
    dataItem := Data[AIndex];

  if FSeriesItems and PI_MONTH <> 0 then
    AItem.X := dataItem.Year + frac(AIndex/12)
  else
    AItem.X := dataItem.Year;

  idx := 0;
  if FSeriesItems and PI_MET_ANNUAL <> 0 then
  begin
    y := dataItem.MetAnnualMean;
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and PI_ANNUAL <> 0 then
  begin
    y := dataItem.MetAnnualMean;
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and PI_WINTER <> 0 then
  begin
    y := dataItem.SeasonalMean[sWinter];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and PI_SPRING <> 0 then
  begin
    y := dataItem.SeasonalMean[sSpring];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and PI_SUMMER <> 0 then
  begin
    y := dataItem.SeasonalMean[sSummer];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and PI_FALL <> 0 then
  begin
    y := dataItem.SeasonalMean[sFall];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and PI_MONTH <> 0 then
  begin
    y := dataItem.MonthlyMean[TMonth(AIndex mod 12)];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;
end;

function TStation.GetCountry: String;
begin
  Result := '';
end;

{ Returns the name of a local data file, with path. }
function TStation.GetDataFileName: String;
begin
  // to be overridden by descendants.
  Result := '';
end;

{ Returns the full url for download of the temperature data of this station }
function TStation.GetDataURL: String;
begin
  // to be overridden by descendants.
  Result := '';
end;

function TStation.GetLegendTitle: String;
begin
  Result := NiceStationName;
end;

function TStation.Info: String;
var
  sLat, sLon, sElev: String;
begin
  if IsErrorValue(FLatitude) then
    sLat := '-'
  else
    sLat := Format('%.4f°', [FLatitude]);

  if IsErrorValue(FLongitude) then
    sLon := '-'
  else
    sLon := Format('%.4f°', [FLongitude]);

  if IsErrorValue(FElevation) then
    sElev := '-'
  else
    sElev := Format('%.0fm', [FElevation]);

  Result := Format('Station ID: %s, Latitude: %s, Longitude: %s, Elevation: %s', [ID, sLat, sLon, sElev]);
end;

procedure TStation.LoadData;
var
  fn: String;
begin
  fn := DataFileName;
  ForceDirectories(ExtractFilePath(fn));
  if FileExists(fn) then
    LoadDataFromCache(fn)
  else
    DownloadData;
end;

procedure TStation.LoadDataFromCache(AFileName: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    ProcessData(stream);
  finally
    stream.Free;
  end;
end;

function TStation.NiceStationName: String;
begin
  Result := FName;
end;

procedure TStation.PopulateGrid(AGrid: TStringGrid);
var
  i, j, r: Integer;
  dataItem: TDataItem;
  value: Double;
begin
  AGrid.ColCount := 19;
  AGrid.RowCount := Data.Count + AGrid.FixedRows;

  AGrid.Cells[0, 0] := 'Year';
  AGrid.Cells[1, 0] := 'Jan';
  AGrid.Cells[2, 0] := 'Feb';
  AGrid.Cells[3, 0] := 'Mar';
  AGrid.Cells[4, 0] := 'Apr';
  AGrid.Cells[5, 0] := 'May';
  AGrid.Cells[6, 0] := 'June';
  AGrid.Cells[7, 0] := 'July';
  AGrid.Cells[8, 0] := 'Aug';
  AGrid.Cells[9, 0] := 'Sep';
  AGrid.Cells[10, 0] := 'Oct';
  AGrid.Cells[11, 0] := 'Nov';
  AGrid.Cells[12, 0] := 'Dec';
  AGrid.Cells[13, 0] := 'Winter' + LineEnding + 'DJF';
  AGrid.Cells[14, 0] := 'Spring' + LineEnding + 'MAM';
  AGrid.Cells[15, 0] := 'Sommer' + LineEnding + 'JJA';
  AGrid.Cells[16, 0] := 'Fall' + LineEnding + 'SON';
  AGrid.Cells[17, 0] := 'Annual' + LineEnding + 'Jan-Dec';
  AGrid.Cells[18, 0] := 'Annual' + LineEnding + 'Dec-Nov';

  AGrid.Canvas.Font.Assign(AGrid.Font);
  AGrid.DefaultColWidth := Max(AGrid.Canvas.TextWidth(' -999.9 '), AGrid.Canvas.TextWidth('  July  '));;
  AGrid.ColWidths[13] := AGrid.Canvas.TextWidth('  Sommer  ');
  AGrid.ColWidths[14] := AGrid.ColWidths[13];
  AGrid.ColWidths[15] := AGrid.ColWidths[13];
  AGrid.ColWidths[16] := AGrid.ColWidths[13];
  AGrid.ColWidths[17] := Max(AGrid.Canvas.TextWidth('Annual'), AGrid.Canvas.TextWidth('Dec-Nov')) + 2*varCellPadding;
  AGrid.ColWidths[18] := AGrid.ColWidths[17];
  AGrid.RowHeights[0] := (AGrid.Canvas.TextHeight('Tg') + varCellPadding) * 2;

  for i := 0 to Data.Count-1 do
  begin
    dataItem := Data[i];
    r := i + AGrid.FixedRows;;
    AGrid.Cells[0, r] := IntToStr(dataItem.Year);
    for j := 1 to 12 do
    begin
      value := dataItem.MonthlyMean[TMonth(j-1)];
      AGrid.Cells[j, r] := IfThen(IsErrorValue(value), '', FormatFloat('0.0', value));
    end;
    for j := 13 to 16 do
    begin
      value := dataItem.SeasonalMean[TSeason(j-13)];
      AGrid.Cells[j, r] := IfThen(IsErrorValue(value), '', FormatFloat('0.0', value));
    end;
    AGrid.Cells[17, r] := IfThen(IsErrorValue(dataItem.AnnualMean), '', FormatFloat('0.0', dataitem.AnnualMean));
    AGrid.Cells[18, r] := IfThen(IsErrorValue(dataItem.MetAnnualMean), '', FormatFloat('0.0', dataitem.MetAnnualMean));
  end;
end;


procedure TStation.ProcessData(AStream: TStream);
begin
end;


{ TStationList }

function TStationList.CreateStationFromLine(const ALine: String): TStation;
begin
  // to be overridden by descendants
  // Must extract the station parameters from ALine and create a TStation.
  Result := nil;
end;

procedure TStationList.Download;
var
  stream: TMemoryStream;
  crs: TCursor;
begin
  crs := Screen.Cursor;
  stream := TMemoryStream.Create;
  try
    Screen.Cursor := crHourglass;
    DownloadFile(GetStationListURL, stream);
    stream.Position := 0;
    Process(stream, true);
    stream.Position := 0;
    stream.SaveToFile(GetStationListFileName);
  finally
    stream.Free;
    Screen.Cursor := crs;
  end;
end;

function TStationList.GetItem(AIndex: Integer): TStation;
begin
  Result := TStation(inherited Items[AIndex]);
end;

function TStationList.GetStationListFileName: String;
begin
  // to be overridden by descendants
  Result := '';
end;

function TStationList.GetStationListURL: String;
begin
  // to be overridden by descendants
  Result := '';
end;

procedure TStationList.Load;
var
  fn: String;
begin
  fn := GetStationListFileName;
  ForceDirectories(ExtractFilePath(fn));
  if FileExists(fn) then
    LoadFromCache(fn)
  else
    Download;
end;

procedure TStationList.LoadFromCache(const AFileName: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    Process(stream, false);
  finally
    stream.Free;
  end;
end;

procedure TStationList.PopulateTreeView(ATreeView: TTreeView;
  LocalOnly: Boolean);
var
  countrynode, stationnode: TTreeNode;
  station: TStation;
  country: String;
  i: Integer;
  isLocal: Boolean;
  crs: TCursor;
begin
  crs := Screen.Cursor;
  ATreeView.Items.BeginUpdate;
  try;
    Screen.Cursor := crHourglass;
    ATreeView.Items.Clear;
    for i:=0 to Count-1 do begin
      station := Items[i] as TStation;
      if station.NiceStationName = '' then
        Continue;
      isLocal := FileExists(station.DataFileName);
      if (LocalOnly and isLocal) or (not LocalOnly) then
      begin
        country := station.GetCountry;
        if country = '' then  // stations without country will be put into a top-level node
          countrynode := nil
        else begin
          countrynode := ATreeView.Items.FindNodeWithText(country);
          if countrynode = nil then
            countrynode := ATreeView.Items.AddChild(nil, country);
        end;
        stationnode := ATreeView.Items.AddChildObject(countrynode, station.NiceStationName, station);
        if isLocal then
          stationnode.ImageIndex := IMG_LOCAL_FILE
        else
          stationnode.ImageIndex := IMG_REMOTE_FILE;
        stationnode.SelectedIndex := stationNode.ImageIndex;
      end;
    end;
    ATreeView.AlphaSort;
  finally
    ATreeView.Items.EndUpdate;
    ATreeView.Invalidate;
    Screen.Cursor := crs;
  end;
end;

procedure TStationList.Process(AStream: TStream; Downloaded: Boolean);
begin
  // To be overridden by descendants.
  // Must extract the station parameters (name, ID, and whatever is needed)
  // from the stream, beginning at the current stream position.
end;

procedure TStationList.SetItem(AIndex: Integer; const AValue: TStation);
begin
  TStation(inherited Items[AIndex]).Assign(AValue);
end;

end.

