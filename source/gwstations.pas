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
      AIndex: Integer; var AItem: TChartDataItem); virtual;
    function GetCountry: String; virtual;
    function GetDataFileName: String; virtual;
    function GetDataURL: String; virtual;
    function GetLegendTitle: String; virtual;
    procedure LoadDataFromCache(AFileName: String); virtual;
    procedure ProcessData(AStream: TStream); virtual;
  public
    constructor Create(const AName, AID: String); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TStation); virtual;
    procedure CreateSeries(AChart: TChart; AOverlaySeries: Boolean; AItems: Integer); virtual;
    function GetImageIndex(IsLocalFile: Boolean): Integer; virtual;
    function Info: String;
    procedure LoadData; virtual;
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
  TATypes, TAStyles, TALegend, TACustomSeries, TASeries,
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
  i: Integer;
begin
  // Single series only --> erase existing series and their chart sources.
  if not AOverlaySeries then
  begin
    for i:=0 to AChart.SeriesCount-1 do
    begin
      if AChart.Series[i] is TChartSeries then
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

  FSeriesItems := Max(AItems, 1);

  // Now descendant must implement how series is constructed.
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
  y: Double;
  dataItem: TBasicDataItem;
begin
  dataItem := Data[AIndex];
  AItem.X := dataItem.Year;
  if IsErrorValue(dataItem.AnnualMean) then
    AItem.Y := NaN
  else
    AItem.Y := dataItem.AnnualMean;
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

function TStation.GetImageIndex(IsLocalFile: Boolean): Integer;
begin
  if IsLocalFile then Result := IMG_LOCAL_FILE else Result := IMG_REMOTE_FILE;
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
  dataItem: TBasicDataItem;
  value: Double;
begin
  AGrid.ColCount := 2;
  AGrid.RowCount := Data.Count + AGrid.FixedRows;

  AGrid.Cells[0, 0] := 'Year';
  AGrid.Cells[1, 0] := 'Annual';

  AGrid.Canvas.Font.Assign(AGrid.Font);
  AGrid.DefaultColWidth := Max(AGrid.Canvas.TextWidth(' -999.9 '), AGrid.Canvas.TextWidth('  Annual  '));;

  for i := 0 to Data.Count-1 do
  begin
    dataItem := Data[i];
    r := i + AGrid.FixedRows;;
    AGrid.Cells[0, r] := IntToStr(dataItem.Year);
    AGrid.Cells[1, r] := IfThen(IsErrorValue(dataItem.AnnualMean), '', FormatFloat('0.0', dataitem.AnnualMean));
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
        stationnode.ImageIndex := station.GetImageIndex(isLocal);
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

