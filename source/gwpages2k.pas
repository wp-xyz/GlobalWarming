unit gwPages2k;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids,
  TAGraph, TACustomSource, TASources,
  gwData, gwStations;

type
  TPages2k_DataItem = class(TBasicDataItem)
    TemperatureError: Double;
    constructor Create; override;
    procedure Assign(ASource: TBasicDataItem); override;
  end;

  TContainsTemperature = (ctUnknown, ctYes, ctNo);

  TPages2k_Station = class(TStation)
  private
    FSiteName: String;
    FContainsTemperature: TContainsTemperature;
    FHasErrorbars: Boolean;
    procedure ParseHeader(ALine: String; AHeader: TStrings);
  protected
    procedure DownloadData; override;
    procedure GetChartDataItemHandler(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem); override;
    function GetCountry: String; override;
    function GetDataFileName: String; override;
    function GetDataURL: String; override;
    procedure ProcessData(AStream: TStream); override;
  public
    constructor Create(const AName, AID: String); override;
    procedure CreateSeries(AChart: TChart; AOverlaySeries: Boolean;
      AItems: Integer); override;
    function GetImageIndex(IsLocalFile: Boolean): Integer; override;
    procedure LoadData; override;
    function NiceStationName: String; override;
    procedure PopulateGrid(AGrid: TStringGrid); override;
    property ContainsTemperature: TContainsTemperature read FContainsTemperature;
  end;

  TPages2k_StationList = class(TStationList)
  protected
    function CreateStationFromLine(const ALine: String): TStation; override;
    procedure Download; override;
    procedure GetStationInfo(AStations: TStrings);
    function GetStationListFileName: String; override;
    function GetStationListURL: String; override;
    procedure Process(AStream: TStream; {%H-}Downloaded: Boolean); override;
  end;

implementation

uses
  math, LazFileUtils, Controls, Forms, Dialogs,
  TATypes, TACustomSeries, TASeries,
  gwGlobal, gwUtils, gwHTMLUtils;

const
  PAGES2K_DATA_DIR = 'pages2k/';
  PAGES2K_STATIONS_FILE = 'sites.txt';
  PAGES2K_STATIONS_URL = 'https://www1.ncdc.noaa.gov/pub/data/paleo/pages2k/pages2k-temperature-v2-2017/data-version-2.0.0/';

  SPACE = #32;
  TAB = #9;

{ TPages2k_DataItem }

constructor TPages2k_DataItem.Create;
begin
  inherited;
  TemperatureError := -999;
end;

procedure TPages2k_DataItem.Assign(ASource: TBasicDataItem);
begin
  if ASource is TPages2k_DataItem then begin
    inherited;
    TemperatureError := TPages2K_DataItem(ASource).TemperatureError;
  end else
    raise Exception.Create('Cannot assign ' + ASource.ClassName + ' to TPages2k_DataItem');
end;


{ TPages2k_Station }

constructor TPages2k_Station.Create(const AName, AID: String);
begin
  inherited Create(AName, AID);
  FContainsTemperature := ctUnknown;
end;

procedure TPages2k_Station.CreateSeries(AChart: TChart; AOverlaySeries: Boolean;
  AItems: Integer);
var
  ser: TLineSeries;
  uds: TUserDefinedChartSource;
begin
  inherited;

  FSeriesItems := 1;

  // Create new series and a chart source for it.
  uds := TUserDefinedChartSource.Create(AChart);
  if FHasErrorBars then uds.YCount := 2 else uds.YCount := 1;
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
  if FHasErrorBars then
  begin
    ser.YErrorBars.Pen.Color := ser.LinePen.Color;
    ser.YErrorBars.Visible := true;
    uds.YErrorBarData.Kind := ebkChartSource;
    uds.YErrorBarData.IndexPlus := 1;
    uds.YErrorBarData.IndexMinus := 1;
  end;
  ser.Tag := PtrInt(self);

  // Add series to chart.
  AChart.AddSeries(ser);
end;

procedure TPages2k_Station.DownloadData;
var
  stream: TMemoryStream;
  url: String;
begin
  stream := TMemoryStream.Create;
  try
    url := DataURL;
    if DownloadFile(url, stream) then
    begin
      ProcessData(stream);
      stream.SaveToFile(DataFileName);
    end else
      MessageDlg('Cannot download from' + LineEnding + url, mtError, [mbOK], 0);
  finally
    stream.Free;
  end;
end;

procedure TPages2k_Station.GetChartDataItemHandler(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
var
  y: Double;
  item: TPages2k_DataItem;
begin
  item := TPages2k_DataItem(Data[AIndex]);
  AItem.X := item.Year;
  if IsErrorValue(item.AnnualMean) then
    AItem.Y := NaN
  else
    AItem.Y := item.AnnualMean;
  if FHasErrorBars then
    AItem.YList[0] := item.TemperatureError;
end;

function TPages2k_Station.GetCountry: String;
var
  s: String;
begin
  s := Copy(Name, 1, 3);
  case s of
    'Afr': Result := 'Africa';
    'Ant': Result := 'Antarctic';
    'Arc': Result := 'Arctic';
    'Asi': Result := 'Asia';
    'Aus': Result := 'Australia';
    'Eur': Result := 'Europe';
    'NAm': Result := 'North America';
    'Ocn': Result := 'Oceania';
    'SAm': Result := 'South America';
    else   Result := '(unknown)';
  end;
end;

// Name of data file in cache
function TPages2k_Station.GetDataFileName: String;
begin
  Result := AppendPathDelim(DataDir) + PAGES2K_DATA_DIR;
  if FContainsTemperature = ctUnknown then
  begin
    if FileExists(Result + FName) then
      FContainsTemperature := ctYes
    else if FileExists(Result + '-' + FName) then
      FContainsTemperature := ctNo
    end;
  if (FContainsTemperature in [ctUnknown, ctYes]) then
    Result := Result + FName
  else
    Result := Result + '-' + FName;
end;

function TPages2k_Station.GetDataURL: String;
begin
  Result := PAGES2K_STATIONS_URL + FName;
end;

procedure TPages2k_Station.LoadData;
begin
  inherited;
end;

function TPages2k_Station.GetImageIndex(IsLocalFile: Boolean): Integer;
begin
  if IsLocalFile then
    case FContainsTemperature of
      ctUnknown: Result := IMG_REMOTE_FILE;
      ctYes: Result := IMG_LOCAL_FILE;
      ctNo: Result := IMG_LOCAL_FILE_NO_TEMPERATURE;
    end
  else
    Result := IMG_REMOTE_FILE;
end;

function TPages2k_Station.NiceStationName: String;
begin
  Result := ChangeFileExt(Copy(FName, 5, MaxInt), '');
end;

procedure TPages2k_Station.ParseHeader(ALine: String; AHeader: TStrings);
var
  i: Integer;
  s: String;
begin
  s := '';
  for i := 1 to Length(ALine) do begin
    if not (ALine[i] in [SPACE, TAB]) then
    begin
      s := s + ALine[i];
      if s = 'uncertainty_temperature' then begin
        AHeader.Add(s);
        s := '';
      end;
    end else
    if s <> '' then
    begin
      AHeader.Add(s);
      s := '';
    end;
  end;
  if s <> '' then
    AHeader.Add(s);
end;

procedure TPages2k_Station.PopulateGrid(AGrid: TStringGrid);
var
  i, r: Integer;
  item: Tpages2k_DataItem;
  s: String;
begin
  AGrid.ColCount := 2;
  AGrid.RowCount := Data.Count + AGrid.FixedRows;

  AGrid.Cells[0, 0] := 'Year';
  AGrid.Cells[1, 0] := 'Annual';

  AGrid.Canvas.Font.Assign(AGrid.Font);
  AGrid.ColWidths[0] := AGrid.Canvas.TextWidth('  Year  ') + 2*varCellPadding;
  AGrid.ColWidths[1] := AGrid.Canvas.TextWidth('  -99.99 ± 0.99  ') + 2*varCellPadding;

  for i := 0 to Data.Count-1 do
  begin
    item := Data[i] as TPages2k_DataItem;
    r := i + AGrid.FixedRows;;
    AGrid.Cells[0, r] := IntToStr(item.Year);
    if FContainsTemperature = ctNo then
      AGrid.Cells[1, r] := 'n.v.'
    else
    if IsErrorValue(item.AnnualMean) then
      AGrid.cells[1, r] := ''
    else
    begin
      if IsErrorValue(item.TemperatureError) then
        s := Format('%.2f', [item.AnnualMean])
      else
        s := Format('%.2f ± %.2f', [item.AnnualMean, item.TemperatureError]);
      AGrid.Cells[1, r] := s;
    end;
  end;
end;

procedure TPages2k_Station.ProcessData(AStream: TStream);
var
  L: TStringList;
  i: Integer;
  s: String;
  sLonE, sLonW, sLatN, sLatS, sElev: String;
  p: Integer;
  item: TPages2k_DataItem;
  hdr: TStrings;
  idxYear: Integer;
  idxTemperature: Integer;
  idxTemperatureError: Integer;
  sa: TStringArray;
begin
  L := TStringList.Create;
  try
    L.LoadFromStream(AStream);
    AStream.Position := 0;

    if L.Count = 0 then
      exit;

    // Extract meta data
    sLatN := '';
    sLatS := '';
    sLonE := '';
    sLonW := '';
    sElev := '';
    i := 0;
    while i < L.Count do
    begin
      s := L[i];

      if pos('# Data:', s) = 1 then
        break;

      if s = '' then begin
        inc(i);
        Continue;
      end;

      if s[1] = '#' then begin
        p := pos('Site_Name: ', s);
        if p <> 0 then
          FSiteName := trim(Copy(s, p + Length('Site_Name: '), MaxInt));

        p := pos('Northernmost_Latitude: ', s);
        if p <> 0 then
          sLatN := trim(Copy(s, p + Length('Northernmost_Latitude: '), MaxInt));

        p := pos('Southernmost_Latitude: ', s);
        if p <> 0 then
          sLatS := trim(Copy(s, p + Length('Southernmost_Latitude: '), MaxInt));

        p := pos('Easternmost_Longitude: ', s);
        if p <> 0 then
          sLonE := trim(Copy(s, p + Length('Easternmost_Latitude: '), MaxInt));

        p := pos('Westernmost_Longitude: ', s);
        if p <> 0 then
          sLonW := trim(Copy(s, p + Length('Westernmost_Latitude: '), MaxInt));

        p := pos('Elevation: ', s);
        if p <> 0 then
          sElev := trim(Copy(s, p + Length('Elevation: '), MaxInt));
      end;

      inc(i);
    end;

    if (sLatN <> '') and (sLatS <> '') then
      FLatitude := (PtStrToFloat(sLatN) + PtStrToFloat(sLatS)) / 2
    else if (sLatN <> '') then
      FLatitude := PtStrToFloat(sLatN)
    else if (sLatS <> '') then
      FLatitude := PtStrToFloat(sLatS)
    else
      FLatitude := -999;

    if (sLonE <> '') and (sLonW <> '') then
      FLongitude := (PtStrToFloat(sLonE) + PtStrToFloat(sLonW)) / 2
    else if (sLonE <> '') then
      FLongitude := PtStrToFloat(sLonE)
    else if (sLonW <> '') then
      FLongitude := PtStrToFloat(sLonW)
    else
      FLongitude := -999;

    if sElev <> '' then
      FElevation := PtStrToFloat(sElev)
    else
      FElevation := -999;

    // Read header
    while (i < L.Count) do
    begin
      s := L[i];
      if (s <> '') and (s[1] <> '#') then
      begin
        hdr := TStringList.Create;
        try
          ParseHeader(s, hdr);
          idxYear := hdr.IndexOf('year');
          idxTemperature := hdr.IndexOf('temperature');
          idxTemperatureError := hdr.IndexOf('uncertainty_temperature');
          inc(i);
          Break;
        finally
          hdr.Free;
        end;
      end;
      inc(i);
    end;

    if idxTemperature > -1 then
      FContainsTemperature := ctYes
    else
      FContainsTemperature := ctNo;

    FHasErrorBars := idxTemperatureError > -1;

    // Read data
    while i < L.Count do
    begin
      s := L[i];
      if s <> '' then begin
        sa := s.Split(TAB);
        item := TPages2k_DataItem.Create;
        item.Year := round(PtStrToFloat(sa[idxYear]));
        if (FContainsTemperature = ctNo) or SameText(sa[idxTemperature], 'NaN') then
          item.AnnualMean := -999
        else
          item.AnnualMean := PtStrToFloat(sa[idxTemperature]);
        if (idxTemperatureError > -1) and not SameText(sa[idxTemperatureError], 'NaN') then
          item.TemperatureError := PtStrToFloat(sa[idxTemperatureError]);
        Data.Add(item);
      end;
      inc(i);
    end;
  finally
    L.Free;
  end;
end;


{ TPages2k_StationList }

function TPages2k_StationList.CreateStationFromLine(const ALine: String): TStation;
begin
  Result := TPages2k_Station.Create(ALine, '');
end;

procedure TPages2k_StationList.Download;
var
  stream: TMemoryStream;
  crs: TCursor;
  extractor: TLinkExtractor;
  lines: TStringList;
begin
  crs := Screen.Cursor;
  stream := TMemoryStream.Create;
  try
    Screen.Cursor := crHourglass;
    DownloadFile(GetStationListURL, stream);
    stream.Position := 0;
    lines := TStringList.Create;
    try
      extractor := TLinkExtractor.Create;
      try
        extractor.Process(stream, lines, '.txt');
      finally
        extractor.Free;
      end;
     // GetStationInfo(lines);
      lines.SaveToFile(GetStationListFileName);
    finally
      lines.Free;
    end;
  finally
    stream.Free;
    Screen.Cursor := crs;
  end;
end;

procedure TPages2k_StationList.GetStationInfo(AStations: TStrings);
var
  url: String;
  fn: String;
  stream: TMemoryStream;
  lines: TStringList;
  i: Integer;
  ok: Boolean;
  s: String;
  sLonE, sLonW, sLon, sLatN, sLatS, sLat, sElev: String;
  p: Integer;
begin
  stream := TMemoryStream.Create;
  lines := TStringList.Create;
  try
    for i := AStations.Count - 1 downto 0 do
    begin
      fn := AStations[i];
      url := PAGES2k_STATIONS_URL + fn;
      stream.Clear;
      DownloadFile(url, stream);
      stream.Position := 0;
      lines.LoadFromStream(stream);

      // Remove file names which do not contain explicit temperature data.
      ok := false;
      for s in lines do
        if (s <> '') and (s[1] <> '#') and (pos('temperature', s) <> 0) then
        begin
          ok := true;
          break;
        end;
      if not ok then
      begin
        AStations.Delete(i);
        Continue;
      end;

      sLonE := '';
      sLonW := '';
      sLatN := '';
      sLatS := '';
      sElev := '';
      for s in lines do
      begin
        if (s <> '') and (s[1] = '#') then
        begin
          p := pos('Northernmost_Latitude: ', s);
          if p <> 0 then
            sLatN := trim(Copy(s, p + Length('Northernmost_Latitude: ') + 1, MaxInt));
          p := pos('Southernmost_Latitude: ', s);
          if p <> 0 then
            sLatS := trim(Copy(s, p + Length('Southernmost_Latitude: ') + 1, MaxInt));
          p := pos('Easternmost_Longitude: ', s);
          if p <> 0 then
            sLonE := trim(Copy(s, p + Length('Easternmost_Latitude: ') + 1, MaxInt));
          p := pos('Westernmost_Longitude: ', s);
          if p <> 0 then
            sLonE := trim(Copy(s, p + Length('Westernmost_Latitude: ') + 1, MaxInt));
          p := pos('Elevation: ', s);
          if p <> 0 then
            sElev := trim(Copy(s, p + Length('Elevation: ') + 1, MaxInt));
        end else
          break;
      end;

      if (sLatN <> '') and (sLatS <> '') then
        sLat := Format('%.5f', [(PtStrTofloat(sLatN) + PtStrToFloat(sLatS)) / 2], PtFormatSettings)
      else if (sLatN <> '') then
        sLat := sLatN
      else if (sLatS <> '') then
        sLat := sLatS
      else
        sLat := '';

      if (sLonE <> '') and (sLonW <> '') then
        sLon := Format('%.5f', [(PtStrToFloat(sLonE) + PtStrToFloat(sLonW)) / 2], PtFormatSettings)
      else if (sLonE <> '') then
        sLon := sLonE
      else if (sLonW <> '') then
        sLon := sLonW
      else
        sLon := '';

      AStations[i] := Format('File=%s;Lon=%s;Lat=%s;Elev=%s', [AStations[i], sLon, sLat, sElev]);
    end;
  finally
    lines.Free;
    stream.Free;
  end;
end;

function TPages2k_StationList.GetStationListFileName: String;
begin
  Result := AppendPathDelim(DataDir) + PAGES2K_DATA_DIR + PAGES2K_STATIONS_FILE;
end;

function TPages2k_StationList.GetStationListURL: String;
begin
  Result := PAGES2K_STATIONS_URL;
end;

procedure TPages2k_StationList.Process(AStream: TStream; Downloaded: Boolean);
var
  L: TStringList;
  i: Integer;
  station: TStation;
begin
  Clear;
  L := TStringList.Create;
  try
    L.LoadFromStream(AStream);
    for i := 0 to L.Count - 1 do begin  // i:=2 --> skip 2 header lines
      if L[i] = '' then
        Continue;
      station := CreateStationFromLine(L[i]);
      if assigned(station) then
        Add(station);
    end;
  finally
    L.Free;
  end;
end;

end.

