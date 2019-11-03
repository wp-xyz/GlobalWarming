unit gwDeutscherWetterDienst;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SysUtils, Grids, TAGraph, TACustomSource, TASources,
  gwData, gwStations;

type
  TDWD_DataItem = class(TDataItem)
  public
    TotalSunshineHours: array[TMonth] of double;
    TotalPrecipitation: array[TMonth] of double;
    MaxDailyPrecipitation: array[TMonth] of double;
  end;

  { TDWD_Station }

  TDWD_Station = class(TStation)
  private
    FStartDate: TDate;
    FEndDate: TDate;
    FState: String;
    //function CalcDataFileName: String;
    //function CalcDataURL: String;
    function CalcMean(var AValues; ACount: Integer): Double;
    procedure DoCreateSeries(AChart: TChart; AItems: Integer);
    procedure GetChartDataItemHandler_DWD(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  protected
    procedure DownloadData; override;
    function GetDataFileName: String; override;
    function GetDataURL: String; override;
    function GetLegendTitle: String; override;
    procedure ProcessData(AStream: TStream); override;
    procedure ReadDir(ADirURL: String; AList: TStrings);
  public
    procedure CreateSeries(AChart: TChart; AOverlaySeries: Boolean;
      AItems: Integer); override;
    function GetCountry: String; override;
    procedure PopulateGrid(AGrid: TStringGrid); override;
    property EndDate: TDate read FEndDate;
    property StartDate: TDate read FStartDate;
    property State: String read FState;
  end;

  TDWD_StationList = class(TStationList)
  private
  protected
    function CreateStationFromLine(const ALine: String): TStation; override;
    function GetStationListFileName: String; override;
    function GetStationListURL: String; override;
    procedure Process(AStream: TStream; Downloaded: boolean); override;
  public
  end;

implementation

uses
  LConvEncoding, Math, DateUtils, LazFileUtils,
  StrUtils, Dialogs,
  TATypes, TALegend, TASTyles, TACustomSeries, TASeries, TAEnumerators,
  gwGlobal, gwUtils, gwHTMLUtils;

const
  DWD_STATION_SEPARATOR = ';';
  DWD_DATE_MASK = 'yyyymmdd';
  DWD_STATIONS_FILE = DEFAULT_STATIONS_FILE;
  DWD_DATA_DIR = 'dwd/';
//  DWD_STATIONLIST_URL = 'https://www.dwd.de/DE/leistungen/klimadatendeutschland/stationsuebersicht.html';
  DWD_STATIONLIST_URL = 'https://opendata.dwd.de/climate_environment/CDC/help/KL_Monatswerte_Beschreibung_Stationen.txt';

  DWD_DATA_SEPARATOR = ';';
  DWD_MONTHLY_DATA_DIR = 'https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/monthly/kl/historical/';
  DWD_DATA_URL = 'https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/monthly/kl/historical/monatswerte_KL_%s_%s_%s_hist.zip';
  DWD_DATA_FILE_MASK = 'produkt_klima_monat_%s_%s_%s.txt';
  DWD_CACHE_FILE_MASK = 'climate_mon_%s_%s.txt';


{ TDWD_Station }
                                      (*
function TDWD_Station.CalcDataFileName: String;
// https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/monthly/kl/historical/monatswerte_KL_00044_19710301_20181231_hist.zip
begin
  Result := Format(DWD_DATA_FILE_MASK, [
    ID,
    NiceFileName,
    FormatDateTime(DWD_DATE_MASK, FStartDate),
    FormatDateTime(DWD_DATE_MASK, FEndDate)
  ]);
end;                                    *)
                                  {
function TDWD_Station.CalcDataURL: String;
begin
  Result := Format(DWD_DATADIR + 'DWD_DATA_FILE_MASK, [
    FormatDateTime(DWD_DATE_MASK, FStartDate),
    FormatDateTime(DWD_DATE_MASK, FEndDate),
    ID
  ]);
end;                               }

function TDWD_Station.CalcMean(var AValues; ACount: Integer): Double;
type
  TDoubleArray = array[0..11] of Double;
var
  i: Integer;
  sum: Double;
begin
  sum := 0.0;
  for i:=0 to ACount-1 do
  begin
    if IsErrorValue(TDoubleArray(AValues)[i]) then
    begin
      Result := -999;
      exit;
    end;
    sum := sum + TDoubleArray(AValues)[i];
  end;
  Result := sum / ACount;
end;

procedure TDWD_Station.CreateSeries(AChart: TChart; AOverlaySeries: Boolean;
  AItems: Integer);
var
  ser: TCustomChartSeries;
  n: Integer;
begin
  inherited;
  (*
  // Single series only --> erase existing series and their chart sources.
  if not AOverlaySeries then
  begin
    for ser in CustomSeries(AChart) do
      if (ser is TChartSeries) and (TChartSeries(ser).Source is TUserDefinedChartSource) then
      begin
        TChartSeries(ser).Source.Free;
        if ser is TLineSeries then
          TLineSeries(ser).Styles.Free;
        if ser is TBarSeries then
          TBarSeries(ser).Styles.Free;
      end;
    AChart.ClearSeries;
    AChart.Legend.Visible := false;
  end;
  *)

  AItems := Max(AItems, 1);
  FSeriesItems := AItems;

  if AItems and PI_TEMPERATURE <> 0 then
    DoCreateSeries(AChart, AItems and PI_TEMPERATURE);
  if AItems and PI_SUNSHINE <> 0 then
    DoCreateSeries(AChart, AItems and PI_SUNSHINE);
  if AItems and PI_PRECIPITATION <> 0 then
    DoCreateSeries(AChart, AItems and PI_PRECIPITATION);

  AChart.AxisList[AX_TEMPERATURE].Visible := SeriesCountPerAxis(AChart, AX_TEMPERATURE) > 0;
  AChart.AxisList[AX_SUNSHINE].Visible := SeriesCountPerAxis(AChart, AX_SUNSHINE) > 0;
  AChart.AxisList[AX_PRECIPITATION].Visible := SeriesCountPerAxis(AChart, AX_PRECIPITATION) > 0;
end;

procedure TDWD_Station.DoCreateSeries(AChart: TChart; AItems: Integer);
var
  ser: TChartSeries;
  uds: TUserDefinedChartSource;
  styles: TChartStyles;
  i: Integer;
  clr: TColor;
  n: Integer;
begin

  // Determine how many "stack levels" are required
  n := 0;
  if AItems and PI_ANNUAL <> 0 then inc(n);         // Annual average
  if AItems and PI_MET_ANNUAL <> 0 then inc(n);     // Meteorological annual average
  if AItems and PI_WINTER <> 0 then inc(n);         // Winter average
  if AItems and PI_SPRING <> 0 then inc(n);         // Spring average
  if AItems and PI_SUMMER <> 0 then inc(n);         // Sommer average
  if AItems and PI_FALL <> 0 then inc(n);           // Fall average
  if AItems and PI_MONTH <> 0 then inc(n);          // Monthly average
  if AItems and PI_SUNSHINE <> 0 then inc(n);       // Sunshine duration
  if AItems and PI_MONTHLY_PRECIP <> 0 then inc(n); // total precipitation per month
  if AItems and PI_DAILY_PRECIP <> 0 then inc(n);   // max daily precipition of month
  if n = 0 then n := 1;

  // Create new series and a chart source for it.
  uds := TUserDefinedChartSource.Create(AChart);
  uds.YCount := n;
  if AItems and PI_MONTH_ITEMS <> 0 then
    uds.PointsNumber := Data.Count * 12    // 12 values per record!
  else
    uds.PointsNumber := Data.Count;
  uds.OnGetChartDataItem := @GetChartDataItemHandler_DWD;
  uds.Tag := AItems; //PtrInt(self);

  clr := GetSeriesColor(AChart);

  {
  if AItems and PI_TEMPERATURE <> 0 then
  begin
    }
    ser := TLineSeries.Create(AChart);
    with TLineSeries(ser) do
    begin
      LinePen.Color := clr;
      Pointer.Brush.Color := clr;
      if AItems and PI_TEMPERATURE <> 0 then
        Pointer.Style := psCircle
      else
        Pointer.Style := psRectangle;
      ShowPoints := true;
    end;
    {
  end else
  begin
    ser := TBarseries.Create(AChart);
    with TBarSeries(ser) do
      BarBrush.Color := clr;
  end;
  }
  ser.Source := uds;
  ser.Title := GetLegendTitle;
  ser.Tag := PtrInt(self);

  if AItems and PI_TEMPERATURE <> 0 then
    ser.AxisIndexY := AX_TEMPERATURE
  else if AItems and PI_SUNSHINE <> 0 then
    ser.AxisIndexY := AX_SUNSHINE
  else if AItems and PI_PRECIPITATION <> 0 then
    ser.AxisIndexY := AX_PRECIPITATION;

  if n >= 1 then begin
    AChart.Legend.Visible := true;
    ser.Legend.Multiplicity := lmStyle;
    styles := TChartStyles.Create(AChart);
    if ser is TLineSeries then
      TLineSeries(ser).Styles := styles
    else if ser is TBarSeries then
      TBarSeries(ser).Styles := styles;

    if AItems and PI_ANNUAL <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (annual mean/Jan-Dec)';
        Pen.Style := psSolid;
        Pen.Color := clr;
        Brush.Color := clr;
      end;
    if AItems and PI_MET_ANNUAL <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (annual mean/Dec-Nov)';
        Pen.Style := psSolid;
        Pen.Color := clr;
        Pen.Width := 2;
        Brush.Color := clr;
      end;
    if AItems and PI_WINTER <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (winter mean)';
        Pen.Style := psDash;
        Pen.Color := clr;
        Brush.Color := clr;
      end;
    if AItems and PI_SPRING <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (spring mean)';
        Pen.Style := psDot;
        Pen.Color := clr;
        Brush.Color := clr;
      end;
    if AItems and PI_SUMMER <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (sommer mean)';
        Pen.Style := psDashDot;
        Pen.Color := clr;
        Brush.Color := clr;
      end;
    if AItems and PI_FALL <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (fall mean)';
        Pen.Style := psDashDotDot;
        Pen.Color := clr;
        Brush.Color := clr;
      end;
    if AItems and PI_MONTH <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (monthly mean)';
        Pen.Style := psSolid;
        Pen.Color := clr;
        (ser as TLineSeries).Pointer.Style := psNone;
      end;
    if AItems and PI_SUNSHINE <> 0then
      with styles.Add do
      begin
        Text := ser.Title + ' (sunshine hours)';
        Pen.Style := psSolid;
        Pen.Color := clr;
        Brush.Color := clr;
      end;
    if AItems and PI_MONTHLY_PRECIP <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (total monthly precipitation)';
        Pen.Color := clr;
        Brush.Color := clr;
      end;
    if AItems and PI_DAILY_PRECIP <> 0 then
      with styles.Add do
      begin
        Text := ser.Title + ' (max daily precipitation)';
        Pen.Color := clr;
        Pen.Style := psDot;
        Brush.Color := clr;
      end;
  end;

  // Add series to chart.
  AChart.AddSeries(ser);
end;

procedure TDWD_Station.DownloadData;
var
  zipped_stream: TMemoryStream;
  stream: TMemoryStream;
  url: String;
  unzipper: TStreamUnzipper;
  fn: String;
  startDt, endDt: String;
begin
  zipped_stream := TMemoryStream.Create;
  try
    url := DataURL;
    if url = '' then
    begin
      MessageDlg('No measurement file found for ' + FName + ' (ID ' + FID + ')',
        mtError, [mbOK], 0);
      exit;
    end;
    if DownloadFile(url, zipped_stream) then
    begin
      zipped_stream.Position := 0;
      unzipper := TStreamUnzipper.Create(zipped_stream);
      stream := TMemoryStream.Create;
      try
        fn := ExtractFileName(url);   //monatswerte_KL_02410_20140101_20181231_hist.zip
        startDt := Copy(fn, 22, 8);
        endDt := Copy(fn, 31, 8);
        fn := Format(DWD_DATA_FILE_MASK, [startDt, endDt, ID]);           // Name in zip
        unzipper.UnzipFile(fn, stream);
        stream.Position := 0;
        stream.SaveToFile(DataFileName);
        stream.Position := 0;
        ProcessData(stream);
      finally
        stream.Free;
        unzipper.Free;
      end;
    end else
      MessageDlg('Cannot download from' + LineEnding + url, mtError, [mbOK], 0);
  finally
    zipped_stream.Free;
  end;
end;

procedure TDWD_Station.GetChartDataItemHandler_DWD(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  idx: Integer;
  y: Double;
  m: TMonth;
  dataItem: TDWD_DataItem;
  serItems: Integer;
begin
  serItems := ASource.Tag;
  if serItems and PI_MONTH_ITEMS <> 0 then
  begin
    dataItem := TDWD_DataItem(Data[AIndex div 12]);
    AItem.X := dataItem.Year + frac(AIndex/12);
    m := TMonth(AIndex mod 12);
  end else
  begin
    dataItem := TDWD_DataItem(Data[AIndex]);
    AItem.X := dataItem.Year;
  end;

  idx := 0;
  if serItems and PI_ANNUAL <> 0 then
  begin
    y := dataItem.MetAnnualMean;
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_MET_ANNUAL <> 0 then
  begin
    y := dataItem.MetAnnualMean;
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_WINTER <> 0 then
  begin
    y := dataItem.SeasonalMean[sWinter];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_SPRING <> 0 then
  begin
    y := dataItem.SeasonalMean[sSpring];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_SUMMER <> 0 then
  begin
    y := dataItem.SeasonalMean[sSummer];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_FALL <> 0 then
  begin
    y := dataItem.SeasonalMean[sFall];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_MONTH <> 0 then
  begin
    y := dataItem.MonthlyMean[m];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_SUNSHINE <> 0 then
  begin
    y := dataItem.TotalSunshineHours[m];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_MONTHLY_PRECIP <> 0 then
  begin
    y := dataItem.TotalPrecipitation[m];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if serItems and PI_DAILY_PRECIP <> 0 then
  begin
    y := dataItem.MaxDailyPrecipitation[m];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;
end;

function TDWD_Station.GetCountry: String;
begin
  Result := FState;
end;

// Read page https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/monthly/kl/historical/
// and find the entry with the station id. This contains the correct start/end dates.
function TDWD_Station.GetDataURL: String;
var
  L: TStrings;
  s: String;
begin
  L := TStringList.Create;
  try
    ReadDir(DWD_MONTHLY_DATA_DIR, L);
    for s in L do begin
      if (s = '') or (s[1] <> 'm') then
        Continue;
      if pos('monatswerte_KL_' + ID, s) = 1 then
      begin
        Result := DWD_MONTHLY_DATA_DIR + s;
        exit;
      end;
    end;
    Result := '';
  finally
    L.Free;
  end;
end;

(*
function TDWD_Station.GetDataURL: String;
// https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/monthly/kl/historical/monatswerte_KL_00044_19710301_20181231_hist.zip
begin
  result := Format(DWD_DATA_URL, [
    ID,
    FormatDateTime(DWD_DATE_MASK, FStartDate),
    FormatDateTime(DWD_DATE_MASK, FEndDate)
  ]);
end;
*)

// Name of data file in cache
function TDWD_Station.GetDataFileName: String;
begin
  Result := AppendPathDelim(DataDir) + DWD_DATA_DIR +
    Format(DWD_CACHE_FILE_MASK, [ID, StringReplace(Name,'/', '-', [rfReplaceAll])]);
end;

function TDWD_Station.GetLegendTitle: String;
begin
  Result := NiceStationName + ' (DWD)';
end;

procedure TDWD_Station.PopulateGrid(AGrid: TStringGrid);
var
  dataItem: TDWD_DataItem;
  m: TMonth;
  i, j, r: Integer;
  value: Double;
begin
  AGrid.ColCount := 55;
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
  AGrid.Cells[19, 0] := 'Sunshine Hrs' + LineEnding + 'Jan';
  AGrid.Cells[20, 0] := 'Sunshine Hrs' + LineEnding + 'Feb';
  AGrid.Cells[21, 0] := 'Sunshine Hrs' + LineEnding + 'Mar';
  AGrid.Cells[22, 0] := 'Sunshine Hrs' + LineEnding + 'Apr';
  AGrid.Cells[23, 0] := 'Sunshine Hrs' + LineEnding + 'May';
  AGrid.Cells[24, 0] := 'Sunshine Hrs' + LineEnding + 'Jun';
  AGrid.Cells[25, 0] := 'Sunshine Hrs' + LineEnding + 'Jul';
  AGrid.Cells[26, 0] := 'Sunshine Hrs' + LineEnding + 'Aug';
  AGrid.Cells[27, 0] := 'Sunshine Hrs' + LineEnding + 'Sep';
  AGrid.Cells[28, 0] := 'Sunshine Hrs' + LineEnding + 'Oct';
  AGrid.Cells[29, 0] := 'Sunshine Hrs' + LineEnding + 'Nov';
  AGrid.Cells[30, 0] := 'Sunshine Hrs' + LineEnding + 'Dec';
  AGrid.Cells[31, 0] := 'Precip/Month' + LineEnding + 'Jan';
  AGrid.Cells[32, 0] := 'Precip/Month' + LineEnding + 'Febn';
  AGrid.Cells[33, 0] := 'Precip/Month' + LineEnding + 'Mar';
  AGrid.Cells[34, 0] := 'Precip/Month' + LineEnding + 'Apr';
  AGrid.Cells[35, 0] := 'Precip/Month' + LineEnding + 'May';
  AGrid.Cells[36, 0] := 'Precip/Month' + LineEnding + 'Jun';
  AGrid.Cells[37, 0] := 'Precip/Month' + LineEnding + 'Jul';
  AGrid.Cells[38, 0] := 'Precip/Month' + LineEnding + 'Aug';
  AGrid.Cells[39, 0] := 'Precip/Month' + LineEnding + 'Sep';
  AGrid.Cells[40, 0] := 'Precip/Month' + LineEnding + 'Oct';
  AGrid.Cells[41, 0] := 'Precip/Month' + LineEnding + 'Nov';
  AGrid.Cells[42, 0] := 'Precip/Month' + LineEnding + 'Dec';
  AGrid.Cells[43, 0] := 'Max Precip/Day' + LineEnding + 'Jan';
  AGrid.Cells[44, 0] := 'Max Precip/Day' + LineEnding + 'Feb';
  AGrid.Cells[45, 0] := 'Max Precip/Day' + LineEnding + 'Mar';
  AGrid.Cells[46, 0] := 'Max Precip/Day' + LineEnding + 'Apr';
  AGrid.Cells[47, 0] := 'Max Precip/Day' + LineEnding + 'May';
  AGrid.Cells[48, 0] := 'Max Precip/Day' + LineEnding + 'Jun';
  AGrid.Cells[49, 0] := 'Max Precip/Day' + LineEnding + 'Jul';
  AGrid.Cells[50, 0] := 'Max Precip/Day' + LineEnding + 'Aug';
  AGrid.Cells[51, 0] := 'Max Precip/Day' + LineEnding + 'Sep';
  AGrid.Cells[52, 0] := 'Max Precip/Day' + LineEnding + 'Oct';
  AGrid.Cells[53, 0] := 'Max Precip/Day' + LineEnding + 'Nov';
  AGrid.Cells[54, 0] := 'Max Precip/Day' + LineEnding + 'Dec';

  AGrid.Canvas.Font.Assign(AGrid.Font);
  AGrid.DefaultColWidth := Max(AGrid.Canvas.TextWidth(' -999.9 '), AGrid.Canvas.TextWidth('  July  '));;
  AGrid.RowHeights[0] := (AGrid.Canvas.TextHeight('Tg') + varCellPadding) * 2;
  AGrid.ColWidths[13] := AGrid.Canvas.TextWidth('  Summer  ');
  AGrid.ColWidths[14] := AGrid.ColWidths[13];
  AGrid.ColWidths[15] := AGrid.ColWidths[13];
  AGrid.ColWidths[16] := AGrid.ColWidths[13];
  AGrid.ColWidths[17] := Max(AGrid.Canvas.TextWidth('Annual'), AGrid.Canvas.TextWidth('Dec-Nov')) + 2*varCellPadding;
  AGrid.ColWidths[18] := AGrid.ColWidths[17];
  AGrid.ColWidths[19] := AGrid.Canvas.TextWidth(' Sunshine Hrs ');
  for m := mFeb to mDec do
    AGrid.ColWidths[19 + ord(m)] := AGrid.ColWidths[19];
  AGrid.ColWidths[31] := AGrid.Canvas.TextWidth(' Precip/Month ');
  for m := mFeb to mDec do
    AGrid.ColWidths[31 + ord(m)] := AGrid.ColWidths[31];
  AGrid.ColWidths[43] := AGrid.Canvas.TextWidth(' Max Precip/Day ');
  for m := mFeb to mDec do
    AGrid.ColWidths[43 + ord(m)] := AGrid.ColWidths[43];

  for i := 0 to Data.Count-1 do
  begin
    dataItem := TDWD_DataItem(Data[i]);
    r := i + AGrid.FixedRows;;
    AGrid.Cells[0, r] := IntToStr(dataItem.Year);
    for j := 1 to 12 do
    begin
      value := dataItem.MonthlyMean[TMonth(j-1)];
      AGrid.Cells[j, r] := IfThen(IsErrorValue(value), '', FormatFloat('0.0', value));
      value := dataItem.TotalSunshineHours[TMonth(m)];
      AGrid.Cells[j + 18, r] := IfThen(IsErrorValue(value), '', FormatFloat('0.0', value));
      value := dataItem.TotalPrecipitation[m];
      AGrid.Cells[j + 30, r] := IfThen(IsErrorValue(value), '', FormatFloat('0.0', value));
      value := dataItem.MaxDailyPrecipitation[m];
      AGrid.Cells[j + 42, r] := IfThen(IsErrorvalue(value), '', FormatFloat('0.0', value));
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

// STATIONS_ID;MESS_DATUM_BEGINN;MESS_DATUM_ENDE;QN_4;MO_N;MO_TT;MO_TX;MO_TN;MO_FK;MX_TX;MX_FX;MX_TN;MO_SD_S;QN_6;MO_RR;MX_RS;eor
// 0           1                 2               3    4    5     6     7     8     9     10    11    12      13   14    15    16
// MO_TT = monthly average of air temperature 2 m above ground  (index 5)     --> axis 0
// MO_TX = monthly maximum air temperature 2 m above ground  (index 6)
// MO_TN = monthly minimum air temperature 2 m above ground  (index 7)
// MO_SD_S = total sun shine hous per month (index 12)                        --> axis 2
// MO_RR = total precipitation per month in mm (index 14)                     --> axis 3
// MX_RS = monthly maximum of daily precipitation in mm (index 15)
procedure TDWD_Station.ProcessData(AStream: TStream);
var
  fs: TFormatSettings;
  L: TStringList;
  i: Integer;
  s: String;
  sa: TStringArray;
  prevYr: Integer;
  savedYr: Integer;
  yr: Integer;
  m: TMonth;
  monthMean: array[TMonth] of Double;
  monthMax: array[TMonth] of Double;
  monthMin: array[TMonth] of Double;
  totalPrecip: array[TMonth] of Double;
  maxDailyPrecip: array[TMonth] of Double;
  sunshineHours: array[TMonth] of Double;
  item: TDWD_DataItem;
  prevDec: Double;
  tmp: Double;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  Data.Clear;

  L := TStringList.Create;
  try
    L.LoadFromStream(AStream);

    for m in TMonth do
    begin
      monthMean[m] := -999;
      monthMax[m] := -999;
      monthMin[m] := -999;
      sunshineHours[m] := -999;
      totalPrecip[m] := -999;
      maxDailyPrecip[m] := -999;
    end;
    prevDec := -999;
    prevYr := 0;
    savedYr := 0;

    for i := 1 to L.Count - 1 do  // i:=1 --> skip 1 header line
    begin
      s := L[i];
      if s = '' then
        Continue;

      sa := s.Split(DWD_DATA_SEPARATOR);
      yr := StrToInt(Copy(sa[1], 1, 4));
      if prevYr = 0 then
        prevYr := yr;

      if (yr <> prevYr) then begin
        // store previous year in list
        savedYr := prevYr;
        item := TDWD_DataItem.Create;
        item.Year := savedYr;
        for m in TMonth do
        begin
          item.MonthlyMean[m] := monthMean[m];
          item.TotalSunshineHours[m] := sunShineHours[m];
          item.TotalPrecipitation[m] := totalPrecip[m];
          item.MaxDailyPrecipitation[m] := maxDailyPrecip[m];
        end;

        item.AnnualMean := CalcMean(monthMean[mJan], 12);
        item.SeasonalMean[sSpring] := CalcMean(monthMean[mMar], 3);
        item.SeasonalMean[sSummer] := CalcMean(monthMean[mJun], 3);
        item.SeasonalMean[sFall] := CalcMean(monthMean[mSep], 3);
        tmp := monthMean[mDec];      // store december value for next year
        monthMean[mDec] := prevDec;  // Meorological annual mean: Use december value of last year
        item.MetAnnualMean := CalcMean(monthMean[mJan], 12);
        monthMean[mMar] := prevDec;  // Winter mean: prev dec, jan, feb
        item.SeasonalMean[sWinter] := CalcMean(monthMean[mJan], 3);
        Data.Add(item);

        // prepare new year
        prevYr := yr;
        for m in TMonth do
        begin
          monthMean[m] := -999;
          monthMax[m] := -999;
          monthMin[m] := -999;
          sunshineHours[m] := -999;
          totalPrecip[m] := -999;
          maxDailyPrecip[m] := -999;
        end;
        prevDec := tmp;
      end;

      m := TMonth(StrToInt(Copy(sa[1], 5, 2)) - 1);
      monthMean[m] := StrToFloat(trim(sa[5]), fs);
      monthMax[m] := StrToFloat(trim(sa[6]), fs);
      monthMin[m] := StrToFloat(trim(sa[7]), fs);
      sunshineHours[m] := StrToFloat(trim(sa[12]), fs);
      totalPrecip[m] := StrToFloat(trim(sa[14]), fs);
      maxDailyPrecip[m] := StrToFloat(trim(sa[15]), fs);
    end;

    if savedYr <> yr then begin
      item := TDWD_DataItem.Create;
      item.Year := yr;
      for m in TMonth do
      begin
        item.MonthlyMean[m] := monthMean[m];
        item.TotalSunshineHours[m] := sunShineHours[m];
        item.TotalPrecipitation[m] := totalPrecip[m];
        item.MaxDailyPrecipitation[m] := maxDailyPrecip[m];
      end;
      item.AnnualMean := CalcMean(monthMean[mJan], 12);
      monthMean[mDec] := prevDec;
      item.MetAnnualMean := CalcMean(monthMean[mJan], 12);
      item.SeasonalMean[sSpring] := CalcMean(monthMean[mMar], 3);
      item.SeasonalMean[sSummer] := CalcMean(monthMean[mJun], 3);
      item.SeasonalMean[sFall] := CalcMean(monthMean[mSep], 3);
      monthMean[mMar] := prevDec;
      item.SeasonalMean[sWinter] := CalcMean(monthMean[mJan], 3);
      Data.Add(item);
    end;
  finally
    L.Free;
  end;
end;

procedure TDWD_Station.ReadDir(ADirURL: String; AList: TStrings);
var
  stream: TMemoryStream;
  extractor: TTextExtractor;
begin
  stream := TMemoryStream.Create;
  try
    DownLoadFile(ADirURL, stream);
    extractor := TTextExtractor.Create;
    try
      extractor.Process(stream, AList);
    finally
      extractor.Free;
    end;
  finally
    stream.Free;
  end;
end;


{ TDWD_StationList }

function TDWD_StationList.CreateStationFromLine(const ALine: String): TStation;
       // 00001 19310101 19860630            478     47.8413    8.8493 Aach                                     Baden-Württemberg
       // 00183 19360101 20190930             42     54.6792   13.4343 Arkona                                   Mecklenburg-Vorpommern
const
  MASK = 'iiiii bbbbbbbb eeeeeeee vvvvvvvvvvvvvv aaaaaaaaaaa ooooooooo nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn ssssssssssssssssssssssssssssssss';
      //  id    startdat enddate  elevation      latitude    longitude name                                     state
var
  lID: String = '';
  lStartDate: String = '';
  lEndDate: String = '';
  lElevation: String = '';
  lLatitude: String = '';
  lLongitude: String = '';
  lName: String = '';
  lState: String = '';
  i: Integer;
  c: Char;
  n: Integer;
begin
  n := Min(Length(ALine), Length(MASK));
  for i := 1 to n do begin
    c := ALine[i];
    case MASK[i] of
      'i': lID := lID + c;
      'b': lStartDate := lStartDate + c;
      'e': lEndDate := lEndDate + c;
      'v': lElevation := lElevation + c;
      'a': lLatitude := lLatitude + c;
      'o': lLongitude := lLongitude + c;
      'n': lName := lName + c;
      's': lState := lState + c;
    end;
  end;
  Result := TDWD_Station.Create(CP1252ToUTF8(trim(lName)), trim(lID));
  TDWD_Station(Result).FState := CP1252ToUTF8(trim(lState));
  TDWD_Station(Result).FStartDate := ScanDateTime(DWD_DATE_MASK, trim(lStartDate));
  TDWD_Station(Result).FEndDate := ScanDateTime(DWD_DATE_MASK, trim(lEndDate));
  TDWD_Station(Result).FLatitude := PtStrToFloat(lLatitude);
  TDWD_Station(Result).FLongitude := PtStrToFloat(lLongitude);
  TDWD_Station(Result).FElevation := PtStrToFloat(lElevation);
end;


(*
{ this is valid for https://www.dwd.de/DE/leistungen/klimadatendeutschland/stationsuebersicht.html }
function TDWD_StationList.CreateStationFromLine(const ALine: String): TStation;
var
  sa: TStringArray;
begin
  sa := ALine.Split(DWD_STATION_SEPARATOR);
  Result := TStation.Create(sa[3], PadAtLeft(sa[1], 5));
end;
*)

function TDWD_StationList.GetStationListFileName: String;
begin
  Result := AppendPathDelim(DataDir) + DWD_DATA_DIR + DWD_STATIONS_FILE;
end;

function TDWD_StationList.GetStationListURL: String;
begin
  Result := DWD_STATIONLIST_URL;
end;

procedure TDWD_StationList.Process(AStream: TStream; Downloaded: Boolean);
var
  L: TStringList;
  i: Integer;
  station: TStation;
begin
  Clear;
  L := TStringList.Create;
  try
    L.LoadFromStream(AStream);
    for i := 2 to L.Count - 1 do begin  // i:=2 --> skip 2 header lines
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

(*
// this is for https://www.dwd.de/DE/leistungen/klimadatendeutschland/stationsuebersicht.html
procedure TDWD_StationList.Process(AStream: TStream; Downloaded: Boolean);
var
  L: TStringList;
  extractor: TTableExtractor;
  i: Integer;
  station: TStation;
begin
  Clear;
  L := TStringList.Create;
  try
    if Downloaded then
    begin
      extractor := TTableExtractor.Create;
      try
        extractor.Process(AStream, L);
      finally
        extractor.Free;
      end;
    end else
      L.LoadFromStream(AStream);
    AStream.Position := 0;

    for i:=1 to L.Count-1 do begin  // i:=1 --> skip header line
      if L[i] = '' then
        Continue;
      station := CreateStationFromLine(L[i]);
      if Assigned(station) then
        Add(station);
    end;

    if Downloaded then
    begin
      // Local stations file is created from AStream (by Download method)
      // We don't want the html file, but the csv file
      AStream.Size := 0;
      L.SaveToStream(AStream);
    end;
  finally
    L.Free;
  end;
end;
*)

end.


