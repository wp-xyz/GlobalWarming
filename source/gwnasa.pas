unit gwNASA;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, Contnrs, SysUtils, ComCtrls, Grids,
  TAGraph, TATypes, TASeries, TACustomSource, TASources, TAStyles, TALegend,
  gwData, gwStations;

type
  TPopulationType = (ptRural, ptSmallTown, ptUrban);  // R < 10000, S, U > 50000
  TTopographytype = (ttFlat, ttHilly, ttMountainTop, ttMountainValley);  // FL, HI, MT, MV

  TGISS_Station = class;

  TGISS_DataItem = class(TDataItem)
   // MetAnnualMean: Double;  // "Met" = meteorological, i.e. Dec to Nov, mean
  end;


  { TGISSStation }

  TGISS_Station = class(TStation)
  private
//    FSeriesItems: Integer;
    procedure DownloadData; override;
    procedure GetChartDataItemHandler(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  protected
    FSuffixes: array of Byte;
    function FirstDataLine: Integer; virtual;
    function GetDataMask: String; virtual;
    procedure ProcessData(AStream: TStream); override;
  public
    function CalcDataURL(AKind, AIndex: Byte): String; virtual;
  end;

  TGISSv2_Station = class(TGISS_Station)
  private
    FPopulationType: TPopulationType;
    FTopographyType: TTopographyType;
  protected
    function FirstDataLine: Integer; override;
    function GetDataMask: String; override;
    function GetLegendTitle: String; override;
  public
    constructor Create(const AName, AID: String); override;
    procedure Assign(AStation: TStation); override;
    function CalcDataURL(AKind, AIndex: Byte): String; override;
    function GetCountry: String; override;
    function GetDataFileName: String; override;
    function NiceStationName: String; override;
    property PopulationType: TPopulationtype read FPopulationType write FPopulationType;
    property TopographyType: TTopographyType read FTopographyType write FTopographyType;
  end;

  TGISSv4_Station = class(TGISS_Station)
  protected
    function FirstDataLine: Integer; override;
    function GetDataMask: String; override;
    function GetLegendTitle: String; override;
  public
    constructor Create(const AName, AID: String); override;
    procedure Assign(AStation: TStation); override;
    function CalcDataURL(AKind, AIndex: Byte): String; override;
    function GetCountry: String; override;
    function GetDataFileName: String; override;
  end;

  { ------- }

  TGISS_StationList = class(TStationList)
  private

  protected
    function FirstDataLine(AList: TStrings): Integer; virtual;
    procedure Process(AStream: TStream; Downloaded: Boolean); override;
  public

  end;

  TGISSv2_StationList = class(TGISS_StationList)
  protected
    function CreateStationFromLine(const ALine: String): TGISS_Station; override;
    function FirstDataLine(AList: TStrings): Integer; override;
    function GetStationListFileName: String; override;
    function GetStationListURL: String; override;
  end;

  TGISSv4_StationList = class(TGISS_StationList)
  protected
    function CreateStationFromLine(const ALine: String): TGISS_Station; override;
    function FirstDataLine(AList: TStrings): Integer; override;
    function GetStationListFileName: String; override;
    function GetStationListURL: String; override;
  end;

implementation

uses
  Math, Controls, Forms, Dialogs, StrUtils, LazFileUtils,
  gwGlobal, gwUtils;

const
  GISS_STATIONS_FILE = DEFAULT_STATIONS_FILE;

  GISSv2_DATA_DIR = 'giss_v2/';
  GISSv2_STATIONLIST_URL = 'https://data.giss.nasa.gov/gistemp/station_data_v2/v2.temperature.inv.txt';
  GISSv2_DATA_URL_1 = 'https://data.giss.nasa.gov/cgi-bin/gistemp/show_station.cgi?id=%s%d&dt=1&ds=1';
  GISSv2_DATA_URL_2 = 'https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_%s%d_1_0/station.txt';

  GISSv4_DATA_DIR = BASE_DATA_DIR + 'giss_v4/';
//  GISSv4_STATIONLIST_URL = 'https://data.giss.nasa.gov/gistemp/station_data_v4_globe/v4.temperature.inv.txt';
  GISSv4_STATIONLIST_URL = 'https://data.giss.nasa.gov/gistemp/station_data_v4_globe/station_list.txt';
  GISSv4_DATA_URL_1 = 'https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=%s&dt=1&ds=%d';
  GISSv4_DATA_URL_2 = 'https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_%s_%d_0_1/station.txt';


{ TGISS_Station }

function TGISS_Station.CalcDataURL(AKind, AIndex: Byte): String;
begin
  // to be overridden
  Result := '';
end;

//https://data.giss.nasa.gov/cgi-bin/gistemp/show_station.cgi?id=614029120003&dt=1&ds=1
//https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_614029120003_1_0/station.txt
procedure TGISS_Station.DownloadData;
var
  stream: TMemoryStream;
  url: String;
  suffix: Byte;
begin
  stream := TMemoryStream.Create;
  try
    for suffix in FSuffixes do
    begin
      url := CalcDataURL(1, suffix);
      if DownloadFile(url, stream) then   // This site probably prepares the data
      begin
        stream.Clear;
        url := CalcDataURL(2, suffix);   // Download the data
        if DownloadFile(url, stream) then begin
          ProcessData(stream);
          stream.SaveToFile(DataFileName);
          exit;
        end;
      end;
    end;
    MessageDlg('Cannot download from' + LineEnding + url, mtError, [mbOK], 0);
  finally
    stream.Free;
  end;
end;

function TGISS_Station.FirstDataLine: Integer;
begin
  Result := 0;
end;

procedure TGISS_Station.GetChartDataItemHandler(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  idx: Integer;
  y: Double;
  dataItem: TGISS_DataItem;
begin
  dataItem := Data[AIndex] as TGISS_DataItem;
  AItem.X := dataItem.Year;

  idx := 0;
  if FSeriesItems and 1 <> 0 then
  begin
    y := dataItem.MetAnnualMean;
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and 2 <> 0 then
  begin
    y := dataItem.AnnualMean;
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and 4 <> 0 then
  begin
    y := dataItem.SeasonalMean[sWinter];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and 8 <> 0 then
  begin
    y := dataItem.SeasonalMean[sSpring];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and $10 <> 0 then
  begin
    y := dataItem.SeasonalMean[sSummer];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;

  if FSeriesItems and $20 <> 0 then
  begin
    y := dataItem.SeasonalMean[sFall];
    if IsErrorValue(y) then AItem.SetY(idx, NaN) else AItem.SetY(idx, y);
    inc(idx);
  end;
end;

function TGISS_Station.GetDataMask: String;
begin
  // to be overridden by ancestors
end;

procedure TGISS_Station.ProcessData(AStream: TStream);
var
  L: TStringList;
  s: String;
  i, j: Integer;
  item: TGISS_DataItem;
  yearStr: String;
  monthStr: array[TMonth] of string;
  seasonStr: array[TSeason] of String;
  annualStr: String;
  fs: TFormatSettings;
  sum: Double;
  complete: Boolean;
  m: TMonth;
  sn: TSeason;
  mask: String;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  Data.Clear;

  L := TStringList.Create;
  try
    L.LoadFromStream(AStream);

    for i := FirstDataLine to L.Count - 1 do
    begin
      s := L[i];
      if s = '' then
        Continue;

      yearStr := '';
      annualStr := '';
      for m in TMonth do monthStr[m] := '';
      for sn in TSeason do seasonStr[sn] := '';
      mask := GetDataMask;

      for j := 1 to Length(mask) do
        case mask[j] of
          'y': yearStr := yearStr + s[j];
          '1': if s[j] <> ' ' then monthStr[mJan] := monthStr[mJan] + s[j];
          '2': if s[j] <> ' ' then monthStr[mFeb] := monthStr[mFeb] + s[j];
          '3': if s[j] <> ' ' then monthStr[mMar] := monthStr[mMar] + s[j];
          '4': if s[j] <> ' ' then monthStr[mApr] := monthStr[mApr] + s[j];
          '5': if s[j] <> ' ' then monthStr[mMay] := monthStr[mMay] + s[j];
          '6': if s[j] <> ' ' then monthStr[mJun] := monthStr[mJun] + s[j];
          '7': if s[j] <> ' ' then monthStr[mJul] := monthStr[mJul] + s[j];
          '8': if s[j] <> ' ' then monthStr[mAug] := monthStr[mAug] + s[j];
          '9': if s[j] <> ' ' then monthStr[mSep] := monthStr[mSep] + s[j];
          'A': if s[j] <> ' ' then monthStr[mOct] := monthStr[mOct] + s[j];
          'B': if s[j] <> ' ' then monthStr[mNov] := monthStr[mNov] + s[j];
          'C': if s[j] <> ' ' then monthStr[mDec] := monthStr[mDec] + s[j];
          'W': if s[j] <> ' ' then seasonStr[sWinter] := seasonStr[sWinter] + s[j];
          'S': if s[j] <> ' ' then seasonStr[sSpring] := seasonStr[sSpring] + s[j];
          'R': if s[j] <> ' ' then seasonStr[sSummer] := seasonStr[sSummer] + s[j];
          'F': if s[j] <> ' ' then seasonStr[sFall] := seasonStr[sFall] + s[j];
          'Y': if s[j] <> ' ' then annualStr := annualStr + s[j];
        end;

      item := TGISS_DataItem.Create;
      item.Year := StrToInt(yearStr);

      sum := 0;
      complete := true;
      for m in TMonth do
      begin
        item.MonthlyMean[m] := StrToFloat(monthStr[m], fs);
        if not IsErrorValue(item.MonthlyMean[m]) then
          sum := sum + item.MonthlyMean[m]
        else
          complete := false;
      end;
      for sn in TSeason do
        item.SeasonalMean[sn] := StrToFloat(seasonStr[sn], fs);
      item.MetAnnualMean := StrToFloat(annualStr, fs);
      if complete then
        item.AnnualMean := sum / 12
      else
        item.AnnualMean := 999.9;

      Data.Add(item);
    end;
  finally
    L.Free;
  end;
end;


{ TGISSv2_Station }

constructor TGISSv2_Station.Create(const AName, AID: String);
const
  SUFFIXES: array[0..4] of byte = (0,1,2,3,4);
  //SUFFIXES: array[0..14] of byte = (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14);
begin
  inherited Create(AName, AID);
  FDataClass := TGISS_DataItem;
  SetLength(FSuffixes, Length(SUFFIXES));
  Move(SUFFIXES[0], FSuffixes[0], Length(SUFFIXES));
end;

procedure TGISSv2_Station.Assign(AStation: TStation);
begin
  inherited;
  if AStation is TGISSv2_Station then
  begin
    PopulationType := TGISSv2_Station(AStation).PopulationType;
    TopographyType := TGISSv2_Station(AStation).TopographyType;
  end else
    raise Exception.Create('Incorrection station data type.');
end;

function TGISSv2_Station.CalcDataURL(AKind, AIndex: Byte): String;
begin
  case AKind of
    1: Result := Format(GISSv2_DATA_URL_1, [ID, AIndex]);
    2: Result := Format(GISSv2_DATA_URL_2, [ID, AIndex]);
  end;
end;

{ In GISS_v2 data files, the data begin at index 1 (zero-based) }
function TGISSv2_Station.FirstDataLine: Integer;
begin
  Result := 1;
end;

function TGISSv2_Station.GetCountry: String;
var
  country_code: Integer;
begin
  country_code := StrToInt(copy(ID, 1, 3));
  case country_code of
    101: Result := 'Algeria';
    115: Result := 'Egypt';
    124: Result := 'Libya';
    130: Result := 'Morocco';
    137: Result := 'Senegal';
    141: Result := 'South Africa';
    152: Result := 'Tunisia';

    205: Result := 'China';
    207: Result := 'India';
    208: Result := 'Iran';
    210: Result := 'Japan';
    218: Result := 'Oman';
    219: Result := 'Pakistan';
    221: Result := 'South Korea';
    233: Result := 'Yemen';

    301: Result := 'Argentina';
    304: Result := 'Chile';
    305: Result := 'Colombia';
    306: Result := 'Ecuador';
    308: Result := 'Paraguay';
    309: Result := 'Peru';
    314: Result := 'Venezuela';

    401: Result := 'Barbados';
    403: Result := 'Canada';
    407: Result := 'Dominican Republic';
    409: Result := 'Grenada';
    410: Result := 'Guatemala';
    413: Result := 'Jamaica';
    414: Result := 'Mexico';
    415: Result := 'Nicaragua';
    416: Result := 'Panama';
    424: Result := 'Trinidad';
    425: Result := 'United States';
    427: Result := 'Bermuda';
    431: Result := 'Greenland';
    433: Result := 'Martinique';
    435: Result := 'Puerto Rico';
    440: Result := 'Virgin Islands';

    501: Result := 'Australia';
    503: Result := 'Indonesia';
    507: Result := 'New Zealand';
    509: Result := 'Philippines';
    511: Result := 'Singapore';

    603: Result := 'Austria';
    608: Result := 'Bulgaria';
    610: Result := 'Cyprus';
    611: Result := 'Czech Republic';
    612: Result := 'Denmark';
    614: Result := 'Finnland';
    615: Result := 'France';
    617: Result := 'Germany';
    618: Result := 'Greece';
    619: Result := 'Hungary';
    620: Result := 'Iceland';
    622: Result := 'Israel';
    623: Result := 'Italy';
    634: Result := 'Norway';
    635: Result := 'Poland';
    636: Result := 'Portugal';
    637: Result := 'Romania';
    641: Result := 'Slovakia';
    643: Result := 'Spain';
    645: Result := 'Sweden';
    646: Result := 'Switzerland';
    649: Result := 'Turkey';
    651: Result := 'United Kingdom';

    700: Result := 'Antarktis';

    else Result := '(other)';
  end;
end;

function TGISSv2_Station.GetDataFileName: String;
var
  lName: String;
begin
  lName := StringReplace(Name, '/', '-', [rfReplaceAll]);
  Result := AppendPathDelim(DataDir) + GISSv2_DATA_DIR + ID + '_' + lName + '.txt';
end;

function TGISSv2_Station.GetDataMask: String;
begin
  Result := 'yyyy 111111 222222 333333 444444 555555 666666 777777 888888 999999 AAAAAA BBBBBB CCCCCC WWWWWWW SSSSSSS RRRRRRR FFFFFFF  YYYYYY';
           //YEAR    JAN    FEB    MAR    APR    MAY    JUN    JUL    AUG    SEP    OCT    NOV    DEC   D-J-F   M-A-M   J-J-A   S-O-N  metANN
end;

function TGISSv2_Station.GetLegendTitle: String;
begin
  Result := NiceStationName + ' (GISS-v2)';
end;

// Remove consecutive spaces.
function TGISSv2_Station.NiceStationName: String;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(Name) do
  begin
    if Name[i] <> ' ' then
      Result := Result + Name[i]
    else if (i > 1) and (Name[i-1] <> ' ') then
      Result := Result + ' ';
    inc(i);
  end;
end;


{ TGISSv4_Station }

constructor TGISSv4_Station.Create(const AName, AID: String);
const
//  SUFFIXES: array[0..14] of byte = (14, 0,1,2,3,4,5,6,7,8,9,10,11,12,13);
  SUFFIXES: array[0..0] of byte = (14);
begin
  inherited Create(AName, AID);
  SetLength(FSuffixes, Length(SUFFIXES));
  Move(SUFFIXES[0], FSuffixes[0], Length(SUFFIXES));
end;

procedure TGISSv4_Station.Assign(AStation: TStation);
begin
  inherited;
  if AStation is TGISSv4_Station then
  begin
    //
  end else
    raise Exception.Create('Incorrect station data type.');
end;

function TGISSv4_Station.CalcDataURL(AKind, AIndex: Byte): String;
begin
  case AKind of
    1: Result := Format(GISSv4_DATA_URL_1, [ID, AIndex]);
    2: Result := Format(GISSv4_DATA_URL_2, [ID, AIndex]);
  end;
end;

{ In GISS_v4 data files, the data begin at index 3 (zero-based) }
function TGISSv4_Station.FirstDataLine: Integer;
begin
  Result := 3;
end;

function TGISSv4_Station.GetCountry: String;
var
  country_code: String;
begin
  country_code := copy(ID, 1, 2);
  case country_code of
    'AS': Result := 'Australia';
    'AU': Result := 'Austria';
    'BE': Result := 'Belgium';
    'CA': Result := 'Canada';
    'DA': Result := 'Denmark';
    'EZ': Result := 'Czech Republic';
    'FI': Result := 'Finnland';
    'FR': Result := 'France';
    'GL': Result := 'Greenland';
    'GM': Result := 'Germany';
    'GR': Result := 'Greece';
    'IC': Result := 'Iceland';
    'IS': Result := 'Israel';
    'IT': Result := 'Italy';
    'JA': Result := 'Japan';
    'KS': Result := 'South Korea';
    'MO': Result := 'Morocco';
    'MX': Result := 'Mexico';
    'NO': Result := 'Norway';
    'NZ': Result := 'New Zealand';
    'PE': Result := 'Peru';
    'PO': Result := 'Portugal';
    'SP': Result := 'Spain';
    'SW': Result := 'Sweden';
    'SZ': Result := 'Switzerland';
    'TU': Result := 'Turkey';
    'UK': Result := 'United Kingdom';
    'US': Result := 'United States';
    else  Result := '(other)';
  end;
end;

function TGISSv4_Station.GetDataFileName: String;
begin
  Result := GISSv4_DATA_DIR + ID + '_' + Name + '.txt';
end;

function TGISSv4_Station.GetDataMask: String;
begin
  Result := 'yyyy 1111111 2222222 3333333 4444444 5555555 6666666 7777777 8888888 9999999 AAAAAAA BBBBBBB CCCCCCC WWWWWWW SSSSSSS RRRRRRR FFFFFFF YYYYYYY';
          //YEAR     JAN     FEB     MAR     APR     MAY     JUN     JUL     AUG     SEP     OCT     NOV     DEC   D-J-F   M-A-M   J-J-A   S-O-N  metANN
          //1881   -2.16    3.34    5.54    7.54   12.94   15.74   19.64   15.74   13.24    6.04    8.54    2.57    2.31    8.67   17.04    9.27    9.32
end;

function TGISSv4_Station.GetLegendTitle: String;
begin
  Result := NiceStationName + ' (GISS-v4)';
end;


{ TGISS_Stations }

function TGISS_StationList.FirstDataLine(AList: TStrings): Integer;
begin
  Result := 0;
end;

procedure TGISS_StationList.Process(AStream: TStream; Downloaded: Boolean);
var
  L: TStringList;
  station: TStation;
  i, i0: Integer;
begin
  Clear;
  L := TStringList.Create;
  try
    L.LoadFromStream(AStream);
    AStream.Position := 0;

    // Find begin of data
    i0 := FirstDataLine(L);

    // Analyze lines and create a station item from each line
    for i := i0 to L.Count-1 do begin
      station := CreateStationFromLine(L[i]);
      if Assigned(station) then
        Add(station);
    end;
  finally
    L.Free;
  end;
end;


{ TGISSv2_StationList }

function TGISSv2_StationList.GetStationListFileName: String;
begin
  Result := AppendPathDelim(DataDir) + GISSv2_DATA_DIR + GISS_STATIONS_FILE;
end;

function TGISSv2_StationList.GetStationListURL: String;
begin
  Result := GISSv2_STATIONLIST_URL;
end;

function TGISSv2_StationList.CreateStationFromLine(const ALine: String): TGISS_Station;
const
  mask = 'iiiiiiiiiii nnnnnnnnnnnnnnnnnnnnnnnnnnnnnn aaaaaa ooooooo eeee     p     tt';
        //iccWMO_#... Name                              Lat     Lon Elev TEleP<Pop>Tp VLoCoAds<-----Vege----->bi
var
  lName, lID, lLat, lLon, lElev, lPop, lTopo: String;
  lat, long, elev: Double;
  i: Integer;
  station: TGISSv2_Station;
begin
  lID := '';
  lName := '';
  lLat := '';
  lLon := '';
  lElev := '';
  lPop := '';
  lTopo := '';
  for i := 1 to Length(MASK) do
    case MASK[i] of
      'i': lID := lID + ALine[i];
      'n': lName := lName + ALine[i];
      'a': if ALine[i] <> ' '  then lLat := lLat + ALine[i];
      'o': if ALine[i] <> ' ' then lLon := lLon + ALine[i];
      'e': if ALine[i] <> ' ' then lElev := lElev + ALine[i];
      'p': lPop := lPop + ALine[i];
      't': lTopo := lTopo + ALine[i];
    end;

  station := TGISSv2_Station.Create(trim(lName), lID);
  station.FLatitude := StrToDbl(lLat);
  station.FLongitude := StrToDbl(lLon);
  station.FElevation := StrToDbl(lElev);
  case lPop of
    'R': station.PopulationType := ptRural;
    'S': station.PopulationType := ptSmallTown;
    'U': station.PopulationType := ptUrban;
  end;
  case lTopo of
    'FL': station.TopographyType := ttFlat;
    'HI': station.TopographyType := ttHilly;
    'MT': station.TopographyType := ttMountainTop;
    'MV': station.TopographyType := ttMountainValley;
  end;

  Result := station;
end;

// Find begin of data
function TGISSv2_StationList.FirstDataLine(AList: TStrings): Integer;
var
  inHeader: Boolean;
  s: String;
begin
  if AList.Count = 0 then
    exit(-1);

  Result := 0;
  inHeader := true;
  while inHeader do begin
    s := AList[Result];
    if pos('iccWMO_#... Name', s) = 1 then
    begin
      inHeader := false;
      inc(Result);
      Exit;
    end;
    inc(Result);
  end;
end;


{ TGISSv4_StationList }

function TGISSv4_StationList.GetStationListFileName: String;
begin
  Result := GISSv4_DATA_DIR + GISS_STATIONS_FILE;
end;

function TGISSv4_StationList.GetStationListURL: String;
begin
  Result := GISSv4_STATIONLIST_URL;
end;

function TGISSv4_StationList.CreateStationFromLine(const ALine: String): TGISS_Station;
const
  MASK = 'iiiiiiiiiii nnnnnnnnnnnnnnnnnnnnnnnnnnnnnn aaaaaaaa oooooooooo';
       //ID          Station Name                         Lat        Lon   BI
       //CIXLT572018 Isla Huafo                      -43.5300   -74.7500    0
//  MASK = 'iiiiiiiiiii aaaaaaaa ooooooooo eeeeee nnnnnnnnnnnnnnnnnnnnnnnnnnnnnn';
        //ID           Lat       Lon     Elev-m Station Name                   BI
        //ID           Lat       Lon     Elev-m Station Name                   BI
        //AQC00914869 -14.3333 -170.7167    3.0 TAFUNA_AP_TUTUILA              0
        //AEM00041184  25.6170   55.9330   31.0 RAS_AL_KHAIMAH_INTE            17
var
  lName, lID, lLat, lLon, lElev: String;
  lat, long, elev: Double;
  i: Integer;
  station: TGISSv4_Station;
begin
  lID := '';
  lName := '';
  lLat := '';
  lLon := '';
  lElev := '';
  for i := 1 to Length(MASK) do
    case MASK[i] of
      'i': lID := lID + ALine[i];
      'n': lName := lName + ALine[i];
      'a': if ALine[i] <> ' '  then lLat := lLat + ALine[i];
      'o': if ALine[i] <> ' ' then lLon := lLon + ALine[i];
      'e': if ALine[i] <> ' ' then lElev := lElev + ALine[i];
    end;

  station := TGISSv4_Station.Create(trim(lName), lID);
  station.FLatitude := StrToDbl(lLat);
  station.FLongitude := StrToDbl(lLon);
  station.FElevation := StrToDbl(lElev);

  Result := station;
end;

function TGISSv4_StationList.FirstDataLine(AList: TStrings): Integer;
begin
  Result := 1;
end;

end.

