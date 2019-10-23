unit gwDeutscherWetterDienst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids,
  gwData, gwStations;

type
  TDWD_DataItem = class(TDataItem);

  TDWD_Station = class(TStation)
  private
    FStartDate: TDate;
    FEndDate: TDate;
    FState: String;
    //function CalcDataFileName: String;
    //function CalcDataURL: String;
    function CalcMean(var AValues; ACount: Integer): Double;
  protected
    procedure DownloadData; override;
    function GetDataFileName: String; override;
    function GetDataURL: String; override;
    function GetLegendTitle: String; override;
    procedure ProcessData(AStream: TStream); override;
    procedure ReadDir(ADirURL: String; AList: TStrings);
  public
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
  StrUtils, FastHtmlParser,
  Dialogs,
  gwGlobal, gwUtils;

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


{ TTableExtractor }

type
  TTableExtractor = class
  private
    FParser: THtmlParser;
    FInTable: Boolean;
    FInRow: Boolean;
    FInCell: Boolean;
    FLines: TStrings;
    FLine: TStrings;
    FCellText: String;
    procedure FoundTagHandler(NoCaseTag, ActualTag: String);
    procedure FoundTextHandler(AText: String);
  public
    procedure Process(AStream: TStream; ALines: TStrings);
    property Lines: TStrings read FLines;
  end;

procedure TTableExtractor.FoundTagHandler(NoCaseTag, ActualTag: string);
begin
  if Length(NoCaseTag) < 4 then
    exit;
  case NoCaseTag[2] of
    'T': if Pos('<TABLE', NoCaseTag) = 1 then
           // <table> - begin of table --> set flag to activate processing
           FInTable := true
         else
         if (NoCaseTag[3] in ['R']) then
         begin
           // <tr> - begin of row --> clear FLine list
           inc(FInRow);
           FLine.Clear;
         end else
         if (NoCaseTag[3] in ['D','H']) and (NoCaseTag[4] in [' ', '>']) then
         begin
           // <tc>, <th> (not <thead>) --> begin of cell --> reset storage for collected cell text
           inc(FInCell);
           FCellText := '';
         end;
    '/': if pos('</TABLE', NoCaseTag) = 1 then
           // </table> - end of table --> ignore anything outside the table
           FInTable := false
         else if (NoCaseTag[3] = 'T') then
         begin
           if (NoCaseTag[4] in ['R']) then begin
             // </tr> - end of row tag --> copy add FLine to lines list
             dec(FInRow);
             FLines.Add(FLine.DelimitedText);
           end else
           if (NoCaseTag[4] in ['D', 'H']) then
             // </td>, </th> - end of cell node --> write collected texts to FLine
             FLine.Add(FCellText);
         end;
  end;
end;

procedure TTableExtractor.FoundTextHandler(AText: String);
begin
  if FInTable and FInRow and FInCell then
    // append all texts when inside a cell
    FCellText := FCellText + AText;
end;

procedure TTableExtractor.Process(AStream: TStream; ALines: TStrings);
var
  parser: THtmlParser;
  srcStream: TMemoryStream;
begin
  if AStream is TMemoryStream then
    srcStream := TMemoryStream(AStream)
  else
  begin
    srcStream := TMemoryStream.Create;
    srcStream.CopyFrom(AStream, AStream.Size);
  end;

  try
    FLines := ALines;
    FLines.Clear;

    FLine := TStringList.Create;
    try
      FLine.Delimiter := DWD_STATION_SEPARATOR;
      FLine.StrictDelimiter := true;

      srcStream.Position := 0;
      parser := THTMLParser.Create(srcStream.Memory);
      try
        parser.OnFoundTag := @FoundTagHandler;
        parser.OnFoundText := @FoundTextHandler;
        parser.Exec;
      finally
        parser.Free;
      end;
    finally
      FLine.Free;
    end;
  finally
    if not (AStream is TMemoryStream) then
      srcStream.Free;
  end;
end;


{ TTextExtractor }
type
  TTextExtractor = class
  private
    FParser: THtmlParser;
    FLines: TStrings;
    procedure FoundTextHandler(AText: String);
  public
    procedure Process(AStream: TStream; ALines: TStrings);
    property Lines: TStrings read FLines;
  end;

procedure TTextExtractor.FoundTextHandler(AText: String);
begin
  FLines.Add(AText);
end;

procedure TTextExtractor.Process(AStream: TStream; ALines: TStrings);
var
  parser: THtmlParser;
  srcStream: TMemoryStream;
begin
  if AStream is TMemoryStream then
    srcStream := TMemoryStream(AStream)
  else
  begin
    srcStream := TMemoryStream.Create;
    srcStream.CopyFrom(AStream, AStream.Size);
  end;

  try
    FLines := ALines;
    FLines.Clear;

    srcStream.Position := 0;
    parser := THTMLParser.Create(srcStream.Memory);
    try
      parser.OnFoundText := @FoundTextHandler;
      parser.Exec;
    finally
      parser.Free;
    end;
  finally
    if not (AStream is TMemoryStream) then
      srcStream.Free;
  end;
end;


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
begin
  inherited;
end;

// STATIONS_ID;MESS_DATUM_BEGINN;MESS_DATUM_ENDE;QN_4;MO_N;MO_TT;MO_TX;MO_TN;MO_FK;MX_TX;MX_FX;MX_TN;MO_SD_S;QN_6;MO_RR;MX_RS;eor
// 0           1                 2               3    4    5     6     7     8     9     10    11    12      13   14    15    16
// MO_TT = monthly average of air temperature 2 m above ground  (index 5)
// MO_TX = monthly maximum air temperature 2 m above ground  (index 6)
// MO_TN = monthly minimum air temperature 2 m above ground  (index 7)
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
          item.MonthlyMean[m] := monthMean[m];

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
        end;
        prevDec := tmp;
      end;

      m := TMonth(StrToInt(Copy(sa[1], 5, 2)) - 1);
      monthMean[m] := StrToFloat(trim(sa[5]), fs);
      monthMax[m] := StrToFloat(trim(sa[6]), fs);
      monthMin[m] := StrToFloat(trim(sa[7]), fs);
    end;

    if savedYr <> yr then begin
      item := TDWD_DataItem.Create;
      item.Year := yr;
      for m in TMonth do
        item.MonthlyMean[m] := monthMean[m];
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
  TDWD_Station(Result).FLatitude := StrToDbl(lLatitude);
  TDWD_Station(Result).FLongitude := StrToDbl(lLongitude);
  TDWD_Station(Result).FElevation := StrToDbl(lElevation);
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


