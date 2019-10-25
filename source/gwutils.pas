unit gwUtils;

{$mode objfpc}{$H+}

// Activate one of them:
{.$DEFINE SYNAPSE}          // requires packages laz_synapse and laz_synapse_ssl
{$DEFINE FPHTTPCLIENT}

interface

uses
  Classes, SysUtils, Graphics, IniFiles, Zipper,
  TAGraph;

type
  TStreamUnzipper = class(TUnzipper)
  private
    FInputStream: TStream;
    FOutputStream: TStream;
    FSuccess: Boolean;
    procedure CloseInputStream(Sender: TObject; var AStream: TStream);
    procedure CreateStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure DoneStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure OpenInputStream(Sender: TObject; var AStream: TStream);
  public
    constructor Create(AInputStream: TStream);
    function UnzipFile(const AZippedFile: string; ADestStream: TStream): Boolean;
  end;

function CreateIni: TCustomIniFile;
function DownloadFile(const URL: String; AStream: TStream): Boolean;

function GetSeriesColor(AChart: TChart): TColor;
function SeriesCountPerAxis(AChart: TChart; AAxisIndexY: Integer): Integer;

function IsErrorValue(AValue: Double): Boolean;
function PadAtLeft(const AString: String; ALength: Integer): String;
function StrToDbl(AText: String): Double;


implementation

uses
  {$IFDEF FPHTTPCLIENT}
  fphttpclient, openssl,
  {$ENDIF}
  {$IFDEF SYNAPSE}
  ssl_openssl, httpsend,
  {$ENDIF}
  Math,
  TACustomSeries, TAEnumerators;


{ TStreamUnzipper }

constructor TStreamUnzipper.Create(AInputStream: TStream);
begin
  inherited Create;
  OnCloseInputStream := @CloseInputStream;
  OnCreateStream := @CreateStream;
  OnDoneStream := @DoneStream;
  OnOpenInputStream := @OpenInputStream;
  FInputStream := AInputStream
end;

procedure TStreamUnzipper.CloseInputStream(Sender: TObject; var AStream: TStream);
begin
  AStream := nil;
end;

procedure TStreamUnzipper.CreateStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  FSuccess := True;
  AStream := FOutputStream;
end;

procedure TStreamUnzipper.DoneStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream := nil;
end;

procedure TStreamUnzipper.OpenInputStream(Sender: TObject; var AStream: TStream);
begin
  AStream := FInputStream;
end;

function TStreamUnzipper.UnzipFile(const AZippedFile: string;
  ADestStream: TStream): Boolean;
begin
  FOutputStream := ADestStream;
  FSuccess := False;
  Files.Clear;
  Files.Add(AZippedFile);
  UnZipAllFiles;
  Result := FSuccess;
end;


{ Misc routines }

function CreateIni: TCustomIniFile;
begin
  Result := TMemIniFile.Create(GetAppConfigFile(false));
end;

//https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_617104880000_1_0/station.txt
//https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_617107630003_1_0/station.txt        Nürnberg   -- why 3?
//https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_617100200000_1_0/station.txt
//https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_617100350000_1_0/station.txt
//https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_617100460000_1_0/station.txt
{$IFDEF FPHTTPCLIENT}
function DownloadFile(const Url: string; AStream: TStream): Boolean;
var
  http: TFpHttpClient;
begin
  Result := true;
  InitSSLInterface;
  http := TFpHttpClient.Create(nil);
  try
   {$IF FPC_FullVersion >= 30000}
    http.AllowRedirect := true;
   {$IFEND}
    http.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
    try
      http.Get(Url, AStream);
      Result := (http.ResponseStatusCode = 200);
    except
      on EInOutError do raise;
      on EHTTPClient do Result := false;
    end;
    AStream.Position := 0;
  finally
    http.Free;
  end;
end;
{$ENDIF}

{$IFDEF SYNAPSE}
function DownloadFile(const Url: String; AStream: TStream): Boolean;
const
  MaxRetries = 3;
var
  HTTPGetResult: Boolean;
  HTTPSender: THTTPSend;
  retryAttempt: Integer;
begin
  Result := False;
  retryAttempt := 1;
  HTTPSender := THTTPSend.Create;
  try
    try
      // Try to get the file
      HTTPGetResult := HTTPSender.HTTPMethod('GET', URL);
      while (not HTTPGetResult) and (RetryAttempt < MaxRetries) do
      begin
        Sleep(500 * RetryAttempt);
        HTTPSender.Clear;
        HTTPGetResult := HTTPSender.HTTPMethod('GET', URL);
        RetryAttempt := RetryAttempt + 1;
      end;

      // If we have an answer from the server, check if the file
      // was sent to us.
      case HTTPSender.Resultcode of
        100..299:
          begin
            HTTPSender.Document.SaveToStream(AStream);
            AStream.Position := 0;
            Result := True;
          end; //informational, success
        300..399:
          Result := False; // redirection. Not implemented, but could be.
        400..499:
          Result := False; // client error; 404 not found etc
        500..599:
          Result := False; // internal server error
        else
          Result := False; // unknown code
      end;
    except
      // We don't care for the reason for this error; the download failed.
      Result := False;
    end;
  finally
    HTTPSender.Free;
  end;
end;
{$ENDIF}

function GetSeriesColor(AChart: TChart): TColor;
const
  NUM_COLORS = 10;
  COLORS: array[0..NUM_COLORS-1] of TColor = (
    clRed, clBlue, clGreen, clBlack, clFuchsia, clLime, clTeal, clMaroon,
    clYellow, clOlive);
begin
  Result := COLORS[AChart.SeriesCount mod NUM_COLORS];
end;

function SeriesCountPerAxis(AChart: TChart; AAxisIndexY: Integer): Integer;
var
  i: Integer;
  ser: TCustomChartSeries;
begin
  Result := 0;
  for ser in CustomSeries(AChart) do
    if ser.AxisIndexY = AAxisIndexY then inc(Result);
end;

function IsErrorValue(AValue: Double): Boolean;
begin
  Result := IsNaN(AValue) or (AValue <= -999.0) or (AValue >= 999.0);
end;

function PadAtLeft(const AString: String; ALength: Integer): String;
var
  i, j, n: Integer;
begin
  SetLength(Result, ALength);
  FillChar(Result[1], ALength, '0');
  n := Min(ALength, Length(AString));
  i := ALength;
  j := Length(AString);
  while (j >= 1) and (i >= 1) do
  begin
    Result[i] := AString[j];
    dec(i);
    dec(j);
  end;
end;

function StrToDbl(AText: String): Double;
var
  res: Integer;
begin
  Val(AText, Result, res);
  if res <> 0 then
    Result := -999;
end;

end.

