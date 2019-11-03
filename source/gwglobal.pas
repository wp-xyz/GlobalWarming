unit gwGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  BASE_DATA_DIR = 'data/'; //'../data/';
  DEFAULT_STATIONS_FILE = 'stations.txt';

  IMG_REMOTE_FILE = -1;
  IMG_LOCAL_FILE = 0;
  IMG_LOCAL_FILE_NO_TEMPERATURE = 1;

  PI_ANNUAL = 1;            // temperatures
  PI_MET_ANNUAL = 2;
  PI_WINTER = 4;
  PI_SPRING = 8;
  PI_SUMMER = 16;
  PI_FALL = 32;
  PI_MONTH = 64;
  PI_SUNSHINE = 128;      // Sunshine duration
  PI_MONTHLY_PRECIP = 256;  // precipitation
  PI_DAILY_PRECIP = 512;

  // These items require an x axis with month resolution
  PI_MONTH_ITEMS = PI_MONTH + PI_SUNSHINE + PI_MONTHLY_PRECIP + PI_DAILY_PRECIP;
  // These items require a temperature y axis (AX_TEMPERATURE)
  PI_TEMPERATURE = PI_ANNUAL + PI_MET_ANNUAL + PI_WINTER + PI_SPRING + PI_SUMMER + PI_FALL + PI_MONTH;
  // These items require a precipitation y axis (AX_PRECIPITATION)
  PI_PRECIPITATION = PI_MONTHLY_PRECIP + PI_DAILY_PRECIP;

  // TAChart axis indexes
  AX_TEMPERATURE = 0;
  AX_SUNSHINE = 2;
  AX_PRECIPITATION = 3;

var
  DataDir: String = BASE_DATA_DIR;

  PtFormatSettings: TFormatSettings;

  FileViewerLeft: Integer = -1;
  FileViewerTop: Integer = -1;
  FileViewerWidth: Integer = -1;
  FileViewerHeight: Integer = -1;


implementation

initialization
  PtFormatSettings := DefaultFormatSettings;
  PtFormatSettings.DecimalSeparator := '.';
  PtFormatSettings.ThousandSeparator := ',';

end.

