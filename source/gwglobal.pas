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

  PI_ANNUAL = 1;
  PI_MET_ANNUAL = 2;
  PI_WINTER = 4;
  PI_SPRING = 8;
  PI_SUMMER = 16;
  PI_FALL = 32;
  PI_MONTH = 64;

var
  DataDir: String = BASE_DATA_DIR;

implementation

end.

