unit gwData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TMonth = (mJan, mFeb, mMar, mApr, mMay, mJun, mJul, mAug, mSep, mOct, mNov, mDec);
  TSeason = (sWinter, sSpring, sSummer, sFall);

  TBasicDataItem = class
  public
    Year: Integer;
    AnnualMean: Double;     // calendar mean: Jan - Dec
    constructor Create; virtual;
    procedure Assign(ASource: TBasicDataItem); virtual;
  end;

  TDataItem = class(TBasicDataItem)
  public
    MetAnnualMean: Double;  // meteorological mean: Dec - Nev
    MonthlyMean: array[TMonth] of Double;
    SeasonalMean: array[TSeason] of Double;
    constructor Create; override;
    procedure Assign(ASource: TBasicDataItem); override;
  end;
  TDataItemClass = class of TDataItem;

  TDataList = class(TFPObjectList)
  private
    function GetItem(AIndex: Integer): TBasicDataItem;
    procedure SetItem(AIndex: Integer; const AValue: TBasicDataItem);
  public
    property Items[AIndex: Integer]: TBasicDataItem read GetItem write SetItem; default;
  end;


implementation

{ TBasicDataItem}

constructor TBasicDataItem.Create;
begin
  inherited;
end;

procedure TBasicDataItem.Assign(ASource: TBasicDataItem);
begin
  Year := ASource.Year;
  AnnualMean := ASource.AnnualMean;
end;


{ TDataItem }

constructor TDataItem.Create;
begin
  inherited;
end;

procedure TDataItem.Assign(ASource: TBasicDataItem);
var
  m: TMonth;
  s: TSeason;
begin
  if ASource is TDataItem then
  begin
    inherited;
    for m in TMonth do
      MonthlyMean[m] := TDataItem(ASource).MonthlyMean[m];
    for s in TSeason do
      SeasonalMean[s] := TDataItem(ASource).SeasonalMean[s];
  end else
    raise Exception.Create('Cannot assign ' + ASource.ClassName + ' to TDataItem');
end;


{ TDataList }

function TDataList.GetItem(AIndex: Integer): TBasicDataItem;
begin
  Result := TBasicDataItem(inherited Items[AIndex]);
end;

procedure TDataList.SetItem(AIndex: Integer; const AValue: TBasicDataItem);
begin
  TBasicDataItem(inherited Items[AIndex]).Assign(AValue);
end;

end.

