unit gwData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TMonth = (mJan, mFeb, mMar, mApr, mMay, mJun, mJul, mAug, mSep, mOct, mNov, mDec);
  TSeason = (sWinter, sSpring, sSummer, sFall);

  TDataItem = class
  public
    Year: Integer;
    AnnualMean: Double;     // calendar mean: Jan - Dec
    MetAnnualMean: Double;  // meteorological mean: Dec - Nev
    MonthlyMean: array[TMonth] of Double;
    SeasonalMean: array[TSeason] of Double;
    constructor Create; virtual;
    procedure Assign(ASource: TDataItem); virtual;
  end;
  TDataItemClass = class of TDataItem;

  TDataList = class(TFPObjectList)
  private
    function GetItem(AIndex: Integer): TDataItem;
    procedure SetItem(AIndex: Integer; const AValue: TDataItem);
  public
    property Items[AIndex: Integer]: TDataItem read GetItem write SetItem; default;
  end;


implementation

{ TDataItem }

constructor TDataItem.Create;
begin
  inherited;
end;

procedure TDataItem.Assign(ASource: TDataItem);
var
  m: TMonth;
  s: TSeason;
begin
  Year := ASource.Year;
  AnnualMean := ASource.AnnualMean;
  for m in TMonth do
    MonthlyMean[m] := ASource.MonthlyMean[m];
  for s in TSeason do
    SeasonalMean[s] := ASource.SeasonalMean[s];
end;


{ TDataList }

function TDataList.GetItem(AIndex: Integer): TDataItem;
begin
  Result := TDataItem(inherited Items[AIndex]);
end;

procedure TDataList.SetItem(AIndex: Integer; const AValue: TDataItem);
begin
  TDataItem(inherited Items[AIndex]).Assign(AValue);
end;

end.

