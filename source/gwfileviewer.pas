unit gwFileViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  gwStations;

type

  { TFileViewerForm }

  TFileViewerForm = class(TForm)
    Memo: TMemo;
  private
    FStation: TStation;
    procedure SetStation(AValue: TStation);
  public
    property Station: TStation read FStation write SetStation;

  end;

var
  FileViewerForm: TFileViewerForm;

implementation

{$R *.lfm}

uses
  gwGlobal;

procedure TFileViewerForm.SetStation(AValue: TStation);
begin
  FStation := AValue;
  if FStation = nil then
  begin
    Caption := 'File viewer';
    Memo.Lines.Clear;
  end else
  begin
    Caption := 'File viewer - "' + FStation.DataFileName + '"';
    Memo.Lines.LoadFromFile(FStation.DataFileName);
  end;
end;

end.

