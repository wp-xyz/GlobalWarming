program gw;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, gwMain, gwUtils, gwNASA, gwGlobal, gwStations,
  gwDeutscherWetterDienst, gwData, gwAbout, gwDataModule
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainDatamodule, MainDatamodule);
  Application.Run;
end.

