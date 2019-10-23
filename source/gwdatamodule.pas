unit gwDataModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TMainDatamodule }

  TMainDatamodule = class(TDataModule)
    Logos: TImageList;
  private

  public

  end;

var
  MainDatamodule: TMainDatamodule;

implementation

{$R *.lfm}

end.

