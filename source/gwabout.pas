unit gwAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BitBtn1: TBitBtn;
    AppImage: TImage;
    imgNASA: TImage;
    imgDWD: TImage;
    lblFlatIcon: TLabel;
    lblAppIcon: TLabel;
    lblDWD: TLabel;
    lblDataSources: TLabel;
    lblNASA: TLabel;
    lblAcknowledgements: TLabel;
    lblCompiler: TLabel;
    lblFPC: TLabel;
    lblIcons: TLabel;
    lblIcons8: TLabel;
    lblIDE: TLabel;
    lblLazarus: TLabel;
    lblTitle: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure LabelMouseEnter(Sender: TObject);
    procedure LabelMouseLeave(Sender: TObject);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf, Types,
  gwDataModule;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  with AppImage do
  begin
    Picture.Assign(Application.Icon);
    Picture.Icon.Current := Picture.Icon.GetBestIndexForSize(Size(Width, Height));
  end;
  MainDatamodule.Logos.Resolution[32].GetIcon(0, imgNASA.Picture.Icon);
  MainDatamodule.Logos.Resolution[32].GetIcon(1, imgDWD.Picture.Icon);
end;

procedure TAboutForm.LabelClick(Sender: TObject);
begin
  if Sender = lblFPC then
    OpenURL('https://www.freepascal.org/')
  else if Sender = lblLazarus then
    OpenURL('https://www.lazarus-ide.org/')
  else if Sender = lblIcons8 then
    OpenURL('http://www.icons8.com')
  else if (Sender = lblNASA) or (Sender = imgNASA) then
    OpenURL('https://data.giss.nasa.gov/gistemp/')
  else if (Sender = lblDWD) or (Sender = imgDWD) then
    OpenURL('https://www.dwd.de/EN/climate_environment/cdc/cdc_node.html')
  else if Sender = lblFlatIcon then
    OpenURL('https://www.flaticon.com/authors/monkik');
end;

procedure TAboutForm.LabelMouseEnter(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style + [fsUnderline];
end;

procedure TAboutForm.LabelMouseLeave(Sender: TObject);
begin
  with (Sender as TControl).Font do
    Style := Style - [fsUnderline];
end;

end.

