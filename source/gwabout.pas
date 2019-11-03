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
    imgPAGES2k: TImage;
    imgNASA: TImage;
    imgDWD: TImage;
    lblPAGES2k: TLabel;
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

const
  URL_FPC = 'https://www.freepascal.org/';
  URL_Lazarus = 'https://www.lazarus-ide.org/';
  URL_Icons8 = 'http://www.icons8.com';
  URL_FlatIcon_Monkik = 'https://www.flaticon.com/authors/monkik';
  URL_NASA_GISS = 'https://data.giss.nasa.gov/gistemp/';
  URL_DWD = 'https://www.dwd.de/EN/climate_environment/cdc/cdc_node.html';
  URL_PAGES2k = 'http://pastglobalchanges.org/science/wg/2k-network/data/phase-2-data';


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
  MainDatamodule.Logos.Resolution[32].GetIcon(2, imgPAGES2k.Picture.Icon);

  lblFPC.Hint := URL_FPC;
  lblLazarus.Hint := URL_Lazarus;
  lblIcons8.Hint := URL_Icons8;
  lblFlatIcon.Hint := URL_FlatIcon_Monkik;
  lblNASA.Hint := URL_NASA_GISS;
  lblDWD.Hint := URL_DWD;
  lblPAGES2k.Hint := URL_PAGES2k;
  imgNASA.Hint := URL_NASA_GISS;
  imgDWD.Hint := URL_DWD;
  imgPAGES2k.Hint := URL_PAGES2k;
end;

procedure TAboutForm.LabelClick(Sender: TObject);
begin
  if Sender = lblFPC then
    OpenURL(URL_FPC)
  else if Sender = lblLazarus then
    OpenURL(URL_Lazarus)
  else if Sender = lblIcons8 then
    OpenURL(URL_Icons8)
  else if (Sender = lblNASA) or (Sender = imgNASA) then
    OpenURL(URL_NASA_GISS)
  else if (Sender = lblDWD) or (Sender = imgDWD) then
    OpenURL(URL_DWD)
  else if (Sender = lblPAGES2k) or (Sender = imgPAGES2k) then
    OpenURL(URL_PAGES2k)
  else if Sender = lblFlatIcon then
    OpenURL(URL_FlatIcon_Monkik);
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

