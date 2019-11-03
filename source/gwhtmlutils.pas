unit gwHTMLUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TTableExtractor }

  TTableExtractor = class
  private
    FInTable: Boolean;
    FInRow: Boolean;
    FInCell: Boolean;
    FLines: TStrings;
    FLine: TStrings;
    FCellText: String;
    procedure FoundTagHandler(NoCaseTag, ActualTag: String);
    procedure FoundTextHandler(AText: String);
  public
    procedure Process(AStream: TStream; ALines: TStrings; ADelimiter: Char);
  end;


  { TTextExtractor }

  TTextExtractor = class
  private
    FLines: TStrings;
    procedure FoundTextHandler(AText: String);
  public
    procedure Process(AStream: TStream; ALines: TStrings);
  end;


  { TLinkExtractor }

  TLinkExtractor = class
  private
    FLines: TStrings;
    FExt: String;
    procedure FoundTagHandler(NoCaseTag, ActualTag: String);
  public
    procedure Process(AStream: TStream; ALines: TStrings; AExtension: String = '');
  end;


implementation

uses
  FastHTMLParser, HTMLUtil;

{ TTableExtractor }

procedure TTableExtractor.FoundTagHandler(NoCaseTag, ActualTag: string);
begin
  if Length(NoCaseTag) < 4 then
    exit;
  case NoCaseTag[2] of
    'T': if Pos('<TABLE', NoCaseTag) = 1 then
           // <table> - begin of table --> set flag to activate processing
           FInTable := true
         else
         if (NoCaseTag[3] in ['R']) then
         begin
           // <tr> - begin of row --> clear FLine list
           inc(FInRow);
           FLine.Clear;
         end else
         if (NoCaseTag[3] in ['D','H']) and (NoCaseTag[4] in [' ', '>']) then
         begin
           // <tc>, <th> (not <thead>) --> begin of cell --> reset storage for collected cell text
           inc(FInCell);
           FCellText := '';
         end;
    '/': if pos('</TABLE', NoCaseTag) = 1 then
           // </table> - end of table --> ignore anything outside the table
           FInTable := false
         else if (NoCaseTag[3] = 'T') then
         begin
           if (NoCaseTag[4] in ['R']) then begin
             // </tr> - end of row tag --> copy add FLine to lines list
             dec(FInRow);
             FLines.Add(FLine.DelimitedText);
           end else
           if (NoCaseTag[4] in ['D', 'H']) then
             // </td>, </th> - end of cell node --> write collected texts to FLine
             FLine.Add(FCellText);
         end;
  end;
end;

procedure TTableExtractor.FoundTextHandler(AText: String);
begin
  if FInTable and FInRow and FInCell then
    // append all texts when inside a cell
    FCellText := FCellText + AText;
end;

procedure TTableExtractor.Process(AStream: TStream; ALines: TStrings;
  ADelimiter: Char);
var
  parser: THtmlParser;
  srcStream: TMemoryStream;
begin
  if AStream is TMemoryStream then
    srcStream := TMemoryStream(AStream)
  else
  begin
    srcStream := TMemoryStream.Create;
    srcStream.CopyFrom(AStream, AStream.Size);
  end;

  try
    FLines := ALines;
    FLines.Clear;

    FLine := TStringList.Create;
    try
      FLine.Delimiter := ADelimiter;  //DWD_STATION_SEPARATOR;
      FLine.StrictDelimiter := true;

      srcStream.Position := 0;
      parser := THTMLParser.Create(srcStream.Memory);
      try
        parser.OnFoundTag := @FoundTagHandler;
        parser.OnFoundText := @FoundTextHandler;
        parser.Exec;
      finally
        parser.Free;
      end;
    finally
      FLine.Free;
    end;
  finally
    if not (AStream is TMemoryStream) then
      srcStream.Free;
  end;
end;


{ TTextExtractor }

procedure TTextExtractor.FoundTextHandler(AText: String);
begin
  FLines.Add(AText);
end;

procedure TTextExtractor.Process(AStream: TStream; ALines: TStrings);
var
  parser: THtmlParser;
  srcStream: TMemoryStream;
begin
  if AStream is TMemoryStream then
    srcStream := TMemoryStream(AStream)
  else
  begin
    srcStream := TMemoryStream.Create;
    srcStream.CopyFrom(AStream, AStream.Size);
  end;

  try
    FLines := ALines;
    FLines.Clear;

    srcStream.Position := 0;
    parser := THTMLParser.Create(srcStream.Memory);
    try
      parser.OnFoundText := @FoundTextHandler;
      parser.Exec;
    finally
      parser.Free;
    end;
  finally
    if not (AStream is TMemoryStream) then
      srcStream.Free;
  end;
end;


{ TLinkExtractor }

procedure TLinkExtractor.FoundTagHandler(NoCaseTag, ActualTag: String);
var
  s: String;
  p: Integer;
begin
  if pos('<A ', NoCaseTag) = 1 then
  begin
    s := GetVal(ActualTag, 'href');
    if (FExt <> '') and (ExtractFileExt(s) <> FExt) then
      exit;
    if FLines.IndexOf(s) <> -1 then
      exit;
    FLines.Add(s);
  end;
end;

procedure TLinkExtractor.Process(AStream: TStream; ALines: TStrings;
  AExtension: String = '');
var
  parser: THtmlParser;
  srcStream: TMemoryStream;
begin
  Assert(AStream is TMemoryStream);
  srcStream := TMemoryStream(AStream);

  FExt := AExtension;
  FLines := ALines;
  FLines.Clear;

  srcStream.Position := 0;
  parser := THTMLParser.Create(srcStream.Memory);
  try
    parser.OnFoundTag := @FoundTagHandler;
    parser.Exec;
  finally
    parser.Free;
  end;
end;

end.

