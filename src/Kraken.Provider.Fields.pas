unit Kraken.Provider.Fields;

interface

uses
  System.SysUtils,
  System.JSON,
  REST.Response.Adapter,
  REST.Json,
  Winapi.Windows,
  FireDAC.Comp.Client;

type
  TFormatType = (ftString, ftDate, ftDatetime);

  TKrakenProviderFields = class
    constructor Create;
    destructor Destroy; override;
  private
    FField : string;
    FPos   : Integer;
    FDataset  : TFDMemTable;

    function WhatsFormatIs(Value: string; out AResult: String): TFormatType;
  public
    function ResponseJSON( const Value : TFDMemTable ): TKrakenProviderFields;
    function Field       ( AFieldname  : String     ): TKrakenProviderFields;
    function Pos         ( APos        : Integer    ): TKrakenProviderFields;

    function AsString     : string;
    function AsInteger    : Integer;
    function AsFloat      : Double;
    function AsCurrency   : Currency;
    function AsXML        : WideString;
    function AsBoolean    : Boolean;
    function AsDate       : TDate;
    function AsDateTime   : TDateTime;
    function AsWideString : WideString;
    function IsNull       : Boolean;
    function Value        : Variant;
  end;

implementation

{ TKrakenProviderFields }

constructor TKrakenProviderFields.Create;
begin

end;

destructor TKrakenProviderFields.Destroy;
begin

  inherited;
end;

function TKrakenProviderFields.Field(AFieldname: String): TKrakenProviderFields;
begin
  Result := Self;
  FField := AnsiLowerCase( AFieldname );
end;

function TKrakenProviderFields.Pos(APos: Integer): TKrakenProviderFields;
begin
  Result := Self;
  FPos   := APos;
end;

function TKrakenProviderFields.ResponseJSON(const Value: TFDMemTable): TKrakenProviderFields;
begin
  Result := Self;
  FDataset  := Value;
end;

function TKrakenProviderFields.AsCurrency: Currency;
begin

  try
    result := FDataset.FieldByName(FField).AsCurrency;
  except
    Result := 0;
  end;
end;

function TKrakenProviderFields.AsDate: TDate;
var
  LFormatSettings: TFormatSettings;
begin
  LFormatSettings := TFormatSettings.Create;
  LFormatSettings.DateSeparator   := '-';
  LFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  LFormatSettings.TimeSeparator   := ':';
  LFormatSettings.ShortTimeFormat := 'hh:mm';
  LFormatSettings.LongTimeFormat  := 'hh:mm:ss';

  try
    if not TryStrToDate( FDataset.FieldByName(FField).AsString, TDatetime(Result), LFormatSettings ) then
      Result := -1;
  except
    Result := -1;
  end;
end;

function TKrakenProviderFields.AsDateTime: TDateTime;
var
  LFormatSettings: TFormatSettings;
begin
  LFormatSettings := TFormatSettings.Create;
  LFormatSettings.DateSeparator   := '-';
  LFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  LFormatSettings.TimeSeparator   := ':';
  LFormatSettings.ShortTimeFormat := 'hh:mm';
  LFormatSettings.LongTimeFormat  := 'hh:mm:ss';

  try
    if not TryStrToDateTime( FDataset.FieldByName(FField).AsString, Result, LFormatSettings )  then
      Result := -1;
  except
    Result := -1;
  end;
end;

function TKrakenProviderFields.AsFloat: Double;
begin
  try
    result := FDataset.FieldByName(FField).AsFloat;
  except
    Result := 0;
  end;
end;

function TKrakenProviderFields.AsInteger: Integer;
begin
  try
    Result := FDataset.FieldByName(FField).AsInteger;
  except
    Result := 0;
  end;
end;

function TKrakenProviderFields.AsString: string;
begin
  if FDataset.FieldByName(FField).AsString = 'null' then
    Result := ''
  else
  begin
    WhatsFormatIs( FDataset.FieldByName(FField).AsString, Result );

    Result := StringReplace( Result ,'\/' ,'/', [rfReplaceAll] );
    Result := StringReplace( Result ,'\\' ,'\', [rfReplaceAll] );
  end;
end;

function TKrakenProviderFields.AsWideString: WideString;
begin
  if FDataset.FieldByName(FField).AsWideString = 'null' then
    Result := ''
  else
  begin
    Result := FDataset.FieldByName(FField).AsWideString;

    Result := StringReplace( Result ,'\/' ,'/', [rfReplaceAll] );
    Result := StringReplace( Result ,'\\' ,'\', [rfReplaceAll] );
  end;
end;

function TKrakenProviderFields.AsXML: WideString;
begin
  if FDataset.FieldByName(FField).AsWideString = 'null' then
    Result := ''
  else
    Result := FDataset.FieldByName(FField).AsWideString;
end;

function TKrakenProviderFields.AsBoolean: Boolean;
begin
  if not TryStrToBool( FDataset.FieldByName(FField).AsString, Result ) then
    Result := False;
end;

function TKrakenProviderFields.IsNull: Boolean;
begin
  if FDataset.FieldByName(FField).AsString = 'null' then
    Result := False
  else
    Result := FDataset.FieldByName(FField).AsString = '';
end;

function TKrakenProviderFields.WhatsFormatIs(Value: string; out AResult: String): TFormatType;
var
  LSuccess: Boolean;
  LDate: TDateTime;
  LValue: String;
  LFormatSettings: TFormatSettings;
begin
  LFormatSettings := TFormatSettings.Create;
  LFormatSettings.DateSeparator   := '-';
  LFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  LFormatSettings.TimeSeparator   := ':';
  LFormatSettings.ShortTimeFormat := 'hh:mm';
  LFormatSettings.LongTimeFormat  := 'hh:mm:ss';


  // 01-01-2021
  // 01/01/2021

  // 2021-01-01
  // 2021/01/01
  LSuccess := False;
  LValue := Value;

  if Length(LValue) <= 10 then //Nao pode ser datetime e pode ser date
  begin
    if TryStrToDate(LValue, LDate) then
    begin
      LSuccess := True;
      Result   := ftDate;
      AResult  := FormatDateTime('dd-mm-yyyy', LDate);
    end;
  end;

  if not LSuccess then
  begin

    if Length(LValue) > 10 then //Não pode ser date e pode ser datetime
    begin


      if ( ( System.Pos('-', LValue) > 0 ) or ( System.Pos('/', LValue) > 0 ) ) and ( System.Pos(':', LValue) > 0 ) then
      begin
        if System.Pos('.', LValue) > 0 then
          LValue := Copy(
            LValue,
            0,
            System.Pos('.', LValue) -1
          );

        if TryStrToDateTime(LValue, LDate, LFormatSettings) then
        begin
          LSuccess := True;
          Result   := ftDatetime;
          AResult  := FormatDateTime('dd/mm/yyyy hh:MM:ss', LDate);
        end;

      end;

    end;

  end;

  if not LSuccess then
  begin
    Result  := ftString;
    AResult := LValue;
  end;
end;

function TKrakenProviderFields.Value: Variant;
begin
  if FDataset.FieldByName(FField).AsString = 'null' then
    Result := ''
  else
    Result := FDataset.FieldByName(FField).AsString;
end;


end.
