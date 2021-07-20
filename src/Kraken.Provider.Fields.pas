unit Kraken.Provider.Fields;

interface

uses
  System.SysUtils,
  System.JSON,
  REST.Response.Adapter,
  REST.Json,
  Winapi.Windows;

type
  TKrakenProviderFields = class
    constructor Create;
    destructor Destroy; override;
  private
    FField : string;
    FPos   : Integer;
    FJSON  : TJSONArray;

    function GetJSONObject: TJSONObject;
    function ResponsePrepare(const AValue: String): string;
  public
    function ResponseJSON( const Value : TJSONArray ): TKrakenProviderFields;
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
  FField := StringReplace(AFieldname, '_', '', [rfReplaceAll, rfIgnoreCase]);
end;

function TKrakenProviderFields.GetJSONObject: TJSONObject;
var
  LJSONObject: TJSONObject;
begin
  try
    Result := FJSON.Items[FPos] as TJSONObject;
  except
    raise;
  end;
end;

function TKrakenProviderFields.Pos(APos: Integer): TKrakenProviderFields;
begin
  Result := Self;
  FPos   := APos;
end;

function TKrakenProviderFields.ResponseJSON(const Value: TJSONArray): TKrakenProviderFields;
begin
  Result := Self;
  FJSON  := Value;
end;

function TKrakenProviderFields.ResponsePrepare(const AValue: String): string;
var
  LResult : string;
begin
  LResult := '';
  LResult := Trim( AValue );
  LResult := StringReplace( LResult , '"' , '' , [rfReplaceAll] );
  Result  := LResult;
end;

function TKrakenProviderFields.AsCurrency: Currency;
begin
  Result := StrToCurrDef( ResponsePrepare( GetJSONObject.GetValue(FField).ToString ), -1 );
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

  Result := StrToDate( ResponsePrepare( GetJSONObject.GetValue(FField).ToString ), LFormatSettings );
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

  Result := StrToDateTime( ResponsePrepare( GetJSONObject.GetValue(FField).ToString ), LFormatSettings );
end;

function TKrakenProviderFields.AsFloat: Double;
begin
  Result := JsonToFloat( GetJSONObject.GetValue(FField).ToString );
end;

function TKrakenProviderFields.AsInteger: Integer;
begin
  Result := StrToIntDef( ResponsePrepare( GetJSONObject.GetValue(FField).ToString ), -1 );
end;

function TKrakenProviderFields.AsString: string;
begin
  Result := ResponsePrepare( GetJSONObject.GetValue(FField).ToString );
end;

function TKrakenProviderFields.AsWideString: WideString;
begin
  Result := ResponsePrepare( GetJSONObject.GetValue(FField).ToString );
end;

function TKrakenProviderFields.AsXML: WideString;
begin
  Result := ResponsePrepare( GetJSONObject.GetValue(FField).ToString );
end;

function TKrakenProviderFields.AsBoolean: Boolean;
begin
  Result := StrToBoolDef( ResponsePrepare( GetJSONObject.GetValue(FField).ToString ), False );
end;

function TKrakenProviderFields.IsNull: Boolean;
begin
  Result := GetJSONObject.GetValue(FField).ToString = '';
end;

function TKrakenProviderFields.Value: Variant;
begin
  Result := GetJSONObject.GetValue(FField).ToString;
end;


end.
