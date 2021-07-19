unit Kraken.Provider.Fields;

interface

uses
  System.SysUtils,
  System.JSON;

type
  TKrakenProviderFields = class
    constructor Create(AJSON: TJSONArray);
    destructor Destroy; override;
  private
    FField: string;
    FPos: Integer;
    FJSON: TJSONArray;
  public
    function Field(AFieldname: String): TKrakenProviderFields;
    function Pos(APos: Integer): TKrakenProviderFields;

    function AsString: string;
  end;

implementation

{ TKrakenProviderFields }

constructor TKrakenProviderFields.Create(AJSON: TJSONArray);
begin
  FJSON := AJSON;
end;

destructor TKrakenProviderFields.Destroy;
begin

  inherited;
end;

function TKrakenProviderFields.Field(AFieldname: String): TKrakenProviderFields;
begin
  Result := Self;
  FField := AFieldname;
end;

function TKrakenProviderFields.Pos(APos: Integer): TKrakenProviderFields;
begin
  Result := Self;
  FPos := APos;
end;

function TKrakenProviderFields.AsString: string;
var
  LJSONObject: TJSONObject;
begin
  try
    LJSONObject := FJSON.Items[FPos] as TJSONObject;

    Result := LJSONObject.GetValue(FField).ToString;
  except

  end;
end;

end.
