unit Kraken.Provider.Param;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Kraken.Types;

type
  TKrakenParams = TObjectList<TKrakenParamsClass>;

  TKrakenProviderParam = class
    constructor Create;
    destructor Destroy; override;
  strict private
    function ParamExist(AParam: string): Boolean;
    procedure ParamAdd(AParam: String; AValue: Variant);
  private
    FKrakenParams: TKrakenParams;

    FParam: String;

    procedure SetAsString(const Value: String);
    procedure SetAsInteger(const Value: Integer);
  public
    function Param(const AParam: String): TKrakenProviderParam;
    function PrepareSQL(const ASQL: string): String;

    property AsString  : String  write SetAsString;
    property AsInteger : Integer  write SetAsInteger;
  end;

implementation

{ TKrakenProviderParam }

constructor TKrakenProviderParam.Create;
begin
  FKrakenParams := TKrakenParams.Create();
end;

destructor TKrakenProviderParam.Destroy;
begin
  FreeAndNil(FKrakenParams);
  inherited;
end;

procedure TKrakenProviderParam.SetAsInteger(const Value: Integer);
begin
  ParamAdd(FParam, Value);
end;

procedure TKrakenProviderParam.SetAsString(const Value: String);
begin
  ParamAdd(FParam, Value);
end;

function TKrakenProviderParam.Param(const AParam: String): TKrakenProviderParam;
begin
  Result := Self;
  FParam := AParam;
end;

procedure TKrakenProviderParam.ParamAdd(AParam: String; AValue: Variant);
begin
  if not ParamExist(AParam) then
  begin
    with FKrakenParams.Items[ FKrakenParams.Add( TKrakenParamsClass.Create ) ] do
    begin
      DisplayName := ':'+AParam;
      Value := AValue;
    end;
  end;
end;

function TKrakenProviderParam.ParamExist(AParam: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred( FKrakenParams.Count ) do
  begin
    Result := AnsiUpperCase( FKrakenParams.Items[I].DisplayName ) = AnsiUpperCase(AParam);

    if Result then
      Break;
  end;
end;

function TKrakenProviderParam.PrepareSQL(const ASQL: string): String;
var
  I: Integer;
  LSQL: String;
begin
  LSQL := ASQL;
  for I := 0 to Pred(FKrakenParams.Count) do
    LSQL := StringReplace(LSQL, FKrakenParams.Items[I].DisplayName, FKrakenParams.Items[I].Value, [rfReplaceAll,  rfIgnoreCase]);

  Result := LSQL
end;

end.
