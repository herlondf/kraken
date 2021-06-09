unit Kraken.Core;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Kraken.Consts,
  Kraken.Provider,
  Kraken.Provider.Query;

type
  TKrakenProvider      = Kraken.Provider.TKrakenProvider;
  TKrakenProviders     = TObjectList<TKrakenProvider>;
  TKrakenProviderQuery = Kraken.Provider.Query.TKrakenProviderQuery;
  TKrakenProviderType  = Kraken.Consts.TKrakenProviderType;

  TKrakenCore = class
  private
    FProviders: TKrakenProviders;

    class function Builder: TKrakenCore;
  protected

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddProvider: TKrakenProvider;
    function Providers: TKrakenProviders;
    function Provider(const AIndex: Integer = 0): TKrakenProvider;
    function ProviderByIdent(const Value: String): TKrakenProvider;
  end;

  function TKraken: TKrakenCore;

implementation

var
  FKrakenCore: TKrakenCore;

function TKraken: TKrakenCore;
begin
  if not Assigned(FKrakenCore) then
    FKrakenCore := TKrakenCore.Builder;

  Result := FKrakenCore;
end;

{ TKrakenCore }

procedure TKrakenCore.AfterConstruction;
begin
  FProviders := TKrakenProviders.Create;
end;

procedure TKrakenCore.BeforeDestruction;
begin
  FreeAndNil(FProviders);
end;

class function TKrakenCore.Builder: TKrakenCore;
begin
  Result := TKrakenCore.Create;
end;

function TKrakenCore.Provider(const AIndex: Integer): TKrakenProvider;
begin
  try
    Result := FProviders.Items[AIndex];
  except
    raise Exception.Create('Dont have provider with this index.');
  end;
end;

function TKrakenCore.ProviderByIdent(const Value: String): TKrakenProvider;
var
  LProvider: TKrakenProvider;
begin
  Result := nil;

  for LProvider in FProviders do
  begin
    if AnsiUpperCase(LProvider.Identification) = AnsiUpperCase(Value) then
    begin
      Result := LProvider;
      Break;
    end;
  end;
end;

function TKrakenCore.Providers: TKrakenProviders;
begin
  Result := FProviders;
end;

function TKrakenCore.AddProvider: TKrakenProvider;
begin
  Result := FProviders.Items[FProviders.Add(TKrakenProvider.Create(nil))];
end;

initialization

finalization
  if Assigned(FKrakenCore) then
    FKrakenCore.DisposeOf;

end.
