unit Kraken.Core;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Kraken.Consts,
  Kraken.Provider;

type
  TKrakenProvider = Kraken.Provider.TKrakenProvider;

  TKrakenProviderType   = Kraken.Consts.TKrakenProviderType;

  TKrakenCore = class
  private
    FProviders: TObjectList<TKrakenProvider>;

    class function Builder: TKrakenCore;
  protected

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddProvider(AProviderType: TKrakenProviderType): TKrakenProvider;
    function Provider(const AIndex: Integer = 0): TKrakenProvider;
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
  FProviders := TObjectList<TKrakenProvider>.Create;
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

function TKrakenCore.AddProvider(AProviderType: TKrakenProviderType): TKrakenProvider;
begin
  Result := FProviders.Items[ FProviders.Add( TKrakenProvider.Create(AProviderType) ) ];
end;

initialization

finalization
  if Assigned(FKrakenCore) then
    FKrakenCore.DisposeOf;

end.
