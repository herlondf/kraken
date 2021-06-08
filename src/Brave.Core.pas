unit Brave.Core;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Brave.Consts,
  Brave.Provider;

type
  TBraveProvider = Brave.Provider.TBraveProvider;

  TBraveProviderType   = Brave.Consts.TBraveProviderType;

  TBraveCore = class
  private
    FProviders: TObjectList<TBraveProvider>;

    class function Builder: TBraveCore;
  protected

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function AddProvider(AProviderType: TBraveProviderType): TBraveProvider;
    function Provider(const AIndex: Integer = 0): TBraveProvider;
  end;

  function TBrave: TBraveCore;

implementation

var
  FBraveCore: TBraveCore;

function TBrave: TBraveCore;
begin
  if not Assigned(FBraveCore) then
    FBraveCore := TBraveCore.Builder;

  Result := FBraveCore;
end;

{ TBraveCore }

procedure TBraveCore.AfterConstruction;
begin
  FProviders := TObjectList<TBraveProvider>.Create;
end;

procedure TBraveCore.BeforeDestruction;
begin
  FreeAndNil(FProviders);
end;

class function TBraveCore.Builder: TBraveCore;
begin
  Result := TBraveCore.Create;
end;

function TBraveCore.Provider(const AIndex: Integer): TBraveProvider;
begin
  try
    Result := FProviders.Items[AIndex];
  except
    raise Exception.Create('Dont have provider with this index.');
  end;
end;

function TBraveCore.AddProvider(AProviderType: TBraveProviderType): TBraveProvider;
begin
  Result := FProviders.Items[ FProviders.Add( TBraveProvider.Create(AProviderType) ) ];
end;

initialization

finalization
  if Assigned(FBraveCore) then
    FBraveCore.DisposeOf;

end.
