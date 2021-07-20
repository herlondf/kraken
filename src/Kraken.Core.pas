unit Kraken.Core;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Kraken.Types,
  Kraken.Consts,
  Kraken.Provider,
  Kraken.Provider.Query;

type
  TLocateOption        = Kraken.Types.TLocateOption;
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

    function Providers: TKrakenProviders;

    function Provider(const AId: Integer ): TKrakenProvider; overload;
    function Provider(const AId: String  = ''): TKrakenProvider; overload;
  end;

  function TKraken: TKrakenCore;

implementation

var
  FInstnace: TKrakenCore;

function TKraken: TKrakenCore;
begin
  if not Assigned(FInstnace) then
    FInstnace := TKrakenCore.Builder;

  Result := FInstnace;
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

function TKrakenCore.Provider(const AId: String): TKrakenProvider;
var
  LProvider: TKrakenProvider;
begin
  Result := nil;

  for LProvider in FProviders do
  begin
    if AnsiUpperCase(LProvider.Id) = AnsiUpperCase(AId) then
    begin
      Result := LProvider;
      Break;
    end;
  end;

  if Result = nil then
  begin
    LProvider := FProviders.Items[ FProviders.Add( TKrakenProvider.Create(nil) ) ];
    LProvider.Id(AId);
    Result := LProvider;
  end;
end;

function TKrakenCore.Provider(const AId: Integer): TKrakenProvider;
begin
  Result := Provider( IntToStr(AId) );
end;

function TKrakenCore.Providers: TKrakenProviders;
begin
  Result := FProviders;
end;

initialization

finalization
  if Assigned(FInstnace) then
    FInstnace.DisposeOf;

end.
