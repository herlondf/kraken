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
  TKrakenProvider = Kraken.Provider.TKrakenProvider;
  TKrakenProviderQuery = Kraken.Provider.Query.TKrakenProviderQuery;
  
  TKrakenCore = class
  private
    FProviders: TObjectList<TKrakenProvider>;
  protected
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    class function Invoker: TKrakenCore;
    class procedure Clear;

    function TryGetProvider( const AId: Integer     ): TKrakenProvider; overload;
    function TryGetProvider( const AId: String = '' ): TKrakenProvider; overload;
  end;

implementation

var
  FInstance: TKrakenCore;

{ TKrakenCore }

procedure TKrakenCore.AfterConstruction;
begin
  FProviders := TObjectList<TKrakenProvider>.Create;
end;

procedure TKrakenCore.BeforeDestruction;
begin
  FInstance := nil;
  FreeAndNil(FProviders);
end;

class procedure TKrakenCore.Clear;
begin
  Invoker.FProviders.Clear;
end;

class function TKrakenCore.Invoker: TKrakenCore;
begin
  if not Assigned(FInstance) then
    FInstance := TKrakenCore.Create;

  Result := FInstance;
end;

function TKrakenCore.TryGetProvider(const AId: Integer): TKrakenProvider;
begin
  Result := TryGetProvider( IntToStr( AId ) );
end;

function TKrakenCore.TryGetProvider(const AId: String): TKrakenProvider;
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

initialization

finalization
  if Assigned( FInstance ) then
    FreeAndNil( FInstance );

end.
