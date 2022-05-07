unit Kraken.Core;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Kraken.Types,
  Kraken.Consts,
  Kraken.Provider,
  Kraken.PoolManager,
  Kraken.Provider.Query;

type
  TLocateOption        = Kraken.Types.TLocateOption;
  TKrakenProvider      = Kraken.Provider.TKrakenProvider;
  TKrakenProviders     = TObjectList<TKrakenProvider>;
  TKrakenProviderQuery = Kraken.Provider.Query.TKrakenProviderQuery;
  TKrakenProviderType  = Kraken.Consts.TKrakenProviderType;

  TQueryCallback       = reference to procedure( AQuery      : TKrakenProviderQuery );
  TConnectionCallback  = reference to procedure( AConnection : TKrakenProvider      );

  TKrakenCore = class( TPoolManager<TKrakenProvider> )
    class constructor Initialize;
    class destructor UnInitialize;
  private
    class procedure CreateDefaultInstance;

    ///<summary>
    ///   Sobrescrita do metodo abstrato do pool, usado para quando o contador de referencia do objeto solicitado for maior que zero,
    ///   ou seja, estiver em uso, irá instanciar um novo com os mesmos atributos.
    ///</summary>
    procedure DoGetInstance(var AInstance: TKrakenProvider; var AInstanceOwner: Boolean; const AId: Integer = -1); override;
  public
    function Provider( const AId: Integer = 0  ): TKrakenProvider; overload;
    function Provider( const AId: String  = '' ): TKrakenProvider; overload;
  end;

  function TKraken: TKrakenCore;

implementation

var
  FInstance: TKrakenCore;

function TKraken: TKrakenCore;
begin
  if not Assigned(FInstance) then
    TKrakenCore.CreateDefaultInstance;

  Result := FInstance;
end;

{ TKrakenCore }

class constructor TKrakenCore.Initialize;
begin
  CreateDefaultInstance;
end;

class destructor TKrakenCore.UnInitialize;
begin
  if FInstance <> nil then
    FreeAndNil(FInstance);
end;

class procedure TKrakenCore.CreateDefaultInstance;
begin
  FInstance := TKrakenCore.Create(True);
  FInstance.SetMaxIdleSeconds(60);
  FInstance.Start;
end;

procedure TKrakenCore.DoGetInstance(var AInstance: TKrakenProvider; var AInstanceOwner: Boolean; const AId: Integer = -1);
begin
  inherited;

  AInstanceOwner := True;
  AInstance := TKrakenProvider.Create(nil);
  try
    AInstance.Id( IntToStr( AId ) );
  except
    FreeAndNil(AInstance);
    raise;
  end;
end;

function TKrakenCore.Provider(const AId: String): TKrakenProvider;
var
  LItem       : TPoolItem<TKrakenProvider>;
  LConnection : TKrakenProvider;
begin
  LItem       := FInstance.TryGetItemWithoutRefCount( StrToIntDef( AId, 0 ) );
  LConnection := LItem.Acquire;
  Result      := LConnection;
end;

function TKrakenCore.Provider(const AId: Integer): TKrakenProvider;
begin
  Result := Provider( IntToStr( AId ) );
end;

initialization

finalization
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

end.
