unit Kraken.Provider.Query;

interface

uses
  Kraken.Provider.Zeos.Query,
  Kraken.Provider.Firedac.Query;

type
{$IF DEFINED(KRAKEN_FIREDAC)}
  TKrakenProviderQuery = Kraken.Provider.Firedac.Query.TKrakenProviderFiredacQuery;
{$ELSE}
  TKrakenProviderQuery = Kraken.Provider.Zeos.Query.TKrakenProviderZeosQuery;
{$ENDIF}

implementation

end.
