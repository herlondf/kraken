unit Kraken.Provider.Query;

interface

uses
  Kraken.Provider.Zeos.Query,
  Kraken.Provider.Firedac.Query,
  Kraken.Provider.RequestHTTP.Query;

type
{$IF DEFINED(KRAKEN_FIREDAC)}
  TKrakenProviderQuery = Kraken.Provider.Firedac.Query.TKrakenProviderFiredacQuery;
{$ELSE}
  {$IF DEFINED(KRAKEN_REQUESTHTTP)}
    TKrakenProviderQuery = Kraken.Provider.RequestHTTP.Query.TKrakenProviderRequestHTTPQuery;
  {$ELSE}
    TKrakenProviderQuery = Kraken.Provider.Zeos.Query.TKrakenProviderZeosQuery;
  {$ENDIF}
{$ENDIF}




implementation

end.
