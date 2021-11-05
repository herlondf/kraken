unit Kraken.Provider.Query;

interface

uses
  {$IF DEFINED(KRAKEN_FIREDAC)}
    Kraken.Provider.Firedac.Query;
  {$ELSE}
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
      Kraken.Provider.RequestHTTP.Query;
    {$ELSE}
      Kraken.Provider.Zeos.Query;
    {$ENDIF}
  {$ENDIF}

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
