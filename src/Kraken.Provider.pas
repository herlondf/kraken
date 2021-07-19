unit Kraken.Provider;

interface

uses
  Kraken.Provider.Zeos,
  Kraken.Provider.Firedac,
  Kraken.Provider.RequestHTTP;

type
{$IF DEFINED(KRAKEN_FIREDAC)}
  TKrakenProvider = Kraken.Provider.Firedac.TKrakenProviderFireDAC;
{$ELSE}
  {$IF DEFINED(KRAKEN_REQUESTHTTP)}
    TKrakenProvider = Kraken.Provider.RequestHTTP.TKrakenProviderRequestHTTP;
  {$ELSE}
    TKrakenProvider = Kraken.Provider.Zeos.TKrakenProviderZeos;
  {$ENDIF}
{$ENDIF}

implementation

end.

