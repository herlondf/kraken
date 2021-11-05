unit Kraken.Provider;

interface

uses
  {$IF DEFINED(KRAKEN_FIREDAC)}
  Kraken.Provider.Firedac;
  {$ELSE}
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
    Kraken.Provider.RequestHTTP;
    {$ELSE}
    Kraken.Provider.Zeos;
  {$ENDIF}
{$ENDIF}


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

