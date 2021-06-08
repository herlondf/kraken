unit Kraken.Provider;

interface

uses
  Kraken.Provider.Zeos,
  Kraken.Provider.Firedac;

type

{$IF DEFINED(KRAKEN_FIREDAC)}
  TKrakenProvider = Kraken.Provider.Firedac.TKrakenProviderFireDAC;
{$ELSE}
  TKrakenProvider = Kraken.Provider.Zeos.TKrakenProviderZeos;
{$ENDIF}

implementation

end.

