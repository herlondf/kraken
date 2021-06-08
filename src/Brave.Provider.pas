unit Brave.Provider;

interface

uses
  Brave.Provider.Zeos,
  Brave.Provider.Firedac;

type

{$IF DEFINED(BRAVE_FIREDAC)}
  TBraveProvider = Brave.Provider.Firedac.TBraveProviderFireDAC;
{$ELSE}
  TBraveProvider = Brave.Provider.Zeos.TBraveProviderZeos;
{$ENDIF}

implementation

end.

