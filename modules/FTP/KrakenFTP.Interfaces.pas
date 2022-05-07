unit KrakenFTP.Interfaces;

interface

uses
  System.Classes;

type
  TKrakenFTPLog = procedure( const Value: String ) of Object;

  iKrakenFTP = interface
    ['{340AA55F-36B9-4A9E-99C6-4A28E19EDD50}']
    { Attributes of connection }
    function Hostname      ( const Value: String  ): iKrakenFTP;
    function Username      ( const Value: String  ): iKrakenFTP;
    function Password      ( const Value: String  ): iKrakenFTP;
    function Port          ( const Value: Integer ): iKrakenFTP; overload;
    function Port          ( const Value: String  ): iKrakenFTP; overload;
    function ProxyHostname ( const Value: String  ): iKrakenFTP;
    function ProxyUsername ( const Value: String  ): iKrakenFTP;
    function ProxyPassword ( const Value: String  ): iKrakenFTP;
    function ProxyPort     ( const Value: Integer ): iKrakenFTP; overload;
    function ProxyPort     ( const Value: String  ): iKrakenFTP; overload;
    function Passive       ( const Value: Boolean ): iKrakenFTP;
    function UseTLS        ( const Value: Boolean ): iKrakenFTP;

    { Tranfers files }
    function Get( const ASource, aDest: String ): iKrakenFTP;
    function Put( const ASource, aDest: String ): iKrakenFTP;

    { Manipulating of directory and files }
    function RootDir   : iKrakenFTP;
    function CurrentDir: iKrakenFTP;
    function ListDir   : iKrakenFTP;

    function CreateDir( const ADirectory: String ): iKrakenFTP;
    function ChangeDir( const ADirectory: String ): iKrakenFTP;

    function ExistsFile( AFilename: String = '' ): Boolean;

    { Integration }
    function OnLog          ( const Value: TKrakenFTPLog ): iKrakenFTP;
    function OnProgress     ( const Value: TComponent   ): iKrakenFTP;
    function OnProgressLabel( const Value: TComponent   ): iKrakenFTP;

    { Thread Tratament }
    function Start: iKrakenFTP;

  end;

implementation

end.
