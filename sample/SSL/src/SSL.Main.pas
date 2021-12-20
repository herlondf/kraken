unit SSL.Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Kraken.Core;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TKrakenCore
    .Invoker
      .TryGetProvider
        .ProviderType
          .Postgres
        .Settings
          .Host       ('127.0.0.1')
          .Port       (5432)
          .Database   ('casasertanejo')
          .Username   ('dev')
          .Password   ('00viggodev@@')
          .SSLMode    ('verify-ca')
          .SSLCert    ('C:\Projetos-Brave\Kraken\sample\SSL\bin\Win32\Debug\SSL\_client.crt')
          .SSLKey     ('C:\Projetos-Brave\Kraken\sample\SSL\bin\Win32\Debug\SSL\_client.key')
          .SSLRootcert('C:\Projetos-Brave\Kraken\sample\SSL\bin\Win32\Debug\SSL\_root.crt')
          .&End
        .Connect;
end;

end.
