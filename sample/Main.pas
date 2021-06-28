unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Kraken.Core;

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
  TKraken
    .Provider('base')
    .ProviderType( TKrakenProviderType.ptPostgres )
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('postgres')
      .Password('vi7700')
      .Database('massas_jucurutu_nfce');

  TKraken.Provider('base')
    .Connect;

  TKraken.Provider('base').Disconnect;

  with TKraken.Provider('base').Query do
  begin
    SQL.Add('INSERT INTO teste(teste1, teste2) VALUES(''a'', ''u'') RETURNING id');
    Open();
  end;
end;

end.
