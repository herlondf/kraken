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
    .AddProvider( TKrakenProviderType.ptPostgres )
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('your_username')
      .Password('your_password')
      .Database('your_database');

  TKraken
    .Provider()
    .Connect;

  TKraken
    .Provider()
    .Query()
      .SQL.Add('SELECT * FROM yout_table')
      .Open;
end;

end.
