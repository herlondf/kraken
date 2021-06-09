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
var
  LNome: String;
begin
  TKraken
    .AddProvider
    .ProviderType( TKrakenProviderType.ptPostgres )
    .Identification('Sample')
    .Settings

  TKraken
    .ProviderByIdent('Sample')
    .Connect;

  with TKraken.ProviderByIdent('Sample').Query() do
  begin
    try
      TKraken.ProviderByIdent('Sample').StartTransaction;
      try
        SQL.Add('SELECT * FROM nota_fiscal');
        Open;


        while not Eof do
        begin
          //LNome := FieldByName('').AsString;
          Next;
        end;
      finally
        TKraken.ProviderByIdent('Sample').Commit;
      end;
    except
      TKraken.ProviderByIdent('Sample').Rollback;
    end;
  end;
end;

end.
