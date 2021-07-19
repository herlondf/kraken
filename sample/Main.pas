unit Main;

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
  Kraken.Core,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    memConsulta: TMemo;
    memSaida: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    btnOpen: TButton;
    btnExecSQL: TButton;
    btnOpenStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnExecSQLClick(Sender: TObject);
    procedure btnOpenStartClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  TKraken
    .Provider('base')
    .ProviderType( TKrakenProviderType.ptPostgres )
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('postgres')
      .Password('vi7700')
      .Database('viggo')
      .URLRemoto('http://localhost:9000/consulta');

  with TKraken.Provider('base').Query do
  begin
    SQL.Clear;
    SQL.Add('SELECT * FROM participante WHERE codigo > :codigo');
    ParamByName('codigo').AsInteger := 1;

    memSaida.Lines.Clear;

    try
      try
        Open();
      finally
        while not Eof do
        begin
          memSaida.Lines.Add( FieldByName('nome').AsString );

          Next;
        end;
      end;
    except

    end;
  end;
end;

procedure TForm1.btnExecSQLClick(Sender: TObject);
begin
  TKraken
    .Provider('base')
    .ProviderType( TKrakenProviderType.ptPostgres )
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('postgres')
      .Password('vi7700')
      .Database('viggo')
      .URLRemoto('http://localhost:9000/consulta');

  with TKraken.Provider('base').Query do
  begin
    SQL.Clear;
    SQL.Add(memConsulta.Lines.GetText);
    memSaida.Lines.Clear;

    try
      TKraken.Provider('base').StartTransaction;
      ExecSQL();

      TKraken.Provider('base').Commit;
    except
      TKraken.Provider('base').Rollback;
    end;
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
      memSaida.Lines.Add( Response );
    {$ELSE}
      memSaida.Lines.Add('OK');
    {$ENDIF}
  end;
end;

procedure TForm1.btnOpenStartClick(Sender: TObject);
begin
  TKraken
    .Provider('base')
    .ProviderType( TKrakenProviderType.ptPostgres )
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('postgres')
      .Password('vi7700')
      .Database('viggo')
      .URLRemoto('http://localhost:9000/consulta');

  with TKraken.Provider('base').Query do
  begin
    SQL.Clear;
    SQL.Add(memConsulta.Lines.GetText);
    memSaida.Lines.Clear;

    try
      TKraken.Provider('base').StartTransaction;

      open();

      TKraken.Provider('base').Commit;
    except
      TKraken.Provider('base').Rollback;
    end;
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
      memSaida.Lines.Add(Response);
    {$ELSE}
      memSaida.Lines.Add('OK');
    {$ENDIF}
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 { TKraken
    .Provider('base')
    .ProviderType( TKrakenProviderType.ptPostgres );
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('postgres')
      .Password('vi7700')
      .Database('VIGGO_1805');

  TKraken.Provider('base')
    .Connect;

  TKraken.Provider('base').Disconnect;

  with TKraken.Provider('base').Query do
  begin
    SQL.Add('SELECT codigo, nomeg FROM participante');
    Open();
  end; }
end;

end.
