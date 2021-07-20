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
    .Settings
      .URLRemoto( 'http://localhost:9000/consulta');

  with TKraken.Provider('base').Query do
  begin
    SQL.Clear;
    SQL.Add('SELECT * FROM times WHERE valor > :valor');
    ParamByName('valor').AsFloat := 25.001;

    Locate('teste', 'teste', [TLocateOption.loCaseInsensitive]);

    memSaida.Lines.Add( ParamByName('valor').AsString );

    memSaida.Lines.Clear;

    try
      try
        StartTransaction;
        Open();

      finally
        while not Eof do
        begin
          memSaida.Lines.Add(
            FieldByName( 'id'    ).AsString + ' / ' +
            FieldByName( 'nome'  ).AsString + ' / ' +
            FloatToStr( FieldByName( 'valor' ).AsFloat )
          );

          Next;


        end;
      end;
    except
      on E: exception do
        memSaida.Lines.Add(E.Message);
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
    SQL.Add('INSERT INTO times(nome, valor, mundial) VALUES(:nome, :valor, :mundial)');

    ParamByName( 'nome'   ).AsString  := 'PALMEIRAS';
    ParamByName( 'valor'  ).AsCurrency   := 25.123456789;
    ParamByName( 'mundial').AsBoolean := False;

    try
      TKraken.Provider('base').StartTransaction;
      ExecSQL();

      TKraken.Provider('base').Commit;
    except
      TKraken.Provider('base').Rollback;
    end;
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
      memSaida.Lines.Add( '' );
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
    SQL.Add('INSERT INTO times(nome, valor, mundial) VALUES(:nome, :valor, :mundial) RETURNING *');

    ParamByName( 'nome'   ).AsString   := 'PALMEIRAS';
    ParamByName( 'valor'  ).AsCurrency := 25.123456789;
    ParamByName( 'mundial').AsBoolean  := False;

    try
      TKraken.Provider('base').StartTransaction;

      open();

      TKraken.Provider('base').Commit;
    except
      TKraken.Provider('base').Rollback;
    end;
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
    while not Eof do
    begin
      memSaida.Lines.Add(
        FieldByName( 'id'    ).AsString + ' / ' +
        FieldByName( 'nome'  ).AsString + ' / ' +
        FloatToStr( FieldByName( 'valor' ).AsFloat )
      );

      Next;
    end;
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
