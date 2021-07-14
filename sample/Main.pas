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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  TKraken
    .Provider('base')
    .ProviderType( TKrakenProviderType.ptPostgres )
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('postgres')
      .Password('vi7700')
      .Database('VIGGO_1805')
      .URLRemoto('http://localhost:9000/consulta');

  with TKraken.Provider('base').Query do
  begin
    SQL.Clear;
    SQL.Add(memConsulta.Lines.GetText);
    memSaida.Lines.Clear;

    try
      Open();
    except

    end;
    {$DEFINE KRAKEN_REQUESTHTTP}
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
      memSaida.Lines.Add(JSONTmp);
    {$ELSE}
      memSaida.Lines.Add('OK');
    {$ENDIF}
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TKraken
    .Provider('base')
    .ProviderType( TKrakenProviderType.ptPostgres )
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('postgres')
      .Password('vi7700')
      .Database('VIGGO_1805')
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
    {$DEFINE KRAKEN_REQUESTHTTP}
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
      memSaida.Lines.Add(JSONTmp);
    {$ELSE}
      memSaida.Lines.Add('OK');
    {$ENDIF}
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TKraken
    .Provider('base')
    .ProviderType( TKrakenProviderType.ptPostgres )
    .Settings
      .Host('127.0.0.1')
      .Port(5432)
      .Username('postgres')
      .Password('vi7700')
      .Database('VIGGO_1805')
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
    {$DEFINE KRAKEN_REQUESTHTTP}
    {$IF DEFINED(KRAKEN_REQUESTHTTP)}
      memSaida.Lines.Add(JSONTmp);
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

  memConsulta.Lines.Add('SELECT codigo, nome FROM participante');
end;

end.
