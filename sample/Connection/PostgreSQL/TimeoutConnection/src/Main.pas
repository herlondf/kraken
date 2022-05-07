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
  Vcl.StdCtrls,
  Kraken.Core;

type
  TfrmSampleTimeOut = class(TForm)
    edtHost: TEdit;
    edtPort: TEdit;
    edtDatabase: TEdit;
    edtUser: TEdit;
    edtPassword: TEdit;
    edtTimeout: TEdit;
    lblHost: TLabel;
    lblPort: TLabel;
    lblDatabase: TLabel;
    lblUser: TLabel;
    lblPassword: TLabel;
    lblTimeout: TLabel;
    btnConnect: TButton;
    mmoQuery: TMemo;
    btnOpen: TButton;
    btnExecSQL: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnExecSQLClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSampleTimeOut: TfrmSampleTimeOut;

implementation

{$R *.dfm}

procedure TfrmSampleTimeOut.btnConnectClick(Sender: TObject);
begin
  TKraken
    .Provider(0)
      .ProviderType(TKrakenProviderType.ptPostgres)
      .Settings
        .Host( edtHost.Text )
        .Database( edtDatabase.Text )
        .Port( edtPort.Text )
        .Username( edtUser.Text )
        .Password( edtPassword.Text )
        .TimeOut( edtTimeout.Text );

  Tkraken
    .Provider(0)
    .Connect;
end;

procedure TfrmSampleTimeOut.btnExecSQLClick(Sender: TObject);
begin
  TKraken
    .Provider(0)
    .Query
      .SQL.Clear;

  TKraken
    .Provider(0)
    .Query
      .SQL.Add( mmoQuery.Text );

  TKraken
    .Provider(0)
    .Query
      .ExecSQL(True);
end;

procedure TfrmSampleTimeOut.btnOpenClick(Sender: TObject);
begin
  TKraken
    .Provider(0)
    .Query
      .SQL.Clear;

  TKraken
    .Provider(0)
    .Query
      .SQL.Add( mmoQuery.Text );

  TKraken
    .Provider(0)
    .Query
      .Open(True);
end;

end.
