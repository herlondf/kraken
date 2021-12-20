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
    edtParam: TEdit;
    lblParam: TLabel;
    edtParamValue: TEdit;
    lblParamValue: TLabel;
    lstParams: TListBox;
    btnParamAdd: TButton;
    btnParamRemove: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnExecSQLClick(Sender: TObject);
    procedure btnParamAddClick(Sender: TObject);
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
//  TKraken
//    .Provider(0)
//      .ProviderType(TKrakenProviderType.ptPostgres)
//      .Settings
//        .Host( edtHost.Text )
//        .Database( edtDatabase.Text )
//        .Port( edtPort.Text )
//        .Username( edtUser.Text )
//        .Password( edtPassword.Text )
//        .TimeOut( edtTimeout.Text );

  TKraken
    .Provider(0)
      .ProviderType(TKrakenProviderType.ptPostgres)
      .Settings
        .Host('127.0.0.1')
        .Database('_viggo-plano')
        .Port(5432)
        .Username('postgres')
        .Password('vi7700')
        .TimeOut('1000');


  Tkraken
    .Provider(0)
    .Connect;
end;

procedure TfrmSampleTimeOut.btnExecSQLClick(Sender: TObject);
var
  I: Integer;
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
      .StartTransaction;

  for I := 0 to Pred( lstParams.Count ) do
  begin
    TKraken.Provider(0).Query.ParamByName( lstParams.Items.Names[0] ).AsString := lstParams.Items.ValueFromIndex[I];

    TKraken
      .Provider(0)
      .Query
        .ExecSQL(True);
  end;

  TKraken
    .Provider(0)
      .Rollback;
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

procedure TfrmSampleTimeOut.btnParamAddClick(Sender: TObject);
begin
  lstParams.Items.AddPair( edtParam.Text, edtParamValue.Text );
end;

end.
