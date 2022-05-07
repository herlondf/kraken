unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Threading,
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

procedure TfrmSampleTimeOut.btnExecSQLClick(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
  procedure
  var
    I: Integer;
  begin
    for I := 0 to 50 do
    begin
      TKrakenCore.Invoker
        .TryGetProvider(I+1)
          .Settings
            .Host('127.0.0.1')
            .Port(5432)
            .Database('casasertanejo')
            .Username('dev')
            .Password('00viggodev@@')
            .&End
          .ProviderType
            .Postgres
          .Connect;

      TKrakenCore.Invoker
        .TryGetProvider(I+1)
        .Migrate
          .PG;

      TKrakenCore.Invoker
        .TryGetProvider(I+1)
          .Query.SQL.Add('UPDATE produto SET quantidade = quantidade + 1');

      TKrakenCore.Invoker
        .TryGetProvider(I+1)
        .StartTransaction;

      TKrakenCore.Invoker
        .TryGetProvider(I+1)
          .Query
          .OnProcessStart(
          procedure(const Value: String)
          begin
            mmoQuery.Lines.Add(
              Format('Conexao %d', [I]) + sLineBreak +
              Format('MSG: %s', [Value])
            );
          end
          )
          .OnProcessFinish(
          procedure(const Value: String)
          begin
            mmoQuery.Lines.Add(
              Format('Conexao %d', [I]) + sLineBreak +
              Format('MSG: %s', [Value])
            );
          end
          )
          .OnProcessError(
          procedure(const Value: String)
          begin
            mmoQuery.Lines.Add(
              Format('Conexao %d', [I]) + sLineBreak +
              Format('MSG: %s', [Value])
            );
          end
          )
          .ExecSQL;

      TKrakenCore.Invoker
        .TryGetProvider(I+1)
        .Commit;

      TKrakenCore.Invoker
        .TryGetProvider(I+1)
        .Disconnect;
    end;
  end).Start;

end;

end.
