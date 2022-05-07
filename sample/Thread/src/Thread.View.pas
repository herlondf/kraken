unit Thread.View;

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
  KrakenThread,
  KrakenThreadAnimation, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnAnimar: TButton;
    btnReverter: TButton;
    cmbModo: TComboBox;
    pnlAnimacao: TPanel;
    procedure btnAnimarClick(Sender: TObject);
    procedure btnReverterClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Animar(AniFunctionKind: TAniFunctionKind);
    procedure Reverter(AniFunctionKind: TAniFunctionKind);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.btnAnimarClick(Sender: TObject);
begin
  case cmbModo.ItemIndex of
    0: Animar( afkLinear    );
    1: Animar( afkQuadratic );
    2: Animar( afkCubic     );
    3: Animar( afkQuartic   );
    4: Animar( afkQuintic   );
    5: Animar( afkBack      );
  end;
end;

procedure TForm1.btnReverterClick(Sender: TObject);
begin
  case cmbModo.ItemIndex of
    0: Reverter( afkLinear    );
    1: Reverter( afkQuadratic );
    2: Reverter( afkCubic     );
    3: Reverter( afkQuartic   );
    4: Reverter( afkQuintic   );
    5: Reverter( afkBack      );
  end;
end;

procedure TForm1.Animar(AniFunctionKind: TAniFunctionKind);
begin
  TKrakenThreadAnimation.NewJob
    .Animation(
      akIn,
      AniFunctionKind,
      pnlAnimacao.Left,
      pnlAnimacao.Left + 200,
      procedure (Value: Integer)
      begin
        pnlAnimacao.Left := Value;
      end)
      .Start;
end;

procedure TForm1.Reverter(AniFunctionKind: TAniFunctionKind);
begin
  TKrakenThreadAnimation.NewJob
    .Animation(
      akIn,
      AniFunctionKind,
      pnlAnimacao.Left,
      pnlAnimacao.Left - 200,
      procedure (Value: Integer)
      begin
        pnlAnimacao.Left := Value;
      end)
      .Start;
end;

end.
