unit main;

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
  PraButtonStyle,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    PraButtonStyle: TPraButtonStyle;
    CheckBox2: TCheckBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    procedure ComboBox1Select(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure ComboBox2Select(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

const
  bsPrimary = 0;
  bsSecondary = 1;
  bsSuccess = 2;
  bsDanger = 3;
  bsWarning = 4;
  bsInfo = 5;
  bsLight = 6;
  bsDark = 7;

{$R *.dfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  PraButtonStyle.StyleOutline := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  PraButtonStyle.Enabled := CheckBox2.Checked;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  case ComboBox1.ItemIndex - 1 of
    bsPrimary:
      begin
        PraButtonStyle.Style := TPraButtonStyleStyle.bsPrimary;
        PraButtonStyle.Caption := 'Primary';
      end;
    bsSecondary:
      begin
        PraButtonStyle.Style := TPraButtonStyleStyle.bsSecondary;
        PraButtonStyle.Caption := 'Secondary';
      end;
    bsSuccess:
      begin
        PraButtonStyle.Style := TPraButtonStyleStyle.bsSuccess;
        PraButtonStyle.Caption := 'Success';
      end;
    bsDanger:
      begin
        PraButtonStyle.Style := TPraButtonStyleStyle.bsDanger;
        PraButtonStyle.Caption := 'Danger';
      end;
    bsWarning:
      begin
        PraButtonStyle.Style := TPraButtonStyleStyle.bsWarning;
        PraButtonStyle.Caption := 'Warning';
      end;
    bsInfo:
      begin
        PraButtonStyle.Style := TPraButtonStyleStyle.bsInfo;
        PraButtonStyle.Caption := 'Info';
      end;
    bsLight:
      begin
        PraButtonStyle.Style := TPraButtonStyleStyle.bsLight;
        PraButtonStyle.Caption := 'Light';
      end;
    bsDark:
      begin
        PraButtonStyle.Style := TPraButtonStyleStyle.bsDark;
        PraButtonStyle.Caption := 'Dark';
      end;
  end;
end;

procedure TForm1.ComboBox2Select(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0:
      PraButtonStyle.Shape := TPraButtonStyleType.stRoundRect;
    1:
      PraButtonStyle.Shape := TPraButtonStyleType.stRectangle;
  end;
end;

end.
