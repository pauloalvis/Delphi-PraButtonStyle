unit PraAbout;

interface

uses
  Winapi.Windows,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage;

type
  TFPraViewAbout = class(TForm)
    Bevel1: TBevel;
    lbVersion: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Shape1: TShape;
    Label1: TLabel;
    Image1: TImage;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Button1: TButton;
    procedure Label5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  FPraViewAbout: TFPraViewAbout;

implementation

uses
  Winapi.ShellAPI,
  PraConsts;

{$R *.dfm}

procedure TFPraViewAbout.FormCreate(Sender: TObject);
begin
  lbVersion.Caption := 'Version ' + version;
end;

procedure TFPraViewAbout.Label5Click(Sender: TObject);
begin
   ShellExecute(0, 'open', PChar('https://github.com/pauloalvis/Delphi-PraButtonStyle'), nil, nil, SW_SHOWNORMAL);
end;

end.
