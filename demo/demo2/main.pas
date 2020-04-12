unit main;

interface

uses
  System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.WinXCtrls,
  PraButtonStyle, Vcl.StdCtrls, Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    svMenu: TSplitView;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Shape2: TShape;
    Shape3: TShape;
    PraButtonStyle1: TPraButtonStyle;
    PraButtonStyle3: TPraButtonStyle;
    PraButtonStyle4: TPraButtonStyle;
    Panel4: TPanel;
    PraButtonStyle2: TPraButtonStyle;
    PraButtonStyle5: TPraButtonStyle;
    PraButtonStyle6: TPraButtonStyle;
    PraButtonStyle7: TPraButtonStyle;
    Image1: TImage;
    Label1: TLabel;
    PraButtonStyle8: TPraButtonStyle;
    procedure PraButtonStyle2Click(Sender: TObject);
    procedure PraButtonStyle8Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.PraButtonStyle2Click(Sender: TObject);
  procedure UpdateShowCaption(const AShowCaption: Boolean);
  var
    i: integer;
  begin
    for i := 0 to svMenu.ControlCount - 1 do
    begin
      if svMenu.Controls[i].ClassType = TPraButtonStyle then
        TPraButtonStyle(svMenu.Controls[i]).ShowCaption := AShowCaption;
    end;
  end;

begin

  if svMenu.Opened then
  begin
    svMenu.close;
    Panel4.Width := 60;
    Label1.Visible := false;
    UpdateShowCaption(false);
  end
  else
  begin
    svMenu.open;
    Panel4.Width := 204;
    Label1.Visible := true;
    UpdateShowCaption(true);
  end;
end;

procedure TForm1.PraButtonStyle8Click(Sender: TObject);
begin
  close;
end;

end.
