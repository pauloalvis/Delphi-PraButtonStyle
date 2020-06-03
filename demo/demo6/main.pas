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
  System.ImageList,
  Vcl.ImgList,
  Vcl.StdCtrls;

type
  TForm13 = class(TForm)
    ImageList1: TImageList;
    Button1: TButton;
    PraButtonStyle1: TPraButtonStyle;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form13: TForm13;

implementation

{$R *.dfm}

procedure TForm13.Button1Click(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ImageList1.Count do
  begin
    PraButtonStyle1.ImageIndexPicture := i;

    Application.ProcessMessages;
    Sleep(100);
  end;
end;

end.
