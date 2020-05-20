program demo5;

uses
  Vcl.Forms,
  main in 'main.pas' {Form53};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm53, Form53);
  Application.Run;
end.
