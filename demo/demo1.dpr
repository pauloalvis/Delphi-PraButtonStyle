program demo1;

uses
  Vcl.Forms,
  main in 'main.pas' {uMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TuMain, uMain);
  Application.Run;
end.
