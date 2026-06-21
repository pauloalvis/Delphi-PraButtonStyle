unit pkPraButtonStyle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TPraButtonStyle = class(TButton)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I pkprabuttonstyle_icon.lrs}
  RegisterComponents('Pra Components',[TPraButtonStyle]);
end;

end.
