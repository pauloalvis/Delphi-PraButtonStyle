{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NewPackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  pkPraButtonStyle, PraButtonStyle, PraButtonStyleBack, PraButtonStyleCancel, 
  PraButtonStyleClose, PraButtonStyleDelete, PraButtonStyleEdit, 
  PraButtonStyleEmail, PraButtonStyleGear, PraButtonStyleHeart, 
  PraButtonStyleInsert, PraButtonStyleMenu, PraButtonStylePrint, 
  PraButtonStyleReport, PraButtonStyleSave, PraButtonStyleTemplateDanger, 
  PraButtonStyleTemplateDark, PraButtonStyleTemplateGear, 
  PraButtonStyleTemplateHeart, PraButtonStyleTemplateInfo, 
  PraButtonStyleTemplateLight, PraButtonStyleTemplatePrimary, 
  PraButtonStyleTemplateSecondary, PraButtonStyleTemplateSuccess, 
  PraButtonStyleTemplateWarning, PraInterfaces, PraConsts, PraRegister, 
  PraUtils, PraAbout, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('pkPraButtonStyle', @pkPraButtonStyle.Register);
end;

initialization
  RegisterPackage('NewPackage', @Register);
end.
