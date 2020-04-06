{******************************************************************************}
{                                                                              }
{                           pra components library 2020                        }
{                   pauloalvis@hotmail.com | github.com/pauloalvis             }
{                                                                              }
{******************************************************************************}
unit PraRegister;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

procedure Register;

implementation

uses
  PraButtonStyle;

procedure Register;
begin
  RegisterComponents('Pra Components', [TPraButtonStyle]);
end;

end.
