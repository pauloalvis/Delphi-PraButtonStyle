// *************************************************************************** }
//
// Delphi PraButtonStyle
//
// Copyright (c) 2020-2020 Paulo Roberto Alves
//
// https://github.com/pauloalvis/Delphi-PraButtonStyle
//
// ***************************************************************************
//
// MIT License
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// ***************************************************************************

unit PraButtonStyleMenu;

interface

uses
  PraInterfaces,
  Vcl.Graphics;

type
  TPraButtonStyleMenu = class(TInterfacedObject, iPraButtonStyleTemplateType)
  private
    FPicture: TPicture;
    FPictureFocused: TPicture;
    FPictureStyleOutline: TPicture;
    FPictureFocusedStyleOutline: TPicture;

    FPictureDisabled: TPicture;

    function GetPicture: TPicture;
    function GetPictureFocused: TPicture;
    function GetPictureStyleOutline: TPicture;
    function GetPictureFocusedStyleOutline: TPicture;
    function GetPictureDisabled: TPicture;
    function GetSizeWidth: Smallint;
    function GetSizeHeight: Smallint;
  public
    constructor Create;
    destructor Destroy; override;

    property Picture: TPicture read GetPicture;
    property PictureFocused: TPicture read GetPictureFocused;
    property PictureStyleOutline: TPicture read GetPictureStyleOutline;
    property PictureFocusedStyleOutline: TPicture read GetPictureFocusedStyleOutline;
    property PictureDisabled: TPicture read GetPictureDisabled;
    property SizeWidth: Smallint read GetSizeWidth;
    property SizeHeight: Smallint read GetSizeHeight;

    class function New: iPraButtonStyleTemplateType;
  end;

implementation

uses
  PraUtils,
  System.SysUtils;

constructor TPraButtonStyleMenu.Create;
begin
  FPicture := TPicture.Create;
  FPictureFocused := TPicture.Create;
  FPictureStyleOutline := TPicture.Create;
  FPictureFocusedStyleOutline := TPicture.Create;
  FPictureDisabled := TPicture.Create;
end;

destructor TPraButtonStyleMenu.Destroy;
begin
  freeandNil(FPicture);
  freeandNil(FPictureFocused);
  freeandNil(FPictureStyleOutline);
  freeandNil(FPictureFocusedStyleOutline);
  freeandNil(FPictureDisabled);

  inherited;
end;

function TPraButtonStyleMenu.GetPicture: TPicture;
begin
  FPicture.Assign(GetPictureOfResource('PICTURE_MENU_WHITE'));

  result := FPicture;
end;

function TPraButtonStyleMenu.GetPictureDisabled: TPicture;
begin
  FPictureDisabled.Assign(GetPictureOfResource('PICTURE_MENU_WHITE'));

  result := FPictureDisabled;
end;

function TPraButtonStyleMenu.GetPictureFocused: TPicture;
begin
  FPictureFocused.Assign(GetPictureOfResource('PICTURE_MENU_WHITE'));

  result := FPictureFocused;
end;

function TPraButtonStyleMenu.GetPictureFocusedStyleOutline: TPicture;
begin
  FPictureFocusedStyleOutline.Assign(GetPictureOfResource('PICTURE_MENU_WHITE'));

  result := FPictureFocusedStyleOutline;
end;

function TPraButtonStyleMenu.GetPictureStyleOutline: TPicture;
begin
  FPictureStyleOutline.Assign(GetPictureOfResource('PICTURE_MENU_BLUE'));

  result := FPictureStyleOutline;
end;

function TPraButtonStyleMenu.GetSizeHeight: Smallint;
begin
  result := 41;
end;

function TPraButtonStyleMenu.GetSizeWidth: Smallint;
begin
  result := 41;
end;

class function TPraButtonStyleMenu.New: iPraButtonStyleTemplateType;
begin
  result := self.Create;
end;

end.
