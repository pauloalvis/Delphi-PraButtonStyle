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

unit PraButtonStyleDelete;

interface

uses
  PraInterfaces,
  Vcl.Graphics;

type
  TPraButtonStyleDelete = class(TInterfacedObject, iPraButtonStyleTemplateType)
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
  System.SysUtils,
  PraConsts;

constructor TPraButtonStyleDelete.Create;
begin
  FPicture := TPicture.Create;
  FPictureFocused := TPicture.Create;
  FPictureStyleOutline := TPicture.Create;
  FPictureFocusedStyleOutline := TPicture.Create;
  FPictureDisabled := TPicture.Create;
end;

destructor TPraButtonStyleDelete.Destroy;
begin
  freeandNil(FPicture);
  freeandNil(FPictureFocused);
  freeandNil(FPictureStyleOutline);
  freeandNil(FPictureFocusedStyleOutline);
  freeandNil(FPictureDisabled);

  inherited;
end;

function TPraButtonStyleDelete.GetPicture: TPicture;
begin
  FPicture.Assign(GetPictureOfResource('PICTURE_DELETE_WHITE'));

  result := FPicture;
end;

function TPraButtonStyleDelete.GetPictureDisabled: TPicture;
begin
  FPictureDisabled.Assign(GetPictureOfResource('PICTURE_DELETE_WHITE'));

  result := FPictureDisabled;
end;

function TPraButtonStyleDelete.GetPictureFocused: TPicture;
begin
  FPictureFocused.Assign(GetPictureOfResource('PICTURE_DELETE_WHITE'));

  result := FPictureFocused;
end;

function TPraButtonStyleDelete.GetPictureFocusedStyleOutline: TPicture;
begin
  FPictureFocusedStyleOutline.Assign(GetPictureOfResource('PICTURE_DELETE_WHITE'));

  result := FPictureFocusedStyleOutline;
end;

function TPraButtonStyleDelete.GetPictureStyleOutline: TPicture;
begin
  FPictureStyleOutline.Assign(GetPictureOfResource('PICTURE_DELETE_RED'));

  result := FPictureStyleOutline;
end;

function TPraButtonStyleDelete.GetSizeHeight: Smallint;
begin
  result := BUTTON_STYLE_TEMPLATE_SIZE_HEIGHT;
end;

function TPraButtonStyleDelete.GetSizeWidth: Smallint;
begin
  result := BUTTON_STYLE_TEMPLATE_SIZE_WIDTH;
end;

class function TPraButtonStyleDelete.New: iPraButtonStyleTemplateType;
begin
  result := self.Create;
end;

end.
