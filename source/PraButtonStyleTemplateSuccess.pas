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

unit PraButtonStyleTemplateSuccess;

interface

uses
  {$IF DEFINED(FPC)}
  PraInterfaces,
  Graphics
  {$ELSE} // DEFINE DELPHI
  PraInterfaces,
  Vcl.Graphics
  {$ENDIF} ;
type
  TPraFontConfiguration = class(TInterfacedObject, iPraFontConfigurationCommon)
  private
    FFont: TFont;
    FFontDisabled: TFont;
    FFontFocused: TFont;
    FFontDown: TFont;

    function GetFont: TFont;
    function GetFontDown: TFont;
    function GetFontFocused: TFont;
    function GetFontDisabled: TFont;
  public
    constructor Create;
    destructor Destroy; override;

    property Font: TFont read GetFont;
    property FontDown: TFont read GetFontDown;
    property FontFocused: TFont read GetFontFocused;
    property FontDisabled: TFont read GetFontDisabled;

    class function New: iPraFontConfigurationCommon;
  end;

  TPraPenConfiguration = class(TInterfacedObject, iPraPenConfigurationCommon)
    function GetPenStyle: TPenStyle;
    function GetPenFocusedStyle: TPenStyle;
    function GetPenDisabledStyle: TPenStyle;
    function GetPenDownStyle: TPenStyle;
    function GetPenDownWidth: Smallint;

  public
    class function New: iPraPenConfigurationCommon;
  end;

  TPraButtonStyleTemplateSuccess = class(TInterfacedObject, iPraButtonStyleTemplate)
  private
    FPenConfigurationCommon: iPraPenConfigurationCommon;
    FFontConfigurationCommon: iPraFontConfigurationCommon;

    function GetBrushColor: TColor;
    function GetBrushFocusedColor: TColor;
    function GetBrushDownColor: TColor;

    function GetPenDownColor: TColor;

    function GetPenConfigurationCommon: iPraPenConfigurationCommon;
    function GetFontConfigurationCommon: iPraFontConfigurationCommon;

    function GetBrushDisabledColor: TColor;

    function GetSizeWidth: Smallint;
    function GetSizeHeight: Smallint;

    constructor Create;
  public
    property BrushColor: TColor read GetBrushColor;
    property BrushFocusedColor: TColor read GetBrushFocusedColor;
    property BrushDownColor: TColor read GetBrushDownColor;
    property PenDownColor: TColor read GetPenDownColor;
    property PenConfigurationCommon: iPraPenConfigurationCommon read GetPenConfigurationCommon;
    property FontConfigurationCommon: iPraFontConfigurationCommon read GetFontConfigurationCommon;
    property BrushDisabledColor: TColor read GetBrushDisabledColor;
    property SizeWidth: Smallint read GetSizeWidth;
    property SizeHeight: Smallint read GetSizeHeight;

    class function New: iPraButtonStyleTemplate;
  end;

implementation

uses
  {$IF DEFINED(FPC)}
    PraUtils,
    SysUtils,
    PraConsts
  {$ELSE} // DEFINE DELPHI
    PraUtils,
    System.SysUtils,
    PraConsts
  {$ENDIF} ;

constructor TPraFontConfiguration.Create;
begin
  FFont := TFont.Create;
  Font.Color := clWhite;
  Font.Name := 'Tahoma';
  Font.Style := [fsBold];
  Font.Size := 10;

  FFontDisabled := TFont.Create;
  FontDisabled.Color := $00FFF0E2;
  FontDisabled.Name := 'Tahoma';
  FontDisabled.Style := [fsBold];
  FontDisabled.Size := 10;

  FFontFocused := TFont.Create;
  FontFocused.Color := clWhite;
  FontFocused.Name := 'Tahoma';
  FontFocused.Style := [fsBold];
  FontFocused.Size := 10;

  FFontDown := TFont.Create;
  FontDown.Color := clWhite;
  FontDown.Name := 'Tahoma';
  FontDown.Style := [fsBold];
  FontDown.Size := 10;
end;

destructor TPraFontConfiguration.Destroy;
begin
  freeandNil(FFont);
  freeandNil(FFontDisabled);
  freeandNil(FFontFocused);
  freeandNil(FFontDown);

  inherited Destroy;
end;

function TPraFontConfiguration.GetFont: TFont;
begin
  result := FFont;
end;

function TPraFontConfiguration.GetFontDisabled: TFont;
begin
  result := FFontDisabled;
end;

function TPraFontConfiguration.GetFontDown: TFont;
begin
  result := FFontDown;
end;

function TPraFontConfiguration.GetFontFocused: TFont;
begin
  result := FFontFocused;
end;

class function TPraFontConfiguration.New: iPraFontConfigurationCommon;
begin
  result := self.Create;
end;

function TPraPenConfiguration.GetPenDisabledStyle: TPenStyle;
begin
  result := psClear;
end;

function TPraPenConfiguration.GetPenDownStyle: TPenStyle;
begin
  result := psSolid;
end;

function TPraPenConfiguration.GetPenDownWidth: Smallint;
begin
  result := 3;
end;

function TPraPenConfiguration.GetPenFocusedStyle: TPenStyle;
begin
  result := psClear;
end;

function TPraPenConfiguration.GetPenStyle: TPenStyle;
begin
  result := psClear;
end;

class function TPraPenConfiguration.New: iPraPenConfigurationCommon;
begin
  result := self.Create;
end;

constructor TPraButtonStyleTemplateSuccess.Create;
begin
  FPenConfigurationCommon := TPraPenConfiguration.New;
  FFontConfigurationCommon := TPraFontConfiguration.New;
end;

function TPraButtonStyleTemplateSuccess.GetBrushColor: TColor;
begin
  result := $0045AA29;
end;

function TPraButtonStyleTemplateSuccess.GetBrushDisabledColor: TColor;
begin
  result := $00FFF0E2;
end;

function TPraButtonStyleTemplateSuccess.GetBrushDownColor: TColor;
begin
  result := $00388A22;
end;

function TPraButtonStyleTemplateSuccess.GetBrushFocusedColor: TColor;
begin
  result := $00388A22;
end;

function TPraButtonStyleTemplateSuccess.GetFontConfigurationCommon: iPraFontConfigurationCommon;
begin
  result := FFontConfigurationCommon;
end;

function TPraButtonStyleTemplateSuccess.GetPenDownColor: TColor;
begin
  result := $00A2D494;
end;

function TPraButtonStyleTemplateSuccess.GetSizeHeight: Smallint;
begin
  result := BUTTON_STYLE_TEMPLATE_SIZE_HEIGHT;
end;

function TPraButtonStyleTemplateSuccess.GetSizeWidth: Smallint;
begin
  result := BUTTON_STYLE_TEMPLATE_SIZE_WIDTH;
end;

class function TPraButtonStyleTemplateSuccess.New: iPraButtonStyleTemplate;
begin
  result := self.Create;
end;

function TPraButtonStyleTemplateSuccess.GetPenConfigurationCommon: iPraPenConfigurationCommon;
begin
  result := FPenConfigurationCommon;
end;

end.
