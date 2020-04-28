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

unit PraInterfaces;

interface

uses
  Vcl.Graphics;

type
  iPraPenConfigurationCommon = interface
    ['{445BF2CB-24A2-47FE-98CD-EB06D9844CEC}']

    function GetPenStyle: TPenStyle;
    function GetPenFocusedStyle: TPenStyle;
    function GetPenDisabledStyle: TPenStyle;
    function GetPenDownStyle: TPenStyle;
    function GetPenDownWidth: Smallint;
  end;

  iPraFontConfigurationCommon = interface
    ['{BC383558-1635-48C5-80FF-F12C16D3A22B}']

    function GetFont: TFont;
    function GetFontDown: TFont;
    function GetFontFocused: TFont;
    function GetFontDisabled: TFont;
  end;

  iPraButtonStyleTemplate = interface
    ['{031E5FF4-2134-4E99-B3E1-7108533DC890}']

    function GetBrushColor: TColor;
    function GetBrushFocusedColor: TColor;
    function GetBrushDownColor: TColor;
    function GetPenDownColor: TColor;
    function GetPenConfigurationCommon: iPraPenConfigurationCommon;
    function GetFontConfigurationCommon: iPraFontConfigurationCommon;
    function GetBrushDisabledColor: TColor;
    function GetSizeWidth: Smallint;
    function GetSizeHeight: Smallint;
  end;

  iPraButtonStyleTemplateType = interface
    ['{97B300BD-3362-49C1-AE07-CC3A37C4987A}']
    function GetPicture: TPicture;
    function GetPictureFocused: TPicture;
    function GetPictureStyleOutline: TPicture;
    function GetPictureFocusedStyleOutline: TPicture;
    function GetPictureDisabled: TPicture;
    function GetSizeWidth: Smallint;
    function GetSizeHeight: Smallint;
  end;

implementation

end.
