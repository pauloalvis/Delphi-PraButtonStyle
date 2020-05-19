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

unit PraButtonStyle;

interface

uses
  Vcl.Graphics,
  Vcl.buttons,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  Vcl.Controls,
  Winapi.Messages,
  Winapi.Windows,
  System.Math,
  PraInterfaces;

type
  TPraAlignment = (paLeftJustify, paCenter);
  TPraButtonStyleType = (stRoundRect, stRectangle);
  TPraButtonStyleStyle = (bsCustom, bsPrimary, bsSecondary, bsSuccess, bsDanger, bsWarning, bsInfo, bsLight, bsDark);
  TTemplateStyle = (tsNone = 0, tsSave = 1, tsCancel = 2, tsEdit = 3, tsDelete = 4, tsPrint = 5, tsGear = 6, tsMenu = 7, tsHeart = 8, tsEmail = 9, tsInsert = 10, tsBack = 11, tsClose = 12,
    tsReport = 13);

  TPraButtonStyle = class;

  TFocusControl = class(TWinControl)
  private
    FGraphicControl: TPraButtonStyle;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyDown); message WM_KEYUP;
  public
    constructor Create(AOwner: TComponent; AGraphicControl: TPraButtonStyle); reintroduce;
    property TabStop default false;
    property TabOrder;
  end;

  TPraButtonChangeFont = procedure(const Value: Boolean) of Object;

  TPraButtonFont = class(TFont)
  private
    FCopyOfFont: Boolean;
    FonChange: TPraButtonChangeFont;

    function IsCopyOfFont: Boolean;
    procedure SetCopyOfFont(const Value: Boolean);
  published
    property CopyOfFont: Boolean read FCopyOfFont write SetCopyOfFont stored IsCopyOfFont default false;
  public
    property onChange: TPraButtonChangeFont read FonChange write FonChange;
  end;

  TPraButtonStyle = class(TGraphicControl)
  private
    FPraButtonStyleTemplate: iPraButtonStyleTemplate;
    FPraButtonStyleTemplateType: iPraButtonStyleTemplateType;

    FMouseEnter: Boolean;
    FState: TButtonState;
    FFocusControl: TFocusControl;
    FControllerStyleTemplate: Boolean;

    FPen: TPen;
    FPenDown: TPen;
    FPenFocused: TPen;
    FPenDisabled: TPen;

    FBrush: TBrush;
    FBrushDown: TBrush;
    FBrushFocused: TBrush;
    FBrushDisabled: TBrush;

    FPicture: TPicture;
    FPictureFocused: TPicture;
    FPictureDisabled: TPicture;
    FPictureMarginLeft: SmallInt;

    FFontDown: TPraButtonFont;
    FFontFocused: TPraButtonFont;
    FFontDisabled: TPraButtonFont;

    FCaption: TCaption;
    FShowCaption: Boolean;

    FRadius: SmallInt;
    FSpacing: SmallInt;

    FClickOnEnter: Boolean;
    FAlignment: TPraAlignment;

    FShape: TPraButtonStyleType;
    FPictureCenter: Boolean;
    FStyle: TPraButtonStyleStyle;
    FStyleOutline: Boolean;
    FAboutInfo: String;
    FTemplateStyle: TTemplateStyle;

    procedure SetPen(Value: TPen);
    procedure SetPenDown(const Value: TPen);
    procedure SetPenFocused(const Value: TPen);
    procedure SetPenDisabled(const Value: TPen);

    procedure SetBrush(Value: TBrush);
    procedure SetBrushDown(const Value: TBrush);
    procedure SetBrushFocused(const Value: TBrush);
    procedure SetBrushDisabled(const Value: TBrush);

    procedure SetPicture(const Value: TPicture);
    procedure SetPictureFocused(const Value: TPicture);
    procedure SetPictureDisabled(const Value: TPicture);
    procedure SetPictureMarginLeft(const Value: SmallInt);

    procedure SetFontDown(const Value: TPraButtonFont);
    procedure SetFontFocused(const Value: TPraButtonFont);
    procedure SetFontDisabled(const Value: TPraButtonFont);

    function IsRadius: Boolean;
    function IsSpacing: Boolean;
    function IsShowCaption: Boolean;
    function IsClickOnEnter: Boolean;
    function IsStoredAlignment: Boolean;
    function IsPictureMarginLeft: Boolean;

    function GetTabOrder: Integer;
    procedure SetTabStop(const Value: Boolean);

    function GetTabStop: Boolean;
    procedure SetTabOrder(const Value: Integer);

    function GetFocused: Boolean;
    function GetCanFocus: Boolean;

    procedure SetRadius(const Value: SmallInt);
    procedure SetCaption(const Value: TCaption);
    procedure SetSpacing(const Value: SmallInt);
    procedure SetShape(Value: TPraButtonStyleType);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetAlignment(const Value: TPraAlignment);

    procedure DestroyFocusControl;
    procedure SetClickOnEnter(const Value: Boolean);
    procedure CreateFocusControl(AOwner: TComponent; AParent: TWinControl);
    procedure WMEraseBkgnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function isPictureCenter: Boolean;
    procedure SetPictureCenter(const Value: Boolean);
    function informedPicture: Boolean;

    function GetPictureMarginLeft: SmallInt;
    function GetPictureWidth: SmallInt;
    function GetPictureHeight: SmallInt;
    function GetSpacing: SmallInt;
    function IsStoredStyle: Boolean;
    procedure SetStyle(const Value: TPraButtonStyleStyle);
    function IsStyleOutline: Boolean;
    procedure SetStyleOutline(const Value: Boolean);
    function IsStoredTemplateStyle: Boolean;
    procedure SetTemplateStyle(const Value: TTemplateStyle);

    procedure StyleChanged(Sender: TObject);
    procedure StyleOutlineConfig;

    procedure FontDownCopyOfFont(const AValue: Boolean);
    procedure FontFocusedCopyOfFont(const AValue: Boolean);
    procedure FontDisabledCopyOfFont(const AValue: Boolean);

    procedure LoadingTemplateInfo;
    procedure LoadingTemplateDark;
    procedure LoadingTemplateLight;
    procedure LoadingTemplateDanger;
    procedure LoadingTemplateSuccess;
    procedure LoadingTemplateWarning;
    procedure LoadingTemplatePrimary;
    procedure LoadingTemplateSecondary;
    procedure LoadingTemplateHeart;
    procedure ApplyTemplate;

    procedure CreateButtonInfo;
    procedure CreateButtonDark;
    procedure CreateButtonLight;
    procedure CreateButtonDanger;
    procedure CreateButtonSuccess;
    procedure CreateButtonWarning;
    procedure CreateButtonPrimary;
    procedure CreateButtonSecondary;

    procedure CreateButtonPrint;
    procedure CreateButtonSave;
    procedure CreateButtonEdit;
    procedure CreateButtonDelete;
    procedure CreateButtonCancel;
    procedure CreateButtonGear;
    procedure CreateButtonMenu;
    procedure CreateButtonHeart;
    procedure CreateButtonEmail;
    procedure CreateButtonInsert;
    procedure CreateButtonBack;
    procedure CreateButtonClose;
    procedure CreateButtonReport;

    procedure SetPictureTemplate;

    procedure CreateButtonDefault;
    procedure SetSize(const AHeight, AWidth: Integer);
    function isTemplateStyle: Boolean;
  protected
    procedure DoKeyUp;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
    procedure CMMouseEnter(var Message: TNotifyEvent); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TNotifyEvent); message CM_MOUSELEAVE;
  public
    property Focused: Boolean read GetFocused;
    property CanFocus: Boolean read GetCanFocus;

    procedure SetFocus;
    procedure Click; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property Anchors;
    property Cursor default crHandPoint;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property ParentShowHint;
    property ShowHint;
    property Touch;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnGesture;
    property OnStartDock;
    property OnStartDrag;
    property OnClick;
    property Font;
    property ParentFont default true;

    property Pen: TPen read FPen write SetPen;
    property PenDown: TPen read FPenDown write SetPenDown;
    property PenFocused: TPen read FPenFocused write SetPenFocused;
    property PenDisabled: TPen read FPenDisabled write SetPenDisabled;

    property Brush: TBrush read FBrush write SetBrush;
    property BrushDown: TBrush read FBrushDown write SetBrushDown;
    property BrushFocused: TBrush read FBrushFocused write SetBrushFocused;
    property BrushDisabled: TBrush read FBrushDisabled write SetBrushDisabled;

    property Picture: TPicture read FPicture write SetPicture;
    property PictureFocused: TPicture read FPictureFocused write SetPictureFocused;
    property PictureDisabled: TPicture read FPictureDisabled write SetPictureDisabled;

    property FontDown: TPraButtonFont read FFontDown Write SetFontDown;
    property FontFocused: TPraButtonFont read FFontFocused Write SetFontFocused;
    property FontDisabled: TPraButtonFont read FFontDisabled Write SetFontDisabled;

    property Caption: TCaption read FCaption write SetCaption;
    property TabOrder: Integer read GetTabOrder write SetTabOrder;
    property TabStop: Boolean read GetTabStop write SetTabStop default false;
    property Radius: SmallInt read FRadius write SetRadius stored IsRadius default 4;
    property Shape: TPraButtonStyleType read FShape write SetShape default stRoundRect;
    property Spacing: SmallInt read FSpacing write SetSpacing stored IsSpacing default 3;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption stored IsShowCaption default true;
    property ClickOnEnter: Boolean read FClickOnEnter write SetClickOnEnter stored IsClickOnEnter default true;
    property Alignment: TPraAlignment read FAlignment write SetAlignment stored IsStoredAlignment default paCenter;
    property PictureCenter: Boolean read FPictureCenter write SetPictureCenter stored isPictureCenter default false;
    property PictureMarginLeft: SmallInt read FPictureMarginLeft write SetPictureMarginLeft stored IsPictureMarginLeft default 3;

    property Style: TPraButtonStyleStyle read FStyle write SetStyle stored IsStoredStyle default bsCustom;
    property StyleOutline: Boolean read FStyleOutline write SetStyleOutline stored IsStyleOutline default false;

    property TemplateStyle: TTemplateStyle read FTemplateStyle write SetTemplateStyle stored IsStoredTemplateStyle default tsNone;

    property AboutInfo: String Read FAboutInfo Write FAboutInfo Stored false;
  end;

implementation

uses
  Vcl.Forms,
  PraButtonStyleSave,
  PraButtonStyleEdit,
  PraButtonStyleGear,
  PraButtonStylePrint,
  PraButtonStyleCancel,
  PraButtonStyleDelete,
  PraButtonStyleTemplateInfo,
  PraButtonStyleTemplateDark,
  PraButtonStyleTemplateLight,
  PraButtonStyleTemplateDanger,
  PraButtonStyleTemplatePrimary,
  PraButtonStyleTemplateSuccess,
  PraButtonStyleTemplateWarning,
  PraButtonStyleTemplateSecondary,
  PraButtonStyleMenu,
  PraButtonStyleHeart,
  PraButtonStyleTemplateHeart,
  PraButtonStyleEmail,
  PraButtonStyleInsert,
  PraButtonStyleBack,
  PraButtonStyleClose,
  PraButtonStyleReport;

procedure TPraButtonStyle.ApplyTemplate;
begin
  Brush.Color := FPraButtonStyleTemplate.GetBrushColor;
  BrushFocused.Color := FPraButtonStyleTemplate.GetBrushFocusedColor;
  BrushDown.Color := FPraButtonStyleTemplate.GetBrushDownColor;
  BrushDisabled.Color := FPraButtonStyleTemplate.GetBrushDisabledColor;

  PenDown.Color := FPraButtonStyleTemplate.GetPenDownColor;

  Pen.Style := FPraButtonStyleTemplate.GetPenConfigurationCommon.GetPenStyle;
  PenFocused.Style := FPraButtonStyleTemplate.GetPenConfigurationCommon.GetPenFocusedStyle;
  PenDisabled.Style := FPraButtonStyleTemplate.GetPenConfigurationCommon.GetPenDisabledStyle;
  PenDown.Style := FPraButtonStyleTemplate.GetPenConfigurationCommon.GetPenDownStyle;
  PenDown.Width := FPraButtonStyleTemplate.GetPenConfigurationCommon.GetPenDownWidth;

  Font.Assign(FPraButtonStyleTemplate.GetFontConfigurationCommon.GetFont);
  FontDown.Assign(FPraButtonStyleTemplate.GetFontConfigurationCommon.GetFontDown);
  FontDisabled.Assign(FPraButtonStyleTemplate.GetFontConfigurationCommon.GetFontDisabled);
  FontFocused.Assign(FPraButtonStyleTemplate.GetFontConfigurationCommon.GetFontFocused);
end;

procedure TPraButtonStyle.Assign(Source: TPersistent);
begin
  if Source is TPraButtonStyle then
  begin
    Brush.Assign(TPraButtonStyle(Source).Brush);
    BrushFocused.Assign(TPraButtonStyle(Source).BrushFocused);
    Pen.Assign(TPraButtonStyle(Source).Pen);
    PenFocused.Assign(TPraButtonStyle(Source).PenFocused);
    Font.Assign(TPraButtonStyle(Source).Font);
    FontFocused.Assign(TPraButtonStyle(Source).FontFocused);
    FontDown.Assign(TPraButtonStyle(Source).FontDown);
    Radius := TPraButtonStyle(Source).Radius;
  end
  else
    inherited Assign(Source);
end;

procedure TPraButtonStyle.ChangeScale(M, D: Integer; isDpiChange: Boolean);
var
  Flags: TScalingFlags;
begin
  FPen.Width := MulDiv(FPen.Width, M, D);
  // Scaling of other Fonts as current Font
  if csLoading in ComponentState then
    Flags := ScalingFlags
  else
    Flags := DefaultScalingFlags;
  if not ParentFont and (sfFont in Flags) then
  begin
    FFontDown.Height := MulDiv(FFontDown.Height, M, D);
    FFontFocused.Height := MulDiv(FFontFocused.Height, M, D);
    FFontDisabled.Height := MulDiv(FFontDisabled.Height, M, D);
  end;
  inherited;
end;

procedure TPraButtonStyle.Click;
begin
  inherited;
end;

procedure TPraButtonStyle.CMEnter(var Message: TCMEnter);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;

  FMouseEnter := false;
  invalidate;
end;

procedure TPraButtonStyle.CMMouseEnter(var Message: TNotifyEvent);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;

  FMouseEnter := true;
  invalidate;
end;

procedure TPraButtonStyle.CMMouseLeave(var Message: TNotifyEvent);
begin
  if not(Enabled) or (csDesigning in ComponentState) then
    Exit;

  FMouseEnter := false;
  invalidate;
end;

procedure TPraButtonStyle.FontDisabledCopyOfFont(const AValue: Boolean);
begin
  if AValue then
    FFontDisabled.Assign(Font);
end;

procedure TPraButtonStyle.FontDownCopyOfFont(const AValue: Boolean);
begin
  if AValue then
    FFontDown.Assign(Font);
end;

procedure TPraButtonStyle.FontFocusedCopyOfFont(const AValue: Boolean);
begin
  if AValue then
    FFontFocused.Assign(Font);
end;

constructor TPraButtonStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFocusControl := nil;
  CreateFocusControl(nil, TWinControl(AOwner));

  Cursor := crHandPoint;
  ControlStyle := ControlStyle + [csReplicatable];
  ParentFont := true;

  Width := 75;
  Height := 25;

  // Pen
  FPen := TPen.Create;
  FPen.onChange := StyleChanged;
  // PenDown
  FPenDown := TPen.Create;
  FPenDown.onChange := StyleChanged;
  // PenFocused
  FPenFocused := TPen.Create;
  FPenFocused.onChange := StyleChanged;
  // PenDisabled
  FPenDisabled := TPen.Create;
  FPenDisabled.onChange := StyleChanged;

  // Brush
  FBrush := TBrush.Create;
  FBrush.onChange := StyleChanged;
  // BrushDown
  FBrushDown := TBrush.Create;
  FBrushDown.onChange := StyleChanged;
  // BrushFocused
  FBrushFocused := TBrush.Create;
  FBrushFocused.onChange := StyleChanged;
  // BrushDisabled
  FBrushDisabled := TBrush.Create;
  FBrushDisabled.onChange := StyleChanged;

  // Picture
  FPicture := TPicture.Create;
  FPicture.onChange := StyleChanged;
  // PictureFocused
  FPictureFocused := TPicture.Create;
  FPictureFocused.onChange := StyleChanged;
  // PictureDisabled
  FPictureDisabled := TPicture.Create;
  FPictureDisabled.onChange := StyleChanged;

  // FontDown
  FFontDown := TPraButtonFont.Create;
  FFontDown.onChange := FontDownCopyOfFont;
  // FontFocused
  FFontFocused := TPraButtonFont.Create;
  FFontFocused.onChange := FontFocusedCopyOfFont;
  // FontDisabled
  FFontDisabled := TPraButtonFont.Create;
  FFontDisabled.onChange := FontDisabledCopyOfFont;

  FMouseEnter := false;
  FShowCaption := true;

  FClickOnEnter := true;

  TabStop := false;

  OnMouseDown := DoMouseDown;
  OnMouseUp := DoMouseUp;

  FControllerStyleTemplate := false;

  FStyleOutline := false;

  CreateButtonDefault;
end;

procedure TPraButtonStyle.CreateButtonBack;
begin
  LoadingTemplateLight;
  FPraButtonStyleTemplateType := TPraButtonStyleBack.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Back';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonCancel;
begin
  LoadingTemplateSecondary;
  FPraButtonStyleTemplateType := TPraButtonStyleCancel.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Cancel';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonClose;
begin
  LoadingTemplateDanger;
  FPraButtonStyleTemplateType := TPraButtonStyleClose.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Close';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonDanger;
begin
  LoadingTemplateDanger;

  Height := FPraButtonStyleTemplate.GetSizeHeight;
  Width := FPraButtonStyleTemplate.GetSizeWidth;
end;

procedure TPraButtonStyle.CreateButtonDark;
begin
  LoadingTemplateDark;

  Height := FPraButtonStyleTemplate.GetSizeHeight;
  Width := FPraButtonStyleTemplate.GetSizeWidth;
end;

procedure TPraButtonStyle.CreateButtonDefault;
begin
  Font.Name := 'Tahoma';

  FBrush.Color := clWhite;
  FBrushDown.Color := clWhite;
  FBrushFocused.Color := clWhite;
  FBrushDisabled.Color := clWhite;

  FFontDown.Assign(Self.Font);
  FFontFocused.Assign(Self.Font);
  FFontDisabled.Assign(Self.Font);

  FRadius := 4;
  FSpacing := 3;
  FPictureMarginLeft := 3;
  FAlignment := paCenter;
  FStyle := bsCustom;
  FTemplateStyle := tsNone;

  if Assigned(FPicture.Graphic) then
    FPicture.Graphic := nil;

  if Assigned(FPictureFocused.Graphic) then
    FPictureFocused.Graphic := nil;

  if Assigned(FPictureDisabled.Graphic) then
    FPictureDisabled.Graphic := nil;
end;

procedure TPraButtonStyle.CreateButtonDelete;
begin
  LoadingTemplateDanger;
  FPraButtonStyleTemplateType := TPraButtonStyleDelete.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Delete';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonEdit;
begin
  LoadingTemplateWarning;
  FPraButtonStyleTemplateType := TPraButtonStyleEdit.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Edit';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonEmail;
begin
  LoadingTemplateLight;
  FPraButtonStyleTemplateType := TPraButtonStyleEmail.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Email';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonGear;
begin
  LoadingTemplatePrimary;
  FPraButtonStyleTemplateType := TPraButtonStyleGear.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Gear';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonHeart;
begin
  LoadingTemplateHeart;
  FPraButtonStyleTemplateType := TPraButtonStyleHeart.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Heart';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonInfo;
begin
  LoadingTemplateInfo;

  Height := FPraButtonStyleTemplate.GetSizeHeight;
  Width := FPraButtonStyleTemplate.GetSizeWidth;
end;

procedure TPraButtonStyle.CreateButtonInsert;
begin
  LoadingTemplateSuccess;
  FPraButtonStyleTemplateType := TPraButtonStyleInsert.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Insert';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonLight;
begin
  LoadingTemplateLight;

  Height := FPraButtonStyleTemplate.GetSizeHeight;
  Width := FPraButtonStyleTemplate.GetSizeWidth;
end;

procedure TPraButtonStyle.CreateButtonMenu;
begin
  LoadingTemplatePrimary;
  FPraButtonStyleTemplateType := TPraButtonStyleMenu.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Menu';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonPrimary;
begin
  LoadingTemplatePrimary;

  Height := FPraButtonStyleTemplate.GetSizeHeight;
  Width := FPraButtonStyleTemplate.GetSizeWidth;
end;

procedure TPraButtonStyle.CreateButtonPrint;
begin
  LoadingTemplateInfo;
  FPraButtonStyleTemplateType := TPraButtonStylePrint.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Print';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonReport;
begin
  LoadingTemplatePrimary;
  FPraButtonStyleTemplateType := TPraButtonStyleReport.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Report';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonSave;
begin
  LoadingTemplateSuccess;
  FPraButtonStyleTemplateType := TPraButtonStyleSave.New;

  Height := FPraButtonStyleTemplateType.GetSizeHeight;
  Width := FPraButtonStyleTemplateType.GetSizeWidth;

  Self.Caption := 'Save';
  Self.Alignment := paLeftJustify;
end;

procedure TPraButtonStyle.CreateButtonSecondary;
begin
  LoadingTemplateSecondary;

  Height := FPraButtonStyleTemplate.GetSizeHeight;
  Width := FPraButtonStyleTemplate.GetSizeWidth;
end;

procedure TPraButtonStyle.CreateButtonSuccess;
begin
  LoadingTemplateSuccess;

  Height := FPraButtonStyleTemplate.GetSizeHeight;
  Width := FPraButtonStyleTemplate.GetSizeWidth;
end;

procedure TPraButtonStyle.CreateButtonWarning;
begin
  LoadingTemplateWarning;

  Height := FPraButtonStyleTemplate.GetSizeHeight;
  Width := FPraButtonStyleTemplate.GetSizeWidth;
end;

procedure TPraButtonStyle.CreateFocusControl(AOwner: TComponent; AParent: TWinControl);
begin
  if not Assigned(FFocusControl) then
  begin
    FFocusControl := TFocusControl.Create(AOwner, Self);
    try
      FFocusControl.TabStop := true;
      FFocusControl.SetBounds(0, 0, 0, 0);
    except
      raise;
    end;
  end;
end;

destructor TPraButtonStyle.Destroy;
begin
  FPen.Free;
  FPenDown.Free;
  FPenFocused.Free;
  FPenDisabled.Free;

  FBrush.Free;
  FBrushDown.Free;
  FBrushFocused.Free;
  FBrushDisabled.Free;

  FPicture.Free;
  FPictureFocused.Free;
  FPictureDisabled.Free;

  FFontDown.Free;
  FFontFocused.Free;
  FFontDisabled.Free;

  DestroyFocusControl;

  inherited Destroy;
end;

procedure TPraButtonStyle.DestroyFocusControl;
begin
  if Assigned(FFocusControl) then
  begin
    if Assigned(FFocusControl.Parent) then
      FreeAndNil(FFocusControl);
  end;
end;

procedure TPraButtonStyle.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if ClickOnEnter and (Key = 13) then
  begin
    FState := bsDown;
    invalidate;

    Self.Click;
  end;
end;

procedure TPraButtonStyle.DoKeyUp;
begin
  FState := bsUp;

  invalidate;
end;

procedure TPraButtonStyle.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FState := bsDown;

  invalidate;
end;

procedure TPraButtonStyle.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FState := bsUp;

  invalidate;
end;

function TPraButtonStyle.GetCanFocus: Boolean;
begin
  if Assigned(FFocusControl) then
    result := FFocusControl.CanFocus
  else
    result := false;
end;

function TPraButtonStyle.GetFocused: Boolean;
begin
  if Assigned(FFocusControl) then
    result := FFocusControl.Focused
  else
    result := false;
end;

function TPraButtonStyle.GetPictureHeight: SmallInt;
begin
  result := 0;
  if informedPicture then
    result := MaxIntValue([FPicture.Height, FPictureFocused.Height, FPictureDisabled.Height]);
end;

function TPraButtonStyle.GetPictureMarginLeft: SmallInt;
begin
  result := 0;
  if informedPicture then
    result := FPictureMarginLeft;
end;

function TPraButtonStyle.GetPictureWidth: SmallInt;
begin
  result := 0;
  if informedPicture then
    result := MaxIntValue([FPicture.Width, FPictureFocused.Width, FPictureDisabled.Width]);
end;

function TPraButtonStyle.GetSpacing: SmallInt;
begin
  result := 0;
  if informedPicture then
    result := FSpacing;
end;

function TPraButtonStyle.GetTabOrder: Integer;
begin
  if Assigned(FFocusControl) then
    result := FFocusControl.TabOrder
  else
    result := -1;
end;

function TPraButtonStyle.GetTabStop: Boolean;
begin
  if Assigned(FFocusControl) then
    result := FFocusControl.TabStop
  else
    result := false;
end;

function TPraButtonStyle.informedPicture: Boolean;
begin
  result := Assigned(FPicture.Graphic) or Assigned(FPictureFocused.Graphic) or Assigned(FPictureDisabled.Graphic);
end;

function TPraButtonStyle.IsClickOnEnter: Boolean;
begin
  result := FClickOnEnter <> true;
end;

function TPraButtonStyle.IsStyleOutline: Boolean;
begin
  result := StyleOutline <> false;
end;

function TPraButtonStyle.isPictureCenter: Boolean;
begin
  result := PictureCenter <> false;
end;

function TPraButtonStyle.IsPictureMarginLeft: Boolean;
begin
  result := PictureMarginLeft <> 3;
end;

function TPraButtonStyle.IsRadius: Boolean;
begin
  result := Radius <> 3;
end;

function TPraButtonStyle.IsShowCaption: Boolean;
begin
  result := ShowCaption <> true;
end;

function TPraButtonStyle.IsSpacing: Boolean;
begin
  result := Spacing <> 3;
end;

function TPraButtonStyle.IsStoredAlignment: Boolean;
begin
  result := Alignment <> paCenter;
end;

function TPraButtonStyle.IsStoredStyle: Boolean;
begin
  result := Style <> bsCustom;
end;

function TPraButtonStyle.IsStoredTemplateStyle: Boolean;
begin
  result := TemplateStyle <> tsNone;
end;

procedure TPraButtonStyle.Paint;
var
  X, Y, w, h: Integer;
begin
  inherited;

  with Canvas do
  begin
    if not(Enabled) then
    begin
      Pen := FPenDisabled;
      Brush := FBrushDisabled;
      Font := FFontDisabled;
    end
    else if FState = bsDown then
    begin
      Pen := FPenDown;
      Brush := FBrushDown;
      Font := FFontDown;
    end
    else if FMouseEnter or Focused then
    begin
      Pen := FPenFocused;
      Brush := FBrushFocused;
      Font := FFontFocused;
    end
    else
    begin
      Pen := FPen;
      Brush := FBrush;
      Font := Self.Font;
    end;

    if (FShape = stRoundRect) and (Pen.Width = 2) then
      Pen.Width := 1;

    X := Pen.Width div 2;

    Y := X;
    w := Width - Pen.Width + 1;
    h := Height - Pen.Width + 1;

    if Pen.Width = 0 then
    begin
      Dec(w);
      Dec(h);
    end;

    case FShape of
      stRectangle:
        Rectangle(X, Y, X + w, Y + h);
      stRoundRect:
        RoundRect(X, Y, X + w, Y + h, Radius, Radius);
    end;

    X := GetPictureMarginLeft;
    if FPictureCenter and (Caption = '') then
      X := (ClientWidth - GetPictureWidth) div 2;

    h := (ClientHeight - GetPictureHeight) div 2;
    if not(Enabled) then
    begin
      if Assigned(PictureDisabled.Graphic) then
        Draw(X, h, PictureDisabled.Graphic)
      else
        Draw(X, h, Picture.Graphic);
    end
    else if FMouseEnter or Focused then
    begin
      if Assigned(PictureFocused.Graphic) then
        Draw(X, h, PictureFocused.Graphic)
      else
        Draw(X, h, Picture.Graphic);
    end
    else
      Draw(X, h, Picture.Graphic);

    if FShowCaption and (Trim(Caption) <> '') then
    begin
      if Alignment = paCenter then
      begin
        if Assigned(Picture.Graphic) or (Assigned(PictureFocused.Graphic) and (FMouseEnter or Focused)) then
          X := (ClientWidth - (GetPictureWidth + PictureMarginLeft)) div 2
        else
          X := (ClientWidth - Canvas.TextWidth(Caption)) div 2
      end
      else
        X := GetPictureWidth + PictureMarginLeft + GetSpacing;

      Y := (ClientHeight - Canvas.TextHeight(Caption)) div 2;
      TextOut(X, Y, Caption);
    end;
  end;
end;

procedure TPraButtonStyle.SetAlignment(const Value: TPraAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Repaint;
end;

procedure TPraButtonStyle.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TPraButtonStyle.SetBrushDisabled(const Value: TBrush);
begin
  FBrushDisabled.Assign(Value);
end;

procedure TPraButtonStyle.SetBrushDown(const Value: TBrush);
begin
  FBrushDown.Assign(Value);
end;

procedure TPraButtonStyle.SetBrushFocused(const Value: TBrush);
begin
  FBrushFocused.Assign(Value);
end;

procedure TPraButtonStyle.SetCaption(const Value: TCaption);
begin
  FCaption := Value;
  invalidate;
end;

procedure TPraButtonStyle.SetClickOnEnter(const Value: Boolean);
begin
  if FClickOnEnter <> Value then
  begin
    FClickOnEnter := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetFocus;
begin
  if Assigned(FFocusControl) then
    if FFocusControl.CanFocus then
      FFocusControl.SetFocus;
end;

procedure TPraButtonStyle.SetFontDisabled(const Value: TPraButtonFont);
begin
  FFontDisabled.Assign(Value);
end;

procedure TPraButtonStyle.SetFontDown(const Value: TPraButtonFont);
begin
  FFontDown.Assign(Value);
end;

procedure TPraButtonStyle.SetFontFocused(const Value: TPraButtonFont);
begin
  FFontFocused.Assign(Value);
end;

procedure TPraButtonStyle.SetStyleOutline(const Value: Boolean);
begin
  if FStyleOutline <> Value then
  begin
    FStyleOutline := Value;

    if Style <> bsCustom then
      SetStyle(FStyle)
    else if isTemplateStyle then
      SetTemplateStyle(FTemplateStyle);
  end;
end;

procedure TPraButtonStyle.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(Self.Parent) then
  begin
    FFocusControl.Parent := Self.Parent;
    FFocusControl.Show;
  end;
end;

procedure TPraButtonStyle.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TPraButtonStyle.SetPenDisabled(const Value: TPen);
begin
  FPenDisabled.Assign(Value);
end;

procedure TPraButtonStyle.SetPenDown(const Value: TPen);
begin
  FPenDown.Assign(Value);
end;

procedure TPraButtonStyle.SetPenFocused(const Value: TPen);
begin
  FPenFocused.Assign(Value);
end;

procedure TPraButtonStyle.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TPraButtonStyle.SetPictureCenter(const Value: Boolean);
begin
  if FPictureCenter <> Value then
  begin
    FPictureCenter := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetPictureDisabled(const Value: TPicture);
begin
  FPictureDisabled.Assign(Value);
end;

procedure TPraButtonStyle.SetPictureTemplate;
begin
  if not Assigned((FPraButtonStyleTemplateType)) then
    Exit;

  if StyleOutline then
  begin
    Picture.Assign(FPraButtonStyleTemplateType.GetPictureStyleOutline);
    PictureFocused.Assign(FPraButtonStyleTemplateType.GetPictureFocusedStyleOutline);
  end
  else
  begin
    Picture.Assign(FPraButtonStyleTemplateType.GetPicture);
    PictureFocused.Assign(FPraButtonStyleTemplateType.GetPictureFocused);
  end;

  PictureDisabled.Assign(FPraButtonStyleTemplateType.GetPictureDisabled);
end;

procedure TPraButtonStyle.SetPictureMarginLeft(const Value: SmallInt);
begin
  if (FPictureMarginLeft >= 0) and (FPictureMarginLeft <> Value) then
  begin
    FPictureMarginLeft := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetPictureFocused(const Value: TPicture);
begin
  FPictureFocused.Assign(Value);
end;

procedure TPraButtonStyle.SetRadius(const Value: SmallInt);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetShape(Value: TPraButtonStyleType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetShowCaption(const Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetSize(const AHeight, AWidth: Integer);
begin
  Height := AHeight;
  Width := AWidth;
end;

procedure TPraButtonStyle.SetSpacing(const Value: SmallInt);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetStyle(const Value: TPraButtonStyleStyle);
begin
  if isTemplateStyle then
  begin
    FControllerStyleTemplate := true;
    TemplateStyle := tsNone;
  end;

  if FControllerStyleTemplate then
  begin
    CreateButtonDefault;
    FControllerStyleTemplate := false;
    Exit;
  end;

  Self.FStyle := Value;

  case Self.FStyle of
    bsPrimary:
      CreateButtonPrimary;
    bsSecondary:
      CreateButtonSecondary;
    bsSuccess:
      CreateButtonSuccess;
    bsDanger:
      CreateButtonDanger;
    bsWarning:
      CreateButtonWarning;
    bsInfo:
      CreateButtonInfo;
    bsLight:
      CreateButtonLight;
    bsDark:
      CreateButtonDark;
  end;

  if Value <> bsCustom then
  begin
    SetSize(32, 90);

    if StyleOutline then
      StyleOutlineConfig;
  end;
end;

procedure TPraButtonStyle.SetTabOrder(const Value: Integer);
begin
  if Assigned(FFocusControl) then
    FFocusControl.TabOrder := Value;
end;

procedure TPraButtonStyle.SetTabStop(const Value: Boolean);
begin
  if Assigned(FFocusControl) then
    FFocusControl.TabStop := Value;
end;

procedure TPraButtonStyle.SetTemplateStyle(const Value: TTemplateStyle);
begin
  if FControllerStyleTemplate then
  begin
    CreateButtonDefault;
    FControllerStyleTemplate := false;
    Exit;
  end;

  if Style <> bsCustom then
  begin
    FControllerStyleTemplate := true;
    Style := bsCustom;
  end;

  Self.FTemplateStyle := Value;

  if not(csReading in ComponentState) then
  begin
    case Value of
      tsSave:
        CreateButtonSave;
      tsEdit:
        CreateButtonEdit;
      tsCancel:
        CreateButtonCancel;
      tsDelete:
        CreateButtonDelete;
      tsPrint:
        CreateButtonPrint;
      tsGear:
        CreateButtonGear;
      tsMenu:
        CreateButtonMenu;
      tsHeart:
        CreateButtonHeart;
      tsEmail:
        CreateButtonEmail;
      tsInsert:
        CreateButtonInsert;
      tsBack:
        CreateButtonBack;
      tsClose:
        CreateButtonClose;
      tsReport:
        CreateButtonReport;
    end;

    SetPictureTemplate;

    if Value <> tsNone then
    begin
      if StyleOutline then
        StyleOutlineConfig;
    end;
  end;
end;

procedure TPraButtonStyle.StyleChanged(Sender: TObject);
begin
  invalidate;
end;

procedure TPraButtonStyle.StyleOutlineConfig;
var
  lBrushColor: TColor;
begin
  lBrushColor := Brush.Color;
  Brush.Color := Font.Color;

  Font.Color := lBrushColor;

  Pen.Style := psSolid;
  Pen.Color := lBrushColor;
  Pen.Width := 1;
end;

function TPraButtonStyle.isTemplateStyle: Boolean;
begin
  result := TemplateStyle <> tsNone;
end;

procedure TPraButtonStyle.LoadingTemplateDanger;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplateDanger.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.LoadingTemplateDark;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplateDark.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.LoadingTemplateHeart;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplateHeart.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.LoadingTemplateInfo;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplateInfo.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.LoadingTemplateLight;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplateLight.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.LoadingTemplatePrimary;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplatePrimary.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.LoadingTemplateSecondary;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplateSecondary.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.LoadingTemplateSuccess;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplateSuccess.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.LoadingTemplateWarning;
begin
  FPraButtonStyleTemplate := TPraButtonStyleTemplateWarning.New;

  ApplyTemplate;
end;

procedure TPraButtonStyle.WMEraseBkgnd(var Message: TWMEraseBkGnd);
begin
  message.result := 1;
end;

constructor TFocusControl.Create(AOwner: TComponent; AGraphicControl: TPraButtonStyle);
begin
  inherited Create(AOwner);

  Assert(Assigned(AGraphicControl), 'Não é possível criar um FocusControl com GraphicControl não atribuído.');

  FGraphicControl := AGraphicControl;
  Self.TabStop := false;
end;

procedure TFocusControl.WMKeyDown(var Message: TWMKeyDown);
var
  Shift: TShiftState;
begin
  if Assigned(FGraphicControl) then
  begin
    Shift := KeyDataToShiftState(Message.KeyData);
    FGraphicControl.DoKeyDown(Message.CharCode, Shift);
  end;

  inherited;
end;

procedure TFocusControl.WMKeyUp(var Message: TWMKeyDown);
begin
  if Assigned(FGraphicControl) then
    FGraphicControl.DoKeyUp;

  inherited;
end;

procedure TFocusControl.WndProc(var Message: TMessage);
begin
  inherited;

  case Message.Msg of
    WM_SETFOCUS, WM_KILLFOCUS:
      begin
        if Assigned(FGraphicControl) then
          FGraphicControl.Repaint;
      end;
  end;
end;

function TPraButtonFont.IsCopyOfFont: Boolean;
begin
  result := CopyOfFont <> false;
end;

procedure TPraButtonFont.SetCopyOfFont(const Value: Boolean);
begin
  if FCopyOfFont <> Value then
  begin
    FCopyOfFont := Value;
    onChange(Value);
  end;
end;

end.
