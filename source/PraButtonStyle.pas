{ ****************************************************************************** }
{ }
{ pra components library 2020 }
{ pauloalvis@hotmail.com | github.com/pauloalvis }
{ }
{ ****************************************************************************** }
unit PraButtonStyle;

interface

uses
  vcl.buttons,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  vcl.Controls,
  vcl.Graphics,
  Winapi.Messages,
  System.Math;

type
  TPraAlignment = (paLeftJustify, paCenter);
  TPraButtonStyleType = (stRoundRect, stRectangle);
  TPraButtonStyleStyle = (bsCustom, bsPrimary, bsSecondary, bsSuccess, bsDanger, bsWarning, bsInfo, bsLight, bsDark);

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
    FMouseEnter: Boolean;
    FState: TButtonState;
    FFocusControl: TFocusControl;

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

    property Style: TPraButtonStyleStyle read FStyle write SetStyle stored IsStoredStyle;
    property StyleOutline: Boolean read FStyleOutline write SetStyleOutline stored IsStyleOutline default false;

    procedure StyleChanged(Sender: TObject);
    procedure StyleOutlineConfig;

    procedure FontDownCopyOfFont(const AValue: Boolean);
    procedure FontFocusedCopyOfFont(const AValue: Boolean);
    procedure FontDisabledCopyOfFont(const AValue: Boolean);
  end;

implementation

uses
  vcl.Forms;

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
  FBrush.Color := clWhite;
  FBrush.onChange := StyleChanged;
  // BrushDown
  FBrushDown := TBrush.Create;
  FBrushDown.Color := $00EBEBEB;
  FBrushDown.onChange := StyleChanged;
  // BrushFocused
  FBrushFocused := TBrush.Create;
  FBrushFocused.Color := $00EBEBEB;
  FBrushFocused.onChange := StyleChanged;
  // BrushDisabled
  FBrushDisabled := TBrush.Create;
  FBrushDisabled.Color := $00EBEBEB;
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

  // Font
  Self.Font.Name := 'Tahoma';
  // FontDown
  FFontDown := TPraButtonFont.Create;
  FFontDown.Assign(Self.Font);
  FFontDown.onChange := FontDownCopyOfFont;
  // FontFocused
  FFontFocused := TPraButtonFont.Create;
  FFontFocused.Assign(Self.Font);
  FFontFocused.onChange := FontFocusedCopyOfFont;
  // FontDisabled
  FFontDisabled := TPraButtonFont.Create;
  FFontDisabled.Assign(Self.Font);
  FFontDisabled.onChange := FontDisabledCopyOfFont;

  FRadius := 4;
  FSpacing := 3;
  FMouseEnter := false;
  FShowCaption := true;

  FPictureMarginLeft := 3;

  FClickOnEnter := true;
  FAlignment := paCenter;

  TabStop := false;

  OnMouseDown := DoMouseDown;
  OnMouseUp := DoMouseUp;

  Style := bsCustom;
  StyleOutline := false;
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
    SetStyle(FStyle);
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

procedure TPraButtonStyle.SetSpacing(const Value: SmallInt);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    invalidate;
  end;
end;

procedure TPraButtonStyle.SetStyle(const Value: TPraButtonStyleStyle);
  procedure PenConfigurationCommon;
  begin
    Pen.Style := psClear;
    PenFocused.Style := psClear;
    PenDisabled.Style := psClear;
    PenDown.Style := psSolid;
    PenDown.Width := 3;
  end;

  procedure FontConfigurationCommon;
  begin
    Font.Name := 'Segoe IU';
    Font.Style := [fsBold];
    Font.Size := 10;
    //
    FontDown.Name := 'Segoe IU';
    FontDown.Style := [fsBold];
    FontDown.Size := 10;
    //
    FontFocused.Name := 'Segoe IU';
    FontFocused.Style := [fsBold];
    FontFocused.Size := 10;
    //
    FontDisabled.Name := 'Segoe IU';
    FontDisabled.Style := [fsBold];
    FontDisabled.Size := 10;
  end;

  procedure FontColorConfigurationCommon(const AValue: TColor);
  begin
    Font.Color := AValue;
    FontDown.Color := AValue;
    FontFocused.Color := AValue;
    FontDisabled.Color := AValue;
  end;

begin
  Self.FStyle := Value;

  case Self.FStyle of
    bsPrimary:
      begin
        Brush.Color := $00FE6F2C;
        BrushFocused.Color := $00D85F24;
        BrushDown.Color := $00D85F24;

        PenDown.Color := $00FFB795;

        PenConfigurationCommon;
        FontConfigurationCommon;
        FontColorConfigurationCommon(clWhite);

        BrushDisabled.Color := $00FFA95A;
        FontDisabled.Color := $00FFF0E2;
      end;
    bsSecondary:
      begin
        Brush.Color := $007D756C;
        BrushFocused.Color := $0068625A;
        BrushDown.Color := $0068625A;

        PenDown.Color := $00BEB9B5;

        PenConfigurationCommon;
        FontConfigurationCommon;
        FontColorConfigurationCommon(clWhite);

        BrushDisabled.Color := $00AEA8A2;
        FontDisabled.Color := $00FFF0E2;
      end;
    bsSuccess:
      begin
        Brush.Color := $0045AA29;
        BrushFocused.Color := $00388A22;
        BrushDown.Color := $00388A22;

        PenDown.Color := $00A2D494;

        PenConfigurationCommon;
        FontConfigurationCommon;
        FontColorConfigurationCommon(clWhite);

        BrushDisabled.Color := $0089DD71;
        FontDisabled.Color := $00FFF0E2;
      end;
    bsDanger:
      begin
        Brush.Color := $004737DA;
        BrushFocused.Color := $003626C6;
        BrushDown.Color := $003626C6;

        PenDown.Color := $00A39BEC;

        PenConfigurationCommon;
        FontConfigurationCommon;
        FontColorConfigurationCommon(clWhite);

        BrushDisabled.Color := $00A9A2EE;
        FontDisabled.Color := $00FFF0E2;
      end;
    bsWarning:
      begin
        Brush.Color := $0016C5FC;
        BrushFocused.Color := $000FACDE;
        BrushDown.Color := $000FACDE;

        PenDown.Color := $008AE2FE;

        PenConfigurationCommon;
        FontConfigurationCommon;
        FontColorConfigurationCommon(clWhite);

        BrushDisabled.Color := $0085E0FE;
        FontDisabled.Color := $00949494;
      end;
    bsInfo:
      begin
        Brush.Color := $00B8A027;
        BrushFocused.Color := $00978420;
        BrushDown.Color := $00978420;

        PenDown.Color := $00DBD093;

        PenConfigurationCommon;
        FontConfigurationCommon;
        FontColorConfigurationCommon(clWhite);

        BrushDisabled.Color := $00E2D07A;
        FontDisabled.Color := $00FFF0E2;
      end;
    bsLight:
      begin
        Brush.Color := $00FAF9F8;
        BrushFocused.Color := $00EAE6E2;
        BrushDown.Color := $00EAE6E2;

        PenDown.Color := $00E5E0DA;

        PenConfigurationCommon;
        FontConfigurationCommon;
        FontColorConfigurationCommon(clBlack);

        BrushDisabled.Color := $00CEC5BB;
        FontDisabled.Color := $00FFF0E2;
      end;
    bsDark:
      begin
        Brush.Color := $00403A34;
        BrushFocused.Color := $002B2723;
        BrushDown.Color := $002B2723;

        PenDown.Color := $009F9C99;

        PenConfigurationCommon;
        FontConfigurationCommon;
        FontColorConfigurationCommon(clWhite);

        BrushDisabled.Color := $0094877A;
        FontDisabled.Color := $00FFF0E2;
      end;
  end;

  if StyleOutline and (FStyle <> bsCustom) then
    StyleOutlineConfig;
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
