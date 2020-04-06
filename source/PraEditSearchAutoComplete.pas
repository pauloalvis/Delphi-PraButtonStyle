unit PraEditSearchAutoComplete;

interface

uses
  Vcl.Controls,
  Data.DB,
  Vcl.VDBConsts,
  Data.DBConsts,
  System.Classes,
  Winapi.Messages,
  System.Generics.Collections;

type
  TPraDBEditSearchAutoComplete = class;

  TDataSourceLink = class(TDataLink)
  private
    FDBLookupControl: TPraDBEditSearchAutoComplete;
  protected
    procedure FocusControl(Field: TFieldRef); override;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create;
  end;

  TListSourceLink = class(TDataLink)
  private
    FDBLookupControl: TPraDBEditSearchAutoComplete;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;
  public
    constructor Create;
  end;

  TPraDBEditSearchAutoComplete = class(TCustomControl)
  private
    FLookupSource: TDataSource;
    FDataLink: TDataSourceLink;
    FListLink: TListSourceLink;
    FDataFieldName: string;
    FKeyFieldName: string;
    FListFieldName: string;
    FListFieldIndex: Integer;
    FDataField: TField;
    FMasterField: TField;
    FKeyField: TField;
    FListField: TField;
    FListFields: TList<TField>;
    FKeyValue: Variant;
    FSearchText: string;
    FLookupMode: Boolean;
    FListActive: Boolean;
    FHasFocus: Boolean;
    FListDataChanging: Integer;
    FNullValueKey: TShortCut;
    procedure CheckNotCircular;
    procedure CheckNotLookup;
    procedure DataLinkRecordChanged(Field: TField);
    function GetDataSource: TDataSource;
    function GetKeyFieldName: string;
    function GetListSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetKeyFieldName(const Value: string);
    procedure SetKeyValue(const Value: Variant);
    procedure SetListFieldName(const Value: string);
    procedure SetListSource(Value: TDataSource);
    procedure SetLookupMode(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    function CanModify: Boolean;
    function GetBorderSize: Integer;
    function GetTextHeight: Integer;
    procedure KeyValueChanged;
    procedure ListLinkDataChanged;
    function LocateKey: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ProcessSearchKey(Key: Char);
    procedure SelectKeyValue(const Value: Variant);
    procedure UpdateDataFields;
    procedure UpdateListFields;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataLink: TDataSourceLink read FDataLink;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property HasFocus: Boolean read FHasFocus;
    property KeyField: string read GetKeyFieldName write SetKeyFieldName;
    property KeyValue: Variant read FKeyValue write SetKeyValue;
    property ListActive: Boolean read FListActive;
    property ListField: string read FListFieldName write SetListFieldName;
    [Default (0)]
    property ListFieldIndex: Integer read FListFieldIndex write FListFieldIndex default 0;
    property ListFields: TList<TField> read FListFields;
    property ListLink: TListSourceLink read FListLink;
    property ListSource: TDataSource read GetListSource write SetListSource;
    [Default (0)]
    property NullValueKey: TShortCut read FNullValueKey write FNullValueKey default 0;
    [Default (False)]
    property ParentColor default False;
    [Default (False)]
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SearchText: string read FSearchText write FSearchText;
    [Default (True)]
    property TabStop default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read FDataField;
  end;

implementation

uses

  Winapi.Windows,
  System.Variants,
  Vcl.Menus,
  Vcl.Forms;

function VarEquals(const V1, V2: Variant): Boolean;
begin
  Result := False;
  try
    Result := V1 = V2;
  except
  end;
end;

var
  SearchTickCount: Integer = 0;

constructor TPraDBEditSearchAutoComplete.Create(AOwner: TComponent);
const
  LookupStyle = [csOpaque, csNeedsBorderPaint];
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := LookupStyle
  else
    ControlStyle := LookupStyle + [csFramed];
  ParentColor := False;
  TabStop := True;
  FLookupSource := TDataSource.Create(Self);
  FDataLink := TDataSourceLink.Create;
  FDataLink.FDBLookupControl := Self;
  FListLink := TListSourceLink.Create;
  FListLink.FDBLookupControl := Self;
  FListFields := TList<TField>.Create;
  FKeyValue := Null;
end;

destructor TPraDBEditSearchAutoComplete.Destroy;
begin
  inherited Destroy;
  FListFields.Free;
  FListFields := nil;
  if FListLink <> nil then
    FListLink.FDBLookupControl := nil;
  FListLink.Free;
  FListLink := nil;
  if FDataLink <> nil then
    FDataLink.FDBLookupControl := nil;
  FDataLink.Free;
  FDataLink := nil;
end;

function TPraDBEditSearchAutoComplete.CanModify: Boolean;
begin
  Result := FListActive and not ReadOnly and ((FDataLink.DataSource = nil) or (FMasterField <> nil) and FMasterField.CanModify);
end;

procedure TPraDBEditSearchAutoComplete.CheckNotCircular;
begin
  if FListLink.Active and FListLink.DataSet.IsLinkedTo(DataSource) then
    DatabaseError(SCircularDataLink);
end;

procedure TPraDBEditSearchAutoComplete.CheckNotLookup;
begin
  if FLookupMode then
    DatabaseError(SPropDefByLookup);
  if FDataLink.DataSourceFixed then
    DatabaseError(SDataSourceFixed);
end;

procedure TPraDBEditSearchAutoComplete.UpdateDataFields;
begin
  FDataField := nil;
  FMasterField := nil;
  if FDataLink.Active and (FDataFieldName <> '') then
  begin
    CheckNotCircular;
    FDataField := GetFieldProperty(FDataLink.DataSet, Self, FDataFieldName);
    if FDataField.FieldKind = fkLookup then
      FMasterField := GetFieldProperty(FDataLink.DataSet, Self, FDataField.KeyFields)
    else
      FMasterField := FDataField;
  end;
  SetLookupMode((FDataField <> nil) and (FDataField.FieldKind = fkLookup));
  DataLinkRecordChanged(nil);
end;

procedure TPraDBEditSearchAutoComplete.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FMasterField) then
    if FMasterField <> nil then
      SetKeyValue(FMasterField.Value)
    else
      SetKeyValue(Null);
end;

function TPraDBEditSearchAutoComplete.GetBorderSize: Integer;
var
  Params: TCreateParams;
  R: TRect;
begin
  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
  Result := R.Bottom - R.Top;
end;

function TPraDBEditSearchAutoComplete.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TPraDBEditSearchAutoComplete.GetKeyFieldName: string;
begin
  if FLookupMode then
    Result := ''
  else
    Result := FKeyFieldName;
end;

function TPraDBEditSearchAutoComplete.GetListSource: TDataSource;
begin
  if FLookupMode then
    Result := nil
  else
    Result := FListLink.DataSource;
end;

function TPraDBEditSearchAutoComplete.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TPraDBEditSearchAutoComplete.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

procedure TPraDBEditSearchAutoComplete.KeyValueChanged;
begin
end;

procedure TPraDBEditSearchAutoComplete.UpdateListFields;
var
  DataSet: TDataSet;
  ResultField: TField;
begin
  FListActive := False;
  FKeyField := nil;
  FListField := nil;
  FListFields.Clear;
  if FListLink.Active and (FKeyFieldName <> '') then
  begin
    CheckNotCircular;
    DataSet := FListLink.DataSet;
    FKeyField := GetFieldProperty(DataSet, Self, FKeyFieldName);
    try
      DataSet.GetFieldList(FListFields, FListFieldName);
    except
      DatabaseErrorFmt(SFieldNotFound, [Self.Name, FListFieldName]);
    end;
    if FLookupMode then
    begin
      ResultField := GetFieldProperty(DataSet, Self, FDataField.LookupResultField);
      if FListFields.IndexOf(ResultField) < 0 then
        FListFields.Insert(0, ResultField);
      FListField := ResultField;
    end
    else
    begin
      if FListFields.Count = 0 then
        FListFields.Add(FKeyField);
      if (FListFieldIndex >= 0) and (FListFieldIndex < FListFields.Count) then
        FListField := FListFields[FListFieldIndex]
      else
        FListField := FListFields[0];
    end;
    FListActive := True;
  end;
end;

procedure TPraDBEditSearchAutoComplete.ListLinkDataChanged;
begin
end;

function TPraDBEditSearchAutoComplete.LocateKey: Boolean;
var
  KeySave: Variant;
begin
  Result := False;
  try
    Inc(FListDataChanging);
    try
      KeySave := FKeyValue;
      if not VarIsNull(FKeyValue) and FListLink.DataSet.Active and FListLink.DataSet.Locate(FKeyFieldName, FKeyValue, []) then
      begin
        Result := True;
        FKeyValue := KeySave;
      end;
    except
    end;
  finally
    Dec(FListDataChanging);
  end;
end;

procedure TPraDBEditSearchAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
    if (FListLink <> nil) and (AComponent = ListSource) then
      ListSource := nil;
  end;
end;

procedure TPraDBEditSearchAutoComplete.ProcessSearchKey(Key: Char);
var
  TickCount: Integer;
  S: string;
begin
  if (FListField <> nil) and (FListField.FieldKind in [fkData, fkInternalCalc]) and (FListField.DataType in [ftString, ftWideString]) then
    case Key of
      #8, #27:
        SearchText := '';
      #32 .. High(Char):
        if CanModify then
        begin
          TickCount := GetTickCount;
          if TickCount - SearchTickCount > 2000 then
            SearchText := '';
          SearchTickCount := TickCount;
          if Length(SearchText) < 32 then
          begin
            S := SearchText + Key;
            try
              if FListLink.DataSet.Locate(FListField.FieldName, S, [loCaseInsensitive, loPartialKey]) then
              begin
                SelectKeyValue(FKeyField.Value);
                SearchText := S;
              end;
            except
              { If you attempt to search for a string larger than what the field
                can hold, and exception will be raised.  Just trap it and
                reset the SearchText back to the old value. }
              SearchText := S;
            end;
          end;
        end;
    end;
end;

procedure TPraDBEditSearchAutoComplete.SelectKeyValue(const Value: Variant);
begin
  if FMasterField <> nil then
  begin
    if FDataLink.Edit then
      FMasterField.Value := Value;
  end
  else
    SetKeyValue(Value);
  Repaint;
  Click;
end;

procedure TPraDBEditSearchAutoComplete.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then
  begin
    FDataFieldName := Value;
    UpdateDataFields;
  end;
end;

procedure TPraDBEditSearchAutoComplete.SetDataSource(Value: TDataSource);
begin
  if not(FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TPraDBEditSearchAutoComplete.SetKeyFieldName(const Value: string);
begin
  CheckNotLookup;
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TPraDBEditSearchAutoComplete.SetKeyValue(const Value: Variant);
begin
  if not VarEquals(FKeyValue, Value) then
  begin
    FKeyValue := Value;
    KeyValueChanged;
  end;
end;

procedure TPraDBEditSearchAutoComplete.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TPraDBEditSearchAutoComplete.SetListSource(Value: TDataSource);
begin
  CheckNotLookup;
  FListLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TPraDBEditSearchAutoComplete.SetLookupMode(Value: Boolean);
begin
  if FLookupMode <> Value then
    if Value then
    begin
      FMasterField := GetFieldProperty(FDataField.DataSet, Self, FDataField.KeyFields);
      FLookupSource.DataSet := FDataField.LookupDataSet;
      FKeyFieldName := FDataField.LookupKeyFields;
      FLookupMode := True;
      FListLink.DataSource := FLookupSource;
    end
    else
    begin
      FListLink.DataSource := nil;
      FLookupMode := False;
      FKeyFieldName := '';
      FLookupSource.DataSet := nil;
      FMasterField := FDataField;
    end;
end;

procedure TPraDBEditSearchAutoComplete.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TPraDBEditSearchAutoComplete.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TPraDBEditSearchAutoComplete.WMKillFocus(var Message: TMessage);
begin
  FHasFocus := False;
  inherited;
  Invalidate;
end;

procedure TPraDBEditSearchAutoComplete.WMSetFocus(var Message: TMessage);
begin
  SearchText := '';
  FHasFocus := True;
  inherited;
  Invalidate;
end;

procedure TPraDBEditSearchAutoComplete.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPraDBEditSearchAutoComplete.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Winapi.Windows.LRESULT(FDataLink);
end;

function TPraDBEditSearchAutoComplete.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and FDataLink.ExecuteAction(Action);
end;

function TPraDBEditSearchAutoComplete.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and FDataLink.UpdateAction(Action);
end;

procedure TPraDBEditSearchAutoComplete.WMKeyDown(var Message: TWMKeyDown);
begin
  if (FNullValueKey <> 0) and CanModify and (FNullValueKey = ShortCut(Message.CharCode, KeyDataToShiftState(Message.KeyData))) then
  begin
    SelectKeyValue(Null);
    Message.CharCode := 0;
  end;
  inherited;
end;

procedure TDataSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.UpdateDataFields;
end;

constructor TDataSourceLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

procedure TDataSourceLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = FDBLookupControl.Field) and (FDBLookupControl <> nil) and FDBLookupControl.CanFocus then
  begin
    Field^ := nil;
    FDBLookupControl.SetFocus;
  end;
end;

procedure TDataSourceLink.LayoutChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.UpdateDataFields;
end;

procedure TDataSourceLink.RecordChanged(Field: TField);
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.DataLinkRecordChanged(Field);
end;

procedure TListSourceLink.ActiveChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.UpdateListFields;
end;

constructor TListSourceLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

procedure TListSourceLink.DataSetChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.ListLinkDataChanged;
end;

procedure TListSourceLink.LayoutChanged;
begin
  if FDBLookupControl <> nil then
    FDBLookupControl.UpdateListFields;
end;

end.
