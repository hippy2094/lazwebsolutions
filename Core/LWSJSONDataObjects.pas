(*
  LazWebSolutions, JSON Data Objects unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSJSONDataObjects;

{$I lazwebsolutions.inc}

interface

uses
  LWSJSONDataObjectsConsts, Classes, SysUtils, SQLdb, DB, TypInfo, Contnrs,
  FPJSON;

type

  ELWSJDOException = class(Exception);

  ELWSJDOConnection = class(ELWSJDOException);

  ELWSJDOQuery = class(ELWSJDOException);

  TLWSJDOFieldTypes = (ftNull, ftStr, ftBool, ftDate, ftFloat, ftInt);

  TLWSJDOQuerySQLOperation = (soNone, soSelect, soInsert, soUpdate, soDelete);

  { TLWSJDOConnection }

  TLWSJDOConnection = class
  private
    FConfig: TStrings;
    FConfigFileName: TFileName;
    FConnection: TSQLConnection;
    FQuery: TSQLQuery;
    FTransaction: TSQLTransaction;
    function GetFields: TFields;
    function GetParams: TParams;
    procedure InternalCreateConnection;
    procedure InternalCreateTransaction;
    procedure InternalCreateQuery;
  public
    constructor Create(const AConfigFileName: TFileName;
      const AConnect: Boolean = True);
    destructor Destroy; override;
    procedure LoadConfig;
    procedure SetProperties;
    procedure Prepare(const ASQL: string);
    function Field(const AFieldByName: string): TField;
    function Param(const AParamName: string): TParam;
    function Open: Boolean;
    function Execute: Boolean;
    procedure StartTrans;
    procedure Commit;
    procedure Rollback;
    property Config: TStrings read FConfig;
    property ConfigFileName: TFileName read FConfigFileName write FConfigFileName;
    property Connection: TSQLConnection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
    property Query: TSQLQuery read FQuery;
    property Fields: TFields read GetFields;
    property Params: TParams read GetParams;
  end;

  { TLWSJDOQuery }

  TLWSJDOQuery = class
  private
    FLastSQLOperation: TLWSJDOQuerySQLOperation;
    FDataBase: TLWSJDOConnection;
    FFields: TJSONObject;
    FItems: TObjectList;
    FOrderByPK: Boolean;
    FPKFieldName: string;
    FShowDateAsString: Boolean;
    FTableName: string;
    function GetItems(AIndex: Integer): TJSONObject;
    procedure SetItems(AIndex: Integer; const AValue: TJSONObject);
  public
    constructor Create;
    constructor Create(ADataBase: TLWSJDOConnection; const ATableName: string);
    destructor Destroy; override;
    procedure Prepare(const ASQLOperation: TLWSJDOQuerySQLOperation;
      const AAdditionalSQL: string = ES); virtual;
    procedure AddField(const AFieldName: ShortString;
      const AFieldType: TLWSJDOFieldTypes;
      const AIsPK: Boolean = False);
    function Insert(AJSONObject: TJSONObject): Boolean; virtual;
    function Insert(AJSONArray: TJSONArray): Boolean; virtual;
    function Update(AJSONObject: TJSONObject): Boolean; virtual;
    function Update(AJSONArray: TJSONArray): Boolean; virtual;
    function Delete(AJSONObject: TJSONObject): Boolean; virtual;
    function Delete(AJSONArray: TJSONArray): Boolean; virtual;
    function Open(const AWhere: string = ES): Boolean; virtual;
    function Count: Integer;
    function First: TJSONObject;
    function Last: TJSONObject;
    function AsJSON: TJSONStringType;
    property DataBase: TLWSJDOConnection read FDataBase write FDataBase;
    property Items[AIndex: Integer]: TJSONObject read GetItems
      write SetItems; default;
    property Fields: TJSONObject read FFields;
    property TableName: string read FTableName write FTableName;
    property PKFieldName: string read FPKFieldName write FPKFieldName;
    property OrderByPK: Boolean read FOrderByPK write FOrderByPK;
    property ShowDateAsString: Boolean read FShowDateAsString
      write FShowDateAsString;
  end;

function LWSFieldTypeToLWSJDOFieldType(
  const AFieldType: TFieldType): ShortString;
function LWSAFieldTypeToLWSJDOFieldTypeEnum(
  const AFieldType: TFieldType): TLWSJDOFieldTypes;
procedure LWSFieldsToJSONObject(AFields: TFields;
  AJSONFiels, AJSONObject: TJSONObject; const AShowDateAsString: Boolean);
procedure LWSJSONObjectToParams(AParams: TParams;
  AJSONFiels, AJSONObject: TJSONObject; const APKFieldName: string = ES);

implementation

function LWSFieldTypeToLWSJDOFieldType(
  const AFieldType: TFieldType): ShortString;
begin
  case AFieldType of
    ftUnknown, ftCursor, ftADT, ftArray, ftReference,
      ftDataSet, ftInterface, ftIDispatch: Result := LWS_FT_NULL;
    ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob, ftOraClob,
      ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes, ftGraphic, ftFmtMemo,
      ftParadoxOle, ftDBaseOle, ftTypedBinary, ftVariant,
      ftGuid: Result := LWS_FT_STR;
    ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc: Result := LWS_FT_INT;
    ftBoolean: Result := LWS_FT_BOOL;
    DB.ftFloat, ftCurrency, ftBCD, ftFMTBcd: Result := LWS_FT_FLOAT;
    DB.ftDate, ftTime, ftDateTime, ftTimeStamp: Result := LWS_FT_DATE;
  end;
end;

function LWSAFieldTypeToLWSJDOFieldTypeEnum(
  const AFieldType: TFieldType): TLWSJDOFieldTypes;
begin
  case AFieldType of
    ftUnknown, ftCursor, ftADT, ftArray, ftReference,
      ftDataSet, ftInterface, ftIDispatch: Result := ftNull;
    ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob, ftOraClob,
      ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes, ftGraphic, ftFmtMemo,
      ftParadoxOle, ftDBaseOle, ftTypedBinary, ftVariant,
      ftGuid: Result := ftStr;
    ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc: Result := ftInt;
    ftBoolean: Result := ftBool;
    DB.ftFloat, ftCurrency, ftBCD, ftFMTBcd: Result := ftFloat;
    DB.ftDate, ftTime, ftDateTime, ftTimeStamp: Result := ftDate;
  end;
end;

procedure LWSFieldsToJSONObject(AFields: TFields;
  AJSONFiels, AJSONObject: TJSONObject; const AShowDateAsString: Boolean);
var
  I: Integer;
  VField: TField;
  VFieldType, VFieldName: ShortString;
begin
  for I := 0 to Pred(AFields.Count) do
  begin
    VField := AFields[I];
    if AJSONFiels.Count > 0 then
    begin
      VFieldType := AJSONFiels.Items[I].AsString;
      VFieldName := AJSONFiels.Names[I];
    end
    else
    begin
      VFieldType := LWSFieldTypeToLWSJDOFieldType(VField.DataType);
      VFieldName := VField.FieldName;
    end;
    if (VFieldType = LWS_FT_NULL) or VField.IsNull then
    begin
      AJSONObject.Add(VFieldName);
      Continue;
    end;
    if VFieldType = LWS_FT_STR then
      AJSONObject.Add(VFieldName, VField.AsString);
    if VFieldType = LWS_FT_BOOL then
      AJSONObject.Add(VFieldName, VField.AsBoolean);
    if VFieldType = LWS_FT_DATE then
    begin
      if AShowDateAsString then
        AJSONObject.Add(VFieldName, VField.AsString)
      else
        AJSONObject.Add(VFieldName, VField.AsFloat);
    end;
    if VFieldType = LWS_FT_FLOAT then
      AJSONObject.Add(VFieldName, VField.AsFloat);
    if VFieldType = LWS_FT_INT then
      AJSONObject.Add(VFieldName, VField.AsInteger);
  end;
end;

procedure LWSJSONObjectToParams(AParams: TParams;
  AJSONFiels, AJSONObject: TJSONObject; const APKFieldName: string);
var
  VParam: TParam;
  VField, VData: TJSONData;
  VFieldType, VName: ShortString;
  I, VJSONObjsCount: Integer;
begin
  VJSONObjsCount := AJSONObject.Count;
  if AJSONFiels.Count <> VJSONObjsCount then
    raise ELWSJDOException.Create(SLWSJSONObjectToParamsError);
  for I := 0 to Pred(VJSONObjsCount) do
  begin
    VName := AJSONFiels.Names[I];
    VField := AJSONFiels.Items[I];
    VParam := AParams.ParamByName(VName);
    VData := AJSONObject[VName];
    if (APKFieldName <> ES) and (APKFieldName = VName) and
      (not VData.IsNull) and Assigned(VParam) then
    begin
      VParam.AsInteger := VData.AsInt64;
      Continue;
    end;
    VFieldType := VField.AsString;
    if (VFieldType = LWS_FT_NULL) or VData.IsNull or VField.IsNull then
    begin
      AParams.Clear;
      Continue;
    end;
    if VFieldType = LWS_FT_STR then
      VParam.AsString := VData.AsString;
    if VFieldType = LWS_FT_BOOL then
      VParam.AsBoolean := VData.AsBoolean;
    if VFieldType = LWS_FT_DATE then
      VParam.AsDateTime := VData.AsFloat;
    if VFieldType = LWS_FT_FLOAT then
      VParam.AsFloat := VData.AsFloat;
    if VFieldType = LWS_FT_INT then
      VParam.AsInteger := VData.AsInteger;
  end;
end;

{ TLWSJDOConnection }

constructor TLWSJDOConnection.Create(const AConfigFileName: TFileName;
  const AConnect: Boolean);
begin
  FConfig := TStringList.Create;
  if AConfigFileName <> ES then
  begin
    FConfigFileName := AConfigFileName;
    LoadConfig;
    InternalCreateConnection;
    InternalCreateTransaction;
    InternalCreateQuery;
    SetProperties;
    if AConnect then
      FConnection.Open;
  end;
end;

destructor TLWSJDOConnection.Destroy;
begin
  FConfig.Free;
  FQuery.Free;
  FTransaction.Free;
  FConnection.Free;
  inherited Destroy;
end;

procedure TLWSJDOConnection.InternalCreateConnection;
var
  VConnectorName: ShortString;
  VConnectionDef: TConnectionDef;
begin
  VConnectorName := FConfig.Values[CONNECTOR_NAME];
  if Trim(VConnectorName) = ES then
    raise ELWSJDOConnection.Create(SLWSConnectorNameEmptyError);
  VConnectionDef := GetConnectionDef(VConnectorName);
  if Assigned(VConnectionDef) then
    FConnection := VConnectionDef.ConnectionClass.Create(nil)
  else
    raise ELWSJDOConnection.CreateFmt(
      SLWSConnectorUnitWasNotDeclaredError, [VConnectorName]);
end;

function TLWSJDOConnection.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TLWSJDOConnection.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

procedure TLWSJDOConnection.InternalCreateTransaction;
begin
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
end;

procedure TLWSJDOConnection.InternalCreateQuery;
begin
  FQuery := TSQLQuery.Create(nil);
  FQuery.DataBase := FConnection;
  FQuery.Transaction := FTransaction;
end;

procedure TLWSJDOConnection.LoadConfig;
begin
  if not FileExists(FConfigFileName) then
    raise ELWSJDOConnection.CreateFmt(
      SLWSConfigFileNotFoundError, [FConfigFileName]);
  FConfig.LoadFromFile(FConfigFileName);
end;

procedure TLWSJDOConnection.SetProperties;
var
  I: Integer;
  VPropName, VToken: ShortString;
begin
  for I := 0 to Pred(FConfig.Count) do
  begin
    VPropName := FConfig.Names[I];
    VToken := Copy(VPropName, 1, 1);
    if (CompareText(VPropName, CONNECTOR_NAME) = 0) or
      (VToken = PO) or (VToken = ES) then
      Continue;
    if IsPublishedProp(FConnection, VPropName) then
      SetPropValue(FConnection, VPropName, FConfig.Values[VPropName])
    else
      raise ELWSJDOConnection.CreateFmt(SLWSInvalidPropInConfigFile,
        [ExtractFileName(FConfigFileName), VPropName]);
  end;
end;

procedure TLWSJDOConnection.Prepare(const ASQL: string);
begin
  FQuery.SQL.Text := ASQL;
end;

function TLWSJDOConnection.Field(const AFieldByName: string): TField;
begin
  Result := FQuery.Fields.FieldByName(AFieldByName);
end;

function TLWSJDOConnection.Param(const AParamName: string): TParam;
begin
  Result := FQuery.Params.ParamByName(AParamName);
end;

function TLWSJDOConnection.Open: Boolean;
begin
  FQuery.Open;
  Result := FQuery.RecordCount > 0;
end;

function TLWSJDOConnection.Execute: Boolean;
begin
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected > 0;
end;

procedure TLWSJDOConnection.StartTrans;
begin
  FTransaction.StartTransaction;
end;

procedure TLWSJDOConnection.Commit;
begin
  FTransaction.Commit;
end;

procedure TLWSJDOConnection.Rollback;
begin
  FTransaction.Rollback;
end;

{ TLWSJDOQuery }

constructor TLWSJDOQuery.Create(ADataBase: TLWSJDOConnection;
  const ATableName: string);
begin
  inherited Create;
  FItems := TObjectList.Create(True);
  FFields := TJSONObject.Create;
  FDataBase := ADataBase;
  FTableName := ATableName;
  FLastSQLOperation := soNone;
  FShowDateAsString := True;
  FPKFieldName := DEFAULT_PK_FIELD;
  FOrderByPK := True;
end;

constructor TLWSJDOQuery.Create;
begin
  inherited Create;
  Create(FDataBase, FTableName);
end;

destructor TLWSJDOQuery.Destroy;
begin
  FFields.Free;
  FItems.Free;
  inherited Destroy;
end;

procedure TLWSJDOQuery.Prepare(const ASQLOperation: TLWSJDOQuerySQLOperation;
  const AAdditionalSQL: string);

  function _SQLSet(const Token: ShortString; const PK: string;
    const SkipPK, Pairs: Boolean): string;
  var
    FN: string;
    I, C: Integer;
  begin
    C := FFields.Count;
    for I := 0 to Pred(C) do
    begin
      FN := FFields.Names[I];
      if SkipPK and (FN = PK) then
        Continue;
      if Pairs then
      begin
        if Succ(I) < C then
          Result += FN + EQ + Token + FN + CS
        else
          Result += FN + EQ + Token + FN;
      end
      else
      begin
        if Succ(I) < C then
          Result += Token + FN + CS
        else
          Result += Token + FN;
      end;
    end;
    if Result = ES then
      Result := AK;
  end;

var
  VSQL: string;
begin
  case ASQLOperation of
    soSelect:
      begin
        if AAdditionalSQL <> ES then
          VSQL := SQL_SELECT_TOKEN + _SQLSet(ES, FPKFieldName, False, False) +
            SQL_FROM_TOKEN + FTableName + SQL_WHERE_TOKEN + AAdditionalSQL
        else
          VSQL := SQL_SELECT_TOKEN + _SQLSet(ES, FPKFieldName, False, False) +
            SQL_FROM_TOKEN + FTableName;
        if FOrderByPK then
          VSQL += SQL_ORDER_BY_TOKEN + FPKFieldName;
        FDataBase.Prepare(VSQL);
      end;
    soInsert: FDataBase.Prepare(SQL_INSERT_TOKEN + FTableName +
      SP + PS + _SQLSet(ES, FPKFieldName, False, False) + PE +
      SQL_VALUES_TOKEN + PS + _SQLSet(CO, FPKFieldName, False, False) + PE);
    soUpdate:
      begin
        if Trim(FPKFieldName) = ES then
          raise ELWSJDOQuery.Create(SLWSPKFieldNameEmptyError);
        FDataBase.Prepare(SQL_UPDATE_TOKEN + FTableName + SQL_SET_TOKEN +
          _SQLSet(CO, FPKFieldName, True, True) + SQL_WHERE_TOKEN + FPKFieldName +
          SQL_EQ_PARAM_TOKEN + FPKFieldName);
      end;
    soDelete:
      begin
        if Trim(FPKFieldName) = ES then
          raise ELWSJDOQuery.Create(SLWSPKFieldNameEmptyError);
        FDataBase.Prepare(SQL_DELETE_TOKEN + SQL_FROM_TOKEN + FTableName +
          SQL_WHERE_TOKEN + FPKFieldName + SQL_EQ_PARAM_TOKEN + FPKFieldName);
      end;
  end;
  FLastSQLOperation := ASQLOperation;
end;

function TLWSJDOQuery.GetItems(AIndex: Integer): TJSONObject;
begin
  Result := FItems[AIndex] as TJSONObject;
end;

procedure TLWSJDOQuery.SetItems(AIndex: Integer; const AValue: TJSONObject);
begin
  FItems[AIndex] := AValue;
end;

procedure TLWSJDOQuery.AddField(const AFieldName: ShortString;
  const AFieldType: TLWSJDOFieldTypes; const AIsPK: Boolean);
begin
  if AIsPK then
    FPKFieldName := AFieldName;
  case AFieldType of
    ftNull: FFields.Add(AFieldName, LWS_FT_NULL);
    ftStr: FFields.Add(AFieldName, LWS_FT_STR);
    ftBool: FFields.Add(AFieldName, LWS_FT_BOOL);
    ftDate: FFields.Add(AFieldName, LWS_FT_DATE);
    ftFloat: FFields.Add(AFieldName, LWS_FT_FLOAT);
    ftInt: FFields.Add(AFieldName, LWS_FT_INT);
  end;
end;

function TLWSJDOQuery.Insert(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soInsert then
    Prepare(soInsert);
  LWSJSONObjectToParams(FDataBase.Params, FFields, AJSONObject);
  Result := FDataBase.Execute;
end;

function TLWSJDOQuery.Insert(AJSONArray: TJSONArray): Boolean;
var
  I: Integer;
  VJSONObject: TJSONObject;
begin
  if FLastSQLOperation <> soInsert then
    Prepare(soInsert);
  for I := 0 to Pred(AJSONArray.Count) do
  begin
    VJSONObject := AJSONArray[I] as TJSONObject;
    LWSJSONObjectToParams(FDataBase.Params, FFields, VJSONObject, FPKFieldName);
    Result := FDataBase.Execute;
  end;
end;

function TLWSJDOQuery.Update(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soUpdate then
    Prepare(soUpdate);
  LWSJSONObjectToParams(FDataBase.Params, FFields, AJSONObject);
  Result := FDataBase.Execute;
end;

function TLWSJDOQuery.Update(AJSONArray: TJSONArray): Boolean;
var
  I: Integer;
  VJSONObject: TJSONObject;
begin
  if FLastSQLOperation <> soUpdate then
    Prepare(soUpdate);
  for I := 0 to Pred(AJSONArray.Count) do
  begin
    VJSONObject := AJSONArray[I] as TJSONObject;
    LWSJSONObjectToParams(FDataBase.Params, FFields, VJSONObject, FPKFieldName);
    Result := FDataBase.Execute;
  end;
end;

function TLWSJDOQuery.Delete(AJSONObject: TJSONObject): Boolean;
begin
  if FLastSQLOperation <> soDelete then
    Prepare(soDelete);
  LWSJSONObjectToParams(FDataBase.Params, FFields, AJSONObject);
  Result := FDataBase.Execute;
end;

function TLWSJDOQuery.Delete(AJSONArray: TJSONArray): Boolean;
var
  I, VCount: Integer;
  VData: TJSONData;
  VJSONObject: TJSONObject;
begin
  if FLastSQLOperation <> soDelete then
    Prepare(soDelete);
  VCount := AJSONArray.Count;
  if VCount > 0 then
    VData := AJSONArray[0];
  case VData.JSONType of
    jtNumber:
      for I := 0 to Pred(VCount) do
      begin
        FDataBase.Param(FPKFieldName).AsInteger := AJSONArray[I].AsInt64;
        Result := FDataBase.Execute;
      end;
    jtObject:
      for I := 0 to Pred(VCount) do
      begin
        VJSONObject := AJSONArray[I] as TJSONObject;
        LWSJSONObjectToParams(FDataBase.Params, FFields, VJSONObject,
          FPKFieldName);
        Result := FDataBase.Execute;
      end;
  end;
end;

function TLWSJDOQuery.Open(const AWhere: string): Boolean;
var
  I: Integer;
  VField: TField;
  VFieldName: string;
  VItem: TJSONObject;
  VIsSQLGeneric: Boolean;
begin
  if FLastSQLOperation <> soSelect then
    Prepare(soSelect, AWhere);
  VIsSQLGeneric := Pos(AK, FDataBase.Query.SQL.Text) <> 0;
  Result := FDataBase.Open;
  FItems.Clear;
  FDataBase.Query.First;
  if VIsSQLGeneric then
  begin
    FDataBase.Query.First;
    while not FDataBase.Query.EOF do
    begin
      for I := 0 to Pred(FDataBase.Fields.Count) do
      begin
        VField := FDataBase.Fields[I];
        VItem := TJSONObject.Create;
        VFieldName := VField.FieldName;
        case VField.DataType of
          ftUnknown, ftCursor, ftADT, ftArray, ftReference, ftDataSet,
            ftInterface, ftIDispatch: VItem.Add(VFieldName);
          ftString, ftBlob, ftMemo, ftFixedChar, ftWideString, ftOraBlob,
            ftOraClob, ftFixedWideChar, ftWideMemo, ftBytes, ftVarBytes,
            ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary,
            ftVariant, ftGuid:
            VItem.Add(VFieldName, VField.AsString);
          ftSmallint, ftInteger, ftLargeint, ftAutoInc:
            VItem.Add(VFieldName, VField.AsInteger);
          ftBoolean: VItem.Add(VFieldName, VField.AsBoolean);
          DB.ftFloat, ftCurrency, ftBCD, ftFMTBcd:
            VItem.Add(VFieldName, VField.AsFloat);
          DB.ftDate, ftTime, ftDateTime, ftTimeStamp:
            if FShowDateAsString then
              VItem.Add(VFieldName, VField.AsString)
            else
              VItem.Add(VFieldName, VField.AsDateTime);
        end;
        FItems.Add(VItem);
      end;
      FDataBase.Query.Next;
    end;
  end
  else
    while not FDataBase.Query.EOF do
    begin
      VItem := TJSONObject.Create;
      LWSFieldsToJSONObject(FDataBase.Query.Fields, FFields, VItem,
        FShowDateAsString);
      FItems.Add(VItem);
      FDataBase.Query.Next;
    end;
end;

function TLWSJDOQuery.Count: Integer;
begin
  Result := FItems.Count;
end;

function TLWSJDOQuery.First: TJSONObject;
begin
  Result := FItems.First as TJSONObject;
end;

function TLWSJDOQuery.Last: TJSONObject;
begin
  Result := FItems.Last as TJSONObject;
end;

function TLWSJDOQuery.AsJSON: TJSONStringType;
var
  I, C: Integer;
begin
  C := FItems.Count;
  for I := 0 to Pred(C) do
    if Succ(I) < C then
      Result += TJSONObject(FItems[I]).AsJSON + CS
    else
      Result += TJSONObject(FItems[I]).AsJSON;
  Result := BS + Result + BE;
end;

end.

