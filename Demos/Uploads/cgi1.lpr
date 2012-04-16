program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI,
  LWSConsts,
  LWSUploads,
  SysUtils,
  Classes;

const
  HEAD = {$I head.inc};
  FORM = {$I form.inc};
  RESULT =
    'All fields:' + BR + LF + '%s' + BR + BR + LF +
    'All values:' + BR + LF + '%s' + BR + BR + LF +
    'All files:' + BR + LF + '%s' + BR + BR + LF +
    'All files saved in:' + BR + LF + '%s';

type

  { TCGI }

  TCGI = class(TLWSCGI)
  private
    FUploads: TLWSUploads;
  protected
    procedure FillUploads(const AData: string); override;
    procedure Request; override;
    procedure Respond; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  constructor TCGI.Create;
  begin
    inherited Create;
    FUploads := TLWSUploads.Create;
  end;

  destructor TCGI.Destroy;
  begin
    FUploads.Free;
    inherited Destroy;
  end;

    procedure TCGI.FillUploads(const AData: string);
  begin
    FUploads.ReadUploads(AData, @Fields, ContentType);
  end;

  procedure TCGI.Request;
  var
    I: Integer;
    VFormItem: TLWSUploadItem;
    VFields, VValues, VFiles, VSep: string;
  begin
    VFields := '';
    VValues := '';
    VFiles := '';
    VSep := '||';
    for I := 0 to Pred(FUploads.Count) do
    begin
      VFormItem := FUploads[I];
      VFields += VFormItem.FieldName + VSep;
      if not VFormItem.IsFile then
        VValues += VFormItem.Content + VSep;
      if VFormItem.FileName <> ES then
        VFiles += VFormItem.FileName + VSep;
    end;
    Contents.Text := Format(HEAD, [Format(RESULT,
      [VFields,  VValues, VFiles, FUploads.SavePath])]);
  end;

  procedure TCGI.Respond;
  begin
    Contents.Text := Format(HEAD, [FORM]);
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.

