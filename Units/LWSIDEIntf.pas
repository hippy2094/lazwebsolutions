(*
  LazWebSolutions, IDE intf unit
  Copyright (C) 2012-2014 Silvio Clecio, Luciano Souza.

  https://github.com/silvioprog/lazwebsolutions

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit LWSIDEIntf;

{$I lazwebsolutions.inc}

interface

uses
  LWSConsts, ProjectIntf, NewItemIntf, LazIDEIntf, Controls, Forms;

type

  { TLWSCustomProjectDescriptor }

  TLWSCustomProjectDescriptor = class(TProjectDescriptor)
  protected
    class function SourceText: string; virtual; abstract;
  public
    constructor Create; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
  end;

  { TLWSCGIProjectDescriptor }

  TLWSCGIProjectDescriptor = class(TLWSCustomProjectDescriptor)
  protected
    class function SourceText: string; override;
  public
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TLWSCGICookiesProjectDescriptor }

  TLWSCGICookiesProjectDescriptor = class(TLWSCustomProjectDescriptor)
  protected
    class function SourceText: string; override;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TLWSCGISessionsProjectDescriptor }

  TLWSCGISessionsProjectDescriptor = class(TLWSCustomProjectDescriptor)
  protected
    class function SourceText: string; override;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

  { TLWSCGIUploadsProjectDescriptor }

  TLWSCGIUploadsProjectDescriptor = class(TLWSCustomProjectDescriptor)
  protected
    class function SourceText: string; override;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;

resourcestring
  SLWSCGIAppName = 'LWS CGI application';
  SLWSCGIAppDescr = 'Create a basic CGI application.' + LineEnding +
    'It''s ideal for projects which don''t require cookies, sessions or uploads.';
  SLWSCGICookiesAppName = 'LWS CGI cookies application';
  SLWSCGICookiesAppDescr =
    'Create a CGI application with cookies support.' + LineEnding +
    'It''s ideal for projects with persistance implemented by means of cookies.';
  SLWSCGISessionsAppName = 'LWS CGI sessions application';
  SLWSCGISessionsAppDescr =
    'Create a CGI application with sessions support.' + LineEnding +
    'It''s ideal for projects with persistance implemented by means of sessions.';
  SLWSCGIUploadsAppName = 'LWS CGI uploads application';
  SLWSCGIUploadsAppDescr =
    'Create a CGI application with uploads support.' + LineEnding +
    'It''s ideal for projects with uploads support.';

procedure Register;

implementation

procedure Register;
begin
  RegisterNewItemCategory(TNewIDEItemCategory.Create(LWS));
  RegisterProjectDescriptor(TLWSCGIProjectDescriptor.Create, LWS);
  RegisterProjectDescriptor(TLWSCGICookiesProjectDescriptor.Create, LWS);
  RegisterProjectDescriptor(TLWSCGISessionsProjectDescriptor.Create, LWS);
  RegisterProjectDescriptor(TLWSCGIUploadsProjectDescriptor.Create, LWS);
end;

{ TLWSCustomProjectDescriptor }

constructor TLWSCustomProjectDescriptor.Create;
begin
  inherited Create;
  Flags := Flags - [pfMainUnitHasCreateFormStatements, pfRunnable,
    pfMainUnitHasTitleStatement] + [pfUseDefaultCompilerOptions];
  Name := SLWSCGIAppName;
end;

function TLWSCustomProjectDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result := LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename, -1, -1,
    [ofProjectLoading, ofRegularFile]);
end;

function TLWSCustomProjectDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  VProject: TLazProjectFile;
begin
  Result := inherited InitProject(AProject);
  VProject := AProject.CreateProjectFile('cgi1.lpr');
  AProject.AddFile(VProject, False);
  AProject.AddPackageDependency('LazWebSolutionsRT');
  VProject.IsPartOfProject := True;
  AProject.LazCompilerOptions.LinkSmart := True;
  AProject.LazCompilerOptions.OptimizationLevel := 2;
{$IFDEF LSNEWLAZARUS}
  AProject.LazCompilerOptions.TargetFilenameApplyConventions := False;
{$ELSE}
  AProject.LazCompilerOptions.TargetFilenameAppplyConventions := False;
{$ENDIF}
  AProject.LazCompilerOptions.UseExternalDbgSyms := True;
  AProject.LazCompilerOptions.UseLineInfoUnit := False;
  AProject.MainFileID := 0;
  AProject.Title := 'cgi1';
  AProject.MainFile.SetSourceText(SourceText);
end;

{ TLWSCGIProjectDescriptor }

function TLWSCGIProjectDescriptor.GetLocalizedName: string;
begin
  Result := SLWSCGIAppName;
end;

function TLWSCGIProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := SLWSCGIAppDescr;
end;

class function TLWSCGIProjectDescriptor.SourceText: string;
var
  LE: ShortString;
begin
  LE := LineEnding;
  Result :=
     'program cgi1;'+LE
    +LE
    +'{$mode objfpc}{$H+}'+LE
    +LE
    +'uses'+LE
    +'  LWSCGI;'+LE
    +LE
    +'type'+LE
    +LE
    +'  { TCGI }'+LE
    +LE
    +'  TCGI = class(TLWSCGI)'+LE
    +'  protected'+LE
    +'    procedure Respond; override;'+LE
    +'  end;'+LE
    +LE
    +'  procedure TCGI.Respond;'+LE
    +'  begin'+LE
    +'    // Your response code here ...'+LE
    +'  end;'+LE
    +LE
    +'begin'+LE
    +'  with TCGI.Create do'+LE
    +'    try'+LE
    +'      Run;'+LE
    +'    finally'+LE
    +'      Free;'+LE
    +'    end;'+LE
    +'end.'+LE;
end;

{ TLWSCGICookiesProjectDescriptor }

constructor TLWSCGICookiesProjectDescriptor.Create;
begin
  inherited Create;
  Name := SLWSCGICookiesAppName;
end;

class function TLWSCGICookiesProjectDescriptor.SourceText: string;
var
  LE: ShortString;
begin
  LE := LineEnding;
  Result :=
     'program cgi1;'+LE
    +LE
    +'{$mode objfpc}{$H+}'+LE
    +LE
    +'uses'+LE
    +'  LWSCGI,'+LE
    +'  LWSCookies;'+LE
    +LE
    +'type'+LE
    +LE
    +'  { TCGI }'+LE
    +LE
    +'  TCGI = class(TLWSCGI)'+LE
    +'  private'+LE
    +'    FCookies: TLWSCookies;'+LE
    +'  protected'+LE
    +'    procedure FillHeaders; override;'+LE
    +'    procedure FillProperties; override;'+LE
    +'    procedure Respond; override;'+LE
    +'  public'+LE
    +'    destructor Destroy; override;'+LE
    +'  end;'+LE
    +LE
    +'  destructor TCGI.Destroy;'+LE
    +'  begin'+LE
    +'    FCookies.Free;'+LE
    +'    inherited Destroy;'+LE
    +'  end;'+LE
    +LE
    +'  procedure TCGI.FillHeaders;'+LE
    +'  begin'+LE
    +'    inherited;'+LE
    +'    Headers.Add(FCookies.Header);'+LE
    +'  end;'+LE
    +LE
    +'  procedure TCGI.FillProperties;'+LE
    +'  begin'+LE
    +'    inherited;'+LE
    +'    FCookies := TLWSCookies.Create(HTTPCookie);'+LE
    +'  end;'+LE
    +LE
    +'  procedure TCGI.Respond;'+LE
    +'  begin'+LE
    +'    // Your response code here ...'+LE
    +'  end;'+LE
    +LE
    +'begin'+LE
    +'  with TCGI.Create do'+LE
    +'    try'+LE
    +'      Run;'+LE
    +'    finally'+LE
    +'      Free;'+LE
    +'    end;'+LE
    +'end.'+LE;
end;

function TLWSCGICookiesProjectDescriptor.GetLocalizedName: string;
begin
  Result := SLWSCGICookiesAppName;
end;

function TLWSCGICookiesProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := SLWSCGICookiesAppDescr;
end;

{ TLWSCGISessionsProjectDescriptor }

constructor TLWSCGISessionsProjectDescriptor.Create;
begin
  inherited Create;
  Name := SLWSCGISessionsAppName;
end;

class function TLWSCGISessionsProjectDescriptor.SourceText: string;
var
  LE: ShortString;
begin
  LE := LineEnding;
  Result :=
     'program cgi1;'+LE
    +LE
    +'{$mode objfpc}{$H+}'+LE
    +LE
    +'uses'+LE
    +'  LWSCGI,'+LE
    +'  LWSSessions;'+LE
    +LE
    +'type'+LE
    +LE
    +'  { TCGI }'+LE
    +LE
    +'  TCGI = class(TLWSCGI)'+LE
    +'  private'+LE
    +'    FSessions: TLWSSessions;'+LE
    +'  protected'+LE
    +'    procedure FillHeaders; override;'+LE
    +'    procedure FillProperties; override;'+LE
    +'    procedure Respond; override;'+LE
    +'  public'+LE
    +'    destructor Destroy; override;'+LE
    +'  end;'+LE
    +LE
    +'  destructor TCGI.Destroy;'+LE
    +'  begin'+LE
    +'    FSessions.Free;'+LE
    +'    inherited Destroy;'+LE
    +'  end;'+LE
    +LE
    +'  procedure TCGI.FillHeaders;'+LE
    +'  begin'+LE
    +'    inherited;'+LE
    +'    Headers.Add(FSessions.Header);'+LE
    +'  end;'+LE
    +LE
    +'  procedure TCGI.FillProperties;'+LE
    +'  begin'+LE
    +'    inherited;'+LE
    +'    FSessions := TLWSSessions.Create(HTTPCookie);'+LE
    +'  end;'+LE
    +LE
    +'  procedure TCGI.Respond;'+LE
    +'  begin'+LE
    +'    FSessions.Start;'+LE
    +'    // Your response code here ...'+LE
    +'  end;'+LE
    +LE
    +'begin'+LE
    +'  with TCGI.Create do'+LE
    +'    try'+LE
    +'      Run;'+LE
    +'    finally'+LE
    +'      Free;'+LE
    +'    end;'+LE
    +'end.'+LE;
end;

function TLWSCGISessionsProjectDescriptor.GetLocalizedName: string;
begin
  Result := SLWSCGISessionsAppName;
end;

function TLWSCGISessionsProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := SLWSCGISessionsAppDescr;
end;

{ TLWSCGIUploadsProjectDescriptor }

constructor TLWSCGIUploadsProjectDescriptor.Create;
begin
  inherited Create;
  Name := SLWSCGIUploadsAppName;
end;

class function TLWSCGIUploadsProjectDescriptor.SourceText: string;
var
  LE: ShortString;
begin
  LE := LineEnding;
  Result :=
    'program cgi1;'+LE
   +LE
   +'{$mode objfpc}{$H+}'+LE
   +LE
   +'uses'+LE
   +'  LWSCGI,'+LE
   +'  LWSUploads,'+LE
   +'  Classes;'+LE
   +LE
   +'type'+LE
   +LE
   +'  { TCGI }'+LE
   +LE
   +'  TCGI = class(TLWSCGI)'+LE
   +'  private'+LE
   +'    FUploads: TLWSUploads;'+LE
   +'  protected'+LE
   +'    procedure FillUploads(AData: TMemoryStream); override;'+LE
   +'    procedure Request; override;'+LE
   +'    procedure Respond; override;'+LE
   +'  public'+LE
   +'    constructor Create; override;'+LE
   +'    destructor Destroy; override;'+LE
   +'  end;'+LE
   +LE
   +'  constructor TCGI.Create;'+LE
   +'  begin'+LE
   +'    inherited Create;'+LE
   +'    FUploads := TLWSUploads.Create;'+LE
   +'  end;'+LE
   +LE
   +'  destructor TCGI.Destroy;'+LE
   +'  begin'+LE
   +'    FUploads.Free;'+LE
   +'    inherited Destroy;'+LE
   +'  end;'+LE
   +LE
   +'  procedure TCGI.FillUploads(AData: TMemoryStream);'+LE
   +'  begin'+LE
   +'    inherited;'+LE
   +'    FUploads.ReadUploads(AData, @Fields, ContentType);'+LE
   +'  end;'+LE
   +LE
   +'  procedure TCGI.Request;'+LE
   +'  begin'+LE
   +'    // Your request code here ...'+LE
   +'  end;'+LE
   +LE
   +'  procedure TCGI.Respond;'+LE
   +'  begin'+LE
   +'    // Your response code here ...'+LE
   +'  end;'+LE
   +LE
   +'begin'+LE
   +'  with TCGI.Create do'+LE
   +'    try'+LE
   +'      Run;'+LE
   +'    finally'+LE
   +'      Free;'+LE
   +'    end;'+LE
   +'end.'+LE;
end;

function TLWSCGIUploadsProjectDescriptor.GetLocalizedName: string;
begin
  Result := SLWSCGIUploadsAppName;
end;

function TLWSCGIUploadsProjectDescriptor.GetLocalizedDescription: string;
begin
  Result := SLWSCGIUploadsAppDescr;
end;

end.

