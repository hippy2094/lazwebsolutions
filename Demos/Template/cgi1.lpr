program cgi1;

{$mode objfpc}{$H+}

uses
  LWSCGI,
  LWSConsts,
  LWSActionView;

type

  { TCGI }

  TCGI = class(TLWSCGI)
  private
    FTemplate: TLWSActionView;
  protected
    procedure Init; override;
    procedure Finit; override;
    procedure Respond; override;
  end;

  procedure TCGI.Init;
  begin
    FTemplate := TLWSActionView.Create(
      ['body', '', 'div.body', 'Test', 'title', 'Hello'],
      '/home/your_username/your_project/view', 'main.lws.html');
  end;

  procedure TCGI.Finit;
  begin
    FTemplate.Free;
  end;

  procedure TCGI.Respond;
  begin
    FTemplate['body'].AsString := 'Hello world!';
    FTemplate['div.body'].AsString := BR + 'Test!';
    Contents.Text := FTemplate.Content;
  end;

begin
  with TCGI.Create do
    try
      Run;
    finally
      Free;
    end;
end.
