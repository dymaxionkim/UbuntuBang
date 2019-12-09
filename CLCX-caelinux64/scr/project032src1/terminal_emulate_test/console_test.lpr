program console_test;

{$mode objfpc}{$H+}
{$IFDEF LINUX}
{$DEFINE UseCThreads}
{$ENDIF}
{$IFDEF UNIX}
{$DEFINE UseCThreads}
{$ENDIF}

uses
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, uTester
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Initialize;
		Application.CreateForm(TForm1, Form1);
    Application.Run;
end.

