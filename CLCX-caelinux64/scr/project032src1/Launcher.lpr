program Launcher;

//removed after adding monitor (need to divide memory)
{{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF} }

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
  Forms, tachartlazaruspkg, runtimetypeinfocontrols, Unit1, Unit3, Unit4,
  uMain, uHighliter, ufiletransform, uSettings, uGraph, uMathParser,
  uRender, uMonitor, uFrmMonitor, uDatStat, uChartCursors,
  uVtkpost, ueigval, uHelpTools
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);

  Application.CreateForm(TFrMain, FrMain);
  Application.CreateForm(TFrmGraph, FrmGraph);
  Application.CreateForm(TFrmVtkpost, FrmVtkpost);
  Application.CreateForm(TFrmMonitor, FrmMonitor);
  Application.CreateForm(TFrmDatStat, FrmDatStat);
  Application.Run;
end.

