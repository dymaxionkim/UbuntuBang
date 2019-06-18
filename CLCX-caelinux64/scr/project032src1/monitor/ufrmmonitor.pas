unit uFrmMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LCLType, FileUtil, FileCtrl, TAGraph, TASeries,
  TATransformations, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  StdCtrls, ComCtrls, ExtCtrls, uMonitor, TAChartAxisUtils, TASources,
  TAFuncSeries, TALegend, TADrawUtils, TAChartUtils, TATools, TAChartExtentLink,
  TANavigation, TATextElements;

type

  { TFrmMonitor }

  TFrmMonitor = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Chart1: TChart;
    Chart2: TChart;
    cmdFileOpen: TAction;
    ChartNavScrollBar1: TChartNavScrollBar;
    cmdMonitorActive: TAction;
    caLog1: TUserDefinedAxisTransform;
    cmdShowLegend: TAction;
    cmdScanTime5: TAction;
    cmdScanTime4: TAction;
    cmdScanTime3: TAction;
    cmdScanTime2: TAction;
    cmdScanTime1: TAction;
    cmdShowHint: TAction;
    IlStatus: TImageList;
    LeftAxisAutoScaleTransform: TAutoScaleAxisTransform;
    LeftAxisTransformations: TChartAxisTransformations;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    pbLeftAxis: TPaintBox;
    pbRightAxis: TPaintBox;
    pnChart1: TPanel;
    pnChart2: TPanel;
    pnConteiner: TPanel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pbProgress: TProgressBar;
    RightAxisAutoScaleTransform: TAutoScaleAxisTransform;
    RightAxisTransformations: TChartAxisTransformations;
    Splitter2: TSplitter;
    srCont: TLineSeries;
    srDisp: TLineSeries;
    srDt: TLineSeries;
    srForce: TLineSeries;
    srStepTime: TLineSeries;
    trScan: TTimer;
    tlHand: TPanDragTool;
    cmdHand: TAction;
    cmdZoomOutAll: TAction;
    tlZoomVer: TZoomDragTool;
    cmdZoomVer: TAction;
    tlZoomIn: TZoomDragTool;
    cmdZoomIn: TAction;
    cmdZoomHor: TAction;
    AlToolbars: TActionList;
    catLog1: TChartAxisTransformations;
    ceLink: TChartExtentLink;
    ChartToolset1: TChartToolset;
    tlZoomHor: TZoomDragTool;
    IlActionsMain: TImageList;
    IlTbNorm: TImageList;
    IlTbHot: TImageList;
    IlTbDisb: TImageList;
    MenuItem3: TMenuItem;
    cmdFileExit: TAction;
    AlMain: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    DlgOpen: TOpenDialog;
    sbMain: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tbSep1: TToolButton;
    ToolButton6: TToolButton;
    procedure BottomAxisGetShape(ASender: TChartTextElement;
      const ABoundingBox: TRect; var APolygon: TPointArray);
    procedure caLog1AxisToGraph(AX: Double; out AT: Double);
    procedure caLog1GraphToAxis(AX: Double; out AT: Double);
    procedure Chart1AxisList0MarkToText(var AText: String; AMark: Double);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Chart2AxisList0MarkToText(var AText: String; AMark: Double);
    procedure cmdFileExitExecute(Sender: TObject);
    procedure cmdFileOpenExecute(Sender: TObject);
    procedure cmdMonitorActiveExecute(Sender: TObject);
    procedure cmdMonitorActiveUpdate(Sender: TObject);
    procedure cmdScanTime1Execute(Sender: TObject);
    procedure cmdScanTime1Update(Sender: TObject);
    procedure cmdShowHintExecute(Sender: TObject);
    procedure cmdShowLegendExecute(Sender: TObject);
    procedure cmdZoomInExecute(Sender: TObject);
    procedure cmdZoomOutAllExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbLeftAxisPaint(Sender: TObject);
    procedure sbMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure trScanTimer(Sender: TObject);
  private
    fIniPath: string;
    fJobName: string;
    fStaFileName: string;
    fCvgFileName: string;
    fStaFileSize: Int64;
    fCvgFileSize: Int64;
    fScanTime: Integer;
    fIteration: Integer;
    fPbRight: Integer;
    fThread: TMonitorThread;
    procedure ThreadProgress(Sender: TMonitorThread; PercentDone: Byte);
    procedure ThreadDone(Sender: TMonitorThread; const Dt, Disp, Force,
              StepTime, Cont: TDoubleArr);
    procedure ThreadTerminate(Sender: TObject);
    procedure PostChartCmd(Data: PtrInt);
  public
    procedure LoadJob(const JobName: string);
  end;

var
  FrmMonitor: TFrmMonitor;

implementation

{$R *.lfm}

uses Math, IniFiles, TAGeometry, uChartCursors;

const

  Ln10 = ln(10);
  LOW_LIMIT: double = 1.0E-100;
  HIGH_LIMIT: double = 1.0E+100;

{ TFrmMonitor }

procedure TFrmMonitor.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  Screen.Cursors[CRS_HAND]:=LoadCursor('CRS_HAND');
  Screen.Cursors[CRS_ZIN]:=LoadCursor('CRS_ZIN');
  Screen.Cursors[CRS_ZIN_H]:=LoadCursor('CRS_ZIN_H');
  Screen.Cursors[CRS_ZIN_V]:=LoadCursor('CRS_ZIN_V');
  tlHand.ActiveCursor:=CRS_HAND;
  tlZoomIn.ActiveCursor:=CRS_ZIN;
  tlZoomVer.ActiveCursor:=CRS_ZIN_V;
  tlZoomHor.ActiveCursor:=CRS_ZIN_H;
  {$IFDEF WINDOWS} // in Linux Legend.Transparency not worked!
  Chart1.Legend.Transparency:=50;
  Chart2.Legend.Transparency:=50;
  {$ENDIF}


  fPbRight:=Width-pbProgress.Left;
  fThread:=nil;
  fIniPath:=ExtractFilePath(Application.ExeName)+'monitor.ini';
  ini:=TIniFile.Create(fIniPath);
  try
    cmdShowHint.Checked:=ini.ReadBool('Options','ShowHint',true);
    cmdShowLegend.Checked:=ini.ReadBool('Options','ShowLegend',true);
    fScanTime:=EnsureRange(ini.ReadInteger('Options','ScanTime',3),1,5);
  finally
    ini.Free;
  end;

  Chart2.Legend.Visible:=Chart1.Legend.Visible;
  cmdShowHintExecute(nil);
  cmdShowLegendExecute(nil);
  cmdScanTime1Execute(TAction(FindComponent(Format('cmdScanTime%d',[fScanTime]))));
  //LoadJob(ParamStr(1)); //if use it as standalon application then uncomment this line
end;

procedure TFrmMonitor.FormDestroy(Sender: TObject);
var
  ini: TIniFile;
begin
  ini:=TIniFile.Create(fIniPath);
  try
    ini.WriteBool('Options','ShowHint',cmdShowHint.Checked);
    ini.WriteBool('Options','ShowLegend',cmdShowLegend.Checked);
    ini.WriteInteger('Options','ScanTime',fScanTime);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TFrmMonitor.FormResize(Sender: TObject);
begin
  with sbMain do
    Panels[1].Width:=(self.Width-fPbRight)-(Panels[0].Width+Panels[2].Width)-4;
end;

procedure TFrmMonitor.FormShow(Sender: TObject);
begin
  tbSep1.Height:=8; // bugfix of vertical toolbar
  cmdHand.Execute;
end;

procedure TFrmMonitor.pbLeftAxisPaint(Sender: TObject);
var
  c: TCanvas;
  s: string;
  x,y: integer;
begin
  case TPaintBox(Sender).Tag of
    1: begin s:='step time'; x:=4; end;
    2: begin s:='# cont. el.'; x:=0; end;
  end;
  c:=TPaintBox(Sender).Canvas;
  c.Font.Orientation:=900;
  y:=20+(TPaintBox(Sender).Height-c.TextWidth(s)) div 2;
  c.TextOut(x,y,s);
end;


procedure TFrmMonitor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  trScan.Enabled:=false;
  if Assigned(fThread) then begin
     fThread.Terminate;
     fThread.WaitFor;
     fThread:=nil;
  end;
end;

procedure TFrmMonitor.cmdFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrmMonitor.cmdFileOpenExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  if not DlgOpen.Execute then exit;
  Application.ProcessMessages;
  LoadJob(ExtractFileNameWithoutExt(DlgOpen.FileName));
  cmdZoomOutAll.Execute;
  trScanTimer(nil);
end;

procedure TFrmMonitor.LoadJob(const JobName: string);
begin
  fJobName:=JobName;
  Chart1.Title.Text.Text:=Format('sta and cvg data of "%s"',[ExtractFileNameOnly(fJobName)]);
  fStaFileName:=fJobName+'.sta';
  fCvgFileName:=fJobName+'.cvg';
  fStaFileSize:=0;
  fCvgFileSize:=0;
  fIteration:=0;
  trScan.Enabled:=true;
  sbMain.InvalidatePanel(0,[ppText]);
  trScanTimer(nil); // <- форсированно построить графики
end;

procedure TFrmMonitor.cmdMonitorActiveExecute(Sender: TObject);
begin
  trScan.Enabled:=not trScan.Enabled;
  sbMain.InvalidatePanel(0,[ppText]);
end;

procedure TFrmMonitor.cmdMonitorActiveUpdate(Sender: TObject);
begin
  cmdMonitorActive.Checked:=trScan.Enabled;
end;

procedure TFrmMonitor.sbMainDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  x,y: integer;
  s: string;
begin
  if Panel.Index=3 then begin
     if pbProgress.Visible then
        s:=Format('%d%%',[pbProgress.Position])
        else
        s:='Ready';
     with sbMain.Canvas do begin
       Brush.Color:=sbMain.Color;
       x:=Rect.Left+pbProgress.Width+8;
       y:=Rect.Top+(Rect.Bottom-Rect.Top-TextHeight(s)) div 2;
       TextOut(x,y,s);
     end;
     Exit;
  end;
  x:=Rect.Left+1;
  if Panel.Index=0 then
     Inc(x,3); // fix ststusbar bug
  y:=Rect.Top+(Rect.Bottom-Rect.Top-IlStatus.Height) div 2;
  IlStatus.Draw(sbMain.Canvas,x,y,Panel.Index);
  case Panel.Index of
    0: if trScan.Enabled then s:=Format('%d sec',[fScanTime]) else s:='No Active';
    1: s:=MiniMizeName(fJobName,sbMain.Canvas,Panel.Width-IlStatus.Width-8);
    2: s:=Format('%d iteration',[fIteration]);
  end;
  inc(x,IlStatus.Width+4);
  with sbMain.Canvas do begin
    Brush.Color:=sbMain.Color;
    y:=Rect.Top+(Rect.Bottom-Rect.Top-TextHeight(s)) div 2;
    TextOut(x,y,s);
  end;
end;

procedure TFrmMonitor.cmdScanTime1Execute(Sender: TObject);
begin
  fScanTime:=TAction(Sender).Tag;
  trScan.Interval:=fScanTime*1000;
  sbMain.InvalidatePanel(0,[ppText]);
end;

procedure TFrmMonitor.cmdScanTime1Update(Sender: TObject);
begin
  TAction(Sender).Checked:=fScanTime=TAction(Sender).Tag;
end;

procedure TFrmMonitor.cmdShowHintExecute(Sender: TObject);
begin
  ShowHint:=cmdShowHint.Checked;
end;

procedure TFrmMonitor.cmdShowLegendExecute(Sender: TObject);
begin
  Chart1.Legend.Visible:=cmdShowLegend.Checked;
  Chart2.Legend.Visible:=cmdShowLegend.Checked;
end;

procedure TFrmMonitor.ThreadProgress(Sender: TMonitorThread; PercentDone: Byte);
begin
  pbProgress.Position:=PercentDone;
  sbMain.InvalidatePanel(3,[ppText]);
end;

procedure TFrmMonitor.ThreadDone(Sender: TMonitorThread; const Dt, Disp, Force,
  StepTime, Cont: TDoubleArr);
begin
  srDt.Clear;
  srDisp.Clear;
  srCont.Clear;
  srForce.Clear;
  srStepTime.Clear;
  srDt.AddArray(Dt);
  srDisp.AddArray(Disp);
  srForce.AddArray(Force);
  srStepTime.AddArray(StepTime);
  srCont.AddArray(Cont);
  fIteration:=Length(Cont);
  Sender.Terminate;
end;

procedure TFrmMonitor.ThreadTerminate(Sender: TObject);
begin
  fThread:=nil;
  pbProgress.Hide;
  sbMain.InvalidatePanel(2,[ppText]);
  sbMain.InvalidatePanel(3,[ppText]);
end;

procedure TFrmMonitor.trScanTimer(Sender: TObject);
var
  f1,f2: Int64;
begin
  // If the monitor thread exists that we wait for the following clock period of the timer
  if Assigned(fThread) then
     Exit;
  // What we do if there are no files
  if (not FileExists(fStaFileName)) or (not FileExists(fCvgFileName)) then
     Exit;
  // If the file size did not change, therefore it does not make a sense
  // to launch a monitor thread
  f1:=FileSize(fStaFileName);
  f2:=FileSize(fCvgFileName);
  if (f1=fStaFileSize) and (f2=fCvgFileSize) then
     Exit;
  fStaFileSize:=f1;
  fCvgFileSize:=f2;

  fThread:=TMonitorThread.Create(fStaFileName,fCvgFileName);
  if Assigned(fThread.FatalException) then
     raise fThread.FatalException;
  fThread.OnProgress:=@ThreadProgress;
  fThread.OnDone:=@ThreadDone;
  fThread.OnTerminate:=@ThreadTerminate;
  fThread.Start;
  pbProgress.Position:=0;
  pbProgress.Show;
  sbMain.InvalidatePanel(3,[ppText]);
end;

procedure TFrmMonitor.Chart1AxisList0MarkToText(var AText: String; AMark: Double);
begin
  AText:=FormatFloat('0.0E-0',AMark,DefaultFormatSettings);
end;

procedure TFrmMonitor.Chart2AxisList0MarkToText(var AText: String; AMark: Double);
begin
  if (AMark>99.99) or (AMark<0.1) then
    AText:=FormatFloat('0.0E-0',AMark,DefaultFormatSettings)
  else
    AText:=FormatFloat('#0.##',AMark,DefaultFormatSettings);
end;

procedure TFrmMonitor.BottomAxisGetShape(ASender: TChartTextElement;
  const ABoundingBox: TRect; var APolygon: TPointArray);
var
  r: TRect;
begin
  r:=ABoundingBox;
  InflateRect(r,5,0);
  APolygon:=TesselateRect(r);
end;

procedure TFrmMonitor.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // bugfix of tool cursor
  TChart(Sender).Cursor:=crDefault;
end;

procedure TFrmMonitor.caLog1AxisToGraph(AX: Double; out AT: Double);
begin
  // bugfix of TLogarithmAxisTransform; ERROR: Floating Point Overflow;
  // NegInfinity replaced -MaxDouble
  if AX>0 then
    AT:=Log10(AX)
  else
    AT:=-HIGH_LIMIT;
end;

procedure TFrmMonitor.caLog1GraphToAxis(AX: Double; out AT: Double);
begin
  // bugfix of TLogarithmAxisTransform; MinDouble<=Range<=MaxDouble
  AT:=EnsureRange(Exp(AX*Ln10),LOW_LIMIT,HIGH_LIMIT);
end;

procedure TFrmMonitor.cmdZoomInExecute(Sender: TObject);
begin
  tlHand.Enabled:=false;
  tlZoomIn.Enabled:=false;
  tlZoomHor.Enabled:=false;
  tlZoomVer.Enabled:=false;
  TAction(Sender).Checked:=true;
  case TAction(Sender).Tag of
    1: tlHand.Enabled:=true;
    2: tlZoomIn.Enabled:=true;
    3: tlZoomHor.Enabled:=true;
    4: tlZoomVer.Enabled:=true;
  end;
end;

procedure TFrmMonitor.cmdZoomOutAllExecute(Sender: TObject);
begin
  ceLink.Enabled:=false;
  try
    Chart1.ZoomFull(true);
    Chart2.ZoomFull(true);
  finally
    Application.QueueAsyncCall(@PostChartCmd,0);
  end;
end;

procedure TFrmMonitor.PostChartCmd(Data: PtrInt);
begin
  ceLink.Enabled:=true;
end;




end.

