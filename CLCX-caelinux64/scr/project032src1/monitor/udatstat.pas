unit uDatStat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, FileCtrl, TAGraph, TASeries, Forms, Controls,
  Graphics, Dialogs, Menus, ActnList, ComCtrls, StdCtrls, Buttons, ExtCtrls,
  Contnrs, LCLType, TAChartAxisUtils, TANavigation, TATools, Types,
  TATextElements, TAChartUtils;

type

  TCol = 1..3;
  TVal = array[TCol] of double;

  { TDatItem }

  TDatItem = class
    Sum: TVal;
    Name: string;
    Time: double;
    Empty: boolean;
    constructor Create;
  end;

  { TFrmDatStat }

  TFrmDatStat = class(TForm)
    AlMain: TActionList;
    AlToolbars: TActionList;
    Bevel1: TBevel;
    cbColumns: TComboBox;
    cbSetNames: TComboBox;
    Chart1: TChart;
    scrChart: TChartNavScrollBar;
    ChartToolset1: TChartToolset;
    cmdFileExit: TAction;
    cmdFileOpen: TAction;
    cmdHand: TAction;
    cmdShowHint: TAction;
    cmdZoomHor: TAction;
    cmdZoomIn: TAction;
    cmdZoomOutAll: TAction;
    cmdZoomVer: TAction;
    IlActionsMain: TImageList;
    IlStatus: TImageList;
    IlTbDisb: TImageList;
    IlTbHot: TImageList;
    IlTbNorm: TImageList;
    lbSetNames: TLabel;
    lbColumns: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    dlgOpenDat: TOpenDialog;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    pnToolbar: TPanel;
    pnChart: TPanel;
    pbProcess: TProgressBar;
    SpeedButton1: TSpeedButton;
    sbMain: TStatusBar;
    srSum: TLineSeries;
    tbSep1: TToolButton;
    tlHand: TPanDragTool;
    tlZoomHor: TZoomDragTool;
    tlZoomIn: TZoomDragTool;
    tlZoomVer: TZoomDragTool;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    procedure cbSetNamesChange(Sender: TObject);
    procedure Chart1AxisList0MarkToText(var AText: String; AMark: Double);
    procedure Chart1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmdFileExitExecute(Sender: TObject);
    procedure cmdFileOpenExecute(Sender: TObject);
    procedure cmdHandExecute(Sender: TObject);
    procedure cmdHandUpdate(Sender: TObject);
    procedure cmdShowHintExecute(Sender: TObject);
    procedure cmdZoomOutAllExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbMainDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure BottomAxisGetShape(ASender: TChartTextElement;
      const ABoundingBox: TRect; var APolygon: TPointArray);
  private
    fIniPath: string;
    fFileName: string;
    fDat: TStringList;
    fItems: TObjectList;
    fPercentDone: byte;
    procedure SetPercentDone(AValue: byte);
    procedure UpdateActions;
    procedure UpdateChart(Data: PtrInt);
  public
    procedure LoadDat(const aFileName: string);
    property PercentDone: byte read fPercentDone write SetPercentDone;
  end;

var
  FrmDatStat: TFrmDatStat;

implementation

{$R *.lfm}

uses IniFiles, TAGeometry, uChartCursors;

resourcestring
  ERR_PARSE = 'This file "%s" does not contain data for display.';


function ClearStr(const s: string): string;
var
  i: integer;
begin
  Result:=s;
  for i:=1 to Length(result) do
    if result[i]=#9 then
      result[i]:=' ';
  i:=1;
  while (i<=Length(Result)) do begin
    if ((Result[i]=' ') and (Result[i+1]=' ')) then begin
      Delete(Result,i,1);
      Dec(i);
    end;
    Inc(i);
  end;
end;

function ParseSetStr(const s: string; out Name: string; out Time: double): boolean;
var
  p1,p2: integer;
  a: string;
begin
  result:=false;
  a:=LowerCase(s);
  p1:=Pos('for set',a);
  if p1=0 then exit;
  p2:=Pos('and time',a);
  if (p2=0) or (p1>p2) then exit;
  a:=s;
  Delete(a,1,p1+7);
  p1:=Pos(' ',a);
  Name:=Copy(a,1,p1-1);
  p2:=Pos('and time',a);
  Delete(a,1,p2+8);
  Val(a,Time,p2);
  result:=p2=0;
end;

function ParseParamStr(s: string; d: TDatItem): boolean;
var
  i: TCol;
  n: string;
  p,x,e: integer;
  v: double;
begin
  result:=false;
  p:=Pos(' ',s);
  if p=0 then
    Exit;
  n:=Copy(s,1,p-1);
  Val(n,x,e);
  if e=0 then begin
    Delete(s,1,p);
  end else begin
    Val(n,v,e);
    if e<>0 then
      Exit;
	end;
  for i in TCol do begin
    p:=Pos(' ',s);
    if p=0 then
       n:=s
    else
       n:=Copy(s,1,p-1);
    if Length(n)=0 then
       Exit;
    Val(n,v,e);
    if e<>0 then
       Exit;
    d.Sum[i]+=v;
    if p>0 then
       Delete(s,1,p);
  end;
  d.Empty:=false;
  result:=true;
end;

{ TDatItem }

constructor TDatItem.Create;
begin
  Time:=0;
  Empty:=true;
  FillChar(Sum,SizeOf(Sum),0);
end;

{ TFrmDatStat }

procedure TFrmDatStat.FormCreate(Sender: TObject);
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
  fDat:=TStringList.Create;
  fItems:=TObjectList.Create;
  fIniPath:=ExtractFilePath(Application.ExeName)+'datstat.ini';
  ini:=TIniFile.Create(fIniPath);
  try
    cmdShowHint.Checked:=ini.ReadBool('Options','ShowHint',true);
  finally
    ini.Free;
  end;
  fFileName:='No file specified';
  cmdShowHintExecute(nil);
  sbMain.Panels[1].Text:='';
end;

procedure TFrmDatStat.FormDestroy(Sender: TObject);
var
  ini: TIniFile;
begin
  ini:=TIniFile.Create(fIniPath);
  try
    ini.WriteBool('Options','ShowHint',cmdShowHint.Checked);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
  fDat.Free;
  fItems.Free;
end;

procedure TFrmDatStat.FormShow(Sender: TObject);
begin
  tbSep1.Height:=8;
  tlHand.Enabled:=true;
  Application.QueueAsyncCall(@UpdateChart,0);
end;

procedure TFrmDatStat.sbMainDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  x,y,w: integer;
  s: string;
begin
  if Panel.Index=2 then
     with sbMain.Canvas do begin
       w:=Rect.Right-Rect.Left-IlStatus.Width-20;
       s:=MiniMizeName(fFileName,sbMain.Canvas,w);
       x:=Rect.Left+1;
       y:=Rect.Top+(Rect.Bottom-Rect.Top-IlStatus.Height) div 2;
       IlStatus.Draw(sbMain.Canvas,x,y,0);
       Brush.Color:=sbMain.Color;
       inc(x,IlStatus.Width+4);
       y:=Rect.Top+(Rect.Bottom-Rect.Top-TextHeight(s)) div 2;
       TextOut(x,y,s);
     end;
end;

procedure TFrmDatStat.BottomAxisGetShape(ASender: TChartTextElement;
  const ABoundingBox: TRect; var APolygon: TPointArray);
var
  r: TRect;
begin
  r:=ABoundingBox;
  InflateRect(r,4,0);
  APolygon:=TesselateRect(r);
end;

procedure TFrmDatStat.SetPercentDone(AValue: byte);
begin
  if fPercentDone=AValue then Exit;
  fPercentDone:=AValue;
  pbProcess.Position:=fPercentDone;
  sbMain.Panels[1].Text:=Format('%d%% ',[fPercentDone]);
  Application.ProcessMessages;
end;

procedure TFrmDatStat.UpdateActions;

  procedure UpdateActionList(ActionList: TActionList);
  var
    i: integer;
  begin
    for i:=0 to ActionList.ActionCount-1 do
      ActionList.Actions[i].Update;
  end;

begin
  UpdateActionList(AlMain);
  UpdateActionList(AlToolbars);
end;

procedure TFrmDatStat.UpdateChart(Data: PtrInt);
begin
  Chart1.Enabled:=Boolean(Data);
  scrChart.Enabled:=Chart1.Enabled;
  tlHand.MouseUp(Point(0,0));
end;

procedure TFrmDatStat.cmdFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrmDatStat.cmdShowHintExecute(Sender: TObject);
begin
  ShowHint:=cmdShowHint.Checked;
end;

procedure TFrmDatStat.LoadDat(const aFileName: string);
var
  s,n: string;
  i,e: integer;
  Parsing: boolean;
  d: TDatItem;
  kProgress,t: double;
  j: TCol;
begin
  fFileName:=aFileName;
  sbMain.InvalidatePanel(2,[ppText]);
  pbProcess.Show;
  sbMain.Panels[0].Text:='Parsing...';
  fItems.Clear;
  cbSetNames.Clear;
  srSum.Clear;
  UpdateActions;
  cbSetNames.Enabled:=false;
  lbSetNames.Enabled:=false;
  cbColumns.Enabled:=false;
  lbColumns.Enabled:=false;
  Chart1.Enabled:=false;
  scrChart.Enabled:=false;
  cbColumns.ItemIndex:=0;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  try
    fDat.LoadFromFile(aFileName);
    Parsing:=false;
    fPercentDone:=0;
    if fDat.Count>0 then
      kProgress:=100/fDat.Count;
    for i:=0 to fDat.Count-1 do begin
      PercentDone:=Round(kProgress*i);
      s:=Trim(fDat[i]);

      if Length(s)=0 then
        Continue;
      s:=ClearStr(s);
      if ParseSetStr(s,n,t) then begin
        d:=TDatItem.Create;
        d.Name:=n;
        d.Time:=t;
        FillChar(d.Sum,SizeOf(d.Sum),0);
        fItems.Add(d);
        Parsing:=true;
        Continue;
      end
      else
        if parsing then begin
          if not ParseParamStr(s,d) then begin
            Parsing:=false;
            if d.Empty then
               fItems.Remove(d);
            Continue;
          end;
        end
        else
          Parsing:=false;
    end;

    if fItems.Count>0 then begin
      cbSetNames.Items.BeginUpdate;
      try
        cbSetNames.Items.Clear;
        for i:=0 to fItems.Count-1 do begin
          d:=fItems[i] as TDatItem;
          if cbSetNames.Items.IndexOf(d.Name)<0 then
             cbSetNames.Items.Add(d.Name);
        end;

      finally
        cbSetNames.Items.EndUpdate;
      end;

      if cbSetNames.Items.Count>0 then begin
         cbSetNames.ItemIndex:=0;
         cbSetNamesChange(nil);
      end;
    end;

  finally
    Screen.Cursor:=crDefault;
    pbProcess.Hide;
    sbMain.Panels[0].Text:='Done';
    sbMain.Panels[1].Text:='';
    cbSetNames.Enabled:=cbSetNames.Items.Count>0;
    lbSetNames.Enabled:=true;
    cbColumns.Enabled:=cbSetNames.Enabled;
    lbColumns.Enabled:=true;
    Application.QueueAsyncCall(@UpdateChart,Ord(cbSetNames.Enabled));
  end;
  if fItems.Count<1 then begin
     Application.MessageBox(PChar(Format(ERR_PARSE,[fFileName])),'Warning',MB_ICONWARNING);
  end;

end;

procedure TFrmDatStat.cmdFileOpenExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  if not dlgOpenDat.Execute then
     Exit;
  Application.ProcessMessages;
  LoadDat(dlgOpenDat.FileName);
end;

procedure TFrmDatStat.cmdHandExecute(Sender: TObject);
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

procedure TFrmDatStat.cmdHandUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=fItems.Count>1;
end;


procedure TFrmDatStat.cmdZoomOutAllExecute(Sender: TObject);
begin
  Chart1.ZoomFull(true);
end;

procedure TFrmDatStat.cbSetNamesChange(Sender: TObject);
var
  sn: string;
  Col,i: integer;
  d: TDatItem;
begin
  if (cbSetNames.ItemIndex<0) or (fItems.Count<1) then
     Exit;
  Chart1.Extent.UseXMin:=false;
  sn:=cbSetNames.Items[cbSetNames.ItemIndex];
  Col:=cbColumns.ItemIndex+1;
  srSum.Clear;
  srSum.AddXY(0,0);
  for i:=0 to fItems.Count-1 do begin
    d:=fItems[i] as TDatItem;
    if d.Name<>sn then continue;
    srSum.AddXY(d.Time,d.Sum[Col]);
  end;
  Chart1.ZoomFull();
  Chart1.Title.Text.Text:=Format('sum %s for column%d',[sn,Col]);
  Chart1.Title.Visible:=true;
end;


procedure TFrmDatStat.Chart1AxisList0MarkToText(var AText: String; AMark: Double);
begin
  AText:=FormatFloat('0.0E-0',AMark,DefaultFormatSettings);
end;

procedure TFrmDatStat.Chart1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // bugfix of tool cursor
  TChart(Sender).Cursor:=crDefault;
end;


end.

