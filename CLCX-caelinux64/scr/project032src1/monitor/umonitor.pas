unit uMonitor;

interface

uses Classes, Sysutils;

type

  EMonitor = class(Exception);
  TMonitorThread = class;
  TDoubleArr = array of Double;
  TStaItem = array[0..6] of Double;
  { Step - 0
    Inc - 1
    Att - 2
    Iter - 3
    TotTime - 4
    StepTime - 5
    IncTime - 6 }

  TCvgItem = array[0..8] of Double;
  { Step - 0
    Inc - 1
    Att - 2
    Iter - 3
    ContEl - 4
    ResidForce - 5
    CorrDisp - 6
    ResidFlux - 7
    CorrTemp - 8 }

  TProgressEvent = procedure(Sender: TMonitorThread; PercentDone: Byte) of object;
  TDoneEvent = procedure(Sender: TMonitorThread; const Dt, Disp, Force,
             StepTime, Cont: TDoubleArr) of object;

  { TMonitorThread }

  TMonitorThread = class(TThread)
  private
    fStaFileName: string;
    fCvgFileName: string;
    fDt: TDoubleArr;
    fDisp: TDoubleArr;
    fForce: TDoubleArr;
    fStepTime: TDoubleArr;
    fCont: TDoubleArr;
    fPercentDone: byte;
    fOnProgress: TProgressEvent;
    fOnDone: TDoneEvent;
    procedure DoDone;
    procedure DoProgress;
    procedure ParseSta(s: string; out sta: TStaItem);
    procedure ParseCvg(s: string; out cvg: TCvgItem);
  protected
    procedure Execute; override;
  public
    constructor Create(const aStaFileName, aCvgFileName: string);
    destructor Destroy; override;
    property OnProgress: TProgressEvent read fOnProgress write fOnProgress;
    property OnDone: TDoneEvent read fOnDone write fOnDone;
  end;

implementation

// This function allows reading busy files
function LoadStringsFromFile(const FileName: string; list: TStrings): boolean;
var
  f: TFileStream;
begin
  result:=false;
  try
  f:=TFileStream.Create(FileName,fmShareDenyNone);
	except
    exit;
	end;
  try
    try
      list.LoadFromStream(f);
  	except
      exit;
  	end;
	finally
    f.Free;
	end;
  result:=true;
end;

{ TMonitorThread }

constructor TMonitorThread.Create(const aStaFileName, aCvgFileName: string);
begin
  fStaFileName:=aStaFileName;
  fCvgFileName:=aCvgFileName;
  fPercentDone:=0;
  FreeOnTerminate:=true;
  inherited Create(true);
end;

destructor TMonitorThread.Destroy;
begin
  // finalize arrays
  fDt:=nil;
  fDisp:=nil;
  fForce:=nil;
  fStepTime:=nil;
  fCont:=nil;
  inherited Destroy;
end;

procedure TMonitorThread.DoDone;
begin
  if Assigned(fOnDone) then
    fOnDone(self,fDt,fDisp,fForce,fStepTime,fCont);
end;

procedure TMonitorThread.DoProgress;
begin
  if Assigned(fOnProgress) then
    fOnProgress(self,fPercentDone);
end;

procedure TMonitorThread.ParseSta(s: string; out sta: TStaItem);
var
  i,p,e: integer;
  n: string;
begin
for i:=0 to 6 do begin
  s:=TrimLeft(s);
  p:=Pos(' ',s);
  if p=0 then
    n:=s
    else
    n:=Copy(s,1,p-1);
  Val(n,sta[i],e);
  if (e<>0) and (i<>2) then begin
    //col 2 (ATT) can be like as '1U'
    FillChar(sta,SizeOf(sta),0);
    for p:=0 to 2 do
       sta[p]:=1;
    Exit;
  end;
  Delete(s,1,p);
end;
end;

procedure TMonitorThread.ParseCvg(s: string; out cvg: TCvgItem);
var
  i,p,e: integer;
  n: string;
begin
  for i:=0 to 8 do begin
    s:=TrimLeft(s);
    p:=Pos(' ',s);
    if p=0 then
      n:=s
      else
      n:=Copy(s,1,p-1);
    Val(n,cvg[i],e);
    if e<>0 then begin
      FillChar(cvg,SizeOf(cvg),0);
      Exit;
    end;
    Delete(s,1,p);
  end;
end;

procedure TMonitorThread.Execute;
var
  StaList: TStringList;
  CvgList: TStringList;
  sta: array of TStaItem;
  cvg: TCvgItem;
  kProgress: double;
  i,j,p,cnt,iters,step,incr,imax: integer;
  str: TFileStream;
begin
  StaList:=TStringList.Create;
  try
    {$IFDEF WINDOWS}
    if not LoadStringsFromFile(fStaFileName,StaList) then begin
      Terminate;
      Exit;
		end;
		{$ELSE}
    try
    StaList.LoadFromFile(fStaFileName);
    except
      Terminate;
      Exit;
    end;
    {$ENDIF}
    cnt:=StaList.Count-2;
    if cnt<1 then
      exit;
    SetLength(sta,cnt);
    for i:=2 to StaList.Count-1 do
       ParseSta(StaList[i],sta[i-2]);
  finally
    StaList.Free;
  end;

  CvgList:=TStringList.Create;
  try
    {$IFDEF WINDOWS}
    if not LoadStringsFromFile(fCvgFileName,CvgList) then begin
      Terminate;
      Exit;
	  end;
	  {$ELSE}
    try
    CvgList.LoadFromFile(fCvgFileName);
    except
      Terminate;
      Exit;
    end;
    {$ENDIF}
  iters:=CvgList.Count-4;
  if iters<1 then
    exit;
  SetLength(fDt,iters);
  SetLength(fDisp,iters);
  SetLength(fForce,iters);
  SetLength(fStepTime,iters);
  SetLength(fCont,iters);
  imax:=0;  p:=0;
  if iters>0 then
     kProgress:=100/iters;
  for i:=0 to iters-1 do begin
    ParseCvg(CvgList[i+4],cvg);
    if cvg[5]=0 then
      cvg[5]:=0.5;
    step:=Trunc(cvg[0]);
    incr:=Trunc(cvg[1]);
    fDisp[i]:=cvg[6];
    fForce[i]:=cvg[5];
    fCont[i]:=cvg[4];
    for j:=0 to cnt-1 do
      if (step=Trunc(sta[j,0])) and (incr=Trunc(sta[j,1])) then begin
         fDt[i]:=sta[j,6];
         fStepTime[i]:=sta[j,5];
         imax:=i;
         if Terminated then
           break;
      end;
    if Terminated then
       break;
    p:=Trunc(kProgress*(i+1));
    if p>fPercentDone then begin
       fPercentDone:=p;
       Synchronize(@DoProgress);
    end;
  end;
  finally
    CvgList.Free;
  end;
  sta:=nil;
  // Slice arrays
  SetLength(fDt,imax);
  SetLength(fDisp,imax);
  SetLength(fForce,imax);
  SetLength(fStepTime,imax);
  SetLength(fCont,imax);

  // Display result
  Synchronize(@DoDone);
end;


end.
