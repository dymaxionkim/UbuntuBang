unit uTester;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
    StdCtrls, ActnList, Process;

type

    { TProcessWrapper }

    TReadEvent = procedure(const aText: PChar; const aSize: integer) of object;

    TProcessWrapper = class
    private
      fProcess: TProcess;
      fOwner: TThread;
      fBuffer: PChar;
      fSize: integer;
      fCommandLine: string;
      fOnRead: TReadEvent;
      procedure AppendData(const aSize: integer);
    public
      constructor Create(aOwner: TThread); reintroduce;
      destructor Destroy; override;
      function Execute: integer;
      property OnRead: TReadEvent read fOnRead write fOnRead;
		end;

    { TProcessThread }

    TProcessThread = class(TThread)
    private
      fWrapper: TProcessWrapper;
      fBuffer: PChar;
      fBufferSize: integer;
      fOnRead: TReadEvent;
      procedure WrapperRead(const aText: PChar; const aSize: integer);
      procedure DoRead;
    protected
      procedure Execute; override;
    public
      constructor Create(const aCommandLine: string);
      property Terminated;
      property OnRead: TReadEvent read fOnRead write fOnRead;
    end;

    { TForm1 }

    TForm1 = class(TForm)
      cmdTerminate: TAction;
      cmdRun: TAction;
      ActionList1: TActionList;
      Button1: TButton;
      Button2: TButton;
      Terminal: TSynEdit;
      procedure cmdRunExecute(Sender: TObject);
      procedure cmdRunUpdate(Sender: TObject);
      procedure cmdTerminateExecute(Sender: TObject);
      procedure cmdTerminateUpdate(Sender: TObject);
      procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
    private
      fBuffer: TStringList;
      fThread: TProcessThread;
      procedure DoRead(const aText: PChar; const aSize: integer);
      procedure DoTerminate(Sender: TObject);
    public
      { public declarations }
    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

const
  BUFF_SIZE = 4096;  // размер буфера символов
  BUFF_COUNT = 80;   // максимальное количество строк на экране


{ TProcessWrapper }

constructor TProcessWrapper.Create(aOwner: TThread);
begin
  inherited Create;
  fOwner:=aOwner;
  fSize:=0;
  fBuffer:=StrAlloc(BUFF_SIZE);
  fProcess:=TProcess.Create(nil);
  fProcess.Options:=[poUsePipes,poStderrToOutPut];
  fProcess.ShowWindow:=swoHide;
end;

destructor TProcessWrapper.Destroy;
begin
  fProcess.Free;
  StrDispose(fBuffer);
  inherited Destroy;
end;

procedure TProcessWrapper.AppendData(const aSize: integer);
begin
  if aSize=0 then
    exit;
  Inc(fSize,aSize);
  if assigned(fOnRead) then
    fOnRead(fBuffer,aSize);
end;

function TProcessWrapper.Execute: integer;
var
  numread: integer;
begin
  if fCommandLine='' then
    exit;
  fProcess.CommandLine:=fCommandLine;
  fProcess.Execute;
  while fProcess.Running do begin
    if TProcessThread(fOwner).Terminated then
      fProcess.Terminate(0);
    if fProcess.Output.NumBytesAvailable=0 then
      Sleep(50)
    else begin
      numread:=fProcess.Output.Read(fBuffer^,BUFF_SIZE);
      AppendData(numread);
    end;
  end;

  repeat
    numread:=fProcess.Output.Read(fBuffer^,BUFF_SIZE);
    AppendData(numread);
  until numread=0;

  if assigned(fOnRead) then
    fOnRead(fBuffer,0);

  result:=fProcess.ExitStatus;
end;


{ TProcessThread }

constructor TProcessThread.Create(const aCommandLine: string);
begin
  FreeOnTerminate:=true;
  inherited Create(true);
  fWrapper:=TProcessWrapper.Create(self);
  fWrapper.fCommandLine:=aCommandLine;
  fWrapper.OnRead:=@WrapperRead;
end;

procedure TProcessThread.DoRead;
begin
  if assigned(fOnRead) then
    fOnRead(fBuffer,fBufferSize);
end;

procedure TProcessThread.WrapperRead(const aText: PChar; const aSize: integer);
begin
  fBuffer:=aText;
  fBufferSize:=aSize;
  Synchronize(@DoRead);
end;

procedure TProcessThread.Execute;
begin
  fWrapper.Execute;
  FreeAndNil(fWrapper);
end;



{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fBuffer:=TStringList.Create;
  fThread:=nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fBuffer.Free;
end;

procedure TForm1.cmdRunExecute(Sender: TObject);
  var
    cmd: string;
    ccx: string;
    path: string;
begin
  // путь к файлу .inp
  path:=ExtractFileNameWithoutExt('/home/zef/Документы/MKraska/test1/static-dynamic-remove-support.inp');
  // путь к ссх
  ccx:='/home/zef/Документы/MKraska/ccx212';
  cmd:=Format('%s "%s"',[ccx,path]);
  fThread:=TProcessThread.Create(cmd);
  fThread.OnRead:=@DoRead;
  fThread.OnTerminate:=@DoTerminate;
  fThread.Start;
  if Terminal.CanFocus then
     Terminal.SetFocus;
end;

procedure TForm1.cmdRunUpdate(Sender: TObject);
begin
  cmdRun.Enabled:=fThread=nil;
end;

procedure TForm1.cmdTerminateExecute(Sender: TObject);
begin
  if fThread=nil then
    exit;
  fThread.Terminate;
  fThread.WaitFor;
  if Terminal.CanFocus then
     Terminal.SetFocus;
end;

procedure TForm1.cmdTerminateUpdate(Sender: TObject);
begin
  cmdTerminate.Enabled:=Assigned(fThread);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  cmdTerminateExecute(nil);
end;

procedure TForm1.DoRead(const aText: PChar; const aSize: integer);
var
  s: string;
begin
  Terminal.Lines.BeginUpdate;
  SetString(s,aText,aSize);
  fBuffer.Text:=s;
  Terminal.Lines.AddStrings(fBuffer);
  while Terminal.Lines.Count>BUFF_COUNT do
    Terminal.Lines.Delete(0);
  Terminal.TopLine:=Terminal.Lines.Count;
  Terminal.CaretXY:=Point(1,Terminal.Lines.Count);
  Terminal.Lines.EndUpdate;
end;

procedure TForm1.DoTerminate(Sender: TObject);
begin
  fThread:=nil;
end;


end.

