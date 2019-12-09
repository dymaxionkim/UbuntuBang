unit Unit1;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, FileUtil, IpHtml, RTTICtrls, Forms, Controls, Graphics,
  Dialogs, EditBtn, StdCtrls, Menus, ComCtrls, Process, Unit2, Unit3, Unit4,
  uMain, uVtkpost, uHighliter, lclintf, ExtDlgs, ExtCtrls, uFrmMonitor,
  uDatSTat, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CalculatorDialog1: TCalculatorDialog;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    Edit1: TEdit;
    FileNameEdit1: TFileNameEdit;
    ilHlpTree: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ListBox2: TListBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    pnInfoView: TIpHtmlPanel;
    PopupMenu1: TPopupMenu;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    tvExamples: TTreeView;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox2DblClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem26Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure pnInfoViewHotClick(Sender: TObject);
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure tvExamplesDblClick(Sender: TObject);
    procedure tvExamplesDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvExamplesSelectionChanged(Sender: TObject);
  private
    { private declarations }
    procedure OpenEditor(const aFileName: string);
  public
    { public declarations }
    procedure  RunAppWithOutput(commandstringline:string)  ;
    procedure  RunApp(commandstringline:string)  ;
  end;

var
  Form1: TForm1;
  AProcess: TProcess;
  ExeAppPath,PathToIni,Path2CCX,Path2CGX,Terminal,TextEditor,PyPath,Workdir: String;
  inifile,shfile: TextFile;
  WizardCase: integer;

implementation

uses
  uFileTransform, uHelpTools;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FillHlpTree(tvExamples);
  pnInfoView.DefaultFontSize:=Font.Size;
end;

procedure TForm1.OpenEditor(const aFileName: string);
begin
SetCurrentDir(ExtractFileDir(aFileName));
with TFrMain.Create(Application) do begin
LoadFile(aFileName);
Show;
end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
newpathforclcx,Path2CCXl,commandtext:String;

begin

  //Initial checks
  If  FileNameEdit1.Text='' Then
  begin
  ShowMessage('Select INP file!');
  exit;
  end;

{$IFDEF LINUX}

newpathforclcx:= FileNameEdit1.Text;
if ComboBox3.Text='ccx2.8p2-oldstable' Then Path2CCXl:=  ExeAppPath + 'bin/ccx28p2';
if ComboBox3.Text='ccx2.12-stable' Then Path2CCXl:=  ExeAppPath + 'bin/ccx212';
if ComboBox3.Text='ccx2.13' Then Path2CCXl:=  ExeAppPath + 'bin/ccx213';
if (Path2CCX='') And (ComboBox3.Text='custom') Then  ShowMessage('Setup custom path for CCX at Settings!');
if (Path2CCX<>'') And (ComboBox3.Text='custom') Then Path2CCXl:=  Path2CCX;
If  newpathforclcx='' Then ShowMessage('Select CalculiX INP file!' )

Else
begin
//Remove Extention from the .INP Filepath&FileName
delete(newpathforclcx,length(FileNameEdit1.Text)-3,4);

SetCurrentDir(ExtractFileDir(FileNameEdit1.Text));


If Terminal=''
then commandtext:='xterm -hold -e '+ Path2CCXl+ ' '+newpathforclcx
else  commandtext:='/usr/bin/'+Terminal+' '+'''--command='+Path2CCXl+ ' '+newpathforclcx +''''+' --hold';

//Finally
RunApp(commandtext);

end;
{$ENDIF}

{$IFDEF WINDOWS}

newpathforclcx:= FileNameEdit1.Text;
if (Path2CCX='') And (ComboBox3.Text='ccx2.8p2-oldstable') Then Path2CCXl:=  ExeAppPath + 'bin/ccx/ccx28p2.exe';
if (Path2CCX='') And (ComboBox3.Text='ccx2.9') Then Path2CCXl:=  ExeAppPath + 'bin/ccx/ccx29.exe';
if (Path2CCX='') And (ComboBox3.Text='ccx2.12') Then Path2CCXl:=  ExeAppPath + 'bin/ccx/ccx212.exe';
if (Path2CCX='') And (ComboBox3.Text='ccx2.11-stable') Then Path2CCXl:=  ExeAppPath + 'bin/ccx/ccx211.exe';
if (Path2CCX='') And (ComboBox3.Text='ccx2.12-stable') Then Path2CCXl:=  ExeAppPath + 'bin/ccx/ccx212.exe';
if (Path2CCX='') And (ComboBox3.Text='ccx2.13') Then Path2CCXl:=  ExeAppPath + 'bin/ccx/ccx213.exe';
If  newpathforclcx='' Then ShowMessage('Select CalculiX INP file!' )

Else
begin

//Remove Extention from the .INP Filepath&FileName
delete(newpathforclcx,length(FileNameEdit1.Text)-3,4);

SetCurrentDir(ExtractFileDir(FileNameEdit1.Text));

commandtext:='cmd /k '+'"'+Path2CCXl+' '+newpathforclcx+'"';
//ShowMessage(commandtext);

//Finally
RunApp(commandtext);

end;
{$ENDIF}

 end;

procedure TForm1.Button10Click(Sender: TObject);
begin
  MenuItem27.Click ;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
      Form4.Memo1.Lines.LoadFromFile(ExeAppPath + 'hlp/tips/wizardtips');
    Form4.Showmodal;
end;

procedure TForm1.Button12Click(Sender: TObject);
begin
  MenuItem21.Click;
end;

procedure TForm1.Button13Click(Sender: TObject);
begin
  FrmMonitor:=TFrmMonitor.Create(Application);
try
FrmMonitor.LoadJob(ExtractFileNameWithoutExt(FileNameEdit1.FileName));
FrmMonitor.ShowModal;
finally
FrmMonitor.Release;
end;
end;

procedure TForm1.Button14Click(Sender: TObject);
begin

//Initial checks
If  FileNameEdit1.Text='' Then
begin
ShowMessage('Select .dat file with written reaction forces, etc!');
exit;
end;

FrmDatSTat:=TFrmDatSTat.Create(Application);
try
//FrmDatSTat.LoadDat(ExtractFileNameWithoutExt(FileNameEdit1.FileName)+ '.dat');
FrmDatStat.LoadDat(ChangeFileExt(FileNameEdit1.FileName, '.dat'));
FrmDatSTat.ShowModal;
finally
FrmDatSTat.Release;
end;
end;



procedure TForm1.Button2Click(Sender: TObject);
var
  AStringList: TStringList;
  commandtext:String;
  newpathforclcx,Path2CGXl:String;
begin

//Initial checks
If  FileNameEdit1.Text='' Then
begin
ShowMessage('Select file!');
exit;
end;

newpathforclcx:= FileNameEdit1.Text;
{$IFDEF LINUX}
if Path2CGX='' Then Path2CGXl:=  ExeAppPath + 'bin/cgx2.12';

If  newpathforclcx='' Then ShowMessage('Select CalculiX INP file!' )

else
 begin
   SetCurrentDir(ExtractFileDir(newpathforclcx));

If Terminal=''
then  commandtext:='xterm -e ' +Path2CGXl+ ' '+ComboBox1.Text+' '+newpathforclcx
else commandtext:=Terminal+' '+'''--command='+Path2CGXl+ ' '+ComboBox1.Text+' '+newpathforclcx +'''' ;

//Finally
RunApp(commandtext);

end
{$ENDIF}


{$IFDEF WINDOWS}
if (Path2CGX='') And (Form1.Combobox5.Text='cgx2.10') Then Path2CGXl:=  ExeAppPath + 'bin/cgx210/cgx.exe';
if (Path2CGX='') And (Form1.Combobox5.Text='cgx2.5') Then Path2CGXl:=  ExeAppPath + 'bin/cgx25/cgx.exe';

If  newpathforclcx='' Then ShowMessage('Select CalculiX INP file!' )

else
 begin
   SetCurrentDir(ExtractFileDir(newpathforclcx));


   commandtext:='cmd /c '+'"'+Path2CGXl+ ' '+ComboBox1.Text+' '+newpathforclcx+'"';
   RunApp(commandtext);

 end
{$ENDIF}






  //The Code Below Is For Portable Version with MemoTerminal
 // RunAppWithOutput(ExeAppPath +'cgx_2.5.cde -c '+FileNameEdit1.Text);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  AStringList: TStringList;
  newpathforclcx,commandtext, Path2CGXl,MeshFileExt,MeshFileName:String;
begin

//Initial checks
If  FileNameEdit1.Text='' Then
begin
ShowMessage('Select file!');
exit;
end;




//Trying to catch frd file by inp
MeshFileExt:=ExtractFileExt(FileNameEdit1.Text)  ;
If  (MeshFileExt='.inp') Or  (MeshFileExt='.INP') Then
begin
MeshFileName:=ExtractFileName(FileNameEdit1.Text)  ;
delete(MeshFileName,length(MeshFileName)-3,4);
FileNameEdit1.Text:=ExtractFileDir(FileNameEdit1.Text)+'/'+MeshFileName+'.frd';
end;


//If FileExists(MeshFileName+'.inp') Then  ccxinpadd=MeshFileName+'.inp';

newpathforclcx:= FileNameEdit1.Text;
If  newpathforclcx='' Then ShowMessage('Select CalculiX FRD file!' )

else
 begin

   {$IFDEF LINUX}
   SetCurrentDir(ExtractFileDir(newpathforclcx));
   if Path2CGX='' Then Path2CGXl:=  ExeAppPath + 'bin/cgx2.12';
    If Terminal=''
   then  commandtext:='xterm -e ' +Path2CGXl+ ' -r '+newpathforclcx
   else commandtext:=Terminal+' '+'''--command='+Path2CGXl+ ' -v '+newpathforclcx +'''' ;

   //Finally
   RunApp(commandtext);

   {$ENDIF}

   {$IFDEF WINDOWS}
   SetCurrentDir(ExtractFileDir(newpathforclcx));
   if (Path2CGX='') And (Form1.Combobox5.Text='cgx2.10') Then Path2CGXl:=  ExeAppPath + 'bin/cgx210/cgx.exe';
   if (Path2CGX='') And (Form1.Combobox5.Text='cgx2.5') Then Path2CGXl:=  ExeAppPath + 'bin/cgx25/cgx.exe';
   commandtext:='cmd /c '+'"'+Path2CGXl+ ' -v '+newpathforclcx+'"';
   RunApp(commandtext);
   {$ENDIF}
 end


end;

procedure TForm1.Button4Click(Sender: TObject);
var
 Hbk:TProcess;
 //dip28 variables
 MeshFileExt,MeshFileExtSel,MeshFileName,PyPathl,ScriptPath,MeshPath,PathOut,PostKey,commandtext:String;
 //prool's variables
  ProolPath:String;
  //unical variables
  UnicalPath:String;
begin

//check extention compability
  MeshPath:= FileNameEdit1.Text;
  MeshFileExt:=ExtractFileExt(MeshPath)  ;

If ComboBox4.Text='VOL Netgen (python script)' Then MeshFileExtSel:='.vol' ;
If ComboBox4.Text='MSH (python script)' Then MeshFileExtSel:='.msh';
If (ComboBox4.Text='UNV (python script)') Or (ComboBox4.Text='UNV (unical converter)') Then MeshFileExtSel:='.unv';
If (ComboBox4.Text='GMSH INP (python script)') Or (ComboBox4.Text='GMSH INP (by Prool)') Then  MeshFileExtSel:='.inp';

If (MeshFileExt='.vol') Or (MeshFileExt='.VOL') Then  MeshFileExt:='.vol';
If (MeshFileExt='.msh') Or (MeshFileExt='.MSH') Then  MeshFileExt:='.msh';
If (MeshFileExt='.unv') Or (MeshFileExt='.UNV') Then  MeshFileExt:='.unv';
If (MeshFileExt='.inp') Or (MeshFileExt='.INP') Then  MeshFileExt:='.inp';

If MeshFileExtSel<>MeshFileExt  Then
begin
  ShowMessage('File extention doesnt match up with selected converter');
  Exit;
end;

//end of check extention compability

If (ComboBox4.Text='UNV (python script)') Or (ComboBox4.Text='VOL Netgen (python script)')  Or (ComboBox4.Text='GMSH INP (python script)') Or (ComboBox4.Text='MSH (python script)') Then

begin
// dip28 converters
//Extension of the file selected
MeshPath:= FileNameEdit1.Text;
MeshFileExt:=ExtractFileExt(MeshPath)  ;
MeshFileName:=ExtractFileName(MeshPath)  ;
delete(MeshFileName,length(MeshFileName)-3,4);

{$IFDEF LINUX}
PathOut:=ExtractFileDir(MeshPath)+'/'+MeshFileName+'_OUT.inp';

 // If (MeshFileExt='.inp') Or  (MeshFileExt='.vol') Or  (MeshFileExt='.msh') Or (MeshFileExt='.unv') Then

 If (MeshFileExt = '.vol') Or (MeshFileExt = '.VOL') Then ScriptPath:=ExeAppPath+'scr/Converters/py_files/vol2ci1.py' ;
 If (MeshFileExt = '.unv') Or (MeshFileExt = '.UNV') Then ScriptPath:=ExeAppPath+'scr/Converters/py_files/unv2calculix.py' ;

 If (MeshFileExt = '.msh') Or (MeshFileExt = '.MSH') Then
 begin
 ScriptPath:=ExeAppPath+'scr/Converters/py_files/g2c3.py ' ;
 PostKey:=Edit1.Text;
 end;

 If (MeshFileExt = '.inp') Or (MeshFileExt = '.INP') Then
 begin
 ScriptPath:=ExeAppPath+'scr/Converters/py_files/gi2ci.py';
 PostKey:=ComboBox2.Text;
 end;



 SetCurrentDir(ExtractFileDir(ScriptPath));
 if PyPath='' Then PyPathl:=  ExeAppPath + 'bin/python2.7-static';
 commandtext:=  PyPathl+' '+ScriptPath+' '+MeshPath+' '+PathOut+' '+PostKey;


 If Terminal=''
then  commandtext:='xterm -e ' +commandtext
else  commandtext:=Terminal+' '+'''--command='+commandtext +'''';



Hbk := TProcess.Create(nil);
Hbk.CommandLine := commandtext;
Hbk.Options := Hbk.Options + [poWaitOnExit];
Hbk.Execute;
Hbk.Free;

If FileExists(PathOut) Then
begin
FileNameEdit1.Text:=PathOut;
ShowMessage('_OUT.INP file was created in the same folder');
end
Else
begin
ShowMessage('Check if new .._OUT.INP file was created in the same folder');
end;
{$ENDIF}

{$IFDEF WINDOWS}
PathOut:=ExtractFileDir(MeshPath)+'\'+MeshFileName+'_OUT.inp';

If (MeshFileExt = '.vol') Or (MeshFileExt = '.VOL') Then ScriptPath:=ExeAppPath+'scr\Converters\py_files\vol2ci1.py' ;
If (MeshFileExt = '.unv') Or (MeshFileExt = '.UNV') Then ScriptPath:=ExeAppPath+'scr\Converters\py_files\unv2calculix.py' ;

If (MeshFileExt = '.msh') Or (MeshFileExt = '.MSH') Then
begin
ScriptPath:=ExeAppPath+'scr\Converters\py_files\g2c3.py ' ;
PostKey:=Edit1.Text;
end;

If (MeshFileExt = '.inp') Or (MeshFileExt = '.INP') Then
begin
ScriptPath:=ExeAppPath+'scr\Converters\py_files\gi2ci.py';
PostKey:=ComboBox2.Text;
end;



SetCurrentDir(ExtractFileDir(ScriptPath));
if PyPath='' Then PyPathl:=  ExeAppPath + 'bin\python\python.exe';


commandtext:='cmd /k '+'"'+PyPathl+' '+ScriptPath+' '+MeshPath+' '+PathOut+' '+PostKey+'"';
//Finally
RunApp(commandtext);
 SetCurrentDir(ExtractFileDir(MeshPath));
 ShowMessage('After successful conversion new .._OUT.INP file should be created in the same folder');
{$ENDIF}

// End of dip28 converters

end;

If ComboBox4.Text='GMSH INP (by Prool)' Then
  begin
   // Prool's Converter
//Extension of the file selected
MeshPath:= FileNameEdit1.Text;
MeshFileName:=ExtractFileName(MeshPath)  ;
delete(MeshFileName,length(MeshFileName)-3,4);
PathOut:=ExtractFileDir(MeshPath)+'/'+MeshFileName+'_OUT.inp';

{$IFDEF LINUX}
ProolPath:= ExeAppPath+'bin/prool' ;
commandtext:=  ProolPath+ ' -' + Edit1.Text+' '+MeshPath+' '+PathOut ;
If Terminal=''
then  commandtext:='xterm -e ' +commandtext
else  commandtext:=Terminal+' '+'''--command='+commandtext +'''';


Hbk := TProcess.Create(nil);
Hbk.CommandLine := commandtext;
Hbk.Options := Hbk.Options + [poWaitOnExit];
Hbk.Execute;
Hbk.Free;

If FileExists(PathOut) Then
begin
FileNameEdit1.Text:=PathOut;
ShowMessage('New .._OUT.INP file was created in the same folder');
end
Else
begin
ShowMessage('Something wrong with conversion...');
end;
{$ENDIF}




{$IFDEF WINDOWS}
ProolPath:= ExeAppPath+'bin\proolconv.exe' ;
commandtext:='cmd /k '+'"'+ProolPath+ ' -' + Edit1.Text+' '+MeshPath+' '+PathOut+'"';

RunApp(commandtext);
SetCurrentDir(ExtractFileDir(MeshPath));
ShowMessage('After successful conversion new .._OUT.INP file should be created in the same folder');
{$ENDIF}

// End Of Prool's Converter
  end;

If ComboBox4.Text='UNV (unical converter)' Then
    begin
    //Unical Converter
//Extension of the file selected
MeshPath:= FileNameEdit1.Text;
MeshFileName:=ExtractFileName(MeshPath)  ;
delete(MeshFileName,length(MeshFileName)-3,4);
PathOut:=ExtractFileDir(MeshPath)+'/'+MeshFileName+'_OUT.inp';

{$IFDEF LINUX}
If Checkbox3.Checked=False Then UnicalPath:= ExeAppPath+'bin/unical' ;
If Checkbox3.Checked=True Then UnicalPath:= ExeAppPath+'bin/unical -f' ;
commandtext:=  UnicalPath+' '+MeshPath+' '+PathOut;

If Terminal=''
then  commandtext:='xterm -e ' +commandtext
else  commandtext:=Terminal+' '+'''--command='+commandtext +'''';



Hbk := TProcess.Create(nil);
Hbk.CommandLine := commandtext;
Hbk.Options := Hbk.Options + [poWaitOnExit];
Hbk.Execute;
Hbk.Free;

If FileExists(PathOut) Then
begin
FileNameEdit1.Text:=PathOut;
ShowMessage('New .._OUT.INP file was created in the same folder');
end
Else
begin
ShowMessage('Something wrong with conversion...');
end;
{$ENDIF}

{$IFDEF WINDOWS}
UnicalPath:= ExeAppPath+'bin\unical.exe' ;
commandtext:='cmd /k '+'"'+UnicalPath+' '+MeshPath+' '+PathOut+'"';


RunApp(commandtext);
SetCurrentDir(ExtractFileDir(MeshPath));
ShowMessage('After successful conversion new .._OUT.INP file should be created in the same folder');
{$ENDIF}

//End Of Unical Converter
end;


 end;

procedure TForm1.Button5Click(Sender: TObject);
var
  pathfortips:String;
begin


  if ComboBox4.Text='UNV (unical converter)' then pathfortips:='hlp/tips/UnicalTips'  ;
  if ComboBox4.Text='GMSH INP (by Prool)' then pathfortips:='hlp/tips/ProolTips'  ;
  if ComboBox4.Text='VOL Netgen (python script)' then pathfortips:='hlp/tips/dip28volTips'  ;
  if ComboBox4.Text='GMSH INP (python script)' then pathfortips:='hlp/tips/dip28inpTips' ;
  if ComboBox4.Text='MSH (python script)' then pathfortips:='hlp/tips/dip28mshTips'  ;
  if ComboBox4.Text='UNV (python script)' then pathfortips:='hlp/tips/dip28unvTips'  ;

{$IFDEF WINDOWS}
    if ComboBox4.Text='UNV (unical converter)' then pathfortips:='hlp\tips\UnicalTips'  ;
  if ComboBox4.Text='GMSH INP (by Prool)' then pathfortips:='hlp\tips\ProolTips'  ;
  if ComboBox4.Text='VOL Netgen (python script)' then pathfortips:='hlp\tips\dip28volTips'  ;
  if ComboBox4.Text='GMSH INP (python script)' then pathfortips:='hlp\tips\dip28inpTips' ;
  if ComboBox4.Text='MSH (python script)' then pathfortips:='hlp\tips\dip28mshTips'  ;
  if ComboBox4.Text='UNV (python script)' then pathfortips:='hlp\tips\dip28unvTips'  ;
{$ENDIF}

  Form4.Memo1.Lines.LoadFromFile(ExeAppPath + pathfortips );
  Form4.Showmodal;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  cmdfile: TextFile;
   MeshFileExt:String;
begin


//windows
{$IFDEF WINDOWS}
 ShowMessage('Sorry! This feature is for linux only. You may try to install it under VirtualBox or try to install Ubuntu on external drive (usb connected, $60 cost). Ubuntu 14 is the best with CalculiX and other opensource software (like Elmer-FEM or Salome_MECA). See calculix09 @ Youtube for more details');
exit;
{$ENDIF}

//Initial checks
  If  FileNameEdit1.Text='' Then
  begin
  ShowMessage('Select INP file!');
  exit;
  end;

  MeshFileExt:=ExtractFileExt(FileNameEdit1.Text)  ;
  If  (MeshFileExt<>'.inp') And  (MeshFileExt<>'.INP') Then
  begin
  ShowMessage('Select INP file!');
  exit;
  end;


  //Catch Wizard Case
   If RadioButton1.Checked=True Then WizardCase:=1;
   If RadioButton2.Checked=True Then WizardCase:=2;
   If RadioButton3.Checked=True Then WizardCase:=3;
   If RadioButton4.Checked=True Then WizardCase:=4;

  //setup working dir
  workdir:=ExtractFileDir(FileNameEdit1.Text) ;
  SetCurrentDir(workdir);
  //create cgxcmd file in the working directory

  AssignFile(cmdfile, workdir+'/cgxcmd');
      ReWrite(cmdfile);
      Writeln(cmdfile,'read '+FileNameEdit1.Text);

      If WizardCase=1 Then Writeln(cmdfile,'writeinone');
      If (WizardCase=2) And (Form1.CheckBox4.Checked=True) Then Writeln(cmdfile,'write4shell 0');
      If (WizardCase=2) And (Form1.CheckBox4.Checked=True) And (Form1.Combobox3.Text='ccx2.12') Then Writeln(cmdfile,'write4shell 0');
      If (WizardCase=2) And (Form1.CheckBox4.Checked=False) Then Writeln(cmdfile,'write4shell 1');
      If WizardCase=3 Then Writeln(cmdfile,'writeone');
      If WizardCase=4 Then Writeln(cmdfile,'writeinone');

      Writeln(cmdfile,'exit');
   Closefile(cmdfile);{Closes file cmdfile}

   //sendcom;

     AssignFile(cmdfile, workdir+'/cgxcmd');
      ReWrite(cmdfile);
     Writeln(cmdfile,'read '+workdir+'/allinone.inp');
     Writeln(cmdfile,'write list');
     Writeln(cmdfile,'exit');
  Closefile(cmdfile);{Closes file cmdfile}



end;

procedure TForm1.Button7Click(Sender: TObject);
begin
If FileNameEdit1.Text=''
  then ShowMessage('File is not selected! Folder is unknown')
  else OpenURL(ExtractFileDir(FileNameEdit1.Text));
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  MenuItem2.Click;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  MenuItem20.Click ;
end;




procedure TForm1.ComboBox4Change(Sender: TObject);
begin

If ComboBox4.Text='UNV (unical converter)' Then
begin
Label4.Visible:=False;
Label5.Visible:=False;
Edit1.Visible:=False;
Combobox2.Visible:=False;
Checkbox3.Visible:=True;
end;

{$IFDEF WINDOWS}
Checkbox3.Visible:=False;

{$ENDIF}


  If (ComboBox4.Text='UNV (python script)') Or (ComboBox4.Text='VOL Netgen (python script)')   Then
  begin
  Label4.Visible:=False;
  Label5.Visible:=False;
  Edit1.Visible:=False;
  Combobox2.Visible:=False;
  Checkbox3.Visible:=False;
  end;


  If ComboBox4.Text='MSH (python script)' Then
  begin
  Label4.Visible:=True;
  Label5.Visible:=False;
  Edit1.Visible:=True;
  Combobox2.Visible:=False;
  Label4.Caption:='PostKey String for .msh converter';
  Edit1.Text:='C3';
  Checkbox3.Visible:=False;
  end;

  If ComboBox4.Text='GMSH INP (by Prool)' Then
  begin
  Label4.Visible:=True;
  Label5.Visible:=False;
  Edit1.Visible:=True;
  Combobox2.Visible:=False;
  Label4.Caption:='Coordinate Scaling';
  Edit1.Text:='1';
  Checkbox3.Visible:=False;
  end;

  If ComboBox4.Text='GMSH INP (python script)'  Then

   begin
  Label4.Visible:=False;
  Label5.Visible:=True;
  Edit1.Visible:=False;
  Combobox2.Visible:=True;
  Checkbox3.Visible:=False;
  end;

end;

procedure TForm1.FileNameEdit1Change(Sender: TObject);
var
 MeshFileExt:string;
begin

  //check for blanks in path
  if FileNameEdit1.Text <>  StringReplace(FileNameEdit1.Text,' ','',[rfReplaceAll, rfIgnoreCase]) Then
   begin
   ShowMessage('You have spaces in selected path, please rename file or move it to the other folder for use!');
   exit;
   end;


  //Initial checks
  MeshFileExt:=ExtractFileExt(FileNameEdit1.Text)  ;

If  (MeshFileExt='.msh') Or  (MeshFileExt='.MSH') Then
begin
  Form1.TabSheet2.Show;
  Combobox4.ItemIndex:=2;

  Label4.Visible:=True;
  Label5.Visible:=False;
  Edit1.Visible:=True;
  Combobox2.Visible:=False;
  Label4.Caption:='PostKey String for .msh converter';
  Edit1.Text:='C3';
end;

If  (MeshFileExt='.unv') Or  (MeshFileExt='.UNV') Then
begin
  Form1.TabSheet2.Show;
  Combobox4.ItemIndex:=0;

  Label4.Visible:=False;
  Label5.Visible:=False;
  Edit1.Visible:=False;
  Combobox2.Visible:=False;
end;

If  (MeshFileExt='.vol') Or  (MeshFileExt='.VOL') Then
begin
  Form1.TabSheet2.Show;
  Combobox4.ItemIndex:=3;

  Form1.Label4.Visible:=False;
  Form1.Label5.Visible:=False;
  Form1.Edit1.Visible:=False;
  Form1.ComboBox2.Visible:=False;
end;

end;


procedure TForm1.FormActivate(Sender: TObject);
var
  str1:String;
  licfileread: TextFile;
  list: TStringList;
  i:integer;
begin
  //The Path to exe application
ExeAppPath := ExtractFilePath(Application.ExeName);

  //Option for Custom Terminal Window
  Memo1.Visible:=False;
 // Form1.Height:=245;

//1st run
If FileExists(ExeAppPath + '/bin/licread')=false Then
begin
   Form4.Button1.Caption:='I Agree';
   Form4.Memo1.Lines.LoadFromFile(ExeAppPath + 'License.txt' );
   Form4.ComboBox1.Visible:=true;
   Form4.Label1.Visible:=true;

   {$IFDEF WINDOWS}
    Form4.ComboBox1.Visible:=false;
   Form4.Label1.Visible:=false;
   ShowMessage('You are running launcher for the first time! Note, that CalculiX is native linux software, and it has many unsolved issues under Windows. You may also download free or extended low-cost package at www.bconverged.com');
   {$ENDIF}

   Form4.ShowModal;


end;



//Try To Read INI File
//Path to Ini
PathToIni:=  ExeAppPath + '/bin/Settings.ini';

//ShowMessage(ExeAppPath);

//If not exists
  if not FileExists(PathToIni) then
  begin
//    Path2CCX:= ExeAppPath + '/bin/ccx2.8';
    Path2CCX:= '';
//    Path2CGX:= ExeAppPath + '/bin/cgx2.8';
    Path2CGX:= '';
    Terminal:='';
    TextEditor:='';
//    PyPath:=ExeAppPath + '/bin/python2.7-static';
     PyPath:='';

   AssignFile(inifile, PathToIni);
      ReWrite(inifile);
      Writeln(inifile,'**line#2 - full path to CCX or empty');
      Writeln(inifile,Path2CCX);
      Writeln(inifile,'**line#4 - full path to CGX or empty');
      Writeln(inifile,Path2CGX);
      Writeln(inifile,'**line#6 - name of terminal emulator (xfce4-terminal, etc) or empty');
      Writeln(inifile,Terminal);
      Writeln(inifile,'**line#8 - name of text editor (scite, leafpad, etc) or empty');
      Writeln(inifile,TextEditor);
      Writeln(inifile,'**line#10 - full path to python (/usr/bin/...) or empty');
      Writeln(inifile,PyPath);
   Closefile(inifile);{Closes file inifile}
  ShowMessage('INI File was not found. Default options will be restored. Go to Menu > Settings and correct it, if needed');
  end  ;



AssignFile(inifile,PathToIni);{Assigns the Filename}
Reset(inifile);{Opens the file for reading}
while not Eof(inifile) do
begin
Readln(inifile,str1);
Readln(inifile,str1);
Path2CCX:=str1;
Readln(inifile,str1);
Readln(inifile,str1);
Path2CGX:=str1;
Readln(inifile,str1);
Readln(inifile,str1);
Terminal:=str1;
Readln(inifile,str1);
Readln(inifile,str1);
TextEditor:=str1;
Readln(inifile,str1);
Readln(inifile,str1);
PyPath:=str1;
end;
 Closefile(inifile);{Closes file inifile}


   list := FindAllFiles(ExeAppPath+'hlp/verifsamples', '*.inp', true {search in subdirectory});


  for i:=0 to list.Count-1 do
    list[i]:=ExtractFileName(list[i]);
  ListBox2.Items.Assign(list);
  list.Free;



end;



procedure TForm1.ListBox1DblClick(Sender: TObject);
var
commandtext:String;
inpfilepath :String;
begin
(*
If ListBox1.Items[ListBox1.ItemIndex]='-----beam elements' then inpfilepath:='hlp/examples/beams/timber.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----truss elements (CCX>2.9)' then inpfilepath:='hlp/examples/ccx2.9-truss/truss.inp' ;
If ListBox1.Items[ListBox1.ItemIndex]='-----shell elements' then inpfilepath:='hlp/examples/plate_test/inp/shell/S6/plate_S6.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with pressure' then  inpfilepath:='hlp/examples/T-shapeBody/pressure.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with concentreted moment' then  inpfilepath:='hlp/examples/T-shapeBody/conc-moment.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with force applied (1)' then  inpfilepath:='hlp/examples/T-shapeBody/conc-force1.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with force applied (2)' then  inpfilepath:='hlp/examples/T-shapeBody/conc-force2.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with Transform' then  inpfilepath:='hlp/examples/cylsup/cyl-static.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with Bolt Pretension' then  inpfilepath:='hlp/examples/bolt_pretension/pret3.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----Mohr-Coulomb' then  inpfilepath:='hlp/examples/nonlin/Mohr-Coulomb/stamp/test1.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----plastic' then   inpfilepath:='hlp/examples/nonlin/plate_loading_and_unloading/shellS8R_composite/_main_plastic.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----concrete' then  inpfilepath:='hlp/examples/nonlin/conc-plate/conc-plate.inp';
If ListBox1.Items[ListBox1.ItemIndex]='----contact ...' then  inpfilepath:='hlp/examples/contact-ccx28/contact.inp';
If ListBox1.Items[ListBox1.ItemIndex]='BONDED CONTACT' then   inpfilepath:='hlp/examples/T-shapeBody/bonded-modal.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----modal with beam elements' then  inpfilepath:='hlp/examples/beams/timber_modal.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----modal with string effect' then   inpfilepath:='hlp/examples/beams/string.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----modal with body' then  inpfilepath:='hlp/examples/T-shapeBody/modal.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----buckling with beam elements' then   inpfilepath:='hlp/examples/beams/beam-buckl.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----buckling with shell elements' then  inpfilepath:='hlp/examples/plate_test/inp/shell/S6/shellbuckl.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----buckling with body' then   inpfilepath:='hlp/examples/T-shapeBody/buckling.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----steady state' then  inpfilepath:='hlp/examples/heatss/coldbridge.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----transient' then   inpfilepath:='hlp/examples/pan/transient.inp';
If ListBox1.Items[ListBox1.ItemIndex]='THERMOMECH' then  inpfilepath:='hlp/examples/dhgalv/dhgalv.inp';
If ListBox1.Items[ListBox1.ItemIndex]='DYNAMIC' then   inpfilepath:='hlp/examples/dynamic/_main.inp';





{$IFDEF WINDOWS}

If ListBox1.Items[ListBox1.ItemIndex]='-----beam elements' then inpfilepath:='hlp\examples\beams\timber.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----truss elements (CCX>2.9)' then inpfilepath:='hlp\examples\ccx2.9-truss\truss.inp' ;
If ListBox1.Items[ListBox1.ItemIndex]='-----shell elements' then inpfilepath:='hlp\examples\plate_test\inp/shell\S6\plate_S6.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with pressure' then  inpfilepath:='hlp\examples\T-shapeBody\pressure.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with concentreted moment' then  inpfilepath:='hlp\examples\T-shapeBody\conc-moment.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with force applied (1)' then  inpfilepath:='hlp\examples\T-shapeBody\conc-force1.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with force applied (2)' then  inpfilepath:='hlp\examples\T-shapeBody\conc-force2.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with Transform' then  inpfilepath:='hlp\examples\cylsup\cyl-static.inp';
If ListBox1.Items[ListBox1.ItemIndex]='------------with Bolt Pretension' then  inpfilepath:='hlp\examples\bolt_pretension\pret3.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----Mohr-Coulomb' then  inpfilepath:='hlp\examples\nonlin\Mohr-Coulomb\stamp/test1.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----plastic' then   inpfilepath:='hlp\examples\nonlin\plate_loading_and_unloading\shellS8R_composite\_main_plastic.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----concrete' then  inpfilepath:='hlp\examples\nonlin\conc-plate\conc-plate.inp';
If ListBox1.Items[ListBox1.ItemIndex]='----contact ...' then  inpfilepath:='hlp\examples\contact-ccx28\contact.inp';
If ListBox1.Items[ListBox1.ItemIndex]='BONDED CONTACT' then   inpfilepath:='hlp\examples\T-shapeBody\bonded-modal.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----modal with beam elements' then  inpfilepath:='hlp\examples\beams\timber_modal.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----modal with string effect' then   inpfilepath:='hlp\examples\beams\string.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----modal with body' then  inpfilepath:='hlp\examples\T-shapeBody\modal.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----buckling with beam elements' then   inpfilepath:='hlp\examples\beams\beam-buckl.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----buckling with shell elements' then  inpfilepath:='hlp\examples\plate_test\inp/shell\S6\shellbuckl.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----buckling with body' then   inpfilepath:='hlp\examples\T-shapeBody\buckling.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----steady state' then  inpfilepath:='hlp\examples\heatss\coldbridge.inp';
If ListBox1.Items[ListBox1.ItemIndex]='-----transient' then   inpfilepath:='hlp\examples\pan\transient.inp';
If ListBox1.Items[ListBox1.ItemIndex]='THERMOMECH' then  inpfilepath:='hlp\examples\dhgalv\dhgalv.inp';
If ListBox1.Items[ListBox1.ItemIndex]='DYNAMIC' then   inpfilepath:='hlp\examples\dynamic\_main.inp';

{$ENDIF}


If (TextEditor='') And (inpfilepath<>'') Then
begin
FrMain.Editor.Lines.LoadFromFile(ExeAppPath + inpfilepath );
frMain.Show;
end;
If (TextEditor<>'')  And (inpfilepath<>'') Then
begin
commandtext:=TextEditor+' '+ExeAppPath + inpfilepath;
RunApp(commandtext);
end;

If CheckBox1.Checked=True Then
begin;
FileNameEdit1.Text:=ExeAppPath + inpfilepath ;
ShowMessage('Path to file has been copied to Launcher. You may run CGX as Preprocessor to investigate model and then CCX solver to obtain calculation results');
end;
If inpfilepath=''   Then ShowMessage('Select other line (by topic) to see CCX command file for example');
*)
end;

procedure TForm1.ListBox2DblClick(Sender: TObject);
var
inpfilepath,commandtext,SelectedListItem:String;
begin

SelectedListItem:=  ListBox2.Items[ ListBox2.ItemIndex ];

inpfilepath:='hlp/verifsamples/'+SelectedListItem;


  If (TextEditor='') And (inpfilepath<>'') Then
begin
//FrMain.Editor.Lines.LoadFromFile(ExeAppPath + inpfilepath );
SetCurrentDir(ExtractFileDir(ExeAppPath + inpfilepath));
frMain.LoadFile(ExeAppPath + inpfilepath )   ;
frMain.Show;
end;
If (TextEditor<>'')  And (inpfilepath<>'') Then
begin
commandtext:=TextEditor+' '+ExeAppPath + inpfilepath;
RunApp(commandtext);
end;

If CheckBox2.Checked=True Then
begin;
FileNameEdit1.Text:=ExeAppPath + inpfilepath ;
ShowMessage('Path to file has been copied to Launcher. You may run CGX as Preprocessor to investigate model and then CCX solver to obtain calculation results');
end;


end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin
  Form4.Memo1.Lines.LoadFromFile(ExeAppPath + 'hlp/tips/CGXTipsmenu1' );
Form4.Showmodal;
   OpenURL('http://calculixforwin.blogspot.com/2016/08/calculix-cgx-as-preprocessor.html');
end;

procedure TForm1.MenuItem12Click(Sender: TObject);
begin
   Form4.Memo1.Lines.LoadFromFile(ExeAppPath + 'hlp/tips/CGXTipsmenu2' );
Form4.Showmodal;
OpenURL('http://calculixforwin.blogspot.com/2016/08/calculix-cgx-as-postprocessor.html');
end;

procedure TForm1.MenuItem13Click(Sender: TObject);
var
MeshFileName,ccxinpadd:String;
newpathforclcx,commandtext, Path2CGXl,MeshFileExt:String;
begin


//Trying to catch frd file by inp
MeshFileExt:=ExtractFileExt(FileNameEdit1.Text)  ;
If  (MeshFileExt='.inp') Or  (MeshFileExt='.INP') Then
begin
MeshFileName:=ExtractFileName(FileNameEdit1.Text)  ;
delete(MeshFileName,length(MeshFileName)-3,4);
FileNameEdit1.Text:=ExtractFileDir(FileNameEdit1.Text)+'/'+MeshFileName+'.frd';
end;

MeshFileName:=ExtractFileName(FileNameEdit1.Text)  ;
delete(MeshFileName,length(MeshFileName)-3,4);


newpathforclcx:= FileNameEdit1.Text;
If  newpathforclcx='' Then ShowMessage('Select CalculiX FRD file!' )

else
 begin

   {$IFDEF LINUX}
   SetCurrentDir(ExtractFileDir(newpathforclcx));
   if Path2CGX='' Then Path2CGXl:=  ExeAppPath + 'bin/cgx2.12';
    If Terminal=''
   then  commandtext:='xterm -e ' +Path2CGXl+ ' '+MeshFileName+'.frd '+ MeshFileName+'.inp'
   else commandtext:=Terminal+' '+'''--command='+Path2CGXl+ ' '+MeshFileName+'.frd '+ MeshFileName+'.inp' +'''' ;

   //Finally
   RunApp(commandtext);

   {$ENDIF}
   end ;

end;

procedure TForm1.MenuItem15Click(Sender: TObject);
begin

  OpenURL('https://groups.yahoo.com/groups/calculix/');

end;

procedure TForm1.MenuItem16Click(Sender: TObject);
begin
    OpenURL('http://www.dhondt.de/');
end;

procedure TForm1.MenuItem17Click(Sender: TObject);
begin
  OpenURL('http://calculixforwin.blogspot.com/');
end;

procedure TForm1.MenuItem18Click(Sender: TObject);
begin
     OpenURL('http://salome-platform.org/');
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
begin
   OpenURL('http://gmsh.info/');
end;

procedure TForm1.MenuItem20Click(Sender: TObject);
begin
If  FileNameEdit1.Text='' then
begin
ShowMessage( 'Select main INP file! This procedure is designed to combine all .INP fies into one, without ref. links');
exit;
end;

if FileExists(ExtractFileDir(FileNameEdit1.Text)+'/allinone.inp') then  CopyFile(ExtractFileDir(FileNameEdit1.Text)+'/allinone.inp',ExtractFileDir(FileNameEdit1.Text)+'/allinone-copy.inp');

TransformFile( FileNameEdit1.Text);

if FileExists(ExtractFileDir(FileNameEdit1.Text)+'/allinone-copy.inp') then  CopyFile(ExtractFileDir(FileNameEdit1.Text)+'/allinone-copy.inp',ExtractFileDir(FileNameEdit1.Text)+'/allinone.inp');
if FileExists(ExtractFileDir(FileNameEdit1.Text)+'/allinone-copy.inp') then  DeleteFile(ExtractFileDir(FileNameEdit1.Text)+'/allinone-copy.inp');
ShowMessage('You may open folder and check if combined INP file of current date and time is created');
end;

procedure TForm1.MenuItem21Click(Sender: TObject);
begin
    if not FileExists(FileNameEdit1.FileName) then
    begin
    ShowMessage('Select .dat file! This feature is designed to read CCX DAT files to get reaction forces and other output from *NODE PRINT card. Read CCX help for details!') ;
    exit;
    end;
    SumMinMax(FileNameEdit1.FileName,Form4.Memo1.Lines);
    Form4.Show;
end;

procedure TForm1.MenuItem22Click(Sender: TObject);
begin
  // https://github.com/mkraska/CalculiX-Examples
  OpenURL('https://github.com/mkraska/CalculiX-Examples');
end;

procedure TForm1.MenuItem23Click(Sender: TObject);
begin
  OpenURL('http://bconverged.com/');
end;

procedure TForm1.MenuItem24Click(Sender: TObject);
begin
  OpenURL('https://www.efatigue.com/');
end;

procedure TForm1.MenuItem25Click(Sender: TObject);
begin
  {$IFDEF LINUX}
    OpenURL(ExeAppPath + 'hlp/ccx_2.13.pdf');
  {$ENDIF}

  {$IFDEF WINDOWS}
     OpenURL(ExeAppPath + '\hlp\ccx_2.13.pdf');
  {$ENDIF}

end;

procedure TForm1.MenuItem26Click(Sender: TObject);
begin

  {$IFDEF LINUX}
    OpenURL(ExeAppPath + 'hlp/cgx_2.12.pdf');
  {$ENDIF}

  {$IFDEF WINDOWS}
     OpenURL(ExeAppPath + '\hlp\cgx_2.12.pdf');
  {$ENDIF}

end;









procedure TForm1.MenuItem27Click(Sender: TObject);
begin
{$IFDEF LINUX}
  RunApp(ExeAppPath+'bin/gmsh') ;
{$ENDIF}

{$IFDEF WINDOWS}
SetCurrentDir(ExtractFileDir(ExeAppPath+'\bin\gmsh-2.7.0-Windows\'));
  RunApp(ExeAppPath+'\bin\gmsh-2.7.0-Windows\gmsh.exe') ;
{$ENDIF}
end;

procedure TForm1.MenuItem28Click(Sender: TObject);
begin
{$IFDEF LINUX}
  OpenURL(ExeAppPath + 'hlp/gmsh-doc/gmsh.html');
{$ENDIF}

{$IFDEF WINDOWS}
  OpenURL(ExeAppPath + '\hlp\gmsh-doc\gmsh.html');
 {$ENDIF}
end;

procedure TForm1.MenuItem29Click(Sender: TObject);
begin
   FrmVTKpost.show;
end;






procedure TForm1.MenuItem2Click(Sender: TObject);
var
commandtext:String;
begin
  SetCurrentDir(ExtractFileDir(FileNameEdit1.Text));
  If TextEditor='' Then
  begin

 { If FileNameEdit1.Text <>'' Then frMain.LoadFile(FileNameEdit1.Text );
  frMain.Show; }

  If FileNameEdit1.Text <>'' Then OpenEditor(FileNameEdit1.Text );


  end;
  If TextEditor<>'' Then
  begin
   commandtext:=TextEditor+' '+FileNameEdit1.Text;
    RunApp(commandtext);
  end;
end;





procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  Form3.ShowModal;
end;




procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  Form1.Close;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  Form1.Button7.Click;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  CalculatorDialog1.Execute;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
{$IFDEF LINUX}
  OpenURL(ExeAppPath + 'hlp/ccx213/index.html');
{$ENDIF}

{$IFDEF WINDOWS}
   OpenURL(ExeAppPath + '\hlp\ccx213\index.html');
{$ENDIF}
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
begin
{$IFDEF LINUX}
  OpenURL(ExeAppPath + 'hlp/cgx212/index.html');
{$ENDIF}

{$IFDEF WINDOWS}
   OpenURL(ExeAppPath + '\hlp\cgx212\index.html');
{$ENDIF}
end;

procedure TForm1.pnInfoViewHotClick(Sender: TObject);
begin
  if pnInfoView.HotNode is TIpHtmlNodeA then
    OpenURL(TIpHtmlNodeA(pnInfoView.HotNode).HRef);
end;

procedure TForm1.TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TForm1.tvExamplesDblClick(Sender: TObject);
var
  data: PHlpNodeData;
begin
  if (tvExamples.Selected=nil) or (tvExamples.Selected.Data=nil) then
    exit;
  data:=tvExamples.Selected.Data;
  if data^.NodeType=ntFile then begin
    if FileExists(TextEditor) then
      RunApp(Format('%s "%s"',[TextEditor,data^.Path]))
    else
      FrMain.OpenFile(data^.Path);
    if CheckBox1.Checked then begin
      FileNameEdit1.FileName:=data^.Path;
      ShowMessage('Path to file has been copied to Launcher. You may run CGX as Preprocessor to investigate model and then CCX solver to obtain calculation results');
    end;
  end else begin
     OpenDocument(data^.Path);
  end;
end;

procedure TForm1.tvExamplesDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) and Assigned(Node.Data) then begin
    FreeMem(Node.Data);
    Node.Data:=nil;
	end;
end;

procedure TForm1.tvExamplesSelectionChanged(Sender: TObject);
begin
  pnInfoView.SetHtml(GetHlpInfo(tvExamples.Selected));
end;

procedure TForm1.RunApp(commandstringline:string) ;
var
  Hbk: TProcess;
begin
Hbk := TProcess.Create(nil);
//Hbk.Options := Hbk.Options + [poWaitOnExit];
Hbk.CommandLine := commandstringline;
Hbk.Execute;
Hbk.Free;
end;

procedure TForm1.RunAppWithOutput(commandstringline:string) ;
 //This code is from here: http://forum.lazarus.freepascal.org/index.php?topic=12249.0#
const
  C_BUFSIZE = 2048;
var
  Hbk: TProcess;
  Buffer: pointer;
  SStream: TStringStream;
  nread: longint;
begin
  Hbk := TProcess.Create(nil);

  // Replace the line below with your own command string
  Hbk.CommandLine := commandstringline;
  //

  Hbk.Options := [poUsePipes, poStderrToOutPut];
  Hbk.Execute;

  // Prepare for capturing output
  Getmem(Buffer, C_BUFSIZE);
  SStream := TStringStream.Create('');

  // Start capturing output
  while Hbk.Running do
  begin
    nread := Hbk.Output.Read(Buffer^, C_BUFSIZE);
    if nread = 0 then
      sleep(3000)
    else
      begin
        // Translate raw input to a string
        SStream.size := 0;
        SStream.Write(Buffer^, nread);
        // And add the raw stringdata to the memo
        Memo1.Lines.Text := Memo1.Lines.Text + SStream.DataString;
// Added this--
  Application.ProcessMessages;
  Form1.Memo1.SelStart := Length(Form1.Memo1.Lines.Text);
//
      end;
  end;

  // Capture remainder of the output
  repeat
    nread := Hbk.Output.Read(Buffer^, C_BUFSIZE);
    if nread > 0 then
    begin
      SStream.size := 0;
      SStream.Write(Buffer^, nread);
      Memo1.Lines.Text := Memo1.Lines.Text + SStream.Datastring;

    end
  until nread = 0;





  // Clean up
  Hbk.Free;
  Freemem(buffer);
  SStream.Free;
end;





end.

