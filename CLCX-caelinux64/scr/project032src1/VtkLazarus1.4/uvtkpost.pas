{  Vtkpost version 1.3
   written by Gary Bollenbach
   13 December 2008


***** BEGIN GPL LICENSE BLOCK *****
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# ***** END GPL LICENSE BLOCK *****
# --------------------------------------------------------------------------
 }

unit uVtkpost;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, Buttons, ComCtrls, ExtCtrls, Math, ueigval, utypes,
  ujacobi, urootpol, urtpol1, urtpol2, urtpol3;
type

  { TFrmVtkpost }

  TFrmVtkpost = class(TForm)
    Gauge1: TProgressBar;
    lbInfo: TLabel;
    StringGrid1: TStringGrid;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    BitBtn4: TBitBtn;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Edit7: TEdit;
    BitBtn3: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn6: TBitBtn;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Panel2: TPanel;
    Edit1: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    RadioButton12: TRadioButton;
    RadioButton13: TRadioButton;
    RadioButton14: TRadioButton;
    RadioButton15: TRadioButton;
    RadioButton16: TRadioButton;
    RadioButton17: TRadioButton;
    RadioButton18: TRadioButton;
    RadioButton19: TRadioButton;
    RadioButton20: TRadioButton;
    RadioButton21: TRadioButton;
    RadioButton22: TRadioButton;
    RadioButton23: TRadioButton;
    RadioButton24: TRadioButton;
    RadioButton25: TRadioButton;
    RadioButton26: TRadioButton;
    RadioButton27: TRadioButton;
    RadioButton28: TRadioButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    procedure StringGrid1labels(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState); 
    procedure BitBtn4Click(Sender: TObject);
    procedure SaveDialog1TypeChange(Sender: TObject);
    procedure vtkWriteNodes(Sender: TObject);
    procedure vtkWriteElements(Sender: TObject);
    procedure vtkWritevonMises(Sender: TObject);
    procedure vtkWritePrincipal(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject); 
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure RadioButton5Click(Sender: TObject);
    procedure RadioButton6Click(Sender: TObject);
    procedure RadioButton7Click(Sender: TObject);
    procedure RadioButton8Click(Sender: TObject);
    procedure RadioButton9Click(Sender: TObject);
    procedure RadioButton10Click(Sender: TObject);
    procedure RadioButton11Click(Sender: TObject);
    procedure RadioButton12Click(Sender: TObject);
    procedure RadioButton13Click(Sender: TObject);
    procedure RadioButton14Click(Sender: TObject);
    procedure RadioButton15Click(Sender: TObject);
    procedure RadioButton16Click(Sender: TObject);
    procedure RadioButton18Click(Sender: TObject);
    procedure RadioButton19Click(Sender: TObject);
    procedure RadioButton20Click(Sender: TObject);
    procedure RadioButton22Click(Sender: TObject);
    procedure RadioButton21Click(Sender: TObject);
    procedure RadioButton23Click(Sender: TObject);
    procedure RadioButton24Click(Sender: TObject);
    procedure RadioButton25Click(Sender: TObject);
    procedure RadioButton26Click(Sender: TObject);
    procedure RadioButton27Click(Sender: TObject);
    procedure RadioButton28Click(Sender: TObject);
    procedure RadioButton17Click(Sender: TObject);
    procedure vtkWriteThermo(Sender: TObject);
    procedure Delay(ms : longint);
    procedure FormDestroy(Sender: TObject);
    procedure DisplayTableMsg(Sender: TObject);

  private
    { Private declarations }
    // Add for Lazarus
    procedure ShowError;
    function ShowOkCancel: Integer;
    function ReadLine(const Index: Integer): string;
    function ReadShortLine(const Index: Integer): string;
    function CheckLine(const Index: Integer): boolean;
  public
    { Public declarations }
    myvar2: string;
    typeL, nulltuple : string;
    sxxnodemax, sxxnodemin, syynodemax, syynodemin,
    szznodemax, szznodemin, sxynodemax, sxynodemin, syznodemax, syznodemin,
    szxnodemax, szxnodemin, misesnodemax, misesnodemin, trescanodemax,
    trescanodemin, ponenodemax, ponenodemin, ptwonodemax, ptwonodemin,
    pthreenodemax, pthreenodemin, dispxnodemax, dispxnodemin, dispynodemax,
    dispynodemin, dispznodemax, dispznodemin : string[18];
    numberofnodes : integer;
    startelline : integer;
    numberofelements : integer;
    edges : integer;
    r : integer;
    prinone, printwo, printhree, trescone, vonmiscone, sxxcone,
    sxxconemax, sxxconemin, syyconemax, syyconemin, szzconemax,
    szzconemin, sxyconemax, sxyconemin, syzconemax, syzconemin,
    szxconemax, szxconemin,
    misesconemax, misesconemin, trescaconemax,
    trescaconemin, poneconemax, poneconemin, ptwoconemax, ptwoconemin,
    pthreeconemax, pthreeconemin, dispxconemax, dispxconemin, dispyconemax,
    dispyconemin, dispzconemax, dispzconemin,
    dispxcone, dispycone, dispzcone : double;
    Snodearray : array of string[3];
    SnodearrayN : array of string[3];
    SnodearrayC : array of string[3];
    Nostress : boolean;
    Nodenum : array of string[8];
    Nodetuple : array of string[50];
    Splice : array of string[13];
    PrincipalSone : array of double;
    PrincipalStwo : array of double;
    PrincipalSthree : array of double;
    TrescaS : array of double;
    sxxArray : array of double;
    syyArray : array of double;
    szzArray : array of double;
    sxyArray : array of double;
    syzArray : array of double;
    szxArray : array of double;
    vMArray : array of double;
    DispxArray : array of double;
    DispyArray : array of double;
    DispzArray : array of double;
    ErrBuf: TStringList;
    InBuf:  TStringList;
    OutBuf:  TStringList;
  end;

const
  MaxIter = 1000;    { Maximum number of iterations }
  Tol     = 1.0E-8;  { Required precision }
  
var
  FrmVtkpost: TFrmVtkpost;

implementation

{$R *.lfm}


procedure TFrmVtkpost.StringGrid1labels(Sender: TObject);

begin
   with StringGrid1 do begin
     Cells[0,1] := '   SXX ';
     Cells[0,2] := '   SYY ';
     Cells[0,3] := '   SZZ ';
     Cells[0,4] := '   SYZ ';
     Cells[0,5] := '   SZX ';
     Cells[0,6] := '   SXY ';
     Cells[0,7] := '   von Mises ';
     Cells[0,8] := '   Tresca ';
     Cells[0,9] := '   Principal 1 ';
     Cells[0,10] := '   Principal 2 ';
     Cells[0,11] := '   Principal 3 ';
     Cells[0,12] := '   Disp X ';
     Cells[0,13] := '   Disp Y ';
     Cells[0,14] := '   Disp Z ';
     Cells[1,0] := ' Nodal Value ';
     Edit7.text := '1';
     end;
end;

procedure TFrmVtkpost.FormCreate(Sender: TObject);
begin
  ErrBuf:=TStringList.Create;
  InBuf:=TStringList.Create;
  OutBuf:=TStringList.Create;
  Edit7.text := ' ';
  Edit1.text := ' ';

  Shape1.brush.color := clRed;
  Shape2.brush.color := clGreen;
  Shape3.brush.color := clRed;
  Shape4.brush.color := clGreen;
  Shape5.brush.color := clRed;
  Shape6.brush.color := clGreen;

  StringGrid1labels(nil);
end;

procedure TFrmVtkpost.BitBtn2Click(Sender: TObject);
var
  i, F : integer;
  tempStr : string;
  err: boolean;
begin
   F := 0;

  if opendialog1.execute then
    begin
    Application.ProcessMessages;
    Shape1.brush.color := clRed;
    Shape2.brush.color := clGreen;
    myvar2:= opendialog1.filename;
    screen.Cursor:=crHourGlass;
    try
    InBuf.clear;
    InBuf.LoadFromFile(myvar2);

    err:=false;

    for i := 0 to InBuf.Count-1 do
    if AnsiPos(#1#0#0#0,InBuf[i])<>0 then begin
       ErrBuf.Clear;
       ErrBuf.Add( Format('File "%s" has not a text format (binary data).',[myvar2]));
       err:=true;
       break;
    end;

    tempStr:=ReadLine(F);
    if AnsiPos('    1C', tempStr) = 0 then begin
       ErrBuf.Clear;
       ErrBuf.Add( Format('File "%s" is not .frd file.',[myvar2]));
       ErrBuf.Add(' ');
       ErrBuf.Add( '(faulty header)');
       err:=true;
    end;

    finally
    screen.Cursor:=crDefault;
    end;

    if err then begin
     ShowError;
     InBuf.Clear;
     Shape1.brush.color := clRed;
     Shape2.brush.color := clGreen;
     Shape3.brush.color := clRed;
     Shape4.brush.color := clGreen;
     Shape5.brush.color := clRed;
     Shape6.brush.color := clGreen;
     Exit;
    end;

     OutBuf.clear;
     Shape1.brush.color := clMaroon;
     Shape2.brush.color := clLime;
     Shape3.brush.color := clRed;
     Shape4.brush.color := clGreen;
     Shape5.brush.color := clRed;
     Shape6.brush.color := clGreen; 
     Nostress := true;
     for i := 1 to 14 do
     StringGrid1.cells[1, i] := '';
     StringGrid1.cells[0, 0] := '';
     Edit1.text := '';
     Edit7.text := '1';
  end;
end;


procedure TFrmVtkpost.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  with StringGrid1.Canvas do begin
    // Fill rectangle with background color of grid 
    Brush.Color := StringGrid1.Color;
    FillRect(Rect);
    // Next, draw the text in the rectangle
    Font.Color := StringGrid1.Font.Color;
    TextOut(Rect.Left + 2, Rect.Top + 2, StringGrid1.Cells[ACol, ARow]);
  end;
end;

procedure TFrmVtkpost.BitBtn4Click(Sender: TObject);
begin
     if InBuf.Count < 5 then begin
     ErrBuf.Clear;
     ErrBuf.Add( Format('File "%s" must be loaded.',[myvar2]));
     ErrBuf.Add('The file is corrupted.');
     ShowError;
     InBuf.Clear;
     Exit;
      end;
     Shape1.brush.color := clMaroon;
     Shape2.brush.color := clLime;
     Shape5.brush.color := clRed;
     Shape6.brush.color := clGreen;
     try
     vtkWriteNodes(Sender);
     except
     lbInfo.Hide;
     Gauge1.Hide;
     Screen.Cursor:=crDefault;
     ErrBuf.Clear;
     ErrBuf.Add( Format('File "%s" must be loaded.',[myvar2]));
     ErrBuf.Add( 'Error reading file.');
     ShowError;
     end;
end;

procedure TFrmVtkpost.SaveDialog1TypeChange(Sender: TObject);
begin
// dummy
end;

procedure TFrmVtkpost.vtkWriteNodes(Sender: TObject);
var
   F, i, j, k, linesleft, pos2C : Integer;
   tempStr , charz, Sintset, C,  nodenumtemp,
     nodetupletemp : String; 
begin
{##########################################################################
  count the number of elements, set the size of dynamic arrays}
  F := 1;
  i := 0;
  linesleft := InBuf.Count;

  repeat
   tempStr:=ReadLine(F);
   C := tempstr[6];
   if tempStr[5] <> '3' then
     Inc(F);
   Dec(linesleft);
   until (tempStr[5] = '3') or (linesleft = 2) ;
   if linesleft = 2 then begin
      ErrBuf.Clear;
      ErrBuf.Add( 'Error writing nodes.');
      ShowError;
      Exit;
   end;

   Inc(F);
  repeat
   tempStr:=ReadLine(F);
   if tempStr[3] = '1' then
   Inc(i);
   Inc(F);
   until (tempStr[2] <> '-') ;

   numberofelements := i;
   Setlength(Snodearray, i + 2);
   Setlength(SnodearrayN, i + 2);
   Setlength(SnodearrayC, i + 2);
   nulltuple := ' 0.00000E+00  0.00000E+00  0.00000E+00';
   Setlength(Splice, 20*i + 2);
   Setlength(sxxArray, 20*i + 2);
   Setlength(syyArray, 20*i + 2);
   Setlength(szzArray, 20*i + 2);
   Setlength(sxyArray, 20*i + 2);
   Setlength(syzArray, 20*i + 2);
   Setlength(szxArray, 20*i + 2);
   Setlength(vMArray, 20*i + 2);
   Setlength(DispxArray, 20*i + 2);
   Setlength(DispyArray, 20*i + 2);
   Setlength(DispzArray, 20*i + 2);
   Setlength(Nodenum, 20*i + 2);
   Setlength(Nodetuple, 20*i + 2);
   Setlength(PrincipalSone, 20*i + 2);
   Setlength(PrincipalStwo, 20*i + 2);
   Setlength(PrincipalSthree, 20*i + 2);
   Setlength(TrescaS, 20*i + 2);


    {######################################################################### }
    {%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       capture CalculiX element type numbers in an array}
   F := 1;
   repeat
   tempStr:=ReadLine(F);
   Inc(F);
   until AnsiPos('    2C', tempStr) <> 0;
   pos2C:=F-1;
   Inc(F);
   j := 1;
   Sintset := '-0123456789';

   repeat
   typeL := '';
   tempStr:=ReadLine(F);
   if tempStr[3] = '1' then
   Inc(F);
   until tempStr[3] = '3';
   Inc(F, 2);
   repeat
   tempStr:=ReadLine(F);
   typeL := '';
   i := 0;
   if tempStr[3] = '1' then begin
   repeat
   if (AnsiPos(tempStr[i], Sintset) = 0) then Inc(i);
   until (AnsiPos(tempStr[i], Sintset) <> 0);
   repeat
   if (AnsiPos(tempStr[i], Sintset)<> 0) then Inc(i);
   until (AnsiPos(tempStr[i], Sintset) = 0);
   repeat
   if (AnsiPos(tempStr[i], Sintset)= 0) then Inc(i);
   until (AnsiPos(tempStr[i], Sintset) <> 0);
   repeat
   if (AnsiPos(tempStr[i], Sintset)<> 0) then Inc(i);
   until (AnsiPos(tempStr[i], Sintset) = 0);
   repeat
   if (AnsiPos(tempStr[i], Sintset)= 0) then Inc(i);
   until (AnsiPos(tempStr[i], Sintset) <> 0);

   repeat
   if (AnsiPos(tempStr[i], Sintset) <> 0) then begin
   typeL := typeL + tempStr[i];
   Inc(i);
   end;
   until (AnsiPos(tempStr[i], Sintset) = 0);
   SnodearrayC[j] := typeL;
   Inc(j);
   end;

   Inc(F);
   until (j = numberofelements + 1) ;
   {%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

    OutBuf.Add('# vtk DataFile Version 2.0');
    OutBuf.Add('Converted by Vtkpost version 1.3');
    OutBuf.Add('ASCII');
    OutBuf.Add('DATASET UNSTRUCTURED_GRID');

   {  ***********************************************************************
       count the number of nodes }
      F := pos2C;
      i := 0;
    tempStr:=ReadLine(F);
    repeat
    if (AnsiPos('3C', tempStr) = 0) then
    begin
    Inc(F);
    tempStr:=ReadLine(F);
    if (tempStr[3] = '1') then
    Inc(i);
    end;
    until (AnsiPos('3C', tempStr) <> 0);

    numberofnodes := i; 
    OutBuf.Add('POINTS ' + InttoStr(numberofnodes) + ' double');  
  {  *********************************************************************** }
  {  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      put the node tuples in one array, and the node numbers in another }
    F := pos2C;

     repeat
     tempStr:=ReadLine(F);
     charz := '';
     if (AnsiPos('.', tempStr) = 0) and (AnsiPos('3C', tempStr) = 0) then
     Inc(F);
     until (AnsiPos('.', tempStr) <> 0) or (AnsiPos('3C', tempStr) <> 0);

     j := 1;
     repeat
     nodenumtemp := '';
     nodetupletemp := '';
     i := 0;
     tempStr:=ReadLine(F);
     tempStr := Copy(tempStr, 1, 13);
     nodetupletemp:=ReadLine(F);
     nodetupletemp := Copy(nodetupletemp, 14, Length(nodetupletemp));
     repeat
     if (AnsiPos(tempStr[i], Sintset) = 0) then Inc(i);
     until (AnsiPos(tempStr[i], Sintset) <> 0);
     repeat
     if (AnsiPos(tempStr[i], Sintset)<> 0) then Inc(i);
     until (AnsiPos(tempStr[i], Sintset) = 0);
     repeat
     if (AnsiPos(tempStr[i], Sintset)= 0) then Inc(i);
     until (AnsiPos(tempStr[i], Sintset) <> 0);
     repeat
     if (AnsiPos(tempStr[i], Sintset) <> 0) then begin
     nodenumtemp := nodenumtemp + tempStr[i];
     Inc(i);
     end;
     until (AnsiPos(tempStr[i], Sintset) = 0);
     Nodenum[j] := nodenumtemp;
     Insert(' ', Nodetupletemp, 13);
     Insert(' ', Nodetupletemp, 26);
     Nodetuple[j] := Nodetupletemp;

     Inc(j);
     Inc(F);

     until (j = numberofnodes + 1) ;

     {  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  }

  Inc(F);
  startelline := F;

     tempStr := '';
     i := 1 ;
     repeat
     for j := 2*i -1 to 2*i do
     tempStr := tempStr + ' ' + Nodetuple[j];
     OutBuf.Add(tempStr);
     tempStr := '';
     Inc(i);
     until i = (numberofnodes div 2) + 1;

     k := 2*(numberofnodes div 2);
     tempStr := '';
     if numberofnodes mod 2 <> 0 then begin
     tempStr :=  ' ' + Nodetuple[k + 1];
     OutBuf.Add(tempStr);
     end;

     Gauge1.visible := true;
     Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
     lbInfo.Caption:='Process...';
     lbInfo.Show;

 vtkWriteElements(sender);
 end;


procedure TFrmVtkpost.vtkWriteElements(Sender: TObject);
var
  e, i, j, k, p, m, n, x, z, F, b, tempspliceindex, linesleft, cumu  : integer;
 tempStr, tempStrtwo, Sintset, s, tempn, tempntwo : string;
 Toggle, nonseq  : boolean;

begin
  nonseq := false;

 {&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
   establish equivalence of array indexing between CalculiX and vtk }
   for i := 1 to numberofelements do begin
   if SnodearrayC[i] = '1' then Snodearray[i] := '12';
   if SnodearrayC[i] = '2' then Snodearray[i] := '13';
   if SnodearrayC[i] = '3' then Snodearray[i] := '10';
   if SnodearrayC[i] = '4' then Snodearray[i] := '25';
   if SnodearrayC[i] = '5' then Snodearray[i] := '99';
   if SnodearrayC[i] = '6' then Snodearray[i] := '24';
   if SnodearrayC[i] = '7' then Snodearray[i] := '5';
   if SnodearrayC[i] = '8' then Snodearray[i] := '22';
   if SnodearrayC[i] = '9' then Snodearray[i] := '9';
   if SnodearrayC[i] = '10' then Snodearray[i] := '23';
   if SnodearrayC[i] = '11' then Snodearray[i] := '3';
   if SnodearrayC[i] = '12' then Snodearray[i] := '21';
   end;

   for i := 1 to (Length(Snodearray)-1) do begin
   if Snodearray[i] = '12' then SnodearrayN[i] := '8';
   if Snodearray[i] = '13' then SnodearrayN[i] := '6';
   if Snodearray[i] = '10' then SnodearrayN[i] := '4';
   if Snodearray[i] = '25' then SnodearrayN[i] := '20';
   if Snodearray[i] = '99' then SnodearrayN[i] := '15';
   if Snodearray[i] = '24' then SnodearrayN[i] := '10';
   if Snodearray[i] = '5' then SnodearrayN[i] := '3';
   if Snodearray[i] = '22' then SnodearrayN[i] := '6';
   if Snodearray[i] = '9' then SnodearrayN[i] := '4';
   if Snodearray[i] = '23' then SnodearrayN[i] := '8';
   if Snodearray[i] = '3' then SnodearrayN[i] := '2';
   if Snodearray[i] = '21' then SnodearrayN[i] := '3';
   end;
 {&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&}
  {#######################################################################
    Fill in first slot in various arrays to allow alternate loop limits}
    Snodearray[0] := Snodearray[1];
    SnodearrayC[0] := SnodearrayC[1];
    SnodearrayN[0] := SnodearrayN[1];
    Nodenum[0] := '0';

   Screen.Cursor := crHourGlass;
   edges := 0;
   for k := 1 to numberofelements do begin
   edges := edges + StrtoInt(SnodearrayN[k]);
   end;
   edges := edges + numberofelements;

   F := startelline;
   Sintset := '0123456789';
   r := 0;
   s := '';
   Toggle := true;
   linesleft := InBuf.Count - startelline;
   cumu := 0;
 {###########################################################################}
 {@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   find a line position to start reading elements from}
  repeat
   tempStr:=ReadLine(F);
   if (tempStr[3] <> '1') {or (tempStr[3] = '2')} then begin
   Inc(F);
   Dec(linesleft);
   end;
   until (tempStr[3] = '1') or (linesleft = 2);

   if linesleft = 2 then begin
      ErrBuf.Clear;
      ErrBuf.Add('Error writing elements.');
      ShowError;
      Exit;
   end;


   Inc(F);
   k := 0;
   r := 0;

   {@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@}
 {  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  put the element descriptions in an array}

 begin

  repeat
  tempStr:=ReadLine(F);
  if (tempStr[3] = '2') then
   {^^^skip alternate lines which do not have node numbers}
  begin
    if Toggle = True then Toggle := False
    else if Toggle = False then Toggle := True;

  tempStrtwo := '';
  Insert(' ', tempStr, (Length(tempStr)+1)); {pad the right hand side with one blank}
  Delete(tempstr, 1, 3);
    { ^^^ gets rid of the pesky -2 and descendents, making Splice full of data
    without junk entries. }

  repeat
   repeat
   s := '';
   if (AnsiPos(tempStr[1], Sintset) = 0) then Delete(tempStr, 1, 1);
   until (tempStr[1] <> ' ') or (Length(tempStr) <= 1);
   {^^^trim the left whitespace.}

   repeat
   if (AnsiPos(tempStr[1], Sintset) <> 0) then begin
    {if tempStr[1] <> ' ' then begin}
   s := s + tempStr[1];
   {^^^put actual line of node numbers in variable s}
   Delete(tempStr, 1, 1);
   end;
   until (AnsiPos(tempStr[1], Sintset) = 0);

   s := ( ' ' + InttoStr(strtoInt(s) - 1));
   { ^^^ change from CalculiX node numbering system to vtk numbering system   }

   Splice[k] := s;
   Inc(k);

   until Length(tempStr) <= 1;
  { ^^^ until all the lines of descriptive node data are in }

    end;
   Inc(F);
   Inc(r);
    Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
   until AnsiPos(' -3', tempStr) <> 0;
   end;
   {   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   }

    {  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   Option to rename nodes if they are not in perfect sequence, a requirement
     for vtk  }
   nonseq := false;
   for i := 1 to numberofnodes do
    if StrtoInt(Nodenum[i]) <> i then begin
    nonseq := true;
    break;
    end;

    if nonseq then begin
    begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Node numbering is not sequential,');
     ErrBuf.Add( 'as required by vtk format rules.');
     ErrBuf.Add( 'Selecting OK renames nodes (may take long time)');

     if ShowOkCancel = IDOK then begin
     x:=Gauge1.Position;
     lbInfo.Caption:='Rename nodes...';
     Application.ProcessMessages;

   for i := 1 to numberofnodes do begin
   k := 0;
   { ^^^ had to change initialization of k to 0, when junk digits were
   purged from Splice. }
   if StrtoInt(Nodenum[i]) <> i then
   repeat
   Val(Splice[k],n,e);
   Val(Nodenum[i],z,e);
   Dec(z);
   if n=z then
   Splice[k] := ' ' + InttoStr(i - 1);
   { Z: very slow!!!
   if Splice[k] = ' ' + InttoStr(StrtoInt(Nodenum[i]) - 1) then
   Splice[k] := ' ' + InttoStr(i - 1);
   }
   Inc(k);
   until k = edges - numberofelements;
   Gauge1.Position:=Round(100*i/numberofnodes);
   end;   // for  i

   Gauge1.Position:=x;
   lbInfo.Caption:='Process...';
   lbInfo.Update;
   end; // if ShowOkCancel
   end; // if nonseq
   end;

   for b := 1 to numberofnodes do
   Nodenum[b] := InttoStr(b);

 {  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  }

     m := 0;
   {  ^^^ m is used several times below to track the existence of Type 5
     elements }

   OutBuf.Add(' ');
   for p := 1 to numberofelements do begin
   if SnodearrayN[p] = '15' then
   Inc(m);
   end;
   OutBuf.Add('CELLS ' + InttoStr(numberofelements + 4*m) + ' ' +
      InttoStr(edges + 21*m + 4*m));

  { +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    allow converting sets of elements of mixed type.  This section is now
      the consolidated sorting and writing section. }

   for k := 1 to numberofelements do begin

   if (SnodearrayC[k] <> '4') and (SnodearrayC[k] <> '5')then begin
   tempn := '';
   j := 0;
  repeat
   Inc(j);
   for i := 0 to StrtoInt(SnodearrayN[k]) -1 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   cumu := cumu + StrtoInt(SnodearrayN[k]);
   until j = 1;
   OutBuf.Add(SnodearrayN[k] + tempn {+'    ' +InttoStr(cumu)});
   end
   else if (SnodearrayC[k] = '4') then begin
   tempn := '';
   p := 0;
   repeat
   for i := 0 to 11 do
   tempn := tempn  + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 16 to 19 do
   tempn := tempn  + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 12 to 15 do
   tempn := tempn  + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   cumu := cumu + StrtoInt(SnodearrayN[k]);
   p := p + StrtoInt(SnodearrayN[k])
   until p = StrtoInt(SnodearrayN[k]) ;
   OutBuf.Add(SnodearrayN[k] + tempn {+ '    ' + InttoStr(cumu)});
   end
   else if SnodearrayC[k] = '5' then begin
   tempn := '';
   p := 0;
   repeat
   for i := 0 to 2 do
   tempn := tempn +  Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
    for i := 6 to 8 do
   tempn := tempn +  Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   p := P + 6;
   until p =  6;
   OutBuf.Add('6' + tempn);
   tempn := '';
   repeat
   for i := 3 to 5 do
   tempn := tempn +  Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
    for i := 12 to 14 do
   tempn := tempn +  Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   p := p + 6;
   until p = 12;
   OutBuf.Add('6' + tempn);
   tempn := '';
   repeat
   for i := 0 to 1 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 4 to 4 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 3 to 3 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 6 to 6 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 10 to 10 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 12 to 12 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 9 to 9 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   p := p + 8;
   until p = 20;
   OutBuf.Add('8' + tempn);
   tempn := '';
   repeat
   for i := 1 to 2 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 5 to 5 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 4 to 4 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 7 to 7 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 11 to 11 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 13 to 13 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 10 to 10 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   p := p + 8;
   until p = 28;
   OutBuf.Add('8' + tempn);
   tempn := '';
   repeat
   for i := 2 to 2 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 0 to 0 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 3 to 3 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 5 to 5 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 8 to 9 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 14 to 14 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   for i := 11 to 11 do
   tempn := tempn + Copy(Splice[i + cumu], 1, Length(Splice[i + cumu]));
   p := p + 8;
   until p = 36;
   OutBuf.Add('8' + tempn);
   tempn := '';
   Inc(cumu,15);
   end;
  Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
   end; // for k := 1 to numberofelements

 {++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

   OutBuf.Add(' ');
   OutBuf.Add('CELL_TYPES ' + InttoStr(numberofelements + 4*m));

  for i := 1 to numberofelements do begin
   if SnodearrayN[i] <> '15' then
   OutBuf.Add(Snodearray[i])
   else if SnodearrayN[i] = '15' then begin
   OutBuf.Add('22');
   OutBuf.Add('22');
   OutBuf.Add('23');
   OutBuf.Add('23');
   OutBuf.Add('23');
   end;
   end;
   { ^^^ the above lines ignore the contents of Snodearray in the case of
   Type 5 elements, which is necessary since they split into two different
   element types in vtk. }

    vtkWriteThermo(sender);
    if Nostress = false then begin
    vtkWritevonMises(sender);
    vtkWritePrincipal(sender);
    end; 

     Shape3.brush.color := clMaroon;
     Shape4.brush.color := clLime;
     Screen.Cursor := crDefault;
     Gauge1.Position := 100;
     Delay(300);
     Gauge1.Hide;
     lbInfo.Hide;
end;


procedure TFrmVtkpost.vtkWritevonMises(Sender: TObject);
var
    i, k, j : integer;
    tempStr  : string;
    sxx, syy, szz, syz, szx, sxy, vonMises : double;
begin
    vonmiscone := 0;
    misesconemin := 1E+15;
    misesconemax := -1E+15;

   if Nostress = false then begin

     begin
     OutBuf.Add(' ');
     OutBuf.Add('SCALARS von_Mises-stress double 1');
     OutBuf.Add('LOOKUP_TABLE default');
     end;

     for i := 0 to (numberofnodes - 1) do begin
     sxx := sxxArray[i];
     syy := syyArray[i];
     szz := szzArray[i];
     syz := syzArray[i];
     szx := szxArray[i];
     sxy := sxyArray[i];

     vonMises := Sqrt(0.5*(sqr(sxx-syy)+sqr(syy-szz)+sqr(szz-sxx)+
             6*(sqr(sxy)+sqr(syz)+sqr(szx))));

     vMArray[i] := vonMises;
     
     if misesconemax < vMArray[i] then begin
     misesconemax := vMArray[i];
     misesnodemax := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
     if misesconemin > vMArray[i] then begin
     misesconemin := vMArray[i];
     misesnodemin := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
   end;

     k := 0;

     for i := 0 to ((numberofnodes div 6) - 1) do begin
     tempStr := '';
     k := i*6;
     for j := k to k + 5 do
     tempStr := tempStr + ' ' + FloattoStrF(vMArray[j], ffExponent, 6, 2);
     OutBuf.Add(tempStr);
      Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
     end;

     tempStr := '';
     for i := k + 6 to k + numberofnodes mod 6 + 5 do
     tempStr := tempStr + ' ' + FloattoStrF(vMArray[i], ffExponent, 6, 2);
     OutBuf.Add(tempStr);
end;
end;

procedure TFrmVtkpost.vtkWritePrincipal(Sender: TObject);
var
  i, j, k, N, dbz, Deg : integer;
  sxx, syy, szz, syz, szx, sxy,  
  trescar, ppone, pptwo, ppthree, x : double;
  ione, itwo, ithree : double;
  tempStr : string;
  lambda : PVector;
  A, V : PMatrix;
  Coef : PVector;
  Z : PCompVector;
  dbzero : boolean;
begin
    k := 0; 
    trescone := 0;
    trescaconemin := 1E+15;
    poneconemin := 1E+15;
    ptwoconemin := 1E+15;
    pthreeconemin := 1E+15;
    trescaconemax := -1E+15;
    poneconemax := -1E+15;
    ptwoconemax := -1E+15;
    pthreeconemax := -1E+15;
    ppone := 0;
    pptwo := 0;
    ppthree := 0;
    N := 3;
    x := 0.0;
    dbz := 0;
    dbzero := false;
    Deg := 3;
    DimVector(Coef, Deg);
    DimCompVector(Z, Deg);

    begin
     OutBuf.Add(' ');
     OutBuf.Add('SCALARS Principal-Stress float 3');
     OutBuf.Add('LOOKUP_TABLE default');
     end;

    for i := 0 to (numberofnodes - 1) do begin
     sxx := sxxArray[i];
     syy := syyArray[i];
     szz := szzArray[i];
     syz := syzArray[i];
     szx := szxArray[i];
     sxy := sxyArray[i];

     DimMatrix(A, N, N);
     DimMatrix(V, N, N);
     DimVector(Lambda, N);

     A^[1]^[1] := sxx - x;
     A^[1]^[2] := sxy;
     A^[1]^[3] := szx;
     A^[2]^[1] := sxy;
     A^[2]^[2] := syy - x;
     A^[2]^[3] := syz;
     A^[3]^[1] := szx;
     A^[3]^[2] := syz;
     A^[3]^[3] := szz - x;

     { Compute eigenvalues or solve cubic polynomial }
     try
     Jacobi(A, 1, N, MaxIter, Tol, Lambda, V);
     pptwo := lambda[1];
     ppthree := lambda[2];
     ppone := lambda[3];
     except
     on EAccessViolation do begin
         try
         ione := sxx + syy + szz;
         itwo := sxx*syy + syy*szz + szz*sxx - sqr(sxy) - sqr(syz) - sqr(szx);
         ithree := sxx*syy*szz - sxx*sqr(syz) - syy*sqr(szx) - szz*sqr(sxy) +
           2*sxy*syz*szx;

         Coef^[0] := -1.000000000*ithree;
         Coef^[1] := itwo;
         Coef^[2] := -1.000000000*ione;
         Coef^[3] := 1.000000000;
         
         RootPol3(Coef, Z);
         ppone := Z^[1].X ;
         pptwo := Z^[2].X;
         ppthree := Z^[3].X;
         except
         on EZeroDivide do begin
         dbzero := true;
         Inc(dbz);
         ppone := 5.55555E-15;
         pptwo := 5.55555E-15;
         ppthree := 5.55555E-15;
         end;
         end;
     end;
     end;

     if poneconemax < pptwo then begin
     poneconemax := pptwo;
     ponenodemax := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
     if poneconemin > pptwo then begin
     poneconemin := pptwo;
     ponenodemin := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
     if ptwoconemax < ppthree then begin
     ptwoconemax := ppthree;
     ptwonodemax := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
     if ptwoconemin > ppthree then begin
     ptwoconemin := ppthree;
     ptwonodemin := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
     if pthreeconemax < ppone then begin
     pthreeconemax := ppone;
     pthreenodemax := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
     if pthreeconemin > ppone then begin
     pthreeconemin := ppone;
     pthreenodemin := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;

    tempStr := tempStr + (FloattoStrF(pptwo, ffExponent, 6, 2) + ' ' +
         FloattoStrF(ppthree, ffExponent, 6, 2) + ' ' +
         FloattoStrF(ppone, ffExponent, 6, 2) + ' ');

     if (Length(tempStr) > 40) or (i = numberofnodes - 1) then begin
     OutBuf.Add(tempStr);
     tempStr := '';
     end;

     PrincipalSone[i] := pptwo;
     PrincipalStwo[i] := ppthree;
     PrincipalSthree[i] := ppone;

     DelMatrix(A, N, N);
     DelMatrix(V, N, N);
     DelVector(Lambda, N);

   Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
     end;

     if dbzero = true then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Principal Stresses for ' + InttoStr(dbz));
     ErrBuf.Add( 'node(s) of ' + InttoStr(numberofnodes) + ' nodes');
     ErrBuf.Add( 'were not successfully');
     ErrBuf.Add( 'calculated.  The dummy');
     ErrBuf.Add( 'value of 5.55555E-15');
     ErrBuf.Add( 'was inserted for these');
     ErrBuf.Add( 'undetermined stresses.');
     ShowError;
     end;

    begin
    OutBuf.Add(' ');
    OutBuf.Add('SCALARS Tresca-stress float 1');
    OutBuf.Add('LOOKUP_TABLE default');
    end;

     for i := 0 to numberofnodes - 1 do begin
     trescar := 0.5*(PrincipalSone[i] - PrincipalSthree[i]);
     TrescaS[i] := trescar;
     if trescaconemin > trescar then begin
     trescaconemin := trescar;
     trescanodemin := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
     if trescaconemax < trescar then begin
     trescaconemax := trescar;
     trescanodemax := InttoStr(StrtoInt(Nodenum[i]) + 1);
     end;
     end;

     for i := 0 to ((numberofnodes div 6) - 1) do begin
     tempStr := '';
     k := i*6;
     for j := k to k + 5 do
     tempStr := tempStr + ' ' + FloattoStrF(TrescaS[j], ffExponent, 6, 2);
     OutBuf.Add(tempStr);
     Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
     end;

     tempStr := '';
     for i := k + 6 to k + numberofnodes mod 6 + 5 do
     tempStr := tempStr + ' ' + FloattoStrF(TrescaS[i], ffExponent, 6, 2);
     OutBuf.Add(tempStr);       
end;

procedure TFrmVtkpost.BitBtn6Click(Sender: TObject);
var
  myvarchange : string;
begin
  if (OutBuf.Count < 4) or (InBuf.Count < 4) then begin
     ErrBuf.Clear;
     ErrBuf.Add('Not ready to save.');
     ShowError;
     Exit;
  end;

  myvarchange := ChangeFileExt(myvar2, '.vtk');
  SaveDialog1.filename := myvarchange;
   if SaveDialog1.execute then begin
           OutBuf.SavetoFile(SaveDialog1.filename);
     Shape1.brush.color := clRed;
     Shape2.brush.color := clGreen;
     Shape3.brush.color := clMaroon;
     Shape4.brush.color := clLime;
     Shape5.brush.color := clMaroon;
     Shape6.brush.color := clLime;
   end;
end;

procedure TFrmVtkpost.CheckBox1Click(Sender: TObject);
begin
    if CheckBox1.Checked = true then begin
    SetWindowPos(
  Handle,
  HWND_TOPMOST,
  0,
  0,
  0,
  0,
  SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  end;

    if CheckBox1.Checked = false then begin
    SetWindowPos(
  Handle,
  {HWND_BOTTOM,}
  HWND_NOTOPMOST,
  0, 
  0,
  0,
  0, 
  SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  end;
end;

procedure TFrmVtkpost.BitBtn1Click(Sender: TObject);
var
  i : integer;
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if InBuf.Count > 4 then begin
    for i := 1 to numberofnodes do
   
    if AnsiCompareStr(Nodenum[i], Edit7.text) = 0 then begin
     StringGrid1.cells[1,1] := FloattoStrF(sxxArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,2] := FloattoStrF(syyArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,3] := FloattoStrF(szzArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,4] := FloattoStrF(syzArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,5] := FloattoStrF(szxArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,6] := FloattoStrF(sxyArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,7] := FloattoStrF(vMArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,8] := FloattoStrF(TrescaS[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,9] := FloattoStrF(PrincipalSone[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,10] := FloattoStrF(PrincipalStwo[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,11] := FloattoStrF(PrincipalSthree[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,12] := FloattoStrF(DispxArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,13] := FloattoStrF(DispyArray[i - 1], ffExponent, 6, 2);
     StringGrid1.cells[1,14] := FloattoStrF(DispzArray[i - 1], ffExponent, 6, 2);
     end;
     end; 
end;

procedure TFrmVtkpost.BitBtn3Click(Sender: TObject);
var
  i : integer;
begin
  i := (StrtoInt(Edit7.text));
  if (i >= 1) and (i < numberofnodes ) then
  Inc(i);
  Edit7.Text := InttoStr(i);
    BitBtn1Click(sender);
end;

procedure TFrmVtkpost.BitBtn5Click(Sender: TObject);
var
  i : integer;
begin
  for i := 1 to 14 do
  StringGrid1.cells[1, i] := '';
  i := (StrtoInt(Edit7.text));
  if (i > 1) and (i <= numberofnodes) then
  Dec(i);
  Edit7.Text := InttoStr(i);
  BitBtn1click(sender);
end;

procedure TFrmVtkpost.Edit7Change(Sender: TObject);
var
 i : integer;
begin
  for i := 1 to 14 do
  StringGrid1.cells[1, i] := ''; 
end;

procedure TFrmVtkpost.RadioButton1Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton1.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(sxxconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ sxxnodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton2Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton2.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(sxxconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ sxxnodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton3Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton3.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(syyconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ syynodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton4Click(Sender: TObject);
begin

    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton4.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(syyconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ syynodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton5Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton5.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(szzconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ szznodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton6Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton6.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(szzconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ szznodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton7Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton7.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(syzconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ syznodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton8Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton8.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(syzconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ syznodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton9Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton9.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(szxconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ szxnodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton10Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton10.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(szxconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ szxnodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton11Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton11.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(sxyconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ sxynodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton12Click(Sender: TObject);
begin 
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton12.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(sxyconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ sxynodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton13Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton13.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(misesconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ misesnodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton14Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton14.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(misesconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ misesnodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton15Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton15.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(trescaconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ trescanodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton16Click(Sender: TObject);
begin 
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton16.Checked = true then begin 
     StringGrid1.cells[0,0] := FloattoStrF(trescaconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ trescanodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton18Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton18.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(poneconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ ponenodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton19Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton19.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(ptwoconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ ptwonodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton20Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton20.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(ptwoconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ ptwonodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton22Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton22.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(pthreeconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ pthreenodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton21Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton21.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(pthreeconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ pthreenodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton23Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton23.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(dispxconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ dispxnodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton24Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton24.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(dispxconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ dispxnodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton25Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton25.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(dispyconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ dispynodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton26Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton26.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(dispyconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ dispynodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton27Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton27.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(dispzconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ dispznodemin;
     end;
end;

procedure TFrmVtkpost.RadioButton28Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton28.Checked = true then begin 
     StringGrid1.cells[0,0] := FloattoStrF(dispzconemax, ffExponent, 6, 2);
     Edit1.text := ' '+ dispznodemax;
     end;
end;

procedure TFrmVtkpost.RadioButton17Click(Sender: TObject);
begin
    if Nostress = true then begin
    DisplayTableMsg(Sender);
    Exit;
    end;
    if Radiobutton17.Checked = true then begin
     StringGrid1.cells[0,0] := FloattoStrF(poneconemin, ffExponent, 6, 2);
     Edit1.text := ' '+ ponenodemin;
     end;
end;

procedure TFrmVtkpost.vtkWriteThermo(Sender: TObject);
var
    F, position, i, b, e, j, k, m, n, u, w, h, a, t, g, q, linesleft, minusfive,
    nodecounter : integer;
    charz, sharz, tempStr, tempNode : string;
    Alreadyprinted, Alreadyflaggedhigh, Alreadyflaggedlow : boolean;
    sxxconetemp, syyconetemp, szzconetemp, sxyconetemp, syzconetemp,
    szxconetemp, dispxconetemp, dispyconetemp, dispzconetemp : double;
begin
      F := startelline + r;
       j := 0;
       k := 0;
       m := 0;
       n := 0;
       u := 0;
       e := 0;
       w := 0;
       h := 0;
       a := 0;
       t := 0;
       g := 0;
       q := 0;
       sxxconemax := -1E+15;
       syyconemax := -1E+15;
       szzconemax := -1E+15;
       syzconemax := -1E+15;
       sxyconemax := -1E+15;
       szxconemax := -1E+15;
       sxxconemin := 1E+15;
       syyconemin := 1E+15;
       szzconemin := 1E+15;
       syzconemin := 1E+15;
       sxyconemin := 1E+15;
       szxconemin := 1E+15;
       dispxconetemp := 0;
       dispyconetemp := 0;
       dispzconetemp := 0;
       dispxconemax := -1E+15;
       dispyconemax := -1E+15;
       dispzconemax := -1E+15;
       dispxconemin := 1E+15;
       dispyconemin := 1E+15;
       dispzconemin := 1E+15;
      linesleft := InBuf.Count - (startelline + r);
      Alreadyprinted := false;
      Alreadyflaggedhigh := false;
      Alreadyflaggedlow := false;
      tempStr := '';
      
repeat
    repeat

    tempStr:=ReadLine(F);
    if (AnsiPos(' -4  NDTEMP', tempStr) = 0) and (AnsiPos(' -4  FLUX', tempStr) = 0)
      and (AnsiPos(' -4  RFL', tempStr) = 0) {and (AnsiPos(' -4  DISP', tempStr) = 0) }
      and (AnsiPos(' -4  STRESS', tempStr) = 0) and (AnsiPos(' -4  PDISP', tempStr) = 0)
      and (AnsiPos(' -4  SDV', tempStr) = 0) then
    Inc(F);
    Dec(linesleft);
    if Length(tempStr)=0 then begin
       Inc(F);
       continue;
    end;
    until (AnsiPos(' -4  NDTEMP', tempStr) <> 0) or (AnsiPos(' -4  FLUX', tempStr) <> 0)
      or (AnsiPos(' -4  RFL', tempStr) <> 0) or (AnsiPos(' -4  DISP', tempStr) <> 0)
      or (AnsiPos(' -4  STRESS', tempStr) <> 0) or (AnsiPos(' -4  PDISP', tempStr) <> 0)
      or (AnsiPos(' -4  SDV', tempStr) <> 0) or (AnsiPos(' -4  FORC', tempStr) <> 0)
      or (AnsiPos(' -4  PE', tempStr) <> 0) or (AnsiPos(' -4  ENER', tempStr) <> 0)
      or (AnsiPos(' -4  STRAIN', tempStr) <> 0) or (AnsiPos(' -4  ZZSTR', tempStr) <> 0)
      or (linesleft <= 2);

    if ((AnsiPos(' -4  NDTEMP', tempStr) <> 0) or (AnsiPos(' -4  FLUX', tempStr) <> 0)
      or (AnsiPos(' -4  RFL', tempStr) <> 0) or (AnsiPos(' -4  FORC', tempStr) <> 0)
      or (AnsiPos(' -4  DISP', tempStr) <> 0) or (AnsiPos(' -4  STRESS', tempStr) <> 0)
      or (AnsiPos(' -4  PDISP', tempStr) <> 0) or (AnsiPos(' -4  SDV', tempStr) <> 0)
      or (AnsiPos(' -4  PE', tempStr) <> 0) or (AnsiPos(' -4  ENER', tempStr) <> 0)
      or (AnsiPos(' -4  STRAIN', tempStr) <> 0) or (AnsiPos(' -4  ZZSTR', tempStr) <> 0))
       and (Alreadyprinted = false) then begin

      OutBuf.Add(' ');
      OutBuf.Add( 'POINT_DATA ' + InttoStr(numberofnodes));
      Alreadyprinted := true;
      end;

  if (AnsiPos(' -4  NDTEMP', tempStr) <> 0) then  begin
     Inc(j);
     Inc(F);
     repeat
     tempStr := ReadShortLine(F);
     if (AnsiPos(' -5  ', tempStr) <> 0) then
     Inc(F);
     until (AnsiPos(' -5  ', tempStr) = 0);
     charz := '';
     nodecounter := 0; 
     sharz := '';

    OutBuf.Add(' ');
    OutBuf.Add('SCALARS NDTEMP_' + InttoStr(j) + ' double 1');
    OutBuf.Add('LOOKUP_TABLE default');

     repeat
     tempStr:=ReadLine(F);
     charz := '';
     if Length(sharz) > 70 then sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     Inc(nodecounter);
     if AnsiPos('*', tempStr) <> 0 then charz :=
       '  0.00000E+00';
     Sharz := Sharz + Copy(charz, 1, Length(charz));
     if (Length(sharz) > 70) or (nodecounter = numberofnodes) then
     OutBuf.Add(sharz);
     Inc(F);
   Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
   until CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (Alreadyflaggedlow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in NDTEMP_' + InttoStr(j) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
     end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (Alreadyflaggedhigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in NDTEMP_' + InttoStr(j) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
     end;
 end;

    if (AnsiPos(' -4  FLUX', tempStr) <> 0) then begin
    Inc(k);
    Inc(F);
    repeat
    tempStr:=ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    nodecounter := 0;
    sharz := '';

    OutBuf.Add(' ');
    OutBuf.Add('SCALARS FLUX_' + InttoStr(k) + ' double 3');
    OutBuf.Add('LOOKUP_TABLE default');

     repeat
     tempStr:=ReadLine(F);
     charz := '';
     if Length(sharz) > 70 then sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     Inc(nodecounter);
     if AnsiPos('*', tempStr) <> 0 then Charz :=
       '  0.00000E+00  0.00000E+00  0.00000E+00';
     Sharz := Sharz + Copy(charz, 1, Length(charz));
     if (Length(sharz) > 70) or (nodecounter = numberofnodes) then
     OutBuf.Add(sharz);
     Inc(F);
    Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (AlreadyFlaggedlow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in FLUX_' + InttoStr(k) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
         end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (AlreadyFlaggedhigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in FLUX_' + InttoStr(k) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
     end;
  end;

    if (AnsiPos(' -4  RFL', tempStr) <> 0) then begin
    Inc(m);
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    nodecounter := 0;
    sharz := '';

    OutBuf.Add(' ');
    OutBuf.Add('SCALARS RFL_' + InttoStr(m) + ' double 1');
    OutBuf.Add('LOOKUP_TABLE default');     

     repeat
     tempStr := ReadLine(F);
     charz := '';
     if Length(sharz) > 70 then sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     Inc(nodecounter);
     if AnsiPos('*', tempStr) <> 0 then charz :=
       '  0.00000E+00';
     Sharz := Sharz + Copy(charz, 1, Length(charz));
     if (Length(sharz) > 70) or (nodecounter = numberofnodes) then
     OutBuf.Add(sharz);
     Inc(F);
   Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in RFL_' + InttoStr(m) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
         end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in RFL_' + InttoStr(m) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
     end;
 end; 

    if (AnsiPos(' -4  PE', tempStr) <> 0) then begin
    Inc(a);
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    nodecounter := 0;
    sharz := '';

    OutBuf.Add(' ');
    OutBuf.Add('SCALARS PE_' + InttoStr(a) + ' double 1');
    OutBuf.Add('LOOKUP_TABLE default');     

     repeat
     tempStr := ReadLine(F);
     charz := '';
     if Length(sharz) > 70 then sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     Inc(nodecounter);
     if AnsiPos('*', tempStr) <> 0 then charz :=
       '  0.00000E+00';
     Sharz := Sharz + Copy(charz, 1, Length(charz));
     if (Length(sharz) > 70) or (nodecounter = numberofnodes) then
     OutBuf.Add(sharz);
     Inc(F);
   Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in PE_' + InttoStr(a) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
         end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in PE_' + InttoStr(a) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
     end;
 end;

    if (AnsiPos(' -4  ENER', tempStr) <> 0) then begin
    Inc(t);
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    nodecounter := 0;
    sharz := '';

    OutBuf.Add(' ');
    OutBuf.Add('SCALARS ENER_' + InttoStr(t) + ' double 1');
    OutBuf.Add('LOOKUP_TABLE default');     

     repeat
     tempStr := ReadLine(F);
     charz := '';
     if Length(sharz) > 70 then sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     Inc(nodecounter);
     if AnsiPos('*', tempStr) <> 0 then charz :=
       '  0.00000E+00';
     Sharz := Sharz + Copy(charz, 1, Length(charz));
     if (Length(sharz) > 70) or (nodecounter = numberofnodes) then
     OutBuf.Add(sharz);
     Inc(F);
   Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in ENER_' + InttoStr(t) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
         end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in ENER_' + InttoStr(t) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
      end;
 end;

 if (AnsiPos(' -4  STRESS', tempStr) <> 0) then begin
    Nostress := false;
    nodecounter := 0;
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    b := 0;
    Inc(w);
    OutBuf.Add(' ');
    OutBuf.Add('SCALARS STRESS_' + InttoStr(w) + ' double 6');
    OutBuf.Add('LOOKUP_TABLE default');

     repeat
     tempStr := ReadLine(F);

     charz := '';
     sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     if AnsiPos('*', tempStr) <> 0 then
     charz :=
     '  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00';
     Inc(F);

     sharz := Copy(charz, 40, 39);
     sxxArray[b] := StrtoFloat(Copy(charz, 2, 12));
     syyArray[b] := StrtoFloat(Copy(charz, 15, 12));
     szzArray[b] := StrtoFloat(Copy(charz, 28, 12));
     sxyArray[b] := StrtoFloat(Copy(sharz, 2, 12));
     syzArray[b] := StrtoFloat(Copy(sharz, 15, 12));
     szxArray[b] := StrtoFloat(Copy(sharz, 28, 12));

     if sxxconemax < sxxArray[b] then begin
     sxxconemax := sxxArray[b];
     sxxnodemax := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if sxxconemin > sxxArray[b] then begin
     sxxconemin := sxxArray[b];
     sxxnodemin := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if syyconemax < syyArray[b] then begin
     syyconemax := syyArray[b];
     syynodemax := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if syyconemin > syyArray[b] then begin
     syyconemin := syyArray[b];
     syynodemin := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if szzconemax < szzArray[b] then begin
     szzconemax := szzArray[b];
     szznodemax := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if szzconemin > szzArray[b] then begin
     szzconemin := szzArray[b];
     szznodemin := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if sxyconemax < sxyArray[b] then begin
     sxyconemax := sxyArray[b];
     sxynodemax := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if sxyconemin > sxyArray[b] then begin
     sxyconemin := sxyArray[b];
     sxynodemin := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if syzconemax < syzArray[b] then begin
     syzconemax := syzArray[b];
     syznodemax := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if syzconemin > syzArray[b] then begin
     syzconemin := syzArray[b];
     syznodemin := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if szxconemax < szxArray[b] then begin
     szxconemax := szxArray[b];
     szxnodemax := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;
     if szxconemin > szxArray[b] then begin
     szxconemin := szxArray[b];
     szxnodemin := InttoStr(StrtoInt(Nodenum[b]) + 1);
     end;

     OutBuf.Add(charz);
     Inc(b);
     Inc(nodecounter);
    Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in STRESS_' + InttoStr(w) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
         end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in STRESS_' + InttoStr(w) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
     end;
  end;

    if (AnsiPos(' -4  STRAIN', tempStr) <> 0) then begin
    {Nostress := false; }
    nodecounter := 0;
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    b := 0;
    Inc(g);
    OutBuf.Add(' ');
    OutBuf.Add('SCALARS STRAIN_' + InttoStr(g) + ' double 6');
    OutBuf.Add('LOOKUP_TABLE default');

     repeat
     tempStr := ReadLine(F);
     charz := '';
     sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     if AnsiPos('*', tempStr) <> 0 then
     charz :=
     '  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00';
     Inc(F);

     sharz := Copy(charz, 40, 39);
     sxxArray[b] := StrtoFloat(Copy(charz, 2, 12));
     syyArray[b] := StrtoFloat(Copy(charz, 15, 12));
     szzArray[b] := StrtoFloat(Copy(charz, 28, 12));
     sxyArray[b] := StrtoFloat(Copy(sharz, 2, 12));
     syzArray[b] := StrtoFloat(Copy(sharz, 15, 12));
     szxArray[b] := StrtoFloat(Copy(sharz, 28, 12));

     OutBuf.Add(charz);
     Inc(b);
     Inc(nodecounter);
    Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in STRAIN_' + InttoStr(g) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
         end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in STRAIN_' + InttoStr(g) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
     end;
  end;

  if (AnsiPos(' -4  ZZSTR', tempStr) <> 0) then begin
    Nostress := false;
    nodecounter := 0;
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    b := 0;
    Inc(q);
    OutBuf.Add(' ');
    OutBuf.Add('SCALARS ZZSTR_' + InttoStr(q) + ' double 6');
    OutBuf.Add('LOOKUP_TABLE default');

     repeat
     tempStr := ReadLine(F);
     charz := '';
     sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     if AnsiPos('*', tempStr) <> 0 then
     charz :=
     '  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00';
     Inc(F);

     sharz := Copy(charz, 40, 39);
     sxxArray[b] := StrtoFloat(Copy(charz, 2, 12));
     syyArray[b] := StrtoFloat(Copy(charz, 15, 12));
     szzArray[b] := StrtoFloat(Copy(charz, 28, 12));
     sxyArray[b] := StrtoFloat(Copy(sharz, 2, 12));
     syzArray[b] := StrtoFloat(Copy(sharz, 15, 12));
     szxArray[b] := StrtoFloat(Copy(sharz, 28, 12));

     OutBuf.Add(charz);
     Inc(b);
     Inc(nodecounter);
    Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in ZZSTR_' + InttoStr(q) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
         end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in ZZSTR_' + InttoStr(q) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
        end;
  end;

  if (AnsiPos(' -4  SDV', tempStr) <> 0) then begin
    Inc(e);
    minusfive := 0;
    nodecounter := 0;
    sharz := '';

    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then begin
    Inc(F);
    if AnsiPos('ALL', tempStr) = 0 then
       Inc(minusfive);
    end;
    until (AnsiPos(' -5  ', tempStr) = 0);

    OutBuf.Add(' ');
    OutBuf.Add('SCALARS SDV_' + InttoStr(e) + ' double ' + InttoStr(minusfive));
    OutBuf.Add('LOOKUP_TABLE default');

     repeat
     tempStr := ReadLine(F);
     charz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     Inc(F);
     if (AnsiPos('*', tempStr) <> 0) then begin
     charz := '';
     for i := 1 to minusfive  do
      charz := charz + '  0.00000E+00'
     end;
     OutBuf.Add(charz);
     Inc(nodecounter);
    Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
   if (nodecounter < numberofnodes) and (numberofnodes > 1) and
      (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in SDV_' + InttoStr(e) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
         end;
   if (nodecounter > numberofnodes)
      and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in SDV_' + InttoStr(e) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
        end;
  end;


   until linesleft = 2;

   F := startelline + r;
   linesleft := InBuf.Count - (startelline + r);

  repeat
    repeat
    tempStr := ReadLine(F);
    if (AnsiPos(' -4  DISP', tempStr) = 0) and (AnsiPos(' -4  PDISP', tempStr) = 0)
       and (AnsiPos(' -4  FORC', tempStr) = 0) then
    Inc(F);
    linesleft := linesleft -1;
    until (AnsiPos(' -4  DISP', tempStr) <> 0) or (AnsiPos(' -4  PDISP', tempStr) <> 0)
      or (AnsiPos(' -4  FORC', tempStr) <> 0) or (linesleft <= 2);

  if (AnsiPos(' -4  DISP', tempStr) <> 0) then begin
    Inc(n);
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    sharz := '';
    nodecounter := 0;

    OutBuf.Add(' ');
    OutBuf.Add('VECTORS DISP_' + InttoStr(n) + ' float');

     repeat
     tempStr := ReadLine(F);
     charz := '';
     if Length(sharz) > 70 then sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     if AnsiPos('*', tempStr) <> 0 then charz :=
       '  0.00000E+00  0.00000E+00  0.00000E+00';

     DispxArray[nodecounter] := StrtoFloat(Copy(charz, 2, 12));
     DispyArray[nodecounter] := StrtoFloat(Copy(charz, 15, 12));
     DispzArray[nodecounter] := StrtoFloat(Copy(charz, 28, 12));

     if dispxconemin > DispxArray[nodecounter] then begin
     dispxconemin := DispxArray[nodecounter];
     dispxnodemin := InttoStr(StrtoInt(Nodenum[nodecounter]) + 1);
     end;
     if dispxconemax < DispxArray[nodecounter] then begin
     dispxconemax := DispxArray[nodecounter];
     dispxnodemax := InttoStr(StrtoInt(Nodenum[nodecounter]) + 1);
     end;
     if dispyconemin > DispyArray[nodecounter] then begin
     dispyconemin := DispyArray[nodecounter];
     dispynodemin := InttoStr(StrtoInt(Nodenum[nodecounter]) + 1);
     end;
     if dispyconemax < DispyArray[nodecounter] then begin
     dispyconemax := DispyArray[nodecounter];
     dispynodemax := InttoStr(StrtoInt(Nodenum[nodecounter]) + 1);
     end;
     if dispzconemin > DispzArray[nodecounter] then begin
     dispzconemin := DispzArray[nodecounter];
     dispznodemin := InttoStr(StrtoInt(Nodenum[nodecounter]) + 1);
     end;
     if dispzconemax < DispzArray[nodecounter] then begin
     dispzconemax := DispzArray[nodecounter];
     dispznodemax := InttoStr(StrtoInt(Nodenum[nodecounter]) + 1);
     end;

     Inc(nodecounter);
     Sharz := Sharz + Copy(charz, 1, Length(charz));
     if (Length(sharz) > 70) or (nodecounter = numberofnodes) then
     OutBuf.Add(sharz);
     Inc(F);
     Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
  if (nodecounter < numberofnodes) and (numberofnodes > 1) and
     (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in DISP_' + InttoStr(n) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
        end;
  if (nodecounter > numberofnodes)
     and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in DISP_' + InttoStr(n) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
        end;
  end;

  if (AnsiPos(' -4  PDISP', tempStr) <> 0) then begin
    Inc(u);
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    sharz := '';
    nodecounter := 0;

    OutBuf.Add(' ');
    {OutBuf.Add('VECTORS PDISP_' + InttoStr(u) + ' float');  }
    OutBuf.Add('SCALARS PDISP_' + InttoStr(u) + ' double 6' );
    OutBuf.Add('LOOKUP_TABLE default');

     repeat
     tempStr := ReadLine(F);
     charz := '';
     if Length(sharz) > 70 then sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     if AnsiPos('*', tempStr) <> 0 then charz :=
       '  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00';

     DispxArray[nodecounter] := StrtoFloat(Copy(charz, 2, 12));
     DispyArray[nodecounter] := StrtoFloat(Copy(charz, 15, 12));
     DispzArray[nodecounter] := StrtoFloat(Copy(charz, 28, 12));

     Inc(nodecounter);
     Sharz := Sharz + Copy(charz, 1, Length(charz));
     if (Length(sharz) > 70) or (nodecounter = numberofnodes) then
     OutBuf.Add(sharz);
     Inc(F);
     Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
  if (nodecounter < numberofnodes) and (numberofnodes > 1) and
     (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in PDISP_' + InttoStr(u) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
        end;
  if (nodecounter > numberofnodes)
     and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in PDISP_' + InttoStr(u) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
     end;
  end;

  if (AnsiPos(' -4  FORC', tempStr) <> 0) then begin
    Inc(h);
    Inc(F);
    repeat
    tempStr := ReadShortLine(F);
    if (AnsiPos(' -5  ', tempStr) <> 0) then
    Inc(F);
    until (AnsiPos(' -5  ', tempStr) = 0);
    sharz := '';
    nodecounter := 0;

    OutBuf.Add(' ');
    OutBuf.Add('VECTORS FORC_' + InttoStr(h) + ' float');

     repeat
     tempStr := ReadLine(F);
     charz := '';
     if Length(sharz) > 70 then sharz := '';
     repeat
     if AnsiPos('.', tempStr) <> 0 then begin
     position := AnsiPos('.', tempStr);
     Delete(tempStr, 1, position - 3);
     tempNode := Copy(tempStr, 1, 12);
     charz := (charz + ' ' + tempNode);
     Delete(tempStr, 1, 12);
     end;
     until AnsiPos('.', tempStr) = 0;
     Inc(nodecounter);
     if AnsiPos('*', tempStr) <> 0 then charz :=
       '  0.00000E+00  0.00000E+00  0.00000E+00';
     Sharz := Sharz + Copy(charz, 1, Length(charz));
     if (Length(sharz) > 70) or (nodecounter = numberofnodes) then
     OutBuf.Add(sharz);
     Inc(F);
     Gauge1.Position := trunc(105*(OutBuf.Count/InBuf.Count));
  until  CheckLine(F);
  if (nodecounter < numberofnodes) and (numberofnodes > 1) and
     (AlreadyFlaggedLow = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Insufficient data points ');
     ErrBuf.Add( 'in FORC_' + InttoStr(h) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedlow := true;
        end;
  if (nodecounter > numberofnodes)
     and (numberofnodes > 1) and (AlreadyFlaggedHigh = false) then begin
     ErrBuf.Clear;
     ErrBuf.Add( 'Excess data points ');
     ErrBuf.Add( 'in FORC_' + InttoStr(h) + ' section.');
     ErrBuf.Add( 'ParaView will probably ');
     ErrBuf.Add( 'read this section but');
     ErrBuf.Add( 'none further.');
     ShowError;
     Alreadyflaggedhigh := true;
        end;
  end;
  until  linesleft = 2;
end;

procedure TFrmVtkpost.Delay(ms : longint);
var
  TheTime : LongInt;
begin
  TheTime := GetTickCount + ms;
  while GetTickCount < TheTime do Application.ProcessMessages;
end;

procedure TFrmVtkpost.DisplayTableMsg(Sender: TObject);
begin
     ErrBuf.Clear;
     ErrBuf.Add( 'No STRESS sections ');
     ErrBuf.Add( 'in .frd file, so no ');
     ErrBuf.Add( 'table operations. ');
     ShowError;
end;

procedure TFrmVtkpost.ShowError;
begin
  Application.MessageBox(PChar(ErrBuf.Text),PChar(Caption),MB_ICONEXCLAMATION);
end;

function TFrmVtkpost.ShowOkCancel: Integer;
begin
  result:=Application.MessageBox(PChar(ErrBuf.Text),PChar(Caption),MB_OKCANCEL or MB_ICONQUESTION);
end;

function TFrmVtkpost.ReadLine(const Index: Integer): string;
begin
  result:='';
  if (Index<0) or (Index>=InBuf.Count) then exit;
  result:=InBuf[Index];
end;

function TFrmVtkpost.ReadShortLine(const Index: Integer): string;
begin
  result:=Copy(ReadLine(Index),1,5);
end;

function TFrmVtkpost.CheckLine(const Index: Integer): boolean;
begin
  result:=Length(ReadLine(Index))<5;
end;

procedure TFrmVtkpost.FormDestroy(Sender: TObject);
begin
    Finalize(sxxArray);
    Finalize(Snodearray);
    Finalize(SnodearrayN);
    Finalize(SnodearrayC);
    Finalize(Nodenum);
    Finalize(Nodetuple);
    Finalize(Splice);
    Finalize(PrincipalSone);
    Finalize(PrincipalStwo);
    Finalize(PrincipalSthree);
    Finalize(TrescaS);
    Finalize(sxxArray);
    Finalize(syyArray);
    Finalize(szzArray);
    Finalize(sxyArray);
    Finalize(syzArray);
    Finalize(szxArray);
    Finalize(vMArray);
    Finalize(DispxArray);
    Finalize(DispyArray);
    Finalize(DispzArray);
    FreeMem(sxxArray);
    FreeMem(Snodearray);
    FreeMem(SnodearrayN);
    FreeMem(SnodearrayC);
    FreeMem(Nodenum);
    FreeMem(Nodetuple);
    FreeMem(Splice);
    FreeMem(PrincipalSone);
    FreeMem(PrincipalStwo);
    FreeMem(PrincipalSthree);
    FreeMem(TrescaS);
    FreeMem(sxxArray);
    FreeMem(syyArray);
    FreeMem(szzArray);
    FreeMem(sxyArray);
    FreeMem(syzArray);
    FreeMem(szxArray);
    FreeMem(vMArray);
    FreeMem(DispxArray);
    FreeMem(DispyArray);
    FreeMem(DispzArray);
    FreeAndNil(ErrBuf);
    FreeAndNil(InBuf);
    FreeAndNil(OutBuf);
end;

end.
