unit uGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ExtCtrls, StdCtrls, Spin, ActnList, uMathParser, uRender, Grids, ExtDlgs,
  Menus,LCLType;

type

  { TFrmGraph }

  TFrmGraph = class(TForm)
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    cmdSaveGraph: TAction;
    Button2: TButton;
    Button3: TButton;
    cmdCheckList: TAction;
    cmdRenderGraph: TAction;
    Button1: TButton;
    cmdFileLoad: TAction;
    cmdFileSave: TAction;
    cmdCalc: TAction;
    AlMain: TActionList;
    BtnLoad: TButton;
    BtnCalc: TButton;
    BtnSave: TButton;
    EdExpression: TEdit;
    Edit1: TEdit;
    EdNumPts: TSpinEdit;
    EdMaxTimeValue: TEdit;
    gbParams: TGroupBox;
    Label1: TLabel;
    lbExpression: TLabel;
    LbNumTimePoints: TLabel;
    lbVariable: TLabel;
    DlgLoad: TOpenDialog;
    ListBox1: TListBox;
    MenuItem1: TMenuItem;
    pbGraph: TPaintBox;
    DlgSave: TSaveDialog;
    DlgSaveGraph: TSavePictureDialog;
    PopupMenu1: TPopupMenu;
    VLEGraph: TValueListEditor;


    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure cmdCalcExecute(Sender: TObject);
    procedure cmdCalcUpdate(Sender: TObject);
    procedure cmdCheckListExecute(Sender: TObject);
    procedure cmdFileSaveExecute(Sender: TObject);
    procedure cmdFileSaveUpdate(Sender: TObject);
    procedure cmdFileLoadExecute(Sender: TObject);
    procedure cmdRenderGraphExecute(Sender: TObject);
    procedure cmdSaveGraphExecute(Sender: TObject);
    procedure EdExpressionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EdMaxTimeValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EdNumPtsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure pbGraphPaint(Sender: TObject);
    procedure VLEGraphKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure VLEGraphKeyPress(Sender: TObject; var Key: char);
  private
    fOffScreen: TBitmap;
    function GraphNotEmpty: boolean;
    function CheckList: boolean;
    procedure SelectEditor(const index: integer; const col: integer=0);
  public
    { public declarations }
  end;

var
  FrmGraph: TFrmGraph;

implementation

{$R *.lfm}

uses
  DOM, XMLRead, XMLWrite;

const
  sVLEDuplicateKey = 'A value of time "%s" already exists!';
  sVLEIllegalValue = 'A value "%s" is not a valid floating point value!';
  sVLETimeNotAsc = 'The times must be arranged in ascending order!';
  sVLEEmptyField = 'The field is empty!';
  sCheckListSucc = 'Check the list is done. No errors were found.';
  sInvalidXML = 'Invalid format of XML file';
  sOwerwrire = 'File "%s" already exists.'#13#10'Do you want replace it?';
  //

type
  TGridAccess = class(TValueListEditor);

{ TFrmGraph }

procedure TFrmGraph.FormCreate(Sender: TObject);
begin
  fOffScreen:=TBitmap.Create;
  fOffScreen.PixelFormat:=pf24Bit;
end;

procedure TFrmGraph.FormDestroy(Sender: TObject);
begin
  fOffScreen.Free;
end;

procedure TFrmGraph.FormResize(Sender: TObject);
begin
  fOffScreen.Width:=pbGraph.Width;
  fOffScreen.Height:=pbGraph.Height;
  with fOffScreen.Canvas do
  begin
    Font.Size:=8;
    Brush.Color:=clWhite;
    Pen.Color:=clGray;
    Rectangle(0,0,fOffScreen.Width,fOffScreen.Height);
  end;
  cmdRenderGraph.Execute;
end;

procedure TFrmGraph.FormShow(Sender: TObject);
begin
  SelectEditor(0);
end;



procedure TFrmGraph.pbGraphPaint(Sender: TObject);
begin
  pbGraph.Canvas.Draw(0,0,fOffScreen);
end;

procedure TFrmGraph.SelectEditor(const index, col: integer);
begin
  TGridAccess(VLEGraph).MoveNextSelectable(false,col,index+1);
  if VLEGraph.Editor is TStringCellEditor then begin
    TStringCelleditor(VLEGraph.Editor).SelectAll;
    VLEGraph.Editor.SetFocus;
  end;
end;

function TFrmGraph.CheckList: boolean;
var
  i,j,e: integer;
  d1,d2: TDouble;
begin
  result:=false;
  with VLEGraph.Strings do begin
    // pass1 check valid number & empty field
    for i:=0 to Count-1 do begin
      if Length(Names[i])=0 then begin
         MessageDlg(sVLEEmptyField,mtWarning,[mbOK],0);
         SelectEditor(i);
         exit;
      end;
      if Length(ValueFromIndex[i])=0 then begin
         MessageDlg(sVLEEmptyField,mtWarning,[mbOK],0);
         SelectEditor(i,1);
         exit;
      end;

      Val(Names[i],d1,e);
      if e<>0 then begin
         MessageDlg(Format(sVLEIllegalValue,[Names[i]]),mtWarning,[mbOK],0);
         SelectEditor(i);
         exit;
      end;
      Val(ValueFromIndex[i],d1,e);
      if e<>0 then begin
         MessageDlg(Format(sVLEIllegalValue,[ValueFromIndex[i]]),mtWarning,[mbOK],0);
         SelectEditor(i,1);
         exit;
      end;
    end;
    // pass2 find duplicates
    for i:=0 to Count-1 do begin
      for j:=Count-1 downto i+1 do begin
        if CompareText(Names[j],Names[i])=0 then begin
          MessageDlg(Format(sVLEDuplicateKey,[Names[j]]),mtWarning,[mbOK],0);
          SelectEditor(j);
          exit;
        end;
      end;
    end;
    // pass3 find not ascending order
    Val(Names[0],d1,e);
    for i:=1 to Count-1 do begin
      Val(Names[i],d2,e);
      if d2<=d1 then begin
         MessageDlg(sVLETimeNotAsc,mtWarning,[mbOK],0);
         SelectEditor(i);
         exit;
      end;
      d1:=d2;
    end;
  end;
  result:=true;
end;






procedure TFrmGraph.cmdCheckListExecute(Sender: TObject);
begin
  if CheckList then
  MessageDlg(sCheckListSucc,mtInformation,[mbOK],0);
end;

procedure TFrmGraph.VLEGraphKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TFrmGraph.VLEGraphKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9','.','+','-','E','e',#8]) then Key:=#0;
end;

procedure TFrmGraph.cmdCalcUpdate(Sender: TObject);
begin
  cmdCalc.Enabled:=(Trim(EdExpression.Text)<>'')
  and (Trim(EdMaxTimeValue.Text)<>'')
  and (EdNumPts.Value>1);
end;

procedure TFrmGraph.cmdCalcExecute(Sender: TObject);
var
  p: TMathParser;
  i,e: integer;
  t,d1,d2: TDouble;
  fs: TFormatSettings;
  v: TVector;
begin
  // calc times range
  fs.DecimalSeparator:='.';
  Val(EdMaxTimeValue.Text,d1,e);
  if e<>0 then begin
  MessageDlg(Format(sVLEIllegalValue,[EdMaxTimeValue.Text]),mtWarning,[mbOK],0);
  EdMaxTimeValue.SelectAll;
  EdMaxTimeValue.SetFocus;
  exit;
  end;

  Val(VLEGraph.Strings.Names[0],d2,e);
  if e<>0 then d2:=0;
  t:=(d1-d2)/(VLEGraph.Strings.Count-1);
  p:=TMathParser.Create;
  try
    p.Translate(EdExpression.Text,'t');
    SetLength(v,2);
    for i:=0 to VLEGraph.Strings.Count-1 do begin
    v[1]:=d2;
    d1:=p.Get(v);
    VLEGraph.Strings[i]:=Format('%s=%s',[FormatFloat('0.#######',d2,fs),ExtendedToStr(d1)]);
    d2:=d2+t;
    end;
    SetLength(v,0);
  finally
    p.Free;
  end;
end;

procedure TFrmGraph.Button6Click(Sender: TObject);
begin
  ShowMessage('t is variable; syntax of basic; elementary functions: arccos, arcctan, arcsin, arctan, cos, cosh, ctan, sum, exp, ln, log10, log2, max, min, power, sin, sinh, sqr, sqrt, tan, tanh, round, abs, pi');
end;

procedure TFrmGraph.Button7Click(Sender: TObject);
begin
  VLEGraph.Strings.Clear;
end;



function TFrmGraph.GraphNotEmpty: boolean;
begin
  with VLEGraph.Strings do result:=(Count>0) and (Names[0]<>'');
end;

procedure TFrmGraph.cmdFileSaveUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=GraphNotEmpty;
end;

procedure TFrmGraph.cmdFileSaveExecute(Sender: TObject);

  procedure WriteXml(const FileName: string);
  var
    xml: TXMLDocument;
    root,item: TDOMNode;
    i: integer;
  begin
    xml:=TXMLDocument.Create;
    try
      root:=xml.CreateElement('graph');
      xml.AppendChild(root);
      (root as TDOMElement)['count']:=IntToStr(VLEGraph.Strings.Count);
      for i:=0 to VLEGraph.Strings.Count-1 do begin
        item:=xml.CreateElement('item');
        (item as TDOMElement)['time']:=VLEGraph.Strings.Names[i];
        (item as TDOMElement)['value']:=VLEGraph.Strings.ValueFromIndex[i];
        root.AppendChild(item);
      end;
      WriteXMLFile(xml,FileName);
    finally
      xml.Free;
    end;
  end;

var
  FileName: string;
  List: TStringList;
  i: integer;
begin
  if not DlgSave.Execute then exit;
  case DlgSave.FilterIndex of
    1: FileName:=ChangeFileExt(DlgSave.FileName,'.txt');
    2: FileName:=ChangeFileExt(DlgSave.FileName,'.csv');
    3: FileName:=ChangeFileExt(DlgSave.FileName,'.xml');
    else FileName:=DlgSave.FileName;
  end;
  if FileExists(FileName) then
  case MessageDlg(Format(sOwerwrire,[FileName]),mtWarning,mbYesNoCancel,0) of
    mrCancel: exit;
    mrNo: cmdFileSaveExecute(nil);
  end;
  if SameText(ExtractFileExt(FileName),'.csv') then
  begin
    List:=TStringList.Create;
    try
      for i:=0 to VLEGraph.Strings.Count-1 do
        List.Add(StringReplace(VLEGraph.Strings[i],'=',',',[]));
      List.SaveToFile(FileName);
    finally
      List.Free;
    end;
  end
  else if SameText(ExtractFileExt(FileName),'.xml') then
  begin
    WriteXml(FileName);
  end
  else begin
    VLEGraph.Strings.SaveToFile(FileName);
  end;
end;

procedure TFrmGraph.cmdFileLoadExecute(Sender: TObject);

  procedure ReadXml(const FileName: string);
  var
    xml: TXMLDocument;
    root,item: TDOMNode;
    e: TDOMElement;
    i: integer;
  begin
    xml:=TXMLDocument.Create;
    try
      ReadXMLFile(xml,FileName);
      root:=xml.ChildNodes.Item[0];
      if (root=nil) or (root.ChildNodes.Count=0) then
         raise Exception.Create(sInvalidXML);
      for i:=0 to root.ChildNodes.Count-1 do begin
        item:=root.ChildNodes.Item[i];
        if item=nil then
           raise Exception.Create(sInvalidXML);
        e:=item as TDOMElement;
        if (Length(e['time'])=0) or (Length(e['value'])=0) then
           raise Exception.Create(sInvalidXML);
        VLEGraph.Strings.Add(Format('%s=%s',[e['time'],e['value']]));
      end;
    finally
      xml.Free;
    end;
  end;

var
  List: TStringList;
  i: integer;
begin
  if not DlgLoad.Execute then exit;
  VLEGraph.Strings.Clear;
  if SameText(ExtractFileExt(DlgLoad.FileName),'.csv') then
  begin
    List:=TStringList.Create;
    try
      List.LoadFromFile(DlgLoad.FileName);
      for i:=0 to List.Count-1 do
        VLEGraph.Strings.Add(StringReplace(List[i],',','=',[]));
    finally
      List.Free;
    end;
  end
  else if SameText(ExtractFileExt(DlgLoad.FileName),'.xml') then
  begin
    VLEGraph.Strings.BeginUpdate;
    try
      VLEGraph.Strings.Clear;
      ReadXml(DlgLoad.FileName);
    finally
      VLEGraph.Strings.EndUpdate;
    end;
  end
  else begin
    VLEGraph.Strings.LoadFromFile(DlgLoad.FileName);
  end;
  EdNumPts.Value:=VLEGraph.Strings.Count;
  EdMaxTimeValue.Text:=VLEGraph.Strings.Names[VLEGraph.Strings.Count-1];
  CheckList;
end;

procedure TFrmGraph.cmdRenderGraphExecute(Sender: TObject);
begin
  CheckList;
  RenderGraph(fOffScreen,VLEGraph.Strings);
  pbGraph.Invalidate;
  cmdSaveGraph.Enabled:=true;
end;

procedure TFrmGraph.cmdSaveGraphExecute(Sender: TObject);
begin
  if not DlgSaveGraph.Execute then exit;
  fOffScreen.SaveToFile(DlgSaveGraph.FileName);
end;

procedure TFrmGraph.EdExpressionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if key=VK_Return then begin
     cmdCalc.Execute;
  cmdRenderGraph.Execute ;
   end;
end;

procedure TFrmGraph.EdMaxTimeValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     if key=VK_Return then begin
     cmdCalc.Execute;
  cmdRenderGraph.Execute ;
   end;
end;

procedure TFrmGraph.EdNumPtsChange(Sender: TObject);
begin
  VLEGraph.RowCount:=EdNumPts.Value+1;
end;



end.

