unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, LCLType, Controls, Graphics,
  Dialogs, Menus, ActnList, ComCtrls, StdCtrls, ExtCtrls, uHighliter;

const

  AppTitle = 'Editor';
  Noname = 'Noname';
  Warning = 'Warning';

type

  { TFrMain }

  TFrMain = class(TForm)
    cmdOptionsShowHints: TAction;
    cmdOptionsTrimTrailingSpaces: TAction;
    cmdOptionsTabsToSpaces: TAction;
    cmdOptionsShowSpecialChars: TAction;
    cmdOptionsShowScrollHint: TAction;
    cmdOptionsScrollPastEOL: TAction;
    cmdOptionsScrollPastEOF: TAction;
    cmdOptionsScrollByOneLess: TAction;
    cmdOptionsHalfPageScroll: TAction;
    cmdOptionsHideShowScrollbars: TAction;
    cmdOptionsEnhanceHomeKey: TAction;
    cmdOptionsRightMouseMovesCursor: TAction;
    cmdOptionsSmartTabDelete: TAction;
    cmdOptionsSmartTabs: TAction;
    cmdOptionsKeepCaretX: TAction;
    cmdOptionsAltSetsColumnMode: TAction;
    cmdOptionsDragDropEditing: TAction;
    cmdOptionsAutoIndent: TAction;
    cmdOptionsGroupUndo: TAction;
    cmdOptionsFont: TAction;
    cmdOptionsRemoveComents: TAction;
    cmdEditPasteFromFile: TAction;
    cmdFindGotoLine: TAction;
    cmdFindReplace: TAction;
    cmdFindSearch: TAction;
    cmdEditSelectAll: TAction;
    cmdEditDelete: TAction;
    cmdEditPaste: TAction;
    cmdEditCopy: TAction;
    cmdEditCut: TAction;
    cmdEditRedo: TAction;
    cmdEditUndo: TAction;
    cmdFileNew: TAction;
    cmdFileSaveAs: TAction;
    cmdFileSave: TAction;
    AlMain: TActionList;
    cmdFileExit: TAction;
    cmdFileOpen: TAction;
    Editor: TSynEdit;
    Find: TMenuItem;
    DlgFind: TFindDialog;
    DlgFont: TFontDialog;
    IlMain: TImageList;
    IlStatus: TImageList;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
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
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MiTemplates: TMenuItem;
    MiDelete: TMenuItem;
    MenuItem19: TMenuItem;
    MnMain: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    DlgOpen: TOpenDialog;
    DlgSave: TSaveDialog;
    DlgReplace: TReplaceDialog;
    PmEditor: TPopupMenu;
    sbEditor: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure cmdEditCopyExecute(Sender: TObject);
    procedure cmdEditCutExecute(Sender: TObject);
    procedure cmdEditCutUpdate(Sender: TObject);
    procedure cmdEditDeleteExecute(Sender: TObject);
    procedure cmdEditPasteExecute(Sender: TObject);
    procedure cmdEditPasteUpdate(Sender: TObject);
    procedure cmdEditRedoExecute(Sender: TObject);
    procedure cmdEditRedoUpdate(Sender: TObject);
    procedure cmdEditSelectAllExecute(Sender: TObject);
    procedure cmdEditSelectAllUpdate(Sender: TObject);
    procedure cmdEditUndoExecute(Sender: TObject);
    procedure cmdEditUndoUpdate(Sender: TObject);
    procedure cmdFileExitExecute(Sender: TObject);
    procedure cmdFileNewExecute(Sender: TObject);
    procedure cmdFileOpenExecute(Sender: TObject);
    procedure cmdFileSaveAsExecute(Sender: TObject);
    procedure cmdFileSaveExecute(Sender: TObject);
    procedure cmdFileSaveUpdate(Sender: TObject);
    procedure cmdFindGotoLineExecute(Sender: TObject);
    procedure cmdFindReplaceExecute(Sender: TObject);
    procedure cmdFindSearchExecute(Sender: TObject);
    procedure cmdEditPasteFromFileExecute(Sender: TObject);
    procedure cmdOptionsFontExecute(Sender: TObject);
    procedure cmdOptionsRemoveComentsExecute(Sender: TObject);
    procedure cmdOptionsShowHintsExecute(Sender: TObject);
    procedure DlgFindFind(Sender: TObject);
    procedure DlgReplaceReplace(Sender: TObject);
    procedure EditorMouseEnter(Sender: TObject);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbEditorDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure UpdateEditorOptions(Sender: TObject);
    procedure HintHandler(Sender: TObject);
  private
    fFileName: TFileName;
    fDlgDir: string;
    fIniPath: string;
    function CheckFileSave: boolean;
    procedure SetDlgDir(AValue: string);
    procedure SetFileName(const FileName: TFileName);
    procedure MakeTemplatesMenu;
    procedure LoadIni;
    procedure SaveIni;
    procedure SortMenu(MenuItem: TMenuItem);
    procedure TemplatesSearch(const Path: TFileName; MenuItem: TMenuItem);
    procedure TemplateMenuClick(Sender: TObject);
    procedure PasteTextToNewLine(const PasteText: string);
    procedure PasteFromFile(const FileName: TFileName);
  public
    Highliter: TInpHighlighter;
    property DlgDir: string read fDlgDir write SetDlgDir;
    procedure LoadFile(const FileName: TFileName; const ReadOnly: boolean = false);
    procedure OpenFile(const FileName: TFileName);
    procedure SaveFile(const FileName: TFileName);
  end;

var
  FrMain: TFrMain;

implementation

{$R *.lfm}

uses SynEditTypes, IniFiles, unit1;


{ TFrMain }

procedure TFrMain.FormCreate(Sender: TObject);
begin
  fIniPath:=ExtractFilePath(Application.ExeName)+'bin'+DirectorySeparator+'Editor.ini';
  Highliter:=TInpHighlighter.Create(self);
  Editor.Highlighter:=Highliter;
  LoadIni;
  MakeTemplatesMenu;
  SetFileName(Noname);
end;

procedure TFrMain.FormDestroy(Sender: TObject);
begin
  SaveIni;
end;

procedure TFrMain.LoadIni;
var
  ini: TIniFile;
begin
  ini:=TIniFile.Create(fIniPath);
  try
    Editor.Font.Name:=ini.ReadString('Editor','Font.Name',SynDefaultFontName);
    Editor.Font.Size:=ini.ReadInteger('Editor','Font.Size',SynDefaultFontSize);
    cmdOptionsShowHints.Checked:=ini.ReadBool('Editor','ShowHints',ShowHint);
    cmdOptionsGroupUndo.Checked:=ini.ReadBool('Editor','GroupUndo',eoGroupUndo in Editor.Options);
    cmdOptionsAutoIndent.Checked:=ini.ReadBool('Editor','AutoIndent',eoAutoIndent in Editor.Options);
    cmdOptionsDragDropEditing.Checked:=ini.ReadBool('Editor','DragDropEditing',eoDragDropEditing in Editor.Options);
    cmdOptionsAltSetsColumnMode.Checked:=ini.ReadBool('Editor','AltSetsColumnMode',eoAltSetsColumnMode in Editor.Options);
    cmdOptionsKeepCaretX.Checked:=ini.ReadBool('Editor','KeepCaretX',eoKeepCaretX in Editor.Options);
    cmdOptionsSmartTabs.Checked:=ini.ReadBool('Editor','SmartTabs',eoSmartTabs in Editor.Options);
    cmdOptionsSmartTabDelete.Checked:=ini.ReadBool('Editor','SmartTabDelete',eoSmartTabDelete in Editor.Options);
    cmdOptionsRightMouseMovesCursor.Checked:=ini.ReadBool('Editor','RightMouseMovesCursor',eoRightMouseMovesCursor in Editor.Options);
    cmdOptionsEnhanceHomeKey.Checked:=ini.ReadBool('Editor','EnhanceHomeKey',eoEnhanceHomeKey in Editor.Options);
    cmdOptionsHideShowScrollbars.Checked:=ini.ReadBool('Editor','HideShowScrollbars',eoHideShowScrollbars in Editor.Options);
    cmdOptionsHalfPageScroll.Checked:=ini.ReadBool('Editor','HalfPageScroll',eoHalfPageScroll in Editor.Options);
    cmdOptionsScrollByOneLess.Checked:=ini.ReadBool('Editor','ScrollByOneLess',eoScrollByOneLess in Editor.Options);
    cmdOptionsScrollPastEOF.Checked:=ini.ReadBool('Editor','ScrollPastEof',eoScrollPastEof in Editor.Options);
    cmdOptionsScrollPastEOL.Checked:=ini.ReadBool('Editor','ScrollPastEol',eoScrollPastEol in Editor.Options);
    cmdOptionsShowScrollHint.Checked:=ini.ReadBool('Editor','ShowScrollHint',eoShowScrollHint in Editor.Options);
    cmdOptionsShowSpecialChars.Checked:=ini.ReadBool('Editor','ShowSpecialChars',eoShowSpecialChars in Editor.Options);
    cmdOptionsTabsToSpaces.Checked:=ini.ReadBool('Editor','TabsToSpaces',eoTabsToSpaces in Editor.Options);
    cmdOptionsTrimTrailingSpaces.Checked:=ini.ReadBool('Editor','TrimTrailingSpaces',eoTrimTrailingSpaces in Editor.Options);
    cmdOptionsRemoveComents.Checked:=ini.ReadBool('Editor','RemoveComentsOnLoad',false);
  finally
    ini.Free;
  end;
  UpdateEditorOptions(nil);
  ShowHint:=cmdOptionsShowHints.Checked;
end;

procedure TFrMain.SaveIni;
var
  ini: TIniFile;
begin
  ini:=TIniFile.Create(fIniPath);
  try
    ini.WriteString('Editor','Font.Name',Editor.Font.Name);
    ini.WriteInteger('Editor','Font.Size',Editor.Font.Size);
    ini.WriteBool('Editor','ShowHints',ShowHint);
    ini.WriteBool('Editor','GroupUndo',eoGroupUndo in Editor.Options);
    ini.WriteBool('Editor','AutoIndent',eoAutoIndent in Editor.Options);
    ini.WriteBool('Editor','DragDropEditing',eoDragDropEditing in Editor.Options);
    ini.WriteBool('Editor','AltSetsColumnMode',eoAltSetsColumnMode in Editor.Options);
    ini.WriteBool('Editor','KeepCaretX',eoKeepCaretX in Editor.Options);
    ini.WriteBool('Editor','SmartTabs',eoSmartTabs in Editor.Options);
    ini.WriteBool('Editor','SmartTabDelete',eoSmartTabDelete in Editor.Options);
    ini.WriteBool('Editor','RightMouseMovesCursor',eoRightMouseMovesCursor in Editor.Options);
    ini.WriteBool('Editor','EnhanceHomeKey',eoEnhanceHomeKey in Editor.Options);
    ini.WriteBool('Editor','HideShowScrollbars',eoHideShowScrollbars in Editor.Options);
    ini.WriteBool('Editor','HalfPageScroll',eoHalfPageScroll in Editor.Options);
    ini.WriteBool('Editor','ScrollByOneLess',eoScrollByOneLess in Editor.Options);
    ini.WriteBool('Editor','ScrollPastEof',eoScrollPastEof in Editor.Options);
    ini.WriteBool('Editor','ScrollPastEol',eoScrollPastEol in Editor.Options);
    ini.WriteBool('Editor','ShowScrollHint',eoShowScrollHint in Editor.Options);
    ini.WriteBool('Editor','ShowSpecialChars',eoShowSpecialChars in Editor.Options);
    ini.WriteBool('Editor','TabsToSpaces',eoTabsToSpaces in Editor.Options);
    ini.WriteBool('Editor','TrimTrailingSpaces',eoTrimTrailingSpaces in Editor.Options);
    ini.WriteBool('Editor','RemoveComentsOnLoad',cmdOptionsRemoveComents.Checked);
  finally
    ini.Free;
  end;
end;

procedure TFrMain.MakeTemplatesMenu;
var
  path: string;
  mi: TMenuItem;
begin
  path:=Format('%shlp%sTemplates',[ProgramDirectory,PathDelim]);
  if not DirectoryExists(path) then exit;
  TemplatesSearch(path,MiTemplates);
  SortMenu(MiTemplates);
  if MiTemplates.Count>0 then begin
    MiTemplates.Enabled:=true;
    mi:=TMenuItem.Create(self);
    mi.Caption:='-';
    MiTemplates.Add(mi);
    mi:=TMenuItem.Create(self);
    mi.Action:=cmdOptionsRemoveComents;
    MiTemplates.Add(mi);
  end;
end;

procedure TFrMain.SortMenu(MenuItem: TMenuItem);

procedure SotrMenuItem(mi: TMenuItem);
var
  i: integer;
  sl: TStringList;
begin
  if mi.Count=0 then exit;
  sl:=TStringList.Create;
  try
    sl.Sorted:=true;
    for i:=0 to mi.Count-1 do
      sl.AddObject(mi[i].Caption,mi[i]);
  sl.Sort;
  for i:=0 to sl.Count-1 do
    TMenuItem(sl.Objects[i]).MenuIndex:=i;
  finally
    sl.Free;
  end;
  for i:=0 to mi.Count-1 do begin
    if mi.Items[i].Count=0 then continue;
    SotrMenuItem(mi.Items[i]);
  end;
end;

begin
  SotrMenuItem(MenuItem);
end;

procedure TFrMain.TemplatesSearch(const Path: TFileName; MenuItem: TMenuItem);
var
  si: TSearchRec;
  mi: TMenuItem;
begin
  if FindFirst(AppendPathDelim(Path)+'*',faAnyFile,si)=0 then begin
    repeat
      if (si.Name='.') or (si.Name='..') or (si.Name='') then continue;
      if (si.Attr and faDirectory)=faDirectory then begin
        mi:=TMenuItem.Create(self);
        mi.Caption:=si.Name;
        MenuItem.Add(mi);
        TemplatesSearch(AppendPathDelim(Path)+si.Name,mi);
      end else begin
        mi:=TMenuItem.Create(self);
        mi.Caption:=si.Name;
        mi.Hint:=AppendPathDelim(Path)+si.Name;
        mi.OnClick:=@TemplateMenuClick;
        MenuItem.Add(mi);
      end;
    until FindNext(si)<>0;
    end;
  FindClose(si);
end;

procedure TFrMain.TemplateMenuClick(Sender: TObject);
begin
  PasteFromFile(TMenuItem(Sender).Hint);
end;

procedure TFrMain.FormShow(Sender: TObject);
begin
  EditorStatusChange(nil,[scCaretX,scCaretY,scInsertMode,scReadOnly]);
end;

procedure TFrMain.sbEditorDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  t: integer;
  ts: TTextStyle;
  r: TRect;
begin
  r:=Rect;
  if Panel.Index=3 then begin
    t:=Rect.Top+(Rect.Bottom-Rect.Top-IlStatus.Height) div 2;
    IlStatus.Draw(StatusBar.Canvas,Rect.Left,t,StatusBar.Tag);
    r.Left:=Rect.Left+IlStatus.Width+4;
    Dec(r.Right,StatusBar.Height);
  end;
  FillChar(ts,SizeOf(ts),0);
  ts.Layout:=tlCenter;
  ts.SingleLine:=true;
  ts.Clipping:=true;
  ts.EndEllipsis:=true;
  StatusBar.Canvas.TextRect(r,r.Left,0,Panel.Text,ts);
end;


function TFrMain.CheckFileSave: boolean;
var
  s: string;
begin
  result:=true;
  if not Editor.Modified then exit;
  s:=Format('Current file "%s" is not saved. Save this file?',[fFileName]);
  case Application.MessageBox(PChar(s),Warning,
       MB_YESNOCANCEL or MB_ICONQUESTION) of
    IDYES: cmdFileSaveExecute(nil);
    IDCANCEL: result:=false;
  end;
end;

procedure TFrMain.LoadFile(const FileName: TFileName; const ReadOnly: boolean);
begin
  Editor.Lines.LoadFromFile(FileName);
  Editor.ReadOnly:=ReadOnly;
  SetFileName(FileName);
  SetDlgDir(ExtractFilePath(FileName));
end;

procedure TFrMain.OpenFile(const FileName: TFileName);
begin
  LoadFile(FileName);
  Show;
end;

procedure TFrMain.SaveFile(const FileName: TFileName);
begin
  Editor.Lines.SaveToFile(FileName);
  SetFileName(FileName);
end;

procedure TFrMain.SetDlgDir(AValue: string);
begin
  fDlgDir:=AValue;
  DlgOpen.InitialDir:=fDlgDir;
  DlgSave.InitialDir:=fDlgDir;
end;

procedure TFrMain.SetFileName(const FileName: TFileName);
begin
  fFileName:=FileName;
  Editor.Modified:=false;
  Caption:=Format('%s - [%s]',[AppTitle,ExtractFileName(fFileName)]);
  Application.Title:=Caption;
  sbEditor.Panels[3].Text:=fFileName;
end;

procedure TFrMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=CheckFileSave;
end;

procedure TFrMain.cmdFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFrMain.cmdFileNewExecute(Sender: TObject);
begin
  if not CheckFileSave then exit;
  Editor.ClearAll;
  SetFileName(Noname);
end;

procedure TFrMain.cmdFileOpenExecute(Sender: TObject);
begin
  if not CheckFileSave then exit;
  if not DlgOpen.Execute then exit;
  LoadFile(DlgOpen.FileName,ofReadOnly in DlgOpen.Options);
end;

procedure TFrMain.cmdFileSaveAsExecute(Sender: TObject);
begin
  if not DlgSave.Execute then exit;
  SaveFile(DlgSave.FileName);
end;

procedure TFrMain.cmdFileSaveExecute(Sender: TObject);
begin
  if not FileExists(fFileName) then begin
     cmdFileSaveAsExecute(nil);
     exit;
  end;
  SaveFile(fFileName);
end;

procedure TFrMain.cmdFileSaveUpdate(Sender: TObject);
begin
  cmdFileSave.Enabled:=Editor.Modified;
end;

procedure TFrMain.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges
  );
const
  ModifiedStrs: array[boolean] of string = ('', 'Modified');
  InsertModeStrs: array[boolean] of string = ('Overwrite', 'Insert');
begin
  if Changes*[scCaretX,scCaretY]<>[] then
     sbEditor.Panels[0].Text:=Format('%6d:%3d',[Editor.CaretY,Editor.CaretX]);
  if Changes*[scModified]<>[] then
    sbEditor.Panels[1].Text:=ModifiedStrs[Editor.Modified];
  if Changes*[scInsertMode,scReadOnly]<>[] then begin
    if Editor.ReadOnly then
      sbEditor.Panels[2].Text:='ReadOnly'
    else
      sbEditor.Panels[2].Text:=InsertModeStrs[Editor.InsertMode];
  end;
end;

procedure TFrMain.FormActivate(Sender: TObject);
begin
  Application.OnHint:=@HintHandler;
end;

procedure TFrMain.FormDeactivate(Sender: TObject);
begin
  Application.OnHint:=nil;
end;

procedure TFrMain.cmdEditUndoUpdate(Sender: TObject);
begin
  cmdEditUndo.Enabled:=Editor.CanUndo;
end;

procedure TFrMain.cmdEditUndoExecute(Sender: TObject);
begin
  Editor.Undo;
end;

procedure TFrMain.cmdEditRedoUpdate(Sender: TObject);
begin
  cmdEditRedo.Enabled:=Editor.CanRedo;
end;

procedure TFrMain.cmdEditRedoExecute(Sender: TObject);
begin
  Editor.Redo;
end;

procedure TFrMain.cmdEditCutUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=Editor.SelAvail;
end;

procedure TFrMain.cmdEditPasteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=Editor.CanPaste;
end;

procedure TFrMain.cmdEditCutExecute(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TFrMain.cmdEditCopyExecute(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TFrMain.cmdEditPasteExecute(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;


procedure TFrMain.PasteTextToNewLine(const PasteText: string);
begin
  Editor.CaretX:=0;
  Editor.SelText:=PasteText;
end;

procedure TFrMain.PasteFromFile(const FileName: TFileName);
var
  List: TStringList;
  i: integer;
  s: string;
begin
  List:=TStringList.Create;
  try
     List.LoadFromFile(FileName);
     if cmdOptionsRemoveComents.Checked then // remove comments
       for i:=List.Count-1 downto 0 do begin
         s:=Trim(List[i]);
         if (Length(s)>1) and (s[1]='*') and (s[2]='*') then // detect comment
           List.Delete(i); // remove comment string
       end;

     PasteTextToNewLine(List.Text);
  finally
  List.Free;
  end;
end;

procedure TFrMain.cmdEditPasteFromFileExecute(Sender: TObject);
begin
  if not DlgOpen.Execute then exit;
  PasteFromFile(DlgOpen.FileName);
end;

procedure TFrMain.cmdOptionsRemoveComentsExecute(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TFrMain.cmdOptionsShowHintsExecute(Sender: TObject);
begin
  ShowHint:=cmdOptionsShowHints.Checked;
end;

procedure TFrMain.cmdEditDeleteExecute(Sender: TObject);
begin
  Editor.ClearSelection;
end;

procedure TFrMain.cmdEditSelectAllUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled:=Editor.Lines.Count>0;
end;

procedure TFrMain.cmdEditSelectAllExecute(Sender: TObject);
begin
  Editor.SelectAll;
end;

procedure TFrMain.cmdFindSearchExecute(Sender: TObject);
begin
  DlgFind.FindText:=Editor.SelText;
  DlgFind.Execute;
end;

procedure TFrMain.cmdFindReplaceExecute(Sender: TObject);
begin
  DlgReplace.FindText:=Editor.SelText;
  DlgReplace.Execute;
end;

procedure TFrMain.DlgFindFind(Sender: TObject);
var
  rOptions: TSynSearchOptions;
  dlg: TFindDialog;
  sSearch: string;
begin
  dlg:=TFindDialog(Sender);
  sSearch:=dlg.FindText;
  if Length(sSearch)=0 then begin
    Application.MessageBox('Can''t search for empty text!',Warning,MB_ICONWARNING);
    exit;
  end;
  rOptions:=[];
  if not (frDown in dlg.Options) then
    Include(rOptions,ssoBackwards);
  if frMatchCase in dlg.Options then
    Include(rOptions,ssoMatchCase);
  if frWholeWord in dlg.Options then
    Include(rOptions,ssoWholeWord);
  if Editor.SearchReplace(sSearch,'',rOptions)=0 then
     Application.MessageBox(PChar('Search Text '''+sSearch+''' not found!'),Warning,MB_ICONWARNING);
end;

procedure TFrMain.DlgReplaceReplace(Sender: TObject);
var
  rOptions: TSynSearchOptions;
  sSearch: string;
begin
  sSearch:=DlgReplace.FindText;
  if Length(sSearch)=0 then begin
    Application.MessageBox('Can''t replace an empty text!',Warning,MB_ICONWARNING);
    exit;
  end;
  rOptions:=[ssoReplace];
  if frMatchCase in DlgReplace.Options then
    Include(rOptions,ssoMatchCase);
  if frWholeWord in DlgReplace.Options then
    Include(rOptions,ssoWholeWord);
  if frReplaceAll in DlgReplace.Options then
    Include(rOptions,ssoReplaceAll);
  if Editor.SearchReplace(sSearch,DlgReplace.ReplaceText,rOptions)=0 then
    Application.MessageBox(PChar('Search Text '''+sSearch+''' could not be replaced!'),Warning,MB_ICONWARNING);
end;

procedure TFrMain.cmdFindGotoLineExecute(Sender: TObject);
var
  Line: string;
begin
  with Editor do begin
    Line:=IntToStr(CaretY);
    if not InputQuery('Go to Line Number','Enter new line number:',Line) then exit;
       CaretXY:=Point(1,StrToIntDef(Line,CaretY));
       EnsureCursorPosVisible;
  end;
end;

procedure TFrMain.cmdOptionsFontExecute(Sender: TObject);
begin
  DlgFont.Font:=Editor.Font;
  if not DlgFont.Execute then exit;
  Editor.Font.Name:=DlgFont.Font.Name;
  Editor.Font.Size:=DlgFont.Font.Size;
end;

procedure TFrMain.UpdateEditorOptions(Sender: TObject);
var
  n: TSynEditorOptions;

  procedure SetOption(cmd: TAction; const p: TSynEditorOption);
  begin
    if cmd.Checked then
      Include(n,p)
    else
      Exclude(n,p);
  end;

begin
  n:=Editor.Options;
  SetOption(cmdOptionsGroupUndo,eoGroupUndo);
  SetOption(cmdOptionsAutoIndent,eoAutoIndent);
  SetOption(cmdOptionsDragDropEditing,eoDragDropEditing);
  SetOption(cmdOptionsAltSetsColumnMode,eoAltSetsColumnMode);
  SetOption(cmdOptionsKeepCaretX,eoKeepCaretX);
  SetOption(cmdOptionsSmartTabs,eoSmartTabs);
  SetOption(cmdOptionsSmartTabDelete,eoSmartTabDelete);
  SetOption(cmdOptionsRightMouseMovesCursor,eoRightMouseMovesCursor);
  SetOption(cmdOptionsEnhanceHomeKey,eoEnhanceHomeKey);
  SetOption(cmdOptionsHideShowScrollbars,eoHideShowScrollbars);
  SetOption(cmdOptionsHalfPageScroll,eoHalfPageScroll);
  SetOption(cmdOptionsScrollByOneLess,eoScrollByOneLess);
  SetOption(cmdOptionsScrollPastEOF,eoScrollPastEof);
  SetOption(cmdOptionsScrollPastEOL,eoScrollPastEol);
  SetOption(cmdOptionsShowScrollHint,eoShowScrollHint);
  SetOption(cmdOptionsShowSpecialChars,eoShowSpecialChars);
  SetOption(cmdOptionsTabsToSpaces,eoTabsToSpaces);
  SetOption(cmdOptionsTrimTrailingSpaces,eoTrimTrailingSpaces);
  Editor.Options:=n;
end;

procedure TFrMain.EditorMouseEnter(Sender: TObject);
begin
  sbEditor.Panels[3].Text:=fFileName;
  sbEditor.Tag:=0;
end;

procedure TFrMain.HintHandler(Sender: TObject);
begin
  sbEditor.Panels[3].Text:=Application.Hint;
  sbEditor.Tag:=1;
end;

end.

