unit uHelpTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, ComCtrls, Graphics, IpHtml;

type

  THlpNodeType = (ntFolder, ntFile);
  PHlpNodeData = ^THlpNodeData;
  THlpNodeData = record
    NodeType: THlpNodeType;
    Path: string;
    Caption: string;
    Info: string;
  end;

	{ TIpHtmlEx }

  TIpHtmlEx = class(TIpHtml)
  private
    fNodeData: PHlpNodeData;
    procedure GetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
  public
    constructor Create;
    procedure LoadFromNode(node: TTreeNode);
  end;


function GetHlpInfo(node: TTreeNode): TIpHtmlEx;
procedure FillHlpTree(tree: TTreeView);


implementation

uses
  RegExpr, LCLIntf;

type
  TDecorateURLsFlags = (
    // describes, which parts of hyper-link must be included
    // into VISIBLE part of the link:
    durlProto, // Protocol (like 'ftp://' or 'http://' or 'https://')
    durlAddr,  // TCP address or domain name (like 'example.com')
    durlPort,  // Port number if specified (like ':8080')
    durlPath,  // Path to document (like 'index.html')
    durlBMark, // Book mark (like '#mark')
    durlParam  // URL params (like '?ID=2&User=13')
 );

TDecorateURLsFlagSet = set of TDecorateURLsFlags;

const
  MAX_LEVEL = 2;

function DecorateURLs(const AText: string;
  AFlags: TDecorateURLsFlagSet = [durlProto,durlAddr,durlPath]): string;
const
  URLTemplate =
   '(?i)'
   + '('
   + '(FTP|HTTP|HTTPS)://'             // Protocol
   + '|www\.)'                   // trick to catch links without
                                 // protocol - by detecting of starting 'www.'
   + '([\w\d\-]+(\.[\w\d\-]+)+)' // TCP addr or domain name
   + '(:\d\d?\d?\d?\d?)?'        // port number
   + '(((/[%+\w\d\-\\\.]*)+)*)'  // unix path
   + '(\?[^\s=&]+=[^\s=&]+(&[^\s=&]+=[^\s=&]+)*)?'
                                 // request (GET) params
   + '(#[\w\d\-%+]+)?';          // bookmark
var
  PrevPos : integer;
  s, Proto, Addr, HRef : string;
begin
  Result := '';
  PrevPos := 1;
  with TRegExpr.Create do
    try
      Expression := URLTemplate;
      if Exec(AText) then
        repeat
          s := '';
          if AnsiCompareText (Match[1], 'www.') = 0 then begin
             Proto := 'http://';
             Addr := Match[1] + Match[3];
             HRef := Proto + Match[0];
          end else begin
             Proto := Match[1];
             Addr := Match[3];
             HRef := Match[0];
          end;
          if durlProto in AFlags then
            s := s + Proto;
          if durlAddr in AFlags then
            s := s + Addr;
          if durlPort in AFlags then
            s := s + Match[5];
          if durlPath in AFlags then
            s := s + Match[6];
          if durlParam in AFlags then
            s := s + Match[9];
          if durlBMark in AFlags then
            s := s + Match[11];
          Result := Result + Copy(AText, PrevPos, MatchPos[0] - PrevPos) +
          '<a href="' + HRef + '">' + s + '</a>';
          PrevPos := MatchPos[0] + MatchLen[0];
        until not ExecNext;
      Result := Result + Copy(AText, PrevPos, MaxInt); // Tail
    finally
        Free;
    end;
end;

function GetFolderData(const path: string): PHlpNodeData;
var
  s: string;
  i: integer;
  lst: TStringList;
begin
  s:=AppendPathDelim(path)+'info.txt';
  if not FileExists(s) then
    exit(nil);
  lst:=TStringList.Create;
  try
    lst.LoadFromFile(s);
    if lst.Count=0 then
      exit(nil);
    s:=Trim(lst[0]);
    if Length(s)=0 then
      exit(nil);
    result:=AllocMem(SizeOf(THlpNodeData));
    result^.NodeType:=ntFolder;
    result^.Caption:=s;
    result^.Path:=path;
    for i:=1 to lst.Count-1 do
      result^.Info+=lst[i]+LineEnding;
  finally
    lst.Free;
  end;
end;

function GetFileData(const path: string): PHlpNodeData;
var
  s: string;
  i: integer;
  f,e: boolean;
  lst: TStringList;
begin
  if not FileExists(path) then
    exit(nil);
  lst:=TStringList.Create;
  try
    lst.LoadFromFile(path);
    if lst.Count=0 then
      exit(nil);
    s:=Trim(lst[0]);
    if Length(s)=0 then
      exit(nil);
    result:=AllocMem(SizeOf(THlpNodeData));
    result^.NodeType:=ntFile;
    result^.Path:=path;
    f:=false;
    e:=f;
    for i:=0 to lst.Count-1 do begin
      s:=Trim(lst[i]);
      if Length(s)<2 then
        continue;
      if (s[1]='*') and (s[2]='*') and (not f) then begin
        f:=true;
        result^.Caption:=TrimLeft(Copy(s,3,MAXINT));
        continue;
      end;
      if (s[1]='*') and (s[2]='*') and f and (not e) then begin
        result^.Info+=TrimLeft(Copy(s,3,MAXINT))+LineEnding;
      end else
        e:=true;
    end;
    if result^.Caption='' then
      result^.Caption:=ExtractFileNameOnly(path);
  finally
    lst.Free;
  end;
end;

procedure RecurseSearch(const path: string; tree: TTreeView; parent: TTreeNode; var level: integer);
var
  si: TSearchRec;
  node: TTreeNode;
  data: PHlpNodeData;
begin
  if level>MAX_LEVEL then
    exit;
  if FindFirst(AppendPathDelim(path)+'*',faAnyFile,si)=0 then begin
    repeat
      if (si.Name='.') or (si.Name='..') or (si.Name='') then
        continue;
      if (si.Attr and faDirectory)=faDirectory then begin
        data:=GetFolderData(AppendPathDelim(Path)+si.Name);
        if data=nil then
          continue;
        node:=tree.Items.AddChildObject(parent,data^.Caption,data);
        node.ImageIndex:=0;
        node.SelectedIndex:=0;
        Inc(level);
        RecurseSearch(data^.Path,tree,node,level);
        Dec(level);
      end else begin
        if si.Name='' then
          continue;
        if (si.Name[1]<>'_') or (ExtractFileExt(si.Name)<>'.inp') then
          continue;
        data:=GetFileData(AppendPathDelim(Path)+si.Name);
        if data=nil then
          continue;
        node:=tree.Items.AddChildObject(parent,data^.Caption,data);
        node.ImageIndex:=1;
        node.SelectedIndex:=1;
      end;
    until FindNext(si)<>0;
    end;
  FindClose(si);
end;

procedure FillHlpTree(tree: TTreeView);
var
  path: string;
  level: integer;
begin
  path:=Format('%shlp%sexamples%s',[ProgramDirectory,PathDelim,PathDelim]);
  level:=0;
  RecurseSearch(path,tree,nil,level);
end;

function ColorToHtml(Color: TColor): string;
var
  c: Longint;
begin
  c:=ColorToRGB(Color);
  Result:=Format('#%.2x%.2x%.2x',[Red(c),Green(c),Blue(c)]);
end;

function GetHlpInfo(node: TTreeNode): TIpHtmlEx;
begin
  if (node=nil) or (node.Data=nil)
  or (Trim(PHlpNodeData(node.Data)^.Info)='') then
    exit(nil);
  Result:=TIpHtmlEx.Create;
  Result.LoadFromNode(node);
end;

{ TIpHtmlEx }

constructor TIpHtmlEx.Create;
begin
  inherited Create;
  OnGetImageX:=@GetImageX;
end;

procedure TIpHtmlEx.LoadFromNode(node: TTreeNode);
var
  ss: TStringStream;
  s: string;
begin
  fNodeData:=node.Data;
  s:=Trim(fNodeData^.Info);
  s:=DecorateURLs(s);
  s:=StringReplace(s,LineEnding,'<br>',[rfReplaceAll]);
  ss:=TStringStream.Create('');
  try
    ss.WriteString('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
    ss.WriteString('<html>');
    ss.WriteString('<head>');
    ss.WriteString('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">');
    ss.WriteString('<title>Information</title>');
    ss.WriteString('</head>');
    ss.WriteString(Format('<body bgcolor="%s">',[ColorToHtml(clWindow)]));
    ss.WriteString(s);
    ss.WriteString('</body>');
    ss.WriteString('</html>');
    ss.Seek(0,0);

    LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;

procedure TIpHtmlEx.GetImageX(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  path: string;
begin
  path:=ExtractFilePath(fNodeData^.Path)+URL;
  if FileExists(path) then begin
    if Picture=nil then
      Picture:=TPicture.Create;
    try
      Picture.LoadFromFile(path);
		except
      FreeAndNil(Picture);
		end;
	end;
end;

end.

