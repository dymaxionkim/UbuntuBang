unit uFileTransform;

interface

uses
  Classes, SysUtils;

procedure SumMinMax(const FileName: TFileName; Result: TStrings);
procedure TransformFile(const Filename: string);

implementation

uses
  Controls, Forms, Math, LCLType, Contnrs, FileUtil;

type

  EParce = class(Exception);

  TCol = 1..3;
  TVal = array[TCol] of double;

  TDatStat = record
    Min: double;
    Max: double;
    Sum: double;
  end;

  TDatItem = class
    Title: string;
    Empty: boolean;
    Col: array[TCol] of TDatStat;
    constructor Create;
  end;


constructor TDatItem.Create;
begin
  Empty:=true;
  FillChar(Col,SizeOf(Col),0);
end;


function DoubleToStr(const x: double): string;
begin
  DoubleToStr:=FormatFloat('0.0000000E+00',x,DefaultFormatSettings);
end;

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

function IsSetStr(s: string): boolean;
var
  p1,p2: integer;
begin
  result:=false;
  s:=LowerCase(s);
  p1:=Pos('for set',s);
  if p1=0 then exit;
  p2:=Pos('and time',s);
  if p2=0 then exit;
  result:=(p1<p2);
end;

function CntBsRepet(const s: string): integer;
var
  i: integer;
begin
  result:=0;
  for i:=1 to Length(s) do
    if s[i]=' ' then
      inc(result);
end;

function ParseParamStr(s: string; out v: TVal; out FirstInt: boolean): boolean;
var
  i: TCol;
  n: string;
  d: double;
  p,x,e: integer;
begin
  result:=false;
  FirstInt:=false;
  p:=Pos(' ',s);
  if p=0 then
    Exit;
  n:=Copy(s,1,p-1);
  Val(n,x,e);
  if e=0 then begin
    // Delete First Integer Value;
    Delete(s,1,p);
    FirstInt:=true;
	end else begin
    Val(n,d,e);
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
    Val(n,v[i],e);
    if e<>0 then
       Exit;
    if p>0 then
       Delete(s,1,p);
  end;
  result:=true;
end;

procedure SumMinMax(const FileName: TFileName; Result: TStrings);
var
  List: TStringList;
  Items: TObjectList;
  Item: TDatItem;
  s: string;
  i,j,e: integer;
  ColNum: integer;
  Parsing: boolean;
  FirstInt: boolean;
  v: TVal;
begin
  Assert(Assigned(Result),'Result is not assigned!');
  List:=TStringList.Create;
  Items:=TObjectList.Create;
  Screen.Cursor:=crHourGlass;
  try
     List.LoadFromFile(FileName);
     Parsing:=false;
     ColNum:=0;
     for i:=0 to List.Count-1 do begin
        s:=Trim(List[i]);
        if Length(s)=0 then
           Continue;
        s:=ClearStr(s);
        if IsSetStr(s) then begin
           Item:=TDatItem.Create;
           Item.Title:=Trim(s);
           Items.Add(Item);
           Parsing:=true;
           Continue;
        end
        else
        if parsing then begin
           if not ParseParamStr(s,v,FirstInt) then begin
						 Parsing:=false;
             if Item.Empty then
                Items.Remove(Item);
             Continue;
           end;
           if ColNum=0 then begin
             ColNum:=CntBsRepet(s);
             if not FirstInt then
               Inc(ColNum);
             if not(ColNum in [1,3]) then
               raise EParce.CreateFmt('Parse error. Colunms number(%d) is illegal.',[ColNum]);
					 end;
           for j:=1 to ColNum do begin
              if Item.Empty then begin
                 Item.Col[j].Sum:=v[j];
                 Item.Col[j].Min:=v[j];
                 Item.Col[j].Max:=v[j];
              end else begin
                 Item.Col[j].Sum+=v[j];
                 Item.Col[j].Min:=Min(Item.Col[j].Min,v[j]);
                 Item.Col[j].Max:=Max(Item.Col[j].Max,v[j]);
              end;
           end;
           if Item.Empty then
             Item.Empty:=false;
        end
        else
           Parsing:=false;
     end;

     Result.BeginUpdate;
     try
        Result.Clear;
        for i:=0 to Items.Count-1 do begin
            Item:=Items[i] as TDatItem;
            Result.Add(Item.Title);
            for j:=1 to ColNum do
               with Item.Col[j] do
                  Result.Add(Format('colunm%d: sum=%s, min=%s, max=%s',
                  [j,DoubleToStr(Sum),DoubleToStr(Min),DoubleToStr(Max)]));
            Result.Add('');
        end;
     finally
       Result.EndUpdate;
     end;

  finally
    Screen.Cursor:=crDefault;
    List.Free;
    Items.Free;
  end;
end;


function ProcessFile(const Filename: string; Res, Err, Lev: TStringList): boolean;
var
  i,j,x: integer;
  s,m,f: string;
  p: TStringList;
begin
  result:=false;
  if not FileExists(Filename) then begin
    Err.Add('File "'+Filename+'" not found.');
    exit;
  end;
  p:=TStringList.Create;
  try
  p.LoadFromFile(Filename);
  if p.Count=0 then begin
    Err.Add('File "'+Filename+'" is empty. This reference are deleted.');
    result:=true;
    exit;
  end;
  for i:=0 to p.Count-1 do begin
    s:=Trim(UpperCase(p[i]));
    x:=Pos('=',p[i]);
    if (Length(s)>16) and (x<>0) then begin
      m:=Copy(s,1,8);
      if (m='*INCLUDE') and (Pos('INPUT',s)<>0) then begin
        s:=Trim(Copy(p[i],x+1,Length(p[i])));
        f:=ExtractFilePath(Filename)+s;
        j:=Lev.IndexOf(f);
        if j>=0 then begin
          Err.Add('File "'+Filename+'". Recursive include file.');
          exit;
        end;
        Lev.Add(f);
        try
          if ProcessFile(f,Res,Err,Lev) then begin
            if not DeleteFile(f) then
              Err.Add('Can''t delete file "'+Filename+'".');
            continue;
          end;
        finally
        Lev.Delete(Lev.Count-1);
        end;
      end;
    end;
    Res.Add(p[i]);
  end;
  finally
  p.Free;
  end;
  result:=true;
end;

procedure TransformFile(const Filename: string);
var
  Res: TStringList; // result
  Err: TStringList; // errors
  Lev: TStringList; // levels
  dir,nfn: string;
begin
  Res:=TStringList.Create;
  try
  Err:=TStringList.Create;
    try
    Lev:=TStringList.Create;
    try
      Lev.Add(Filename);
      ProcessFile(Filename,Res,Err,Lev);
      dir:=ExtractFilePath(Filename);
      nfn:=ChangeFileExt(ExtractFileName(Filename),'');
      nfn:=dir+nfn+'_combined.inp';
      Res.SaveToFile(nfn);
      finally
      Lev.Free;
      end;
      if Err.Count>0 then begin
        Err.Insert(0,'Errors:');
        Application.MessageBox(Err.GetText,'Error',MB_ICONSTOP);
      end;
    finally
    Err.Free;
    end;
  finally
  Res.Free;
  end;
end;




end.
