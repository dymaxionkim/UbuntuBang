unit uMathParser;

// Парсер математических выражений
// автор: Дмитрий Швейкус
// адаптация под Lazarus: Василий Макаров

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDouble = Extended;
  TVector = array of TDouble;
  TUserFunc = function(X: TVector): TDouble;
  TFuncType = (tftPointer, tftObj);
  TCommandType = (tcSum, tcDiff, tcMult, tcDiv, tcPow, tcFunc, tcInverse, tcVal);

  TCommand = class
    cType: TCommandType;
    FincID: Integer;
    Valule: TDouble;
  end;

  TVar = class
    V: TDouble;
  end;

  { TCommandList }

  TCommandList = class(TList)
  private
    function Get(Index: Integer): TCommand;
  public
    procedure IncList;
    function Last: TCommand;
    property Commands[Index: Integer]: TCommand read Get; default;
    destructor Destroy; override;
  end;

  { TMathStack }

  TMathStack = class
  private
    A: array of TDouble;
    fCount: Integer;
    aSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(Value: TDouble);
    procedure Clear;
    function Top: TDouble;
    function Pop: TDouble;
    property Count: Integer read fCount;
  end;

  TFuncInfo = class
    PCount: Integer;
    UserFunc: TUserFunc;
    UserFuncObj: TVar;
    FType: TFuncType;
  end;

  { TFuncsCollection }

  TFuncsCollection = class
  public
    X: TVector;
    Funcs: TStringList;
    stFuncs: TStringList;
    constructor Create;
    destructor Destroy; override;
    function EvalNativeFunc(FuncID: Integer; Stack: TMathStack): TDouble;
    function EvalUserFuncObj(FuncID: Integer): TDouble;
    function EvalUserFuncPointer(FuncID: Integer; Stack: TMathStack): TDouble;
    function ExecuteFunc(FuncID: Integer; Stack: TMathStack): TDouble;
    function IsFunction(FuncName: string): Boolean;
    function FuncID(FuncName: string): Integer;
    function GetParameterCount(aFuncID: Integer): Integer;
    function AddUserFunc(FuncName: string; ParameterCount: Integer; UserFunc: TUserFunc): Boolean;
    function AddUserFuncObj(FuncName: string; UserFunc: TVar): Boolean;
  end;

  { TExprParser }

  TExprParser = class
  private
    fStream: TMemoryStream;
    fOrigin: Longint;
    fBuffer: PChar;
    fBufPtr: PChar;
    fBufEnd: PChar;
    fSourcePtr: PChar;
    fSourceEnd: PChar;
    fTokenPtr: PChar;
    fStringPtr: PChar;
    fSourceLine: Integer;
    fSaveChar: Char;
    fToken: Char;
    fFloatType: Char;
    fWideStr: WideString;
    function HexToBin(Text, Buffer: PChar; BufSize: Integer): Integer;
    procedure ReadBuffer;
    procedure SkipBlanks;
  protected
    Code: TCommandList;
    function SkipToken(aValue: char): Boolean;
    procedure EvalItem; virtual;
    procedure EvalSubItem; virtual;
    procedure EvalFactor; virtual;
    procedure EvalTerm; virtual;
    procedure EvalExpr;
  public
    FuncCollection: TFuncsCollection;
    constructor Create(Formula: string; aFuncCollection: TFuncsCollection; aCode:TCommandList);
    destructor Destroy; override;
    procedure CheckToken(T: Char);
    procedure CheckTokenSymbol(const S: string);
    procedure Error(const Ident: string);
    procedure ErrorFmt(const Ident: string; const Args: array of const);
    procedure ErrorStr(const Message: string);
    procedure HexToBinary(Stream: TStream);
    function NextToken: Char;
    function SourcePos: Longint;
    function TokenComponentIdent: string;
    function TokenFloat: Extended;
    function TokenInt: Int64;
    function TokenString: string;
    function TokenWideString: WideString;
    function TokenSymbolIs(const S: string): Boolean;
    property FloatType: Char read fFloatType;
    property SourceLine: Integer read fSourceLine;
    property Token: Char read fToken;
  end;

  { TMathParser }

  TMathParser = class
  private
    stack: TMathStack;//стек - для служебных целей
    fTranslated: Boolean;
    fVars: array of TVar;//массив для хранения переменных
    VarNames: TStringlist;//имена переменнных
    FuncsCollection: TFuncsCollection;//список функций и переменных
    Code: TCommandList; //исполняемый код
    fVarsCount: integer; //число переменных
    function VarID(Name: string): Integer;
    function GetVars(index: Integer): TDouble;
    procedure SetVars(index: Integer; const Value: TDouble);
    function GetVarByName(VarName: string): TDouble;
    procedure SetVarByName(VarName: string; const Value: TDouble);
    function GetVarName(index: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    //добавляет пользовательскую функцию - смотри "UParseTypes.Pas"
    function AddUserFunc(FuncName: string; ParameterCount: Integer; Func: TUserFunc): Boolean;
    //транслирует в байт код
    function Translate(aExpr, aVars: string): boolean;
    property Translated: boolean read fTranslated;
    //имя переменной по индексу
    property VarName[index: Integer]: string read GetVarName ;
    //различные способы обращения к переменным
    //ПЕРЕМЕННЫЕ НУМЕРУЮТСЯ НАЧИНАЯ С 1 А НЕ С 0
    property Vars[index: Integer]: TDouble read GetVars write SetVars;
    property VarByName[aVarName: string]: TDouble read GetVarByName write SetVarByName;
    property VarsCount: Integer read fVarsCount;
    //вычислить значение
    //ПЕРЕМЕННЫЕ в векторе Vars НУМЕРУЮТСЯ НАЧИНАЯ С 1 А НЕ С 0
    function Get(aVars: TVector): TDouble;
  end;

implementation

uses Math, RTLConsts;

const
  ParseBufSize = 4096;
  MaxParameterCount = 32;
  cStFuncCount = 24;
  sFunctionExists = 'Function with name "%s" allready exists';
  sUnknownIdentifier = 'Unknown identifier "%s"';
  sUnknownSymbol = 'Unknown symbol "%s"';
  sDuplicateVariables = 'Duplicate variables "%s"';

  FuncData: array[1..cStFuncCount] of string = (
    {N Name ParCount }
    '1 arccos  1 ',
    '2 arcctan  1 ',
    '3 arcsin  1 ',
    '4 arctan  1 ',
    '5 cos  1 ',
    '6 cosh  1 ',
    '7 ctan  1 ',
    '8 sum  2 ',
    '9 exp  1 ',
    '10 ln  1 ',
    '11 log10  1 ',
    '12 log2  1 ',
    '13 max  2 ',
    '14 min  2 ',
    '15 power  2 ',
    '16 sin  1 ',
    '17 sinh  1 ',
    '18 sqr  1 ',
    '19 sqrt  1 ',
    '20 tan  1 ',
    '21 tanh  1 ',
    '22 round  1 ',
    '23 abs  1 ',
    '24 Pi  0 '
  );

{ TCommandList }

destructor TCommandList.Destroy;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
  Commands[i].Free;
  inherited Destroy;
end;

function TCommandList.Get(Index: Integer): TCommand;
begin
  Result:=TCommand(Items[Index]);
end;

procedure TCommandList.IncList;
begin
  Add(TCommand.Create)
end;

function TCommandList.Last: TCommand;
begin
  result:=Commands[Count-1];
end;


{ TMathStack }

constructor TMathStack.Create;
begin
  aSize:=50;
  SetLength(A,aSize+1);
  fCount:=0;
end;

destructor TMathStack.Destroy;
begin
  SetLength(A,0);
  inherited Destroy;
end;

procedure TMathStack.Push(Value: TDouble);
begin
  inc(fCount);
  if fCount>=ASize then
  begin
    aSize:=(aSize*5) div 4;
    SetLength(A,aSize+1);
  end;
  A[fCount]:=Value;
end;

procedure TMathStack.Clear;
begin
  fCount:=0;
end;

function TMathStack.Top: TDouble;
begin
  Result:=A[fCount];
end;

function TMathStack.Pop: TDouble;
begin
  result:=A[fCount];
  dec(fCount);
end;

{ TFuncsCollection }

constructor TFuncsCollection.Create;
var
  i: integer;
  List: TStringList;
begin
  stFuncs:=TStringList.Create;
  List:=TStringList.Create;   //---список функций с информацией о них
  try
  for i:=0 to cStFuncCount-1 do begin
    ExtractStrings([' '],[' '],@FuncData[i+1][1],List);
    stFuncs.AddObject(List[1],TFuncInfo.Create);
    (stFuncs.Objects[i] as TFuncInfo).PCount:=StrToInt(List[2]);
    List.Clear;
  end;
  finally
  List.Free;
  end;
  SetLength(X,MaxParameterCount+1); //создаём массив длиной MaxParameterCount
  Funcs:=TStringList.Create;
  Funcs.AddStrings(stFuncs);
end;

destructor TFuncsCollection.Destroy;
var
  i: integer;
begin
  for i:=0 to stFuncs.Count-1 do (stFuncs.Objects[i] as TFuncInfo).Free;
  stFuncs.Free;
  Funcs.Free;
  SetLength(X,0);
  inherited Destroy;
end;

function TFuncsCollection.EvalNativeFunc(FuncID: Integer; Stack: TMathStack
  ): TDouble;
var
  v1,v2: TDouble;
begin
  case FuncID of
  0: Result:=ArcCos(Stack.Pop);
  1: Result:=ArcTan(1/Stack.Pop);
  2: Result:=ArcSin(Stack.Pop);
  3: Result:=ArcTan(Stack.Pop);
  4: Result:=Cos(Stack.Pop);
  5: Result:=Cosh(Stack.Pop);
  6: Result:=1/Tan(Stack.Pop);
  7: begin
        v2:=Stack.Pop;
        v1:=Stack.Pop;
        Result:=v1+v2;
      end;
  8: Result:=Exp(Stack.Pop);
  9: Result:=Ln(Stack.Pop);
  10: Result:=Log10(Stack.Pop);
  11: Result:=Log2(Stack.Pop);
  12: begin
        v2:=Stack.Pop;
        v1:=Stack.Pop;
        Result:=Max(v1,v2);
      end;
  13: begin
        v2:=Stack.Pop;
        v1:=Stack.Pop;
        Result:=Min(v1,v2);
      end;
  14: begin
        v2:=Stack.Pop;
        v1:=Stack.Pop;
        Result:=Power(v1,v2);
      end;
  15: Result:=Sin(Stack.Pop);
  16: Result:=Sinh(Stack.Pop);
  17: Result:=Sqr(Stack.Pop);
  18: Result:=Sqrt(Stack.Pop);
  19: Result:=Tan(Stack.Pop);
  20: Result:=Tanh(Stack.Pop);
  21: Result:=Round(Stack.Pop);
  22: Result:=Abs(Stack.Pop);
  23: Result:=Pi;
  end;
end;

function TFuncsCollection.EvalUserFuncObj(FuncID: Integer): TDouble;
begin
  result:=(Funcs.Objects[FuncID] as TFuncInfo).UserFuncObj.V;
end;

function TFuncsCollection.EvalUserFuncPointer(FuncID: Integer; Stack: TMathStack
  ): TDouble;
var
  i,PCount: integer;
begin
  PCount:=(Funcs.Objects[FuncID] as TFuncInfo).PCount;
  for i:=1 to PCount do
  begin
    X[PCount-i+1]:=Stack.Pop;
  end;
  result:=(Funcs.Objects[FuncID] as TFuncInfo).UserFunc(X);
end;

function TFuncsCollection.ExecuteFunc(FuncID: Integer; Stack: TMathStack
  ): TDouble;
begin
   if FuncID < cStFuncCount then Result:=EvalNativeFunc(FuncID,Stack)
   else
   case (Funcs.Objects[FuncID] as TFuncInfo).FType of
     tftPointer :Result:=EvalUserFuncPointer(FuncID,Stack);
     tftObj     :Result:=EvalUserFuncObj    (FuncID);
   end;
end;

function TFuncsCollection.IsFunction(FuncName: string): Boolean;
begin
  Result:=FuncID(FuncName)>-1;
end;

function TFuncsCollection.FuncID(FuncName: string): Integer;
begin
  for Result:=0 to Funcs.Count-1 do
  if CompareText(FuncName,Funcs[Result]) = 0 then Exit;
  Result:=-1;
end;

function TFuncsCollection.GetParameterCount(aFuncID: Integer): Integer;
begin
  Result:=(Funcs.Objects[aFuncID] as TFuncInfo).PCount;
end;

function TFuncsCollection.AddUserFunc(FuncName: string;
  ParameterCount: Integer; UserFunc: TUserFunc): Boolean;
begin
  result:=False;
  if IsFunction(FuncName) then raise
  EParserError.CreateFmt(sFunctionExists,[FuncName]);
  Funcs.AddObject(FuncName,TFuncInfo.Create);
  (Funcs.Objects[Funcs.Count-1] as TFuncInfo).PCount:=ParameterCount;
  (Funcs.Objects[Funcs.Count-1] as TFuncInfo).UserFunc:=UserFunc;
  (Funcs.Objects[Funcs.Count-1] as TFuncInfo).FType:=tftPointer;
  Result:=true;
end;

function TFuncsCollection.AddUserFuncObj(FuncName: string; UserFunc: TVar): Boolean;
begin
  result:=False;
  if IsFunction(FuncName) then raise
  EParserError.CreateFmt(sFunctionExists,[FuncName]);
  Funcs.AddObject(FuncName,TFuncInfo.Create);
  (Funcs.Objects[Funcs.Count-1] as TFuncInfo).PCount:=0;
  (Funcs.Objects[Funcs.Count-1] as TFuncInfo).UserFuncObj:=UserFunc;
  (Funcs.Objects[Funcs.Count-1] as TFuncInfo).FType:=tftObj;
  Result:=true;
end;

{ TExprParser }

constructor TExprParser.Create(Formula: string; aFuncCollection: TFuncsCollection;
  aCode:TCommandList);
begin
  Code:=aCode;
  fStream:=TMemoryStream.Create;
  fStream.SetSize(Length(Formula));
  fStream.WriteBuffer(Formula[1],Length(Formula));
  fStream.Position:=0;
  GetMem(fBuffer,ParseBufSize);
  fBuffer[0]:=#0;
  fBufPtr:=fBuffer;
  fBufEnd:=fBuffer+ParseBufSize;
  fSourcePtr:=fBuffer;
  fSourceEnd:=fBuffer;
  fTokenPtr:=fBuffer;
  fSourceLine:=1;
  NextToken;
  FuncCollection:=aFuncCollection;
  EvalExpr;
end;

destructor TExprParser.Destroy;
begin
  if fBuffer<>nil then begin
    FStream.Seek(PtrInt(fTokenPtr)-PtrInt(fBufPtr),soCurrent);
    FreeMem(fBuffer,ParseBufSize);
  end;
  fStream.Free;
  inherited Destroy;
end;

function TExprParser.HexToBin(Text, Buffer: PChar; BufSize: Integer): Integer;
const
  Convert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
var
  I: Integer;
begin
  I := BufSize;
  while I > 0 do
  begin
    if not (Text[0] in ['0'..'f']) or not (Text[1] in ['0'..'f']) then Break;
    Buffer[0] := Char((Convert[Text[0]] shl 4) + Convert[Text[1]]);
    Inc(Buffer);
    Inc(Text, 2);
    Dec(I);
  end;
  Result := BufSize - I;
end;

procedure TExprParser.ReadBuffer;
var
  Count: Integer;
begin
  Inc(fOrigin,fSourcePtr-fBuffer);
  fSourceEnd[0]:=fSaveChar;
  Count:=fBufPtr-fSourcePtr;
  if Count<>0 then Move(fSourcePtr[0],fBuffer[0],Count);
  fBufPtr:=fBuffer+Count;
  Inc(fBufPtr,fStream.Read(fBufPtr[0],fBufEnd-fBufPtr));
  fSourcePtr:=fBuffer;
  fSourceEnd:=fBufPtr;
  if fSourceEnd=fBufEnd then begin
    fSourceEnd:=LineStart(fBuffer,fSourceEnd-1);
    if fSourceEnd=fBuffer then Error(SLineTooLong);
  end;
  fSaveChar:=fSourceEnd[0];
  fSourceEnd[0]:=#0;
end;

procedure TExprParser.SkipBlanks;
begin
  while True do
  begin
    case fSourcePtr^ of
      #0:
        begin
          ReadBuffer;
          if fSourcePtr^=#0 then Exit;
          Continue;
        end;
      #10:
        Inc(fSourceLine);
      #33..#255:
        Exit;
    end;
    Inc(fSourcePtr);
  end;
end;

procedure TExprParser.CheckToken(T: Char);
begin
  if Token<>T then
    case T of
      Classes.toSymbol:
        Error(SIdentifierExpected);
      Classes.toString, Classes.toWString:
        Error(SStringExpected);
      Classes.toInteger, Classes.toFloat:
        Error(SNumberExpected);
    else
      ErrorFmt(SCharExpected, [T]);
    end;
end;

procedure TExprParser.CheckTokenSymbol(const S: string);
begin
  if not TokenSymbolIs(S) then ErrorFmt(SSymbolExpected, [S]);
end;

procedure TExprParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

procedure TExprParser.ErrorFmt(const Ident: string; const Args: array of const);
begin
  ErrorStr(Format(Ident, Args));
end;

procedure TExprParser.ErrorStr(const Message: string);
begin
  raise EParserError.CreateResFmt(@SParseError, [Message, FSourceLine]);
end;

procedure TExprParser.HexToBinary(Stream: TStream);
var
  Count: Integer;
  Buffer: array[0..255] of Char;
begin
  SkipBlanks;
  while fSourcePtr^ <> '}' do
  begin
    Count := HexToBin(fSourcePtr, Buffer, SizeOf(Buffer));
    if Count = 0 then Error(SInvalidBinary);
    Stream.Write(Buffer, Count);
    Inc(fSourcePtr, Count * 2);
    SkipBlanks;
  end;
  NextToken;
end;

function TExprParser.NextToken: Char;
var
  I, J: Integer;
  IsWideStr: Boolean;
  P, S: PChar;
begin
  SkipBlanks;
  P := fSourcePtr;
  fTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_':
      begin
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do Inc(P);
        Result := toSymbol;
      end;
    '#', '''':
      begin
        IsWideStr := False;
        J := 0;
        S := P;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                if (I > 127) then IsWideStr := True;
                Inc(J);
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error(SInvalidString);
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then Break;
                      end;
                  end;
                  Inc(J);
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        P := S;
        if IsWideStr then SetLength(FWideStr, J);
        J := 1;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                if IsWideStr then
                begin
                  FWideStr[J] := WideChar(SmallInt(I));
                  Inc(J);
                end else
                begin
                  S^ := Chr(I);
                  Inc(S);
                end;
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error(SInvalidString);
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then Break;
                      end;
                  end;
                  if IsWideStr then
                  begin
                    FWideStr[J] := WideChar(P^);
                    Inc(J);
                  end else
                  begin
                    S^ := P^;
                    Inc(S);
                  end;
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        fStringPtr := S;
        if IsWideStr then
          Result := Classes.toWString
        else
          Result := Classes.toString;
      end;
    '$':
      begin
        Inc(P);
        while P^ in ['0'..'9', 'A'..'F', 'a'..'f'] do Inc(P);
        Result := Classes.toInteger;
      end;
    '0'..'9':
      begin
        Inc(P);
        while P^ in ['0'..'9'] do Inc(P);
        Result := Classes.toInteger;
        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do
        begin
          Inc(P);
          Result := toFloat;
        end;
        if (P^ in ['c', 'C', 'd', 'D', 's', 'S']) then
        begin
          Result := Classes.toFloat;
          FFloatType := P^;
          Inc(P);
        end else
          FFloatType := #0;
      end;
  else
    Result := P^;
    if Result <> toEOF then Inc(P);
  end;
  fSourcePtr := P;
  FToken := Result;
end;

function TExprParser.SourcePos: Longint;
begin
  Result := FOrigin + (fTokenPtr - FBuffer);
end;

function TExprParser.TokenComponentIdent: string;
var
  P: PChar;
begin
  CheckToken(Classes.toSymbol);
  P := fSourcePtr;
  while P^ = '.' do
  begin
    Inc(P);
    if not (P^ in ['A'..'Z', 'a'..'z', '_']) then
      Error(SIdentifierExpected);
    repeat
      Inc(P)
    until not (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  end;
  fSourcePtr := P;
  Result := TokenString;
end;

function TExprParser.TokenFloat: Extended;
var
  e: integer;
begin
  if FFloatType <> #0 then Dec(fSourcePtr);
  Val(TokenString,Result,e);
  if e <> 0 then
  raise EConvertError.CreateFmt('"%s" is not a valid floating point value',[TokenString]);
  if FFloatType <> #0 then Inc(fSourcePtr);
end;

function TExprParser.TokenInt: Int64;
begin
  Result := StrToInt64(TokenString);
end;

function TExprParser.TokenString: string;
var
  L: Integer;
begin
  if FToken = toString then
    L := fStringPtr - fTokenPtr
  else
    L := fSourcePtr - fTokenPtr;
  SetString(Result, fTokenPtr, L);
end;

function TExprParser.TokenWideString: WideString;
begin
  if FToken = Classes.toString then
    Result := TokenString
  else
    Result := FWideStr;
end;

function TExprParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (Token = Classes.toSymbol) and SameText(S, TokenString);
end;

function TExprParser.SkipToken(aValue: char): Boolean;
begin
  Result := Token = aValue;
  if Result then
    NextToken;
end;

procedure TExprParser.EvalItem;
begin
  EvalSubItem;
  if SkipToken('^') then
  begin
    EvalItem;
    Code.IncList;
    Code.Last.cType:=tcPow;
  end;
end;

procedure TExprParser.EvalSubItem;
var
  i,FID,PCount: integer;
begin
  case Token of
    toInteger,toFloat:
    begin
      Code.IncList;
      Code.Last.cType:=tcVal;
      Code.Last.Valule:=TokenFloat;
    end;
    '(':
      begin
        NextToken;
        EvalExpr;
        CheckToken(')');
      end;
    toSymbol:
      begin
        FID:=FuncCollection.FuncID(TokenString);
        if FID<>-1 then
        begin
          PCount:=FuncCollection.GetParameterCount(FID);
          if PCount <> 0 then
          begin
            NextToken;
            CheckToken('(');
            for i:=1 to PCount do
            begin
              NextToken;
              EvalExpr;
              if i<>PCount then CheckToken(',');
            end;
            CheckToken(')');
          end;
          Code.IncList;
          Code.Last.cType:=tcFunc;
          Code.Last.FincID:=FID;
        end
        else raise EParserError.CreateFmt(sUnknownIdentifier, [TokenString]);
      end;
  else
    raise EParserError.CreateFmt(sUnknownSymbol, [Token]);
  end;
  NextToken;
end;

procedure TExprParser.EvalFactor;
begin
  case Token of
    '+':
      begin
        NextToken;
        EvalItem;
      end;
    '-':
      begin
        NextToken;
        EvalItem;
        Code.IncList;
        Code.Last.cType:=tcInverse;
      end;
  else
    EvalItem;
  end;
end;

procedure TExprParser.EvalTerm;
begin
  EvalFactor;
  while Token in ['*','/'] do
    if SkipToken('*') then
    begin
      EvalFactor;
      Code.IncList;
      Code.Last.cType:=tcMult;
    end
    else if SkipToken('/') then
    begin
      EvalFactor;
      Code.IncList;
      Code.Last.cType:=tcDiv;
    end
end;

procedure TExprParser.EvalExpr;
begin
  EvalTerm;
  while Token in ['-','+'] do
  begin
    if SkipToken('+') then
    begin
      EvalTerm;
      Code.IncList;
      Code.Last.cType:=tcSum;
    end
    else if SkipToken('-') then
    begin
      EvalTerm;
      Code.IncList;
      Code.Last.cType:=tcDiff;
    end;
  end;
end;

{ TMathParser }

constructor TMathParser.Create;
begin
  fVarsCount:=0;
  stack:=TMathStack.Create;
  FuncsCollection:=TFuncsCollection.Create;
  VarNames:=TStringList.Create;
  Code:=TCommandList.Create;
end;

destructor TMathParser.Destroy;
var
  i: integer;
begin
  Stack.Free;
  FuncsCollection.Free;
  for i:=1 to FVarsCount do FVars[i].Free;
  SetLength(fVars,0);
  VarNames.Free;
  Code.Free;
  inherited Destroy;
end;

function TMathParser.VarID(Name: string): Integer;
begin
  for Result:=0 to VarNames.Count-1 do
    if CompareText(Name,VarNames[Result]) = 0 then Exit;
  Result:=-1;
end;

function TMathParser.GetVars(index: Integer): TDouble;
begin
  result:=fVars[index].V;
end;

procedure TMathParser.SetVars(index: Integer; const Value: TDouble);
begin
  fVars[index].V:=Value;
end;

function TMathParser.GetVarByName(VarName: string): TDouble;
begin
  Result:=fVars[VarID(VarName)+1].V;
end;

procedure TMathParser.SetVarByName(VarName: string; const Value: TDouble);
begin
  fVars[VarID(VarName)+1].V:=Value;
end;

function TMathParser.GetVarName(index: Integer): string;
begin
  result:=VarNames[index-1];
end;

procedure TMathParser.Clear;
var
  i:integer;
begin
  for i:=1 to fVarsCount do
    fVars[i].Free; //удаляем "контейнеры" переменных
  fVarsCount:=0; //обнуляем число переменных
  funcsCollection.Free; //удаляем переменные и функции
  funcsCollection:=TFuncsCollection.create;
  VarNames.Clear;
  FTranslated:=False;
  Code.Clear; //очищаем код
end;

function TMathParser.AddUserFunc(FuncName: string; ParameterCount: Integer;
  Func: TUserFunc): boolean;
begin
  Result:=FuncsCollection.AddUserFunc(FuncName,ParameterCount,Func);
end;

function TMathParser.Translate(aExpr, aVars: string): boolean;
var
  Builder: TExprParser;
  i: integer;
begin
  i:=2;
  while i<Length(aExpr) do // ТParser глючит, если есть выражения типа "2-2"
  begin                   // поэтому превращаем их в "2 -2"
    if (aExpr[i]='+') or (aExpr[i]='-') then
      if not((aExpr[i-1]='E') or (aExpr[i-1]='e')) then
      begin
        Insert(' ',aExpr,i);
        inc(i);
      end;
    inc(i);
  end;
  Builder:=nil;
  try
    try
      result:=true;
      ExtractStrings([','],[' '],PChar(aVars),VarNames);//RTFM
      FVarsCount:=VarNames.Count;
      SetLength(FVars,FVarsCount+1);                   //создаём массив заготовку
      for i:=1 to FVarsCount do FVars[i]:=TVar.Create; //создаём ПОКА нулевые переменные
      for i:=0 to FVarsCount-1 do VarNames[i]:=UpperCase(VarNames[i]);
            //переводим их в верхний регистр - чтоб потом проще сравнивать
      for i:=0 to FVarsCount-1 do  //проверяем переменные на совпадения имён
        if VarNames.IndexOf(VarNames[i])<>i then
        raise Exception.CreateFmt(sDuplicateVariables,[VarNames[i]]);
      for i:=1 to FVarsCount do FuncsCollection.AddUserFuncObj(VarNames[i-1],FVars[i]);
      // добавляем переменные

      Builder :=TExprParser.Create(aExpr,FuncsCollection,code);
           //собственно создаём сам код
    except
    on E:Exception do
      begin
        result:=false;
        VarNames.Clear;
        for i:=1 to FVarsCount do FVars[i].Destroy;
        FVarsCount:=0;
        raise Exception.Create(E.Message);
      end;
    end;
  finally
    FTranslated:=Result;
    Builder.Free;
  end;
end;

function TMathParser.Get(aVars: TVector): TDouble;
var
  i: integer;
  V1: TDouble;
begin
  for i:=1 to fVarsCount do fVars[i].V:=aVars[i];
  //просто пробегаем по коду и выполняем комманды
  for i:=0 to Code.Count-1 do
  begin
    case Code[i].cType of
      tcSum   : stack.Push(stack.Pop+stack.Pop); // +
      tcDiff  :                                  //-
      begin
        V1:=stack.Pop;
        stack.Push(stack.Pop-V1);
      end;
      tcVal      : stack.Push(Code[i].Valule);
      tcMult     : stack.Push(stack.Pop*stack.Pop); //*
      tcDiv      :
      begin
        V1:=stack.Pop;
        stack.Push(stack.Pop/V1);
      end;
      tcPow      :
      begin
        V1:=stack.Pop;
        stack.Push(Power(stack.Pop,V1));
      end;
      tcFunc     : stack.Push(FuncsCollection.ExecuteFunc(Code[i].FincID,stack));
      tcInverse  : stack.Push(-stack.Pop);
    end;
  end;
  Result:=stack.Pop;
end;


end.

