unit uHighliter;

{$IFDEF FPC}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_LAZARUS}
{$ENDIF}
{$H+}

interface

uses
  Classes,
  {$IFDEF SYN_CLX}
  QGraphics,
  {$ELSE}
  Graphics,
  {$ENDIF}
  SynEditTypes, SynEditHighlighter;

type
  TtkTokenKind = (tkComment, tkText, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown);

  TProcTableProc = procedure of object;

type

  { TSynIniSyn }

  TInpHighlighter = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    procedure KeyProc;
    procedure CRProc;
    procedure EqualProc;
    procedure TextProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SignProc;
    procedure SpaceProc;
    procedure MakeMethodTables;
  protected
    {General Stuff}
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber:Integer); override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property TextAttri   : TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property KeyAttri    : TSynHighlighterAttributes read fKeyAttri
      write fKeyAttri;
    property NumberAttri : TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri  : TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri : TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri : TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  SynEditStrConst;

procedure TInpHighlighter.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      #0      : fProcTable[i] := @NullProc;
      #10 {LF}: fProcTable[i] := @LFProc;
      #13 {CR}: fProcTable[i] := @CRProc;
      '0'..'9': fProcTable[i] := @NumberProc;
      #42 {*} : fProcTable[i] := @SignProc;
      #61 {=} : fProcTable[i] := @EqualProc;
      #1..#9, #11, #12, #14..#32: fProcTable[i] := @SpaceProc;
    else
      fProcTable[i] := @TextProc;
    end;
end;

constructor TInpHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri            := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style      := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fTextAttri               := TSynHighlighterAttributes.Create(SYNS_AttrText);
  AddAttribute(fTextAttri);

  fKeyAttri                := TSynHighlighterAttributes.Create(SYNS_AttrKey);
  fKeyAttri.Style          := [fsBold];
  AddAttribute(fKeyAttri);

  fNumberAttri             := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground  := clNavy;
  AddAttribute(fNumberAttri);

  fSpaceAttri              := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri             := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);

  fSymbolAttri             := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clRed;
  AddAttribute(fSymbolAttri);

  SetAttributesOnChange(@DefHighlightChange);

  fDefaultFilter      := SYNS_FilterINI;
  MakeMethodTables;
end; { Create }

procedure TInpHighlighter.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber:Integer);
begin
  inherited;
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TInpHighlighter.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TInpHighlighter.EqualProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TInpHighlighter.KeyProc;
begin
  fTokenID := tkKey;
  inc(Run);
  while FLine[Run] <> #0 do
    case FLine[Run] of
      '=': break;
      #10: break;
      #13: break;
    else inc(Run);
    end;
end;

procedure TInpHighlighter.TextProc;
begin
  inc(Run);
  {$IFDEF SYN_LAZARUS}
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @TextProc)) do inc(Run);
  {$ENDIF}
  fTokenID := tkText;
  //fTokenID := tkText;
  //inc(Run);
end;

procedure TInpHighlighter.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TInpHighlighter.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TInpHighlighter.NumberProc;
begin
  Inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E', '-', '+'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then
          Break
        else
          fTokenID := tkNumber;
      'e', 'E': fTokenID := tkNumber;
      '-', '+':
        begin
          if fTokenID <> tkNumber then // arithmetic
            Break;
          if not (FLine[Run - 1] in ['e', 'E']) then
            Break; //float, but it ends here
        end;
    end;
    Inc(Run);
  end;
end;

// ;
procedure TInpHighlighter.SignProc;
begin
  if fLine[Run+1]='*' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else begin
      fTokenID := tkKey;
      inc(Run);
      while FLine[Run] <> #0 do
      case FLine[Run] of
           #1..#48: break;
           else inc(Run);
      end;
  end;
end;

procedure TInpHighlighter.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TInpHighlighter.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TInpHighlighter.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TInpHighlighter.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TInpHighlighter.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TInpHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart := FLine + fTokenPos;
end;

function TInpHighlighter.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TInpHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkText   : Result := fTextAttri;
    tkKey    : Result := fKeyAttri;
    tkNumber : Result := fNumberAttri;
    tkSpace  : Result := fSpaceAttri;
    tkString : Result := fStringAttri;
    tkSymbol : Result := fSymbolAttri;
    tkUnknown: Result := fTextAttri;
    else Result := nil;
  end;
end;

function TInpHighlighter.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TInpHighlighter.GetTokenPos: Integer;
begin
 Result := fTokenPos;
end;

function TInpHighlighter.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

class function TInpHighlighter.GetLanguageName: string;
begin
  Result := SYNS_LangINI;
end;

function TInpHighlighter.GetSampleSource: String;
begin
  Result := 'Test';
end;

initialization
  RegisterPlaceableHighlighter(TInpHighlighter);

end.
