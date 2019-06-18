unit uSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, IniFiles;

type

  { TSettings }

  TSettings = class(TComponent)
  private
    fIni: TMemIniFile;
    fFileName: TFileName;
    fTemplatesPath: string;
    fDialogsPath: string;
    procedure ReadSettings;
    procedure SetDialogsPath(AValue: string);
    procedure SetTemplatesPath(AValue: string);
    procedure WriteSettings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Store;
  published
    property TemplatesPath: string read fTemplatesPath write SetTemplatesPath;
    property DialogsPath: string read fDialogsPath write SetDialogsPath;
  end;

var
  Settings: TSettings;

implementation

uses
  unit1;

{ TSettings }

constructor TSettings.Create(AOwner: TComponent);
var
  appdir: string;
begin
  inherited Create(AOwner);
  appdir:=ProgramDirectory;
  fFileName:=appdir+'Options.ini';
  fIni:=TMemIniFile.Create(fFileName);
  fTemplatesPath:=appdir+'Templates';
  fDialogsPath:=appdir;
  ReadSettings;
end;

destructor TSettings.Destroy;
begin

  fIni.Free;
  inherited Destroy;
end;



procedure TSettings.ReadSettings;
begin
  Application.ShowHint:=fIni.ReadBool('Common','ShowHint',true);

  // path's
  TemplatesPath:=fIni.ReadString('Directories','Templates',fTemplatesPath);
  DialogsPath:=fIni.ReadString('Directories','Dialogs',fDialogsPath);
end;

procedure TSettings.SetDialogsPath(AValue: string);
begin
  if fDialogsPath=AValue then Exit;
  fDialogsPath:=AppendPathDelim(AValue);
end;

procedure TSettings.SetTemplatesPath(AValue: string);
begin
  if fTemplatesPath=AValue then Exit;
  fTemplatesPath:=AppendPathDelim(AValue);

end;

procedure TSettings.WriteSettings;
begin
  fIni.WriteBool('Common','ShowHint',Application.ShowHint);
  fIni.WriteString('Directories','Templates',fTemplatesPath);
  fIni.WriteString('Directories','Dialogs',fDialogsPath);
  fIni.UpdateFile;
end;

procedure TSettings.Store;
begin
  WriteSettings;
end;

end.

