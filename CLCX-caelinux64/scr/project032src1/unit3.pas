unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { TForm3 }

  TForm3 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  unit1;

{$R *.lfm}

{ TForm3 }

procedure TForm3.FormActivate(Sender: TObject);
begin
  Edit1.Text := Path2CCX;
  Edit2.Text := Path2CGX;
  Edit3.Text := Terminal;
  Edit4.Text := TextEditor;
  Edit5.Text := PyPath;
end;



procedure TForm3.Button1Click(Sender: TObject);
begin
  Path2CCX := Edit1.Text;
  Path2CGX := Edit2.Text;
  Terminal := Edit3.Text;
  TextEditor := Edit4.Text;
  PyPath := Edit5.Text;
  AssignFile(inifile, PathToIni);
  ReWrite(inifile);
  Writeln(inifile, '**line#2 - full path to CCX or empty');
  Writeln(inifile, Edit1.Text);
  Writeln(inifile, '**line#4 - full path to CGX or empty');
  Writeln(inifile, Edit2.Text);
  Writeln(inifile, '**line#6 - name of terminal emulator (xfce4-terminal, etc) or empty');
  Writeln(inifile, Edit3.Text);
  Writeln(inifile, '**line#8 - name of text editor (scite, leafpad, etc) or empty');
  Writeln(inifile, Edit4.Text);
  Writeln(inifile, '**line#10 - full path to python (/usr/bin/...) or empty');
  Writeln(inifile, Edit5.Text);
  Closefile(inifile);{Closes file inifile}




  Form3.Close;
end;

end.
