unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm4 }

  TForm4 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form4: TForm4;

implementation

uses
  unit1;

{$R *.lfm}

{ TForm4 }

procedure TForm4.Button1Click(Sender: TObject);
var
  licfileread: TextFile;
begin

  //1st run
  if FileExists(ExeAppPath + '/bin/licread') = False then
  begin

    AssignFile(licfileread, ExeAppPath + '/bin/licread');
    ReWrite(licfileread);
    Writeln(licfileread, Form4.ComboBox1.Text);
    Writeln(licfileread, 'License has been agreed');
    Closefile(licfileread);{Closes file inifile}

  end;


  Form4.Close;
end;

end.
