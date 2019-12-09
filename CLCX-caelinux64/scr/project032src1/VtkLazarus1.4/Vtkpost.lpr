program Vtkpost;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uVtkpost {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Vtkpost';
  Application.CreateForm(TFrmVtkpost, FrmVtkpost);
  Application.Run;
end.
