unit uChartCursors;

interface

uses
  LCLType;

const

  CRS_HAND  = $1000;
  CRS_ZIN   = $1001;
  CRS_ZIN_H = $1002;
  CRS_ZIN_V = $1003;

  function LoadCursor(const ResName: string): HCURSOR;


implementation

{$R cursors.res}

uses Graphics;

function LoadCursor(const ResName: string): HCURSOR;
var
  CursorImage: TCursorImage;
begin
  CursorImage:=TCursorImage.Create;
  try
    CursorImage.LoadFromResourceName(hInstance,ResName);
    Result:=CursorImage.ReleaseHandle;
  finally
    CursorImage.Free;
  end;
end;

end.
