unit uRender;

// Рендер графика функции
// автор: Василий Макаров
// используется библиотека графики AGG,
// так как стандартная графика
// рисует линии с зазубринами
// /usr/share/lazarus/1.6/components/aggpas/lazarus

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Graphics, ValEdit, FPimage, agg_fpimage, Agg_LCL;

function ExtendedToStr(const x: extended): string;
procedure RenderGraph(bmp: TBitmap; data: TValueListStrings);

implementation

uses Math, fileutil;

const
  M_LEFT = 100;
  M_TOP = 14;
  M_RIGHT = 40;
  M_BOTTOM = 22;
  M_TXT_LEFT = 8;

  sErrParse = 'This string "%s" is not number';
  sErrEmptyData = 'Data is empty';
  sErrEmptyGraph = 'Amplitude of graph is zero';

var
  fs: TFormatSettings;


function ExtendedToStr(const x: extended): string;
var
  fs: TFormatSettings;
begin
  fs.DecimalSeparator:='.';
  Result:=FormatFloat('0.0000000E+00',x,fs);
end;

procedure DrawShape0(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(3.2813,7.2813);
  a.AggCubicCurveTo(3.2813,7.2813,4.0781,7.2813,4.4453,6.5234);
  a.AggCubicCurveTo(4.4453,6.5234,4.8125,5.7656,4.8125,4.0625);
  a.AggCubicCurveTo(4.8125,4.0625,4.8125,3.2344,4.7344,2.6406);
  a.AggLineTo(4.7344,2.6406);
  a.AggLineTo(2.2500,6.7188);
  a.AggCubicCurveTo(2.2500,6.7188,2.6094,7.2813,3.2813,7.2813);
  a.AggMoveTo(3.2813,8.1094);
  a.AggCubicCurveTo(3.2813,8.1094,2.0625,8.1094,1.4219,7.0625);
  a.AggCubicCurveTo(1.4219,7.0625,0.7969,6.0313,0.7969,4.0625);
  a.AggCubicCurveTo(0.7969,4.0625,0.7969,0.0313,3.2813,0.0313);
  a.AggCubicCurveTo(3.2813,0.0313,4.5313,0.0313,5.1719,1.0703);
  a.AggCubicCurveTo(5.1719,1.0703,5.8125,2.1094,5.8125,4.0625);
  a.AggCubicCurveTo(5.8125,4.0625,5.8125,8.1094,3.2813,8.1094);
  a.AggMoveTo(1.8906,5.6406);
  a.AggLineTo(1.8906,5.6406);
  a.AggLineTo(4.3594,1.4688);
  a.AggCubicCurveTo(4.3594,1.4688,4.0000,0.8594,3.2813,0.8594);
  a.AggCubicCurveTo(3.2813,0.8594,2.5156,0.8594,2.1484,1.6094);
  a.AggCubicCurveTo(2.1484,1.6094,1.7813,2.3594,1.7813,4.0625);
  a.AggCubicCurveTo(1.7813,4.0625,1.7813,4.9844,1.8906,5.6406);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape1(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(3.9844,8.0000);
  a.AggLineRel(-0.9844,0.0000);
  a.AggLineRel(0.0000,-3.0938);
  a.AggCubicCurveRel(0.0000,0.0000,0.0000,-0.6875,0.0459,-1.9891);
  a.AggCubicCurveRel(0.0459,-1.9891,-0.1719,-1.8125,-0.6313,-1.4909);
  a.AggLineRel(-0.8218,0.5847);
  a.AggLineRel(-0.5469,-0.5781);
  a.AggLineRel(2.1406,-1.4219);
  a.AggLineRel(0.8438,0.0000);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape2(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(6.0000,8.0000);
  a.AggLineTo(6.0000,8.0000);
  a.AggLineTo(1.0000,8.0000);
  a.AggLineTo(1.0000,7.2344);
  a.AggLineTo(3.0781,5.1406);
  a.AggCubicCurveTo(3.0781,5.1406,4.2500,3.9844,4.6172,3.4063);
  a.AggCubicCurveTo(4.6172,3.4063,4.9844,2.8281,4.9844,2.1406);
  a.AggCubicCurveTo(4.9844,2.1406,4.9844,1.5313,4.6094,1.1719);
  a.AggCubicCurveTo(4.6094,1.1719,4.2344,0.8125,3.5781,0.8125);
  a.AggCubicCurveTo(3.5781,0.8125,2.6250,0.8125,1.6875,1.5625);
  a.AggLineTo(1.6875,1.5625);
  a.AggLineTo(1.0781,0.9219);
  a.AggCubicCurveTo(1.0781,0.9219,2.2031,0.0000,3.5469,0.0000);
  a.AggCubicCurveTo(3.5469,0.0000,4.6875,0.0000,5.3438,0.5781);
  a.AggCubicCurveTo(5.3438,0.5781,6.0000,1.1563,6.0000,2.1406);
  a.AggCubicCurveTo(6.0000,2.1406,6.0000,2.7813,5.6094,3.4609);
  a.AggCubicCurveTo(5.6094,3.4609,5.2188,4.1406,3.6875,5.5156);
  a.AggLineTo(3.6875,5.5156);
  a.AggLineTo(1.8281,7.1406);
  a.AggLineTo(1.8281,7.1875);
  a.AggLineTo(6.0000,7.1875);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape3(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(4.0781,3.3281);
  a.AggLineTo(4.0781,3.3281);
  a.AggCubicCurveTo(4.0781,3.3594,6.0000,3.6406,6.0000,5.4219);
  a.AggCubicCurveTo(6.0000,5.4219,6.0000,6.6250,5.2500,7.3125);
  a.AggCubicCurveTo(5.2500,7.3125,4.5000,8.0000,3.0938,8.0000);
  a.AggCubicCurveTo(3.0938,8.0000,1.7813,8.0000,1.0000,7.5781);
  a.AggLineTo(1.0000,7.5781);
  a.AggCubicCurveTo(1.0000,6.6563,2.0000,7.1875,3.0781,7.1875);
  a.AggCubicCurveTo(3.0781,7.1875,4.9844,7.1875,4.9844,5.4063);
  a.AggCubicCurveTo(4.9844,5.4063,4.9844,3.8125,2.7656,3.8125);
  a.AggLineTo(2.7656,3.8125);
  a.AggLineTo(2.0000,3.8125);
  a.AggLineTo(2.0000,3.0000);
  a.AggCubicCurveTo(2.8438,3.0000,3.8594,3.0000,4.4219,2.6641);
  a.AggCubicCurveTo(4.4219,2.6641,4.9844,2.3281,4.9844,1.7813);
  a.AggCubicCurveTo(4.9844,1.7813,4.9844,1.3281,4.5938,1.0703);
  a.AggCubicCurveTo(4.5938,1.0703,4.2031,0.8125,3.5469,0.8125);
  a.AggCubicCurveTo(3.5469,0.8125,2.5156,0.8125,1.5625,1.4844);
  a.AggLineTo(1.5625,1.4844);
  a.AggLineTo(1.0313,0.8125);
  a.AggCubicCurveTo(1.0313,0.8125,2.0938,0.0000,3.5156,0.0000);
  a.AggCubicCurveTo(3.5156,0.0000,4.6875,0.0000,5.3438,0.4609);
  a.AggCubicCurveTo(5.3438,0.4609,6.0000,0.9219,6.0000,1.6875);
  a.AggCubicCurveTo(6.0000,1.6875,6.0000,2.3438,5.5000,2.7734);
  a.AggCubicCurveTo(5.5000,2.7734,5.0000,3.2031,4.0781,3.3281);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape4(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(6.1406,5.8125);
  a.AggLineTo(6.1406,5.8125);
  a.AggLineTo(5.0000,5.8125);
  a.AggLineTo(5.0000,8.0000);
  a.AggLineTo(4.0156,8.0000);
  a.AggLineTo(4.0156,5.8125);
  a.AggLineTo(1.0000,5.8125);
  a.AggLineTo(1.0000,5.0469);
  a.AggLineTo(4.2031,0.0000);
  a.AggLineTo(4.2031,0.0000);
  a.AggLineTo(5.0000,0.0000);
  a.AggLineTo(5.0000,5.0000);
  a.AggMoveTo(4.0156,5.0000);
  a.AggLineTo(4.0156,5.0000);
  a.AggCubicCurveTo(4.0156,3.1719,4.0156,2.2813,4.0938,0.9688);
  a.AggLineTo(4.0938,0.9688);
  a.AggCubicCurveTo(4.0469,0.9688,3.8750,1.4844,3.6250,1.8906);
  a.AggLineTo(3.6250,1.8906);
  a.AggLineTo(1.7188,5.0000);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape5(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(1.0000,7.5781);
  a.AggLineTo(1.0000,7.5781);
  a.AggCubicCurveTo(1.0000,6.6563,1.8281,7.1875,3.0625,7.1875);
  a.AggCubicCurveTo(3.0625,7.1875,4.9844,7.1875,4.9844,5.4375);
  a.AggCubicCurveTo(4.9844,5.4375,4.9844,3.8125,2.8906,3.8125);
  a.AggCubicCurveTo(2.8906,3.8125,2.3438,3.8125,1.4844,3.9688);
  a.AggLineTo(1.4844,3.9688);
  a.AggLineTo(1.0000,3.6719);
  a.AggLineTo(1.0000,3.6719);
  a.AggLineTo(1.2969,0.0000);
  a.AggLineTo(1.2969,0.0000);
  a.AggLineTo(6.0000,0.0000);
  a.AggLineTo(6.0000,0.8125);
  a.AggLineTo(2.1406,0.8125);
  a.AggLineTo(1.9375,3.1094);
  a.AggCubicCurveTo(1.9375,3.1094,2.5781,3.0000,3.1875,3.0000);
  a.AggCubicCurveTo(3.1875,3.0000,4.4531,3.0000,5.2266,3.6250);
  a.AggCubicCurveTo(5.2266,3.6250,6.0000,4.2500,6.0000,5.3281);
  a.AggCubicCurveTo(6.0000,5.3281,6.0000,6.6094,5.2188,7.3047);
  a.AggCubicCurveTo(5.2188,7.3047,4.4375,8.0000,3.0156,8.0000);
  a.AggCubicCurveTo(3.0156,8.0000,1.7500,8.0000,1.0000,7.5781);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape6(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(6.0000,0.1094);
  a.AggLineTo(6.0000,0.1094);
  a.AggCubicCurveTo(6.0000,0.9375,5.5469,0.8125,4.9531,0.8125);
  a.AggCubicCurveTo(4.9531,0.8125,3.5313,0.8125,2.8125,1.5547);
  a.AggCubicCurveTo(2.8125,1.5547,2.0938,2.2969,2.0313,3.9063);
  a.AggLineTo(2.0313,3.9063);
  a.AggCubicCurveTo(2.0938,3.9063,2.6094,3.0000,3.7500,3.0000);
  a.AggCubicCurveTo(3.7500,3.0000,4.8125,3.0000,5.4063,3.6328);
  a.AggCubicCurveTo(5.4063,3.6328,6.0000,4.2656,6.0000,5.3750);
  a.AggCubicCurveTo(6.0000,5.3750,6.0000,6.5938,5.3438,7.2969);
  a.AggCubicCurveTo(5.3438,7.2969,4.6875,8.0000,3.5938,8.0000);
  a.AggCubicCurveTo(3.5938,8.0000,2.4063,8.0000,1.7031,7.1016);
  a.AggCubicCurveTo(1.7031,7.1016,1.0000,6.2031,1.0000,4.5625);
  a.AggCubicCurveTo(1.0000,4.5625,1.0000,0.0000,4.9375,0.0000);
  a.AggCubicCurveTo(4.9375,0.0000,5.5781,0.0000,6.0000,0.1094);
  a.AggMoveTo(5.0156,5.3750);
  a.AggCubicCurveTo(5.0156,5.3750,5.0156,4.5938,4.6484,4.1797);
  a.AggCubicCurveTo(4.6484,4.1797,4.2813,3.7656,3.6094,3.7656);
  a.AggCubicCurveTo(3.6094,3.7656,2.9375,3.7656,2.4766,4.2031);
  a.AggCubicCurveTo(2.4766,4.2031,2.0156,4.6406,2.0156,5.1875);
  a.AggCubicCurveTo(2.0156,5.1875,2.0156,6.0000,2.4453,6.5938);
  a.AggCubicCurveTo(2.4453,6.5938,2.8750,7.1875,3.5781,7.1875);
  a.AggCubicCurveTo(3.5781,7.1875,4.2500,7.1875,4.6328,6.7188);
  a.AggCubicCurveTo(4.6328,6.7188,5.0156,6.2500,5.0156,5.3750);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape7(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(2.0000,8.0000);
  a.AggLineTo(2.0000,8.0000);
  a.AggLineTo(5.1250,0.8125);
  a.AggLineTo(5.1250,0.8125);
  a.AggLineTo(1.0000,0.8125);
  a.AggLineTo(1.0000,0.0000);
  a.AggLineTo(6.0000,0.0000);
  a.AggLineTo(6.0000,0.7031);
  a.AggLineTo(2.9219,8.0000);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape8(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(4.2031,3.7813);
  a.AggCubicCurveTo(4.2031,3.7813,6.0000,4.6250,6.0000,5.8750);
  a.AggCubicCurveTo(6.0000,5.8750,6.0000,6.8281,5.3125,7.4141);
  a.AggCubicCurveTo(5.3125,7.4141,4.6250,8.0000,3.5000,8.0000);
  a.AggCubicCurveTo(3.5000,8.0000,2.3125,8.0000,1.6563,7.4453);
  a.AggCubicCurveTo(1.6563,7.4453,1.0000,6.8906,1.0000,5.9063);
  a.AggCubicCurveTo(1.0000,5.9063,1.0000,4.5313,2.3594,3.8125);
  a.AggCubicCurveTo(2.3594,3.8125,1.0000,3.0313,1.0000,1.8594);
  a.AggCubicCurveTo(1.0000,1.8594,1.0000,1.0000,1.7109,0.5000);
  a.AggCubicCurveTo(1.7109,0.5000,2.4219,0.0000,3.5000,0.0000);
  a.AggCubicCurveTo(3.5000,0.0000,4.6094,0.0000,5.3047,0.5078);
  a.AggCubicCurveTo(5.3047,0.5078,6.0000,1.0156,6.0000,1.8750);
  a.AggCubicCurveTo(6.0000,1.8750,6.0000,3.0938,4.2031,3.7813);
  a.AggMoveTo(3.3438,3.3750);
  a.AggCubicCurveTo(3.3438,3.3750,5.0156,2.8281,5.0156,1.9063);
  a.AggCubicCurveTo(5.0156,1.9063,5.0156,1.3750,4.6094,1.0938);
  a.AggCubicCurveTo(4.6094,1.0938,4.2031,0.8125,3.4844,0.8125);
  a.AggCubicCurveTo(3.4844,0.8125,2.7969,0.8125,2.3906,1.0938);
  a.AggCubicCurveTo(2.3906,1.0938,1.9844,1.3750,1.9844,1.9063);
  a.AggCubicCurveTo(1.9844,1.9063,1.9844,2.3750,2.2734,2.7109);
  a.AggCubicCurveTo(2.2734,2.7109,2.5625,3.0469,3.3438,3.3750);
  a.AggMoveTo(3.2344,3.8125);
  a.AggCubicCurveTo(3.2344,3.8125,1.9844,4.5313,1.9844,5.7500);
  a.AggCubicCurveTo(1.9844,5.7500,1.9844,7.1875,3.4688,7.1875);
  a.AggCubicCurveTo(3.4688,7.1875,4.2031,7.1875,4.6094,6.7969);
  a.AggCubicCurveTo(4.6094,6.7969,5.0156,6.4063,5.0156,5.7031);
  a.AggCubicCurveTo(5.0156,5.7031,5.0156,5.1719,4.6484,4.7578);
  a.AggCubicCurveTo(4.6484,4.7578,4.2813,4.3438,3.4063,3.8906);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShape9(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(1.0000,7.9063);
  a.AggLineTo(1.0000,7.9063);
  a.AggCubicCurveTo(1.0000,7.0625,1.4219,7.2344,2.0469,7.2344);
  a.AggCubicCurveTo(2.0469,7.2344,3.4688,7.2344,4.1875,6.4844);
  a.AggCubicCurveTo(4.1875,6.4844,4.9063,5.7344,4.9688,4.0938);
  a.AggLineTo(4.9688,4.0938);
  a.AggCubicCurveTo(4.9063,4.0938,4.3906,5.0000,3.2344,5.0000);
  a.AggCubicCurveTo(3.2344,5.0000,2.1875,5.0000,1.5938,4.3672);
  a.AggCubicCurveTo(1.5938,4.3672,1.0000,3.7344,1.0000,2.6250);
  a.AggCubicCurveTo(1.0000,2.6250,1.0000,1.4063,1.6563,0.7031);
  a.AggCubicCurveTo(1.6563,0.7031,2.3125,0.0000,3.4063,0.0000);
  a.AggCubicCurveTo(3.4063,0.0000,4.6094,0.0000,5.3047,0.9141);
  a.AggCubicCurveTo(5.3047,0.9141,6.0000,1.8281,6.0000,3.4375);
  a.AggCubicCurveTo(6.0000,3.4375,6.0000,8.0000,2.0625,8.0000);
  a.AggCubicCurveTo(2.0625,8.0000,1.4063,8.0000,1.0000,7.9063);
  a.AggMoveTo(1.9844,2.6250);
  a.AggCubicCurveTo(1.9844,2.6250,1.9844,3.4063,2.3516,3.8203);
  a.AggCubicCurveTo(2.3516,3.8203,2.7188,4.2344,3.3906,4.2344);
  a.AggCubicCurveTo(3.3906,4.2344,4.0625,4.2344,4.5234,3.7969);
  a.AggCubicCurveTo(4.5234,3.7969,4.9844,3.3594,4.9844,2.8125);
  a.AggCubicCurveTo(4.9844,2.8125,4.9844,2.0000,4.5547,1.4063);
  a.AggCubicCurveTo(4.5547,1.4063,4.1250,0.8125,3.4219,0.8125);
  a.AggCubicCurveTo(3.4219,0.8125,2.7344,0.8125,2.3594,1.2813);
  a.AggCubicCurveTo(2.3594,1.2813,1.9844,1.7500,1.9844,2.6250);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShapeE(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(6.0000,8.0000);
  a.AggLineRel(-5.0000,0.0000);
  a.AggLineRel(0.0000,-8.0000);
  a.AggLineRel(5.0000,0.0000);
  a.AggLineRel(0.0000,0.8750);
  a.AggLineRel(-4.0000,0.0000);
  a.AggLineRel(0.0000,2.1250);
  a.AggLineRel(4.0000,0.0000);
  a.AggLineRel(0.0000,0.8750);
  a.AggLineRel(-4.0000,0.0000);
  a.AggLineRel(0.0000,3.2500);
  a.AggLineRel(4.0000,0.0000);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShapeDot(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(3.5000,6.0000);
  a.AggCubicCurveTo(3.5000,6.0000,4.0000,6.0000,4.0000,7.0000);
  a.AggCubicCurveTo(4.0000,7.0000,4.0000,8.0000,3.5000,8.0000);
  a.AggCubicCurveTo(3.5000,8.0000,3.0000,8.0000,3.0000,7.0000);
  a.AggCubicCurveTo(3.0000,7.0000,3.0000,6.0000,3.5000,6.0000);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShapePlus(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(3.0000,4.8125);
  a.AggLineRel(-2.0781,0.0000);
  a.AggLineRel(0.0000,-0.8125);
  a.AggLineRel(2.0781,0.0000);
  a.AggLineRel(0.0000,-2.0938);
  a.AggLineRel(0.8125,0.0000);
  a.AggLineRel(0.0000,2.0938);
  a.AggLineRel(2.0781,0.0000);
  a.AggLineRel(0.0000,0.8125);
  a.AggLineRel(-2.0781,0.0000);
  a.AggLineRel(0.0000,2.0625);
  a.AggLineRel(-0.8125,0.0000);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawShapeMinus(a: TAggLCLCanvas);
begin
  a.AggResetPath;
  a.AggMoveTo(1.0000,4.9063);
  a.AggLineRel(0.0000,-0.9063);
  a.AggLineRel(4.0000,0.0000);
  a.AggLineRel(0.0000,0.9063);
  a.AggClosePolygon;
  a.AggDrawPath(AGG_FillOnly);
end;

procedure DrawAggString(a: TAggLCLCanvas; const x, y: double; const s: string);
var
  i: integer;
begin
  a.Brush.Color:=clBlack;
  a.AggTranslate(x,Round(y));
  for i:=1 to length(s) do begin
    case s[i] of
      '0': DrawShape0(a);
      '1': DrawShape1(a);
      '2': DrawShape2(a);
      '3': DrawShape3(a);
      '4': DrawShape4(a);
      '5': DrawShape5(a);
      '6': DrawShape6(a);
      '7': DrawShape7(a);
      '8': DrawShape8(a);
      '9': DrawShape9(a);
      'E': DrawShapeE(a);
      '.': DrawShapeDot(a);
      '+': DrawShapePlus(a);
      '-': DrawShapeMinus(a);
      end;
    a.AggTranslate(6,0);
    end;
  a.AggResetTransformations;
end;

procedure RenderGraph(bmp: TBitmap; data: TValueListStrings);
const
  G: double = 0.5; // коррекция пикелей в AGG
var
  a: TAggLCLCanvas;
  minX,maxX,minY,maxY,cX,cY,kX,kY,
  XRange,YRange: double;
  w,h: integer;

  procedure DrawXAxis(const _pos: double);
  var
    f,y: double;
  begin
    f:=minY+YRange*_pos;
    y:=h-(M_BOTTOM+kY*f-cy)+G;
    a.AggLine(M_LEFT+G,y,w-M_RIGHT+G,y);
    DrawAggString(a,M_TXT_LEFT,y-4,ExtendedToStr(f));
  end;

  procedure DrawYAxis(const _pos: double; const time: double);
  var
    x: double;
    s: string;
  begin
    x:=M_LEFT+G+_pos*kX;
    a.AggLine(x,M_TOP+G,x,h-M_BOTTOM+G);
    s:=FormatFloat('0.#',time,fs);
    DrawAggString(a,Round(x-bmp.Canvas.TextWidth(s)/2),h-M_BOTTOM+7,s);
  end;

type
  TPointDouble = record X,Y: double; end;
var
  z: array of TPointDouble;
  i,e: integer;
  x,y,dx: double;
begin
  if data.Count=0 then begin
     MessageDlg(sErrEmptyData,mtWarning,[mbOK],0);
     exit;
  end;
  w:=bmp.Width;
  h:=bmp.Height;
  SetLength(z,data.Count);
  try

  // pass1 Загружаем данные в массив
  for i:=0 to data.Count-1 do begin
    Val(data.Names[i],z[i].X,e);
    if e<>0 then
       raise Exception.CreateFmt(sErrParse,[data.ValueFromIndex[i]]);
    Val(data.ValueFromIndex[i],z[i].Y,e);
    if e<>0 then
       raise Exception.CreateFmt(sErrParse,[data.ValueFromIndex[i]]);
  end;

  // pass2 Ишем минимум и максимум для вычисления масштаба
  minX:=z[0].X;
  maxX:=z[data.Count-1].X;
  XRange:=maxX-minX;

  minY:=z[0].Y; maxY:=z[0].Y;
  for i:=0 to data.Count-1 do begin
    minY:=Min(minY,z[i].Y);
    maxY:=Max(maxY,z[i].Y);
  end;
  YRange:=maxY-minY;

  // Подготавливаем Canvas
  a:=TAggLCLCanvas.Create;
  try
    a.Image.PixelFormat:=afpimRGBA32;
    a.Image.SetSize(bmp.Width,bmp.Height);
    a.Font.Size:=8;
    a.Font.Color:=clBlack;
    a.Brush.Color:=clWhite;
    a.Pen.Color:=clGray;
    a.FillRect(0,0,w,h);
    a.Frame(0,0,w-1,h-1);

    if YRange=0 then begin
      // TODO: просто нарисовать нулевую линнию и выйти!
     MessageDlg(sErrEmptyGraph,mtWarning,[mbOK],0);
     exit;
     end;
    kX:=(w-(M_LEFT+M_RIGHT))/XRange;
    kY:=(h-(M_TOP+M_BOTTOM))/YRange;

    cX:=minX*kX;
    cY:=minY*kY;
  // Рисуем оси
    a.Pen.Color:=clSilver;
    for i:=1 to 10 do
    DrawXAxis(i*0.1);

    a.Font.AggAlignX:=2;
    a.Pen.Color:=clSilver;
    dx:=XRange/10;
    for i:=1 to 10 do
    DrawYAxis(i*dx,minX+i*0.1*XRange);

    a.Pen.Color:=clGray;
    DrawYAxis(0,minX);
    a.Font.AggAlignX:=0;
    DrawXAxis(0);
  // Рисуем график
    a.Pen.AggLineWidth:=1.5;
    a.Pen.Color:=clGreen;
    a.Path.m_path.remove_all;
    i:=0;
    x:=M_LEFT+G;
    y:=M_BOTTOM+kY*z[i].Y-cy;
    a.Path.m_path.move_to(x,h-y+G);
    inc(i);
    while i<data.Count do begin
      x:=M_LEFT+kX*z[i].X-cx;
      y:=M_BOTTOM+kY*z[i].Y-cy;
      a.Path.m_path.line_to(x+G,h-y+G);
      inc(i);
    end;
    a.AggDrawPath(AGG_StrokeOnly);

    bmp.LoadFromIntfImage(a.Image.IntfImg);
  finally
    a.Free;
  end;
  finally
    SetLength(z,0);
  end;

end;

initialization
  fs.DecimalSeparator:='.';

end.

