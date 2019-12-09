

cl1 = 5;
Point(1) = {10, 0, 0, cl1};
Point(2) = {50, 0, 0, cl1};
Point(3) = {100, 0, 0, cl1};
Line(1) = {1, 2};
Line(2) = {2, 3};
Extrude {0, 10, 0} {
  Line{1, 2};
}
Extrude {{0, 1, 0}, {0, 0, 0}, Pi/2} {
  Surface{6, 10};
}
Extrude {{0, 1, 0}, {0, 0, 0}, Pi/2} {
  Surface{54, 32};
}
Extrude {{0, 1, 0}, {0, 0, 0}, Pi/2} {
  Surface{76, 98};
}
Extrude {{0, 1, 0}, {0, 0, 0}, Pi/2} {
  Surface{142, 120};
}
Physical Volume(181) = {4, 6, 7, 1};
Physical Volume(182) = {3, 5, 8, 2};
Physical Surface(183) = {97, 141, 163, 31};
Physical Surface(184) = {49, 71, 180, 115};
