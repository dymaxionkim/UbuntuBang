cl1 = 5;
Point(1) = {10, 0, 0, cl1};
Point(2) = {50, 0, 0, cl1};
Point(3) = {100, 0, 0, cl1};
Line(1) = {1, 2};
Line(2) = {2, 3};

Extrude {{0, 1, 0}, {0, 0, 0}, Pi/2} {
  Line{1, 2};
}
Extrude {{0, 1, 0}, {0, 0, 0}, Pi/2} {
  Line{3, 7};
}
Extrude {{0, 1, 0}, {0, 0, 0}, Pi/2} {
  Line{11, 15};
}
Extrude {{0, 1, 0}, {0, 0, 0}, Pi/2} {
  Line{19, 23};
}
Physical Surface(181) = {6, 14, 22, 30};
Physical Surface(182) = {10, 18, 26, 34};
Physical Line(183) = {4, 12, 20, 28};
