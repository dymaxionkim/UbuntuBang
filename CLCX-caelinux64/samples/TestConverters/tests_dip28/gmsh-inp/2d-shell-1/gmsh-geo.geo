//
//
cl1 = 5;
Point(1) = {10, 0, 0, cl1};
Point(2) = {50, 0, 0, cl1};
Point(3) = {100, 0, 0, cl1};
Line(1) = {1, 2};
Line(2) = {2, 3};
Extrude {0, 10, 0} {
  Line{1, 2};
}
Physical Surface(181) = {6};
Physical Surface(182) = {10};
Physical Line(183) = {4};
Physical Line(184) = {7};
