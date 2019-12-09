//Mesh 3D. set Order 2
//Save as gmsh.inp
cl1 = 5;
Point(1) = {10, 0, 0, cl1};
Point(2) = {50, 0, 0, cl1};
Point(3) = {100, 0, 0, cl1};
Line(1) = {1, 2};
Line(2) = {2, 3};
Extrude {0, 10, 0} {
  Line{1, 2};
}
Extrude {0, 0, 20} {
  Surface{6, 10};
}
Physical Surface(183) = {31};
Physical Surface(184) = {49};
Physical Volume(181) = {1};
Physical Volume(182) = {2};
