//Mesh 3D  and set order 2
// Save as UNV  (check Save Group of nodes)

lc = 5;
Point(1) = {0, 0, 0, lc};
Point(2) = {50, 0, 0, lc};
Point(3) = {50, 10, 0, lc};
Point(4) = {0, 10, 0, lc};
Point(5) = {100, 0, 0, lc};
Point(6) = {100, 10, 0, lc};
Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};
Line(5) = {2, 5};
Line(6) = {5, 6};
Line(7) = {6, 3};
Line Loop(8) = {4, 1, 2, 3};
Plane Surface(9) = {8};
Line Loop(10) = {2, -7, -6, -5};
Plane Surface(11) = {10};
Extrude {0, 0, 20} {
  Surface{9, 11};
}
Physical Surface(56) = {20};
Physical Surface(57) = {46};
Physical Volume(58) = {1};
Physical Volume(59) = {2};
