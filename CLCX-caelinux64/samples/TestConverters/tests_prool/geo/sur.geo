L=10;
b=2;
h=1;

//
k1=L/10;
k2=b/3;
k3=h/4;



Point(1) = {0, 0, 0, k1};
Point(2) = {L, 0, 0, k1};
Point(3) = {L, b, 0, k1};
Point(4) = {0, b, 0, k1};

Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 1};


Line Loop(5) = {3, 4, 1, 2};
Plane Surface(6) = {5};