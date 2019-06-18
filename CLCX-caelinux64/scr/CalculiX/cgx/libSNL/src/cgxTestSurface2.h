int degreeU = 5;
int degreeV = 5;

int sizeU = 6;
int sizeV = 6;

snlCtrlPoint* points = new snlCtrlPoint [ 36 ];

points [ 0 ].components ( 0.250853, -0.38073, 0.312823, 1 );
points [ 1 ].components ( 0.0744006, -0.323007, 0.232129, 1 );
points [ 2 ].components ( -0.0861526, -0.265111, 0.117185, 1 );
points [ 3 ].components ( -0.222683, -0.209981, -0.0286782, 1 );
points [ 4 ].components ( -0.328278, -0.160035, -0.198209, 1 );
points [ 5 ].components ( -0.4, -0.11752, -0.382607, 1 );
points [ 6 ].components ( 0.296824, -0.209462, 0.334447, 1 );
points [ 7 ].components ( 0.150616, -0.172132, 0.267564, 1 );
points [ 8 ].components ( 0.0144308, -0.134629, 0.179058, 1 );
points [ 9 ].components ( -0.107928, -0.0978181, 0.0707066, 1 );
points [ 10 ].components ( -0.213351, -0.0625621, -0.0551149, 1 );
points [ 11 ].components ( -0.298898, -0.02938, -0.194913, 1 );
points [ 12 ].components ( 0.328105, -0.0547851, 0.349116, 1 );
points [ 13 ].components ( 0.217671, -0.0309354, 0.298649, 1 );
points [ 14 ].components ( 0.112249, -0.00708579, 0.237469, 1 );
points [ 15 ].components ( 0.0123569, 0.016591, 0.165938, 1 );
points [ 16 ].components ( -0.080795, 0.040095, 0.0845183, 1 );
points [ 17 ].components ( -0.166515, 0.0630805, -0.00672455, 1 );
points [ 18 ].components ( 0.354029, 0.0902138, 0.361214, 1 );
points [ 19 ].components ( 0.280579, 0.104731, 0.32769, 1 );
points [ 20 ].components ( 0.208684, 0.119248, 0.29054, 1 );
points [ 21 ].components ( 0.138518, 0.133938, 0.249929, 1 );
points [ 22 ].components ( 0.069907, 0.148455, 0.205692, 1 );
points [ 23 ].components ( 0.0028516, 0.163145, 0.158159, 1 );
points [ 24 ].components ( 0.377533, 0.235558, 0.372289, 1 );
points [ 25 ].components ( 0.340894, 0.242298, 0.355511, 1 );
points [ 26 ].components ( 0.304256, 0.249211, 0.338765, 1 );
points [ 27 ].components ( 0.267617, 0.256124, 0.321987, 1 );
points [ 28 ].components ( 0.230806, 0.262864, 0.305044, 1 );
points [ 29 ].components ( 0.194167, 0.269777, 0.288298, 1 );
points [ 30 ].components ( 0.399827, 0.38073, 0.38264, 1 );
points [ 31 ].components ( 0.399827, 0.38073, 0.38264, 1 );
points [ 32 ].components ( 0.399827, 0.38073, 0.38264, 1 );
points [ 33 ].components ( 0.399827, 0.38073, 0.38264, 1 );
points [ 34 ].components ( 0.399827, 0.38073, 0.38264, 1 );
points [ 35 ].components ( 0.399827, 0.38073, 0.38264, 1 );

knot knotVectorU [ 12 ] = { 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1 };

knot knotVectorV [ 12 ] = { 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1 };

knot* vectU = new knot [ 12 ];

for ( int index = 0; index < 12; index ++ )
    vectU [ index ] = knotVectorU [ index ];

knot* vectV = new knot [ 12 ];

for ( int index = 0; index < 12; index ++ )
    vectV [ index ] = knotVectorV [ index ];

snlSurface* retSurface = new snlSurface ( degreeU, degreeV, sizeU, sizeV, points, vectU, vectV );

