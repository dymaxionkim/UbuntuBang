int degreeU = 5;
int degreeV = 1;

int sizeU = 21;
int sizeV = 2;

snlCtrlPoint* points = new snlCtrlPoint [ 42 ];
points [ 0 ].components ( 0.360577, -0.238284, 0.0679317, 1 );
points [ 1 ].components ( 0.360577, -0.231283, 0.15414, 1 );
points [ 2 ].components ( 0.360139, -0.238809, 0.0678482, 1 );
points [ 3 ].components ( 0.360139, -0.231809, 0.15404, 1 );
points [ 4 ].components ( 0.359614, -0.239246, 0.0676813, 1 );
points [ 5 ].components ( 0.359614, -0.232333, 0.153956, 1 );
points [ 6 ].components ( 0.359177, -0.239771, 0.0675812, 1 );
points [ 7 ].components ( 0.359177, -0.232771, 0.153873, 1 );
points [ 8 ].components ( 0.358739, -0.240297, 0.0674977, 1 );
points [ 9 ].components ( 0.358739, -0.233296, 0.153789, 1 );
points [ 10 ].components ( 0.358214, -0.240734, 0.0674143, 1 );
points [ 11 ].components ( 0.358214, -0.233821, 0.153606, 1 );
points [ 12 ].components ( 0.263968, -0.340231, 0.0464172, 1 );
points [ 13 ].components ( 0.263968, -0.333318, 0.132609, 1 );
points [ 14 ].components ( 0.129906, -0.399999, 0.0118505, 1 );
points [ 15 ].components ( 0.129906, -0.393086, 0.098042, 1 );
points [ 16 ].components ( -0.0218332, -0.394049, -0.0313788, 1 );
points [ 17 ].components ( -0.0218332, -0.387048, 0.0548962, 1 );
points [ 18 ].components ( -0.149157, -0.325179, -0.0712031, 1 );
points [ 19 ].components ( -0.149157, -0.318266, 0.0149884, 1 );
points [ 20 ].components ( -0.233778, -0.21947, -0.10113, 1 );
points [ 21 ].components ( -0.233778, -0.212557, -0.0148382, 1 );
points [ 22 ].components ( -0.318748, -0.11341, -0.13114, 1 );
points [ 23 ].components ( -0.318748, -0.106497, -0.0449484, 1 );
points [ 24 ].components ( -0.360577, 0.0298411, -0.151186, 1 );
points [ 25 ].components ( -0.360577, 0.0368408, -0.0649941, 1 );
points [ 26 ].components ( -0.33835, 0.186742, -0.154157, 1 );
points [ 27 ].components ( -0.33835, 0.193743, -0.0679651, 1 );
points [ 28 ].components ( -0.256792, 0.313104, -0.138667, 1 );
points [ 29 ].components ( -0.256792, 0.320105, -0.052476, 1 );
points [ 30 ].components ( -0.144607, 0.391074, -0.111712, 1 );
points [ 31 ].components ( -0.144607, 0.398074, -0.0254368, 1 );
points [ 32 ].components ( -0.144082, 0.391512, -0.111545, 1 );
points [ 33 ].components ( -0.144082, 0.398425, -0.0253534, 1 );
points [ 34 ].components ( -0.143557, 0.391861, -0.111461, 1 );
points [ 35 ].components ( -0.143557, 0.398862, -0.0251698, 1 );
points [ 36 ].components ( -0.142944, 0.392299, -0.111278, 1 );
points [ 37 ].components ( -0.142944, 0.399212, -0.0250863, 1 );
points [ 38 ].components ( -0.142419, 0.392649, -0.111194, 1 );
points [ 39 ].components ( -0.142419, 0.39965, -0.0249027, 1 );
points [ 40 ].components ( -0.141894, 0.392999, -0.111011, 1 );
points [ 41 ].components ( -0.141894, 0.4, -0.0248193, 1 );

knot knotVectorU [ 27 ] = { 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4 };

knot knotVectorV [ 4 ] = { 0, 0, 1, 1 };

knot* vectU = new knot [ 27 ];

for ( int index = 0; index < 27; index ++ )
    vectU [ index ] = knotVectorU [ index ];

knot* vectV = new knot [ 4 ];

for ( int index = 0; index < 4; index ++ )
    vectV [ index ] = knotVectorV [ index ];

snlSurface* retSurface = new snlSurface ( degreeU, degreeV, sizeU, sizeV, points, vectU, vectV );

