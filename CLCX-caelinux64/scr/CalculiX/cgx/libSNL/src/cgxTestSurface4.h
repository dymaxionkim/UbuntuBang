/*
NURBS:N01S patch0
PNT P1 0.073184 -0.302541 -0.167307
PNT P2 0.058620 -0.206606 -0.169105
PNT P3 0.076873 -0.111364 -0.162326
PNT P4 0.119059 -0.028593 -0.134268
PNT P5 0.177067 0.033592 -0.086871
PNT P6 0.246760 0.075340 -0.032956
PNT P7 0.325631 0.097064 0.019753
PNT P8 0.304595 0.154435 0.084254
PNT P9 0.289219 0.227518 0.132393
PNT P10 0.280606 0.311274 0.160780
PNT P11 0.279307 0.399856 0.167554
PNT P12 0.094945 0.377070 0.111060
PNT P13 -0.075838 0.295156 0.069101
PNT P14 -0.210752 0.158021 0.044476
PNT P15 -0.294491 -0.016763 0.035944
PNT P16 -0.323088 -0.208738 0.040243
PNT P17 -0.294497 -0.400000 0.055801
PNT P18 -0.219096 -0.373969 0.060507
PNT P19 -0.144088 -0.350306 0.046402
PNT P20 -0.073652 -0.330371 0.014364
PNT P21 -0.011768 -0.315250 -0.033873
PNT P22 0.038096 -0.305773 -0.095589
PNT P23 0.073184 -0.302541 -0.167307

    evalPts [ 0 ].components ( 0.073184, -0.302541, -0.167307 );
    evalPts [ 1 ].components ( 0.058620, -0.206606, -0.169105 );
    evalPts [ 2 ].components ( 0.076873, -0.111364, -0.162326 );
    evalPts [ 3 ].components ( 0.119059, -0.028593, -0.134268 );
    evalPts [ 4 ].components ( 0.177067, 0.033592, -0.086871 );
    evalPts [ 5 ].components ( 0.246760, 0.075340, -0.032956 );
    evalPts [ 6 ].components ( 0.325631, 0.097064, 0.019753 );
    evalPts [ 7 ].components ( 0.304595, 0.154435, 0.084254 );
    evalPts [ 8 ].components ( 0.289219, 0.227518, 0.132393 );
    evalPts [ 9 ].components ( 0.280606, 0.311274, 0.160780 );
    evalPts [ 10 ].components ( 0.279307, 0.399856, 0.167554 );
    evalPts [ 11 ].components ( 0.094945, 0.377070, 0.111060 );
    evalPts [ 12 ].components ( -0.075838, 0.295156, 0.069101 );
    evalPts [ 13 ].components ( -0.210752, 0.158021, 0.044476 );
    evalPts [ 14 ].components ( -0.294491, -0.016763, 0.035944 );
    evalPts [ 15 ].components ( -0.323088, -0.208738, 0.040243 );
    evalPts [ 16 ].components ( -0.294497, -0.400000, 0.055801 );
    evalPts [ 17 ].components ( -0.219096, -0.373969, 0.060507 );
    evalPts [ 18 ].components ( -0.144088, -0.350306, 0.046402 );
    evalPts [ 19 ].components ( -0.073652, -0.330371, 0.014364 );
    evalPts [ 20 ].components ( -0.011768, -0.315250, -0.033873 );
    evalPts [ 21 ].components ( 0.038096, -0.305773, -0.095589 );
    evalPts [ 22 ].components ( 0.073184, -0.302541, -0.167307 );
    
*/

int degreeU = 5;
int degreeV = 5;

int sizeU = 26;
int sizeV = 6;

snlCtrlPoint* points = new snlCtrlPoint [ 156 ];

points [ 0 ].components ( 0.325631, 0.0970643, 0.0197533, 1 );
points [ 1 ].components ( 0.306886, 0.135524, 0.0767734, 1 );
points [ 2 ].components ( 0.291552, 0.189855, 0.124246, 1 );
points [ 3 ].components ( 0.28139, 0.256468, 0.156745, 1 );
points [ 4 ].components ( 0.277368, 0.328683, 0.17104, 1 );
points [ 5 ].components ( 0.279307, 0.399856, 0.167554, 1 );
points [ 6 ].components ( 0.316977, 0.0959509, 0.0148698, 1 );
points [ 7 ].components ( 0.297836, 0.134267, 0.072431, 1 );
points [ 8 ].components ( 0.281929, 0.188706, 0.120335, 1 );
points [ 9 ].components ( 0.271012, 0.255678, 0.153081, 1 );
points [ 10 ].components ( 0.266236, 0.32836, 0.167375, 1 );
points [ 11 ].components ( 0.267493, 0.4, 0.163608, 1 );
points [ 12 ].components ( 0.308178, 0.094766, 0.0100205, 1 );
points [ 13 ].components ( 0.288715, 0.132938, 0.0680817, 1 );
points [ 14 ].components ( 0.272233, 0.187449, 0.116458, 1 );
points [ 15 ].components ( 0.26067, 0.254745, 0.149458, 1 );
points [ 16 ].components ( 0.255212, 0.327893, 0.163752, 1 );
points [ 17 ].components ( 0.255786, 0.399928, 0.159725, 1 );
points [ 18 ].components ( 0.299416, 0.0934732, 0.00523969, 1 );
points [ 19 ].components ( 0.279522, 0.131502, 0.0638146, 1 );
points [ 20 ].components ( 0.262501, 0.186121, 0.112616, 1 );
points [ 21 ].components ( 0.250256, 0.253775, 0.145869, 1 );
points [ 22 ].components ( 0.24408, 0.327354, 0.160122, 1 );
points [ 23 ].components ( 0.243972, 0.399856, 0.155848, 1 );
points [ 24 ].components ( 0.292522, 0.0914265, 0, 1 );
points [ 25 ].components ( 0.271838, 0.129132, 0.0593215, 1 );
points [ 26 ].components ( 0.253596, 0.183858, 0.108698, 1 );
points [ 27 ].components ( 0.239878, 0.251979, 0.142239, 1 );
points [ 28 ].components ( 0.232158, 0.326241, 0.156314, 1 );
points [ 29 ].components ( 0.230506, 0.399354, 0.151506, 1 );
points [ 30 ].components ( 0.286812, 0.0888409, -0.00552736, 1 );
points [ 31 ].components ( 0.265087, 0.126151, 0.0546914, 1 );
points [ 32 ].components ( 0.2453, 0.18095, 0.104787, 1 );
points [ 33 ].components ( 0.229644, 0.249573, 0.138615, 1 );
points [ 34 ].components ( 0.21984, 0.324481, 0.152471, 1 );
points [ 35 ].components ( 0.216249, 0.398276, 0.147013, 1 );
points [ 36 ].components ( 0.237723, 0.0761647, -0.0355477, 1 );
points [ 37 ].components ( 0.212587, 0.111967, 0.0283697, 1 );
points [ 38 ].components ( 0.187629, 0.167268, 0.0815131, 1 );
points [ 39 ].components ( 0.165616, 0.238226, 0.116848, 1 );
points [ 40 ].components ( 0.149062, 0.316366, 0.130211, 1 );
points [ 41 ].components ( 0.138935, 0.393249, 0.122485, 1 );
points [ 42 ].components ( 0.205692, 0.0567016, -0.070205, 1 );
points [ 43 ].components ( 0.17524, 0.0883739, -3.42463e-05, 1 );
points [ 44 ].components ( 0.140372, 0.141951, 0.0584242, 1 );
points [ 45 ].components ( 0.104821, 0.213592, 0.0963828, 1 );
points [ 46 ].components ( 0.0731844, 0.293563, 0.108588, 1 );
points [ 47 ].components ( 0.0484423, 0.372098, 0.096739, 1 );
points [ 48 ].components ( 0.160302, 0.028728, -0.0923623, 1 );
points [ 49 ].components ( 0.127588, 0.057384, -0.0195683, 1 );
points [ 50 ].components ( 0.0873688, 0.109238, 0.0409723, 1 );
points [ 51 ].components ( 0.0439179, 0.180591, 0.0796843, 1 );
points [ 52 ].components ( 0.00312412, 0.260957, 0.0910336, 1 );
points [ 53 ].components ( -0.0308825, 0.339815, 0.0770268, 1 );
points [ 54 ].components ( 0.140085, 0.000359158, -0.122095, 1 );
points [ 55 ].components ( 0.105755, 0.0225872, -0.0429106, 1 );
points [ 56 ].components ( 0.056271, 0.0671515, 0.02363, 1 );
points [ 57 ].components ( -0.00344732, 0.130927, 0.0658214, 1 );
points [ 58 ].components ( -0.0638835, 0.203501, 0.0768145, 1 );
points [ 59 ].components ( -0.117282, 0.274459, 0.0594996, 1 );
points [ 60 ].components ( 0.113583, -0.0360537, -0.137609, 1 );
points [ 61 ].components ( 0.079002, -0.0174164, -0.0560201, 1 );
points [ 62 ].components ( 0.0248855, 0.020648, 0.0129246, 1 );
points [ 63 ].components ( -0.0436302, 0.0755902, 0.0566297, 1 );
points [ 64 ].components ( -0.115091, 0.138073, 0.0676913, 1 );
points [ 65 ].components ( -0.179621, 0.198833, 0.049301, 1 );
points [ 66 ].components ( 0.103995, -0.051423, -0.146369, 1 );
points [ 67 ].components ( 0.06927, -0.0349047, -0.0630612, 1 );
points [ 68 ].components ( 0.0125684, -0.000215752, 0.00757529, 1 );
points [ 69 ].components ( -0.0609747, 0.0502736, 0.0523558, 1 );
points [ 70 ].components ( -0.138612, 0.10773, 0.0635269, 1 );
points [ 71 ].components ( -0.209319, 0.163497, 0.0444175, 1 );
points [ 72 ].components ( 0.0950892, -0.0677619, -0.153697, 1 );
points [ 73 ].components ( 0.0605081, -0.0531469, -0.0689447, 1 );
points [ 74 ].components ( 0.00172387, -0.0220846, 0.00315751, 1 );
points [ 75 ].components ( -0.0761287, 0.0232695, 0.0489791, 1 );
points [ 76 ].components ( -0.159152, 0.0748719, 0.0603283, 1 );
points [ 77 ].components ( -0.235353, 0.124679, 0.04076, 1 );
points [ 78 ].components ( 0.0856809, -0.0862198, -0.157786, 1 );
points [ 79 ].components ( 0.0510998, -0.0728971, -0.0724652, 1 );
points [ 80 ].components ( -0.00915704, -0.0450669, 0.000253423, 1 );
points [ 81 ].components ( -0.0899184, -0.00466863, 0.0465407, 1 );
points [ 82 ].components ( -0.176676, 0.0410806, 0.058027, 1 );
points [ 83 ].components ( -0.256648, 0.0850704, 0.038349, 1 );
points [ 84 ].components ( 0.078499, -0.104857, -0.161519, 1 );
points [ 85 ].components ( 0.0439539, -0.0930066, -0.0755542, 1 );
points [ 86 ].components ( -0.0177036, -0.0687315, -0.00208218, 1 );
points [ 87 ].components ( -0.101158, -0.0337553, 0.0447805, 1 );
points [ 88 ].components ( -0.1914, 0.00556588, 0.0565201, 1 );
points [ 89 ].components ( -0.274926, 0.0431277, 0.0367737, 1 );
points [ 90 ].components ( 0.0727893, -0.124105, -0.164362, 1 );
points [ 91 ].components ( 0.0383516, -0.113655, -0.0778145, 1 );
points [ 92 ].components ( -0.024311, -0.0929706, -0.00366436, 1 );
points [ 93 ].components ( -0.10992, -0.0636322, 0.0437736, 1 );
points [ 94 ].components ( -0.202891, -0.0310263, 0.055801, 1 );
points [ 95 ].components ( -0.289182, -0.000179793, 0.0361641, 1 );
points [ 96 ].components ( 0.0639195, -0.15075, -0.166012, 1 );
points [ 97 ].components ( 0.0293024, -0.141737, -0.0793624, 1 );
points [ 98 ].components ( -0.0345451, -0.125002, -0.00506161, 1 );
points [ 99 ].components ( -0.122309, -0.101984, 0.0425545, 1 );
points [ 100 ].components ( -0.218009, -0.0768475, 0.054801, 1 );
points [ 101 ].components ( -0.307137, -0.0534341, 0.0353011, 1 );
points [ 102 ].components ( 0.0606519, -0.173876, -0.17104, 1 );
points [ 103 ].components ( 0.0266813, -0.167017, -0.0829172, 1 );
points [ 104 ].components ( -0.0381003, -0.156136, -0.00685611, 1 );
points [ 105 ].components ( -0.128413, -0.142347, 0.0423011, 1 );
points [ 106 ].components ( -0.227489, -0.128234, 0.0553763, 1 );
points [ 107 ].components ( -0.319885, -0.115809, 0.0360545, 1 );
points [ 108 ].components ( 0.055732, -0.206123, -0.167054, 1 );
points [ 109 ].components ( 0.021079, -0.200126, -0.0799378, 1 );
points [ 110 ].components ( -0.0439894, -0.191867, -0.00488352, 1 );
points [ 111 ].components ( -0.134195, -0.182279, 0.0435202, 1 );
points [ 112 ].components ( -0.233019, -0.173193, 0.0565201, 1 );
points [ 113 ].components ( -0.325343, -0.16576, 0.0375956, 1 );
points [ 114 ].components ( 0.0581383, -0.228818, -0.17093, 1 );
points [ 115 ].components ( 0.0244904, -0.224904, -0.0821227, 1 );
points [ 116 ].components ( -0.0405421, -0.222821, -0.00499311, 1 );
points [ 117 ].components ( -0.131753, -0.223288, 0.0453558, 1 );
points [ 118 ].components ( -0.232014, -0.226161, 0.0594653, 1 );
points [ 119 ].components ( -0.325631, -0.230757, 0.0409038, 1 );
points [ 120 ].components ( 0.0615856, -0.256145, -0.169314, 1 );
points [ 121 ].components ( 0.0281894, -0.253524, -0.0801843, 1 );
points [ 122 ].components ( -0.0363409, -0.255642, -0.00247943, 1 );
points [ 123 ].components ( -0.126762, -0.263076, 0.0485476, 1 );
points [ 124 ].components ( -0.226053, -0.274352, 0.0632735, 1 );
points [ 125 ].components ( -0.318628, -0.287315, 0.0451777, 1 );
points [ 126 ].components ( 0.0630937, -0.265266, -0.169601, 1 );
points [ 127 ].components ( 0.0299848, -0.263255, -0.0800816, 1 );
points [ 128 ].components ( -0.0342938, -0.267457, -0.00182875, 1 );
points [ 129 ].components ( -0.124499, -0.278266, 0.0496983, 1 );
points [ 130 ].components ( -0.22361, -0.293671, 0.0648146, 1 );
points [ 131 ].components ( -0.315899, -0.310728, 0.0469312, 1 );
points [ 132 ].components ( 0.064961, -0.274711, -0.169389, 1 );
points [ 133 ].components ( 0.0320674, -0.273238, -0.0795747, 1 );
points [ 134 ].components ( -0.031888, -0.279343, -0.0009315, 1 );
points [ 135 ].components ( -0.121699, -0.293312, 0.0510613, 1 );
points [ 136 ].components ( -0.220307, -0.31256, 0.0665063, 1 );
points [ 137 ].components ( -0.312093, -0.333459, 0.0488763, 1 );
points [ 138 ].components ( 0.0673313, -0.284083, -0.168958, 1 );
points [ 139 ].components ( 0.034617, -0.283149, -0.0788967, 1 );
points [ 140 ].components ( -0.0289433, -0.291122, 0.000178081, 1 );
points [ 141 ].components ( -0.118215, -0.30825, 0.0526092, 1 );
points [ 142 ].components ( -0.216177, -0.331268, 0.0684104, 1 );
points [ 143 ].components ( -0.307209, -0.355975, 0.0509928, 1 );
points [ 144 ].components ( 0.0700603, -0.293491, -0.168204, 1 );
points [ 145 ].components ( 0.0375618, -0.293061, -0.0779241, 1 );
points [ 146 ].components ( -0.0256038, -0.302828, 0.00150684, 1 );
points [ 147 ].components ( -0.114193, -0.322974, 0.0542941, 1 );
points [ 148 ].components ( -0.211294, -0.349619, 0.0704173, 1 );
points [ 149 ].components ( -0.301428, -0.377951, 0.0532873, 1 );
points [ 150 ].components ( 0.0731844, -0.302541, -0.167307, 1 );
points [ 151 ].components ( 0.0408657, -0.302612, -0.0768145, 1 );
points [ 152 ].components ( -0.0217614, -0.314212, 0.00294518, 1 );
points [ 153 ].components ( -0.109489, -0.337445, 0.0561982, 1 );
points [ 154 ].components ( -0.205512, -0.367897, 0.0726844, 1 );
points [ 155 ].components ( -0.294497, -0.4, 0.055801, 1 );

knot knotVectorU [ 32 ] = { 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5 };

knot knotVectorV [ 12 ] = { 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1 };

knot* vectU = new knot [ 32 ];

for ( int index = 0; index < 32; index ++ )
    vectU [ index ] = knotVectorU [ index ];

knot* vectV = new knot [ 12 ];

for ( int index = 0; index < 12; index ++ )
    vectV [ index ] = knotVectorV [ index ];

snlSurface* retSurface = new snlSurface ( degreeU, degreeV, sizeU, sizeV, points, vectU, vectV );
