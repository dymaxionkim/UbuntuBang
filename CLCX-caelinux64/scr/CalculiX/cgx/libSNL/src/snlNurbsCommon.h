// libSNL - Simple Nurbs Library
// Copyright Scott A.E. Lanham, Australia.
// ---------------------------------------
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Library General Public License for more details.
//
//  You should have received a copy of the GNU Library General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// *** NURBS routines that don't belong in classes ***

// *** Legacy opencdsm routines are included ***


#include "snlPoint.h"
#include "snlKnotVector.h"
#include "snlSurface.h"
#include "ptrList.h"

#ifdef SGI_MIPS

    #include <iostream.h>
    #include <math.h>
    
#else

    #include <iostream>
    #include <cmath>
    
    using namespace std;
    
#endif

const int MAX_PROJ_ITER = 64;  // Maximum number of newton iterations for projection functions.

typedef struct
{
    /* surface location */

    knot        paramT;  // Parameter in t direction.
    knot        paramU;  // Parameter in u direction.
    snlPoint    pt;  // Point corresponding to parameters.
    int         flag;  // Indicates whatever you want ;-)

} sLocn;

typedef struct
{
    /* surface location */

    knot        paramT;  // Parameter in t direction.
    knot        paramU;  // Parameter in u direction.
    snlPoint    pt;  // Point corresponding to parameters.
    int         flag;
    basis       dist;  // Distance from point to surface.

} projLocn;

bool newtonIterStepSurf ( snlPoint* derivPts, snlPoint* projPt, knot* deltaU, knot* deltaV );

bool newtonIterStepCurve ( snlPoint* derivPts, snlPoint* projPt, knot* paramDelta );

bool lineIterStepCurve ( snlPoint* derivPts, snlPoint* projPt, knot* paramDelta );

int solve2X2LinEqn ( double a1, double a2, double a3, double a4,
                     double b1, double b2, double* x1, double* x2 );

knot paramDistSqrd ( knot t1, knot u1, knot t2, knot u2 );

void resolveAmbig ( sLocn* projns, int projSize, ptrList < sLocn >* ambig );

ptrList < sLocn >* projPtSurf ( snlSurface& surface, snlPoint* toProj, int toProjNum, sLocn* best,
                                double iterTol, double cosTol, unsigned maxPass );


