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

// *** NURBS Curve of Circular Offset from Base Curve ***

#include "snlCircularOffsetCurve.h"
#include "snlCtrlPointNet.h"
#include "snlUtil.h"

snlCircularOffsetCurve::snlCircularOffsetCurve()
{
    base_curve = 0;    
    chord_offsetCurve = 0;
    angle_offsetCurve = 0;
    tangent_offsetCurve = 0;
    axis_start = 0;
    axis_end = 0;
}

snlCircularOffsetCurve::~snlCircularOffsetCurve()
{
    if ( base_curve ) delete base_curve;
    if ( chord_offsetCurve ) delete chord_offsetCurve;
    if ( angle_offsetCurve ) delete angle_offsetCurve;
    if ( tangent_offsetCurve ) delete tangent_offsetCurve;
    if ( axis_start ) delete axis_start;
    if ( axis_end ) delete axis_end;
}

snlCircularOffsetCurve::snlCircularOffsetCurve ( snlCircularOffsetCurve& copyFrom )
{
    // Copy constructor.
    // -----------------
    
    if ( copyFrom.base_curve )
        base_curve = new snlCurve ( * ( copyFrom.base_curve ) );
    else
        base_curve = 0;
    
    if ( copyFrom.chord_offsetCurve )
        chord_offsetCurve = new snlCurve ( * ( copyFrom.chord_offsetCurve ) );
    else
        chord_offsetCurve = 0;
        
    if ( copyFrom.angle_offsetCurve )
        angle_offsetCurve = new snlCurve ( * ( copyFrom.angle_offsetCurve ) );
    else
        angle_offsetCurve = 0;
        
    if ( copyFrom.tangent_offsetCurve )
        tangent_offsetCurve = new snlCurve ( * ( copyFrom.tangent_offsetCurve ) );
    else
        tangent_offsetCurve = 0;
        
    if ( copyFrom.axis_start )
        axis_start = new snlPoint ( * ( copyFrom.axis_start ) );
    else
        axis_start = 0;
        
    if ( copyFrom.axis_end )
        axis_end = new snlPoint ( * ( copyFrom.axis_end ) );
    else
        axis_end = 0;
}

snlCircularOffsetCurve::snlCircularOffsetCurve ( snlCurve* baseCurve, snlPoint* axisStart, snlPoint* axisEnd )
{
    // Construct new circular offset curve.
    // ------------------------------------
    // baseCurve:        Curve to offset from.    
    // startBaseParam:   Param to start at on base curve.    
    //
    // Notes:    The offset is the circular distance about the given axis from the base curve.
    //           Size must be greater or equal to degree + 1.
    //           All objects passed as pointers are owned by this.
    
    base_curve = baseCurve;
    
    axisStart -> normalise();
    axisEnd -> normalise();
    
    axis_start = axisStart;
    axis_end = axisEnd;
    
    // Generate and zero offset curve.
    
    chord_offsetCurve = new snlCurve ( *baseCurve );
    angle_offsetCurve = new snlCurve ( *baseCurve );
    tangent_offsetCurve = new snlCurve ( *baseCurve );
    
    int arraySize = baseCurve -> controlPointNet().size();    
    
    snlCtrlPoint* ctrlPts = chord_offsetCurve -> controlPointNet().getCtrlPtsPtr();
    
    for ( int index = 0; index < arraySize; index ++ )
        ctrlPts [ index ].zero();
        
    ctrlPts = angle_offsetCurve -> controlPointNet().getCtrlPtsPtr();
    
    for ( int index = 0; index < arraySize; index ++ )
        ctrlPts [ index ].zero();
        
    ctrlPts = tangent_offsetCurve -> controlPointNet().getCtrlPtsPtr();
    
    for ( int index = 0; index < arraySize; index ++ )
        ctrlPts [ index ].zero();
}

void snlCircularOffsetCurve::refine ( double tolerance )
{
    // Refine control point net until tolerance is achieved.
    // -----------------------------------------------------
    //
    // Notes:    This is a little tricky because the offset_curve's control points
    //           have to be evaluted relative to the base_curve to get the points to test
    //           for flatness.
    
    bool tolOk = false;    
    
    int deg = base_curve -> degree();
    
    int numTestPts = deg + 1;
    
    snlPoint* testPoints = new snlPoint [ numTestPts ];
    
    snlPoint** testPtPtrs = new snlPoint* [ numTestPts ];
    
    for ( int index = 0; index < numTestPts; index ++ )
        testPtPtrs [ index ] = testPoints + index;

    while ( ! tolOk )
    {
        tolOk = true;
        
        for ( int index = 0; (unsigned) index < ( base_curve -> controlPointNet().size() ) - deg; index ++ )
        {
            const snlCtrlPoint* basePts = base_curve -> controlPointNet().getCtrlPts();
            
            const snlCtrlPoint* chordOffsetPts = chord_offsetCurve -> controlPointNet().getCtrlPts();
            const snlCtrlPoint* angleOffsetPts = angle_offsetCurve -> controlPointNet().getCtrlPts();
            const snlCtrlPoint* tangentOffsetPts = tangent_offsetCurve -> controlPointNet().getCtrlPts();
        
            // Generate points to test for flatness.
            
            for ( int ptIndex = 0; ptIndex < numTestPts; ptIndex ++ )
            {
                testPoints [ ptIndex ] = basePts [ index + ptIndex ];
                applyOffset ( testPoints [ ptIndex ],
                              chordOffsetPts [ index + ptIndex ],
                              angleOffsetPts [ index + ptIndex ],
                              tangentOffsetPts [ index + ptIndex ] );
            }
        
            // Test for flatness
        
            double flatness = ( base_curve ->controlPointNet() ).snlCtrlPointNet::calcFlatness ( testPtPtrs, numTestPts );            
            
            if ( flatness > tolerance )
            {
                // Insert knot into surface. Half way between existing knots.
                
                const snlKnotVector& knotVect = base_curve -> knotVector();
            
                int insertIndex = index + deg;
            
                knot insertParam = ( ( knotVect.val ( insertIndex + 1 )
                                       - knotVect.val ( insertIndex ) ) / 2 )
                                       + knotVect.val ( insertIndex );

                base_curve -> insertKnot ( insertParam, true );
                chord_offsetCurve -> insertKnot ( insertParam, true );
                angle_offsetCurve -> insertKnot ( insertParam, true );
                tangent_offsetCurve -> insertKnot ( insertParam, true );

                tolOk = false;

                index ++;  // If this is not done then nothing converges if the curvature is too great.
            }
        }
    }
    
    delete[] testPoints;
}

void snlCircularOffsetCurve::applyOffset ( snlPoint& point, snlPoint chordOffset, snlPoint angleOffset, snlPoint tangentOffset ) const
{
    // Rotate point about axis by circular offset.
    // -------------------------------------------
    // point:          Point to rotate.
    // chordOffset:    Point containing weighted chord offset.
    // angleOffset:    Point containing weighted angle offset.
    // tangentOffset:  Point containing weighted tangent offset.    
    
    chordOffset.normalise();
    angleOffset.normalise();
    tangentOffset.normalise();
    
    double angle = 0.0;
    
    // Calculate angle to rotate by by adding all offsets together.
    
    double dist = distToLine ( *axis_start, *axis_end, point );
    
    // CHORD.
    
    if ( chordOffset.x() != 0.0 )
        angle = chordOffset.x() / dist;    
    
    // ANGLE.    
    angle += angleOffset.x();
    
    // TANGENT.
    
    if ( tangentOffset.x() != 0.0 )
    {
        angle += asin ( tangentOffset.x() / dist );
    }
    
    // Apply angle.
    
    if ( angle != 0.0 )
    {
        snlTransform transf;    
    
        transf.rotate ( angle, *axis_start, *axis_end );
    
        transf.transform ( point );
    }
}

int snlCircularOffsetCurve::numOffsets()
{
    return base_curve -> size();
}

void snlCircularOffsetCurve::generateOffsets ( int type, double startOffset, double endOffset )
{
    // Generate offsets as linear interpolation between given start and end points.
    // ----------------------------------------------------------------------------
    //
    // type:           Type of offset to process.
    // startOffset:    Start value of offsets.
    // endOffset:      Ending value of offsets.    
    
    // Generate offset array.
    
    int arraySize = base_curve -> controlPointNet().size();    
    
    double offsetStep = ( endOffset - startOffset ) / (double) ( arraySize - 1 );
    
    snlCtrlPoint* ctrlPts;
    
    switch ( type )
    {        
        case CHORD:
            ctrlPts = chord_offsetCurve -> controlPointNet().getCtrlPtsPtr();
            break;
            
        case ANGLE:
            ctrlPts = angle_offsetCurve -> controlPointNet().getCtrlPtsPtr();                
            break;
            
        case TANGENT:
            ctrlPts = tangent_offsetCurve -> controlPointNet().getCtrlPtsPtr();                
            break;
    }
    
    for ( int index = 0; index < arraySize; index ++ )
    {
        ctrlPts [ index ].x ( startOffset + (double) index * offsetStep );
        ctrlPts [ index ].w ( 1.0 );
    }
}

void snlCircularOffsetCurve::offset ( int index, int type, double val, double weight )
{
    switch ( type )
    {
        case CHORD:
        
            ( chord_offsetCurve -> controlPointNet().getCtrlPtsPtr() ) [ index ].x ( val );
            ( chord_offsetCurve -> controlPointNet().getCtrlPtsPtr() ) [ index ].weight ( weight );
            
            break;
    
        case ANGLE:
        
            ( angle_offsetCurve -> controlPointNet().getCtrlPtsPtr() ) [ index ].x ( val );
            ( angle_offsetCurve -> controlPointNet().getCtrlPtsPtr() ) [ index ].weight ( weight );
            
            break;
        
        case TANGENT:
            
            ( tangent_offsetCurve -> controlPointNet().getCtrlPtsPtr() ) [ index ].x ( val );
            ( tangent_offsetCurve -> controlPointNet().getCtrlPtsPtr() ) [ index ].weight ( weight );
            
            break;            
    }
}

double snlCircularOffsetCurve::offset ( int index, int type )
{
    // Return offset at index of type.
    // -------------------------------
    
    snlCtrlPoint* ctrlPts;
    
    switch ( type )
    {
        case CHORD:
                
            ctrlPts = ( chord_offsetCurve -> controlPointNet().getCtrlPtsPtr() );
            return ctrlPts [ index ].x() / ctrlPts [ index ].w();
            break;
            
        case ANGLE:
            
            ctrlPts = ( angle_offsetCurve -> controlPointNet().getCtrlPtsPtr() );
            return ctrlPts [ index ].x() / ctrlPts [ index ].w();
            break;
        
        case TANGENT:
        
            ctrlPts = ( tangent_offsetCurve -> controlPointNet().getCtrlPtsPtr() );
            return ctrlPts [ index ].x() / ctrlPts [ index ].w();
            break;
    }
    
    return 0.0;
}

void snlCircularOffsetCurve::vertexNet ( snlVertexNet* vNet, double tolerance, bool parametric )
{
    // Return approximation to curve.
    // --------------------------------
    // tolerance:    Tolerance to approximate to.
    // parametric:   Do a parametric analysis as opposed to knot refinement.
    // vNet:         Vertex net to fill with data.
    
    if ( parametric )
    {
        vertexNetParam ( vNet, tolerance );
        return;
    }
    
    snlCtrlPointNetCurve*    ctrlPtNet;
    int                      size;
    
    const snlCtrlPoint*      chordOffsetPts;
    const snlCtrlPoint*      angleOffsetPts;
    const snlCtrlPoint*      tangentOffsetPts;
    
    // Get control point net to work with.
    
    snlCircularOffsetCurve* tmpCurve = 0;
    
    if ( tolerance > 0.0 )
    {
        tmpCurve = new snlCircularOffsetCurve ( *this );
        tmpCurve -> refine ( tolerance );
        ctrlPtNet = new snlCtrlPointNetCurve ( ( tmpCurve -> base_curve ) -> controlPointNet() );
        size = ctrlPtNet -> getNumPts();
        
        chordOffsetPts = ( tmpCurve -> chord_offsetCurve ) -> controlPointNet().getCtrlPts();
        angleOffsetPts = ( tmpCurve -> angle_offsetCurve ) -> controlPointNet().getCtrlPts();
        tangentOffsetPts = ( tmpCurve -> tangent_offsetCurve ) -> controlPointNet().getCtrlPts();
    }
    else
    {    
        ctrlPtNet = new snlCtrlPointNetCurve ( base_curve -> controlPointNet() );
        size = ctrlPtNet -> getNumPts();
        
        chordOffsetPts = chord_offsetCurve -> controlPointNet().getCtrlPts();
        angleOffsetPts = angle_offsetCurve -> controlPointNet().getCtrlPts();
        tangentOffsetPts = tangent_offsetCurve -> controlPointNet().getCtrlPts();
    }
    
    snlCtrlPoint* ctrlPts = ctrlPtNet -> getCtrlPtsPtr();
    
    // Offset base curve control points.
    
    for ( int index = 0; index < size; index ++ )
        applyOffset ( ctrlPts [ index ], chordOffsetPts [ index ], angleOffsetPts [ index ], tangentOffsetPts [ index ] );
    
    // Generate vertex net.
    
    vNet -> vertexNet ( ctrlPts, size );
    
    delete ctrlPtNet;
    
    if ( tmpCurve ) delete tmpCurve;
}

void snlCircularOffsetCurve::vertexNetParam ( snlVertexNet* vNet, double tolerance )
{
    // Generate an approximation to curve using a parametric analysis.
    // ---------------------------------------------------------------
    // vNet:         Vertex network to fill with data.
    // tolerance:    Maximum error to curve.
    
    int              size;    
    
    snlPoint* pts = 0;
    
    if ( tolerance <= 0.0 )
    {
        size = base_curve -> controlPointNet().size();
        
        pts = new snlPoint [ size ];
        
        double minParam = base_curve -> minParam();
        
        double paramStep = ( base_curve -> maxParam() - minParam ) / (double) ( size - 1 );
        
        for ( int index = 0; index < size; index ++ )
        {
            double param = minParam + paramStep * (double) index;            
            pts [ index ] = base_curve -> eval ( param );
            
            applyOffset ( pts [ index ],
                          chord_offsetCurve -> eval ( param ),
                          angle_offsetCurve -> eval ( param ),
                          tangent_offsetCurve -> eval ( param ) );
        }
    }
    
    vNet -> vertexNet ( pts, size );
    
    if ( pts ) delete[] pts;
}

int snlCircularOffsetCurve::size()
{
    // Return number of control points in curve.
    // -----------------------------------------
    
    return base_curve -> size();
}

double snlCircularOffsetCurve::maxParam() const
{
    return base_curve -> maxParam();
}

double snlCircularOffsetCurve::minParam() const
{
    return base_curve -> minParam();
}

snlPoint snlCircularOffsetCurve::eval ( knot param ) const
{
    // Eval curve at param.
    // --------------------
    // param:    Paramter to evaluate at.
    
    snlPoint retPoint = base_curve -> eval ( param );
    
    applyOffset ( retPoint,
                  chord_offsetCurve -> eval ( param ),
                  angle_offsetCurve -> eval ( param ),
                  tangent_offsetCurve -> eval ( param ) );     
    
    return retPoint;
}

