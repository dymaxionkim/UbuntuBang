/* --------------------------------------------------------------------  */
/*                          CALCULIX                                     */
/*                   - GRAPHICAL INTERFACE -                             */
/*                                                                       */
/*     A 3-dimensional pre- and post-processor for finite elements       */
/*              Copyright (C) 1996 Klaus Wittig                          */
/*                                                                       */
/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation; version 2 of           */
/*     the License.                                                      */
/*                                                                       */
/*     This program is distributed in the hope that it will be useful,   */
/*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
/*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/*     GNU General Public License for more details.                      */
/*                                                                       */
/*     You should have received a copy of the GNU General Public License */
/*     along with this program; if not, write to the Free Software       */
/*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */
/* --------------------------------------------------------------------  */

/* TO DO
*/

#define     TEST            0  /* debugging */
#define     TEST2           0  /* debugging */
#define     PRINT_SNL       0  /* debugging */
#define     FAST_SHADING    0  /* scip the line division oriented resolution of filled surfaces and use the coarsest possibility */

#include "uselibSNL.h"

#include <iostream>

#include <snlSurface.h>
#include <snlUtil.h>
#include <snlNurbsCommon.h>

#define ITER_TOL   1.e-6
#define COS_TOL    1.e-2
#define MAX_PASS   500
#define PROJ_SENSITIVITY 10
#define MAX_RETURNED_PNTS_PER_PNT 10
#define MIN_DIST   1.e-5
#define MAXVALUE   1.e99

#ifdef WIN32
extern "C" char  printFlag;                     /* printf 1:on 0:off */
extern "C" Nurbs *nurbs;
#else
extern char  printFlag;                     /* printf 1:on 0:off */
extern Nurbs *nurbs;
#endif



typedef struct {
  int n;
  double paramU[MAX_RETURNED_PNTS_PER_PNT];
  double paramV[MAX_RETURNED_PNTS_PER_PNT];
  double dist[MAX_RETURNED_PNTS_PER_PNT];
}PntProj;


void vl_result( double *A, double *B, double *C )
/**********************************************************/
/*    Vektorbetrag: C =  Vektor(B)-Vektor(A) == Vector(AB)*/
/**********************************************************/
{
register int i;

for (i=0; i<3; i++)
         C[i]=B[i]-A[i];
}
void vl_prod( double *A, double *B, double *C )
/*********************************************************/
/*    Vektormultiplikation: C = A x B                    */
/*********************************************************/
{
      C[0]=A[1]*B[2]-A[2]*B[1];
      C[1]=A[2]*B[0]-A[0]*B[2];
      C[2]=A[0]*B[1]-A[1]*B[0];
}
/*********************************************************/
/*    Scalar Product: C = A o B                   	 */
/*********************************************************/
void vl_sprod( double *A, double *B, double *C )
{
     *C=A[0]*B[0]+A[1]*B[1]+A[2]*B[2];
}
double vl_norm( double *A, double *C )
/*********************************************************/
/*         Vector(B)=Vektor(A)/|A|                       */
/*********************************************************/
{
  int i;
  double    B;

      B=0.;
      for ( i=0; i<3; i++)
       B=B+A[i]*A[i];

      B = sqrt(B);
      if(B==0.) {C[0]=C[1]=C[2]=0.; return(B);} 
      for ( i=0; i<3; i++)
       C[i]=A[i]/B;
      return(B);
}
double vl_betrag(double *a)
/* ********************************************************* */
/*      laenge von Vektor a                                  */
/* ********************************************************* */
{
  register int i;
  double b;
  b=0.;

  for (i=0; i<3; i++)
    {
    b=b+a[i]*a[i];
    }
  b=sqrt(b);
  return(b);
}
void vl_scal( double *A, double *B, double *C )
/**********************************************************/
/* Vektormultiplikation: vektor(C) =  scalar(A)*Vektor(B) */
/**********************************************************/
{
  register int i;

  for (i=0; i<3; i++){
         C[i]= *A * B[i];
        }
}
void vl_add( double *A, double *B, double *C )
/**********************************************************/
/*    Vektoraddition: C =  Vektor(B)+Vektor(A)            */
/**********************************************************/
{
  register int i;

  for (i=0; i<3; i++){
         C[i]=B[i]+A[i];
        }
}

/**********************************************************/
/* Coordinate transformation	: A = M__ * P + t_ 	  */
/**********************************************************/
void vl_trans(double M [][3],double * t, double * P, double * A)
{
  int i,j;
  for(i=0;i<3;i++){
    A[i]=0;
    for(j=0;j<3;j++){
      A[i]=A[i]+M[i][j]*P[j];
    }  
  }
  vl_add(A,t,A);
}

//==============================================================//
// 	a function to generate a torodial NURBS surface		//
//  input: p1 ^=  origin, p2 ^= second point on symmetry axis	//
//  r1 ^= major radius, r2 ^= minur radius, mySurf ^= result	//
//==============================================================//
void makeTorus(double *p1, double *p2, double r1, double r2, BSplineSurface * mySurf)
{
  // construct a B-Spline cirlce that can later be rotated to form a torus
  int l,m,i,j;
  double *resKnot = NULL;
  double *resWeight = NULL;
  double *rescX=NULL, *rescY = NULL, *rescZ = NULL;
  
  BSplineCurve myCurve;
  myCurve.nPol = 9;
  myCurve.nKnt = 12;
  myCurve.deg = 2;
 
  if((rescX = (double *)realloc((double *)rescX, (myCurve.nPol)*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescX\n\n"); }
  
  if((rescY = (double *)realloc((double *)rescY, (myCurve.nPol)*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescY\n\n"); }
  
  if((rescZ = (double *)realloc((double *)rescZ, (myCurve.nPol)*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescZ\n\n"); } 
    
  double circlePoint[9][3]= {{1,0,0},{1,0,-1},{0,0,-1},
			    {-1,0,-1},{-1,0,0},{-1,0,1},
			    {0,0,1},{1,0,1},{1,0,0}};
			    
  double wCircle[9]={1,0.707107,1,0.707107,1,0.707107,1,0.707107,1};
				  
  double kCircle[12]={0,0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1};		
			    
  double circlePointScaled[9][3];
  double resPol[9][3];
			    
  double xAxis[3] ,zAxis[3];//local coordinate system		    
  double p1p2[3];
  double vIndep[3] = {1,0,0}, scalProd;

  //calculate the local coordinate system
  vl_result(p1,p2,p1p2);
  vl_norm(p1p2,zAxis);//scale to length 1
  vl_sprod(zAxis,vIndep,&scalProd);
  if(abs(scalProd)>0.99){// not linear independent 
    vIndep[0] = 0;
    vIndep[1] = 1;
    vIndep[2] = 0;
  }  
  vl_prod(zAxis,vIndep,xAxis);
  vl_norm(xAxis,xAxis);
  
  //Matrix M: 	(x1,  y1,  z1)
  //		(x2,  y2,  z2)
  //		(x3,  y3,  z3)
  // [column,row] bzw. [zeile,spalte]
  double M [3][3];//matrix for new coordinate system
  double t[3];//translation for new coordinate system
  
  //calculate the transformation Matrix M and translation vector t
  for(i=0;i<3;i++)
  {  
    M[i][0]=xAxis[i];//wirte x-Axis to fisrt column
    M[i][1]=0;//wirte y-Axis to second column
    M[i][2]=zAxis[i];//wirte z-Axis to third column
    t[i] = p1[i]+r1*xAxis[i];//set translation to origin
  }
  
  //scale the circular curve to the minor radius r2
  for(l=0;l<9;l++){
    for(m=0;m<3;m++){
      circlePointScaled[l][m] = r2 * circlePoint[l][m];
    }
  } 
  
  //calcutale position of control points
  for(j=0;j<9;j++){//calculate control points of circle
    vl_trans(M,t,circlePointScaled[j], resPol[j]);//calculate controll points of the rotated surface
    rescX[j]=resPol[j][0];
    rescY[j]=resPol[j][1];
    rescZ[j]=resPol[j][2];
  }
  
  //write result to BSplineCurve struct
  myCurve.k = kCircle;
  myCurve.w = wCircle; 
  myCurve.cX =rescX;
  myCurve.cY =rescY;
  myCurve.cZ =rescZ; 
  
  //rotate circular profile curve to get a Torodial NURBS surface
  rotateBSpline(p1, p2, &myCurve, mySurf);
}

//==============================================================//
// 	a function to generate a torodial NURBS surface		//
//  input: p1 ^=  origin, p2 ^= second point on symmetry axis	//
//  r1 ^= major radius, r2 ^= minur radius, mySurf ^= result	//
//==============================================================//
void translateBSpline(double *p1, double *p2, BSplineCurve * myCurve ,BSplineSurface * mySurf)
{
  double p1p2[3]; 
  int i,j;
 
  double *resVKnot = NULL;
  double *resUKnot = NULL;
  double *resWeight = NULL;
  double *rescX=NULL, *rescY = NULL, *rescZ = NULL;
  
  if((resVKnot = (double *)realloc((double *)resVKnot, 4*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure resVKnot\n\n"); }
  
  if((resUKnot = (double *)realloc((double *)resUKnot, (myCurve->nKnt)*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure resUKnot\n\n"); }
  
  if((resWeight = (double *)realloc((double *)resWeight, (myCurve->nPol)*2*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure resWeights\n\n"); }
  
  if((rescX = (double *)realloc((double *)rescX, (myCurve->nPol)*2*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescX\n\n"); }
  
  if((rescY = (double *)realloc((double *)rescY, (myCurve->nPol)*2*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescY\n\n"); }
  
  if((rescZ = (double *)realloc((double *)rescZ, (myCurve->nPol)*2*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescZ\n\n"); }  
  
  //calcutale control points
  vl_result(p1,p2,p1p2);
  i=0;
  for(j=0;j<(myCurve->nPol);j++)
  {
    rescX[i]=(myCurve->cX[j]);
    rescY[i]=(myCurve->cY[j]);
    rescZ[i]=(myCurve->cZ[j]);
    resWeight[i] = myCurve->w[j];//calculate weights     
    i++;
    rescX[i]=(myCurve->cX[j])+p1p2[0];
    rescY[i]=(myCurve->cY[j])+p1p2[1];
    rescZ[i]=(myCurve->cZ[j])+p1p2[2];
    resWeight[i] = myCurve->w[j];//calculate weights     
    i++;
  }
    
    //calculate u-Knots 
    for(j = 0;j<(myCurve->nKnt);j++){    
      resUKnot[j] = myCurve->k[j];
    }
    //calculate v-Knots 
    resVKnot[0] = 0;
    resVKnot[1] = 0;
    resVKnot[2] = 1;
    resVKnot[3] = 1;
       
    //write result to struct
    mySurf->uDeg = myCurve->deg;
    mySurf->vDeg = 1;
    mySurf->nUPol = myCurve->nPol;
    mySurf->nVPol = 2;  
    mySurf->nUKnt = myCurve->nKnt;
    mySurf->nVKnt = 4;  
    mySurf->uKnt = resUKnot;
    mySurf->vKnt = resVKnot;  
    
    mySurf->weights = resWeight;
    mySurf->cX =rescX;
    mySurf->cY =rescY;
    mySurf->cZ =rescZ;    
}



void rotateBSpline(double *p1, double *p2, BSplineCurve * myCurve ,BSplineSurface * mySurf)
{
  double xAxis[3], yAxis[3] ,zAxis[3];//local coordinate system
  double origin[3];
  double p1cPoint[3], cPointTmp[3];
  double tmpVec[3],p1p2[3];
  double scalProd, length;
  double radius;
  
  int i,j,l,m;
  
 //Matrix M: 	(x1,  y1,  z1)
 //		(x2,  y2,  z2)
 //		(x3,  y3,  z3)
 // [column,row] bzw. [zeile,spalte]
  double M [3][3];//matrix for new coordinate system
  double t[3];//translation for new coordinate system
  
  //Define BSpline-circle
  double circlePoint[9][3]= {{1,0,0},{1,1,0},{0,1,0},
			    {-1,1,0},{-1,0,0},{-1,-1,0},
			    {0,-1,0},{1,-1,0},{1,0,0}};
  double circlePointScaled[9][3];
				  
  double wCircle[9]={1,0.707107,1,0.707107,1,0.707107,1,0.707107,1};
				  
  double kCircle[12]={0,0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1};		  
 
  double *resVKnot = NULL;
  double *resUKnot = NULL;
  double *resWeight = NULL;
  double *rescX=NULL, *rescY = NULL, *rescZ = NULL;
  
  if((resVKnot = (double *)realloc((double *)resVKnot, 12*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure resVKnot\n\n"); }
  
  if((resUKnot = (double *)realloc((double *)resUKnot, (myCurve->nKnt)*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure resUKnot\n\n"); }
  
  if((resWeight = (double *)realloc((double *)resWeight, (myCurve->nPol)*9*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure resWeights\n\n"); }
  
  if((rescX = (double *)realloc((double *)rescX, (myCurve->nPol)*9*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescX\n\n"); }
  
  if((rescY = (double *)realloc((double *)rescY, (myCurve->nPol)*9*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescY\n\n"); }
  
  if((rescZ = (double *)realloc((double *)rescZ, (myCurve->nPol)*9*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure rescZ\n\n"); }

  double resPol[(myCurve->nPol)*9][3];
  
  //calculate coordinate axis
  
  vl_result(p1,p2,zAxis);//z-Axis of local coordinate system
  cPointTmp[0]=myCurve->cX[0];//Fist control point
  cPointTmp[1]=myCurve->cY[0];
  cPointTmp[2]=myCurve->cZ[0];
  vl_result(p1,cPointTmp,p1cPoint);
  vl_sprod(p1cPoint,zAxis,&scalProd);
  length=vl_betrag(zAxis);
  length = scalProd/(length*length);
  vl_scal(&length,zAxis,tmpVec);
  vl_add(p1,tmpVec,origin);//origin of local coordinate system
  vl_result(origin,p2,zAxis);//z-Axis of local coordinate system
  vl_norm(zAxis,zAxis);//scale to length 1
  vl_result(origin,cPointTmp,xAxis);//x-Axis of local coordinate system
  vl_norm(xAxis,xAxis);//scale to length 1
  vl_prod(zAxis,xAxis,yAxis);//y-Axis of local coordinate system
  //calculate rotaion matrix
  for(i=0;i<3;i++){
    M[i][0]=xAxis[i];//wirte x-Axis to fisrt column
    M[i][1]=yAxis[i];//wirte y-Axis to second column
    M[i][2]=zAxis[i];//wirte z-Axis to third column
  }
  
  
  for(i=0;i<(myCurve->nPol);i++)
  {
      vl_result(p1,p2,p1p2);
      cPointTmp[0]=myCurve->cX[i];//control point Ci
      cPointTmp[1]=myCurve->cY[i];
      cPointTmp[2]=myCurve->cZ[i];
      vl_result(p1,cPointTmp,p1cPoint);
      vl_sprod(p1cPoint,p1p2,&scalProd);
      length=vl_betrag(p1p2);
      length = scalProd/(length*length);
      vl_scal(&length,p1p2,tmpVec);
      vl_add(p1,tmpVec,t);
      vl_result(t,cPointTmp,tmpVec);
      radius = vl_betrag(tmpVec);
      
      //"scale circle points
      for(l=0;l<9;l++){
	for(m=0;m<3;m++){
	  circlePointScaled[l][m] = radius * circlePoint[l][m];
	}
      }
      
      //calcutale control points
      for(j=0;j<9;j++){//calculate control points of circle
	vl_trans(M,t,circlePointScaled[j], resPol[i*9+j]);//calculate controll points of the rotated surface
	rescX[i*9+j]=resPol[i*9+j][0];
	rescY[i*9+j]=resPol[i*9+j][1];
	rescZ[i*9+j]=resPol[i*9+j][2];
	resWeight[i*9+j] = wCircle[j]*(myCurve->w[i]);//calculate weights
      }
    }
    //calculate u-Knots 
    for(j=0;j<(myCurve->nKnt);j++){    
      resUKnot[j] = myCurve->k[j];
    }
    //calculate v-Knots 
    for(j=0;j<12;j++){    
      resVKnot[j] = kCircle[j];
    }
    //write result to struct
    mySurf->uDeg = myCurve->deg;
    mySurf->vDeg = 2;
    mySurf->nUPol = myCurve->nPol;
    mySurf->nVPol = 9;  
    mySurf->nUKnt = myCurve->nKnt;
    mySurf->nVKnt = 12;  
    mySurf->uKnt = resUKnot;
    mySurf->vKnt = resVKnot;  
    
    mySurf->weights = resWeight;
    mySurf->cX =rescX;
    mySurf->cY =rescY;
    mySurf->cZ =rescZ;    
}

inline double wik(int k,int i,double *u,double x)
{
  if((u[i+k-1]-u[i])!=0){
    return (x-u[i])/(u[i+k-1]-u[i]);
  }
  else
    return 0;
}

inline double deBoor(int k, int i, double * u ,double x)
{     
  if(x>1)x=1;
  if(k==1){
    if(x >= u[i] && x <= u[i+1]) return 1;
    else return 0;
  }
  else {
    return wik(k,i,u,x)*deBoor(k-1,i,u,x)+(1-wik(k,i+1,u,x))*deBoor(k-1,i+1,u,x);
  }
}

inline void calculateBSpline(double * pnt, BSplineCurve * myCurve,double u)
{
  double sumX=0,sumY=0,sumZ=0,N,tmp=0;
  int i,j;
  for(j=0;j<(myCurve->nPol);j++){
    tmp+=(myCurve->w[j])*deBoor(myCurve->deg+1,j,(myCurve->k),u);
  }
  if(tmp!=0){
    for(i=0;i<(myCurve->nPol);i++)
    {
      N=(myCurve->w[i])*deBoor(myCurve->deg+1,i,(myCurve->k),u)/tmp;   
      sumX+= myCurve->cX[i]*(N);
      sumY+= myCurve->cY[i]*(N);
      sumZ+= myCurve->cZ[i]*(N);
    }
  }
  else{
    sumX = sumY = sumZ = 0;
  }
  pnt[0]=sumX;
  pnt[1]=sumY;
  pnt[2]=sumZ;
}

/**************************************************/
/*** 	fitting algorithm for B-Splines		***/
/*** progressive-iterative approximation (PIA)	***/
/**************************************************/
void piaFitting(double pCloud [][3],int nPnt,BSplineCurve * fitCurve, int deg, double tolerance)
{
  fitCurve->k = NULL;fitCurve->cX = NULL;fitCurve->cY = NULL;fitCurve->cZ = NULL;fitCurve->w = NULL;
  int nCPnt = nPnt + 2, i;  
  if((fitCurve->k = (double *)realloc((double *)fitCurve->k,(nPnt+2*deg)*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure\n\n"); }  
  if((fitCurve->cX = (double *)realloc((double *)fitCurve->cX,nCPnt*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure\n\n"); }    
  if((fitCurve->cY = (double *)realloc((double *)fitCurve->cY,nCPnt*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure\n\n"); }    
  if((fitCurve->cZ = (double *)realloc((double *)fitCurve->cZ,nCPnt*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure\n\n"); }    
    if((fitCurve->w = (double *)realloc((double *)fitCurve->w,nCPnt*sizeof(double))) == NULL )
  { printf(" ERROR: realloc failure\n\n"); }   
  
  
  fitCurve->deg = deg;//cubic Bspline
  fitCurve->nPol = nCPnt;
  fitCurve->nKnt = nPnt+2*deg;
  fitCurve->deg = deg;
  //Define curve as clamped: 
  //Start knodes have multiplicity  deg + 1
    //End knodes have multiplicity  deg + 1
  for(i=0;i<=deg;i++)
  {
    fitCurve->k[i]=0;
    fitCurve->k[fitCurve->nKnt-(i+1)]=1;
  }
  
  double dist=0, p1p2[3], p1[3],p2[3];
  double du,dControll[3];
  
  //sum up distances between points
  for(i=1;i<nPnt;i++){
    vl_result(pCloud[i],pCloud[i-1],p1p2);
    dist+=vl_betrag(p1p2);
  }
  
   //set pCloud points as control points 
   fitCurve->cX[0]=pCloud[0][0];
   fitCurve->cY[0]=pCloud[0][1];
   fitCurve->cZ[0]=pCloud[0][2];
   fitCurve->w[0]=1;
   fitCurve->cX[nCPnt-1]=pCloud[nPnt-1][0];
   fitCurve->cY[nCPnt-1]=pCloud[nPnt-1][1];
   fitCurve->cZ[nCPnt-1]=pCloud[nPnt-1][2];
   fitCurve->w[nCPnt-1]=1;
  
  for(i=1;i<=nPnt;i++){
    fitCurve->cX[i]=pCloud[i-1][0];
    fitCurve->cY[i]=pCloud[i-1][1];
    fitCurve->cZ[i]=pCloud[i-1][2];
    fitCurve->w[i]=1;
  }
  
  //calculate initial knot vector via chord-length parametrization
  for(i=1;i<nPnt;i++){
    p1[0]=pCloud[i-1][0];
    p1[1]=pCloud[i-1][1];
    p1[2]=pCloud[i-1][2];
    p2[0]=pCloud[i][0];
    p2[1]=pCloud[i][1];
    p2[2]=pCloud[i][2];
    vl_result(p1,p2,p1p2);
    du=(vl_betrag(p1p2))/dist;
    fitCurve->k[i+deg]=fitCurve->k[i+deg-1]+du;
  }
  
  double error=10, errorOld=0;
  int c, iter = 1;
  //interation loop for PIA fitting
  do{
    //iterate over all inner knots
    c=0;
    errorOld = error;
    error=0;
    for(i=(deg);i<((fitCurve->nKnt)-(deg));i++)
    {     
      calculateBSpline(dControll,fitCurve,fitCurve->k[i]);//calculate point for parameter with current control points
      p1[0]=pCloud[c][0];
      p1[1]=pCloud[c][1];
      p1[2]=pCloud[c][2];
      vl_result(dControll,p1,p2);//get the difference between calculated spline point and control point
      error+=vl_betrag(p2);//get the difference to current control point
      p1[0]=fitCurve->cX[c+1];
      p1[1]=fitCurve->cY[c+1];
      p1[2]=fitCurve->cZ[c+1];
      vl_add(p1,p2,dControll); 
      fitCurve->cX[c+1]=dControll[0];
      fitCurve->cY[c+1]=dControll[1];
      fitCurve->cZ[c+1]=dControll[2];
      c++;
    }
    #if PLOT == 1
    cout<<"Error: "<<error<<" after "<<iter<<"-iterations"<<endl;
    #endif
    iter++;
  }while(sqrt((error-errorOld)*(error-errorOld))>dist*tolerance);
  cout<<"BSpline fitting ... done"<<endl;
  cout<<"Error: "<<error<<" after "<<iter<<"-iterations"<<endl;
}


#ifdef __cplusplus
extern "C" {
#endif
   extern SumGeo    anzGeo[1];
#ifdef __cplusplus
}
#endif

// free cpp array (to be called from c code to free cpp objects)
void cppfreearray(double *array)
{
  delete[] array;
}

int createBlendedNurbs(int nr, Points **pntpntr, Lines *line, Lcmb *lcmb, Gsur *surf )
{
  int i,j,k,l,cl,n,p, nip, flag, np[4], index;
  static double *px[4], *py[4], *pz[4];
  char name[MAX_LINE_LENGTH], buffer[MAX_LINE_LENGTH];
  char sname[MAX_LINE_LENGTH];

  int e;
  double p0[3],p1[3],p0p1[3],ep0p1[3],vl,dvl,p0pn[3],pn[3];

  Points *point;

  extern SumAsci   sumAsci[1];

  point=*pntpntr;

  // printf("surf:%s\n", surf[nr].name);
  if(surf[nr].nl!=4)
  {
    printf("ERROR: nr of edges must be 4 but is %d for surf:%s to create a blended nurbs\n\n",surf[nr].nl,surf[nr].name);
    return(-1);
  }
  /* create an array of points for each edge */
  for(j=0; j<surf[nr].nl; j++)
  {
    p=0;
    nip=0; 
    if(surf[nr].typ[j]=='l')
    {
      // printf("line:%s\n", line[surf[nr].l[j]].name);
      l=surf[nr].l[j];
      nip+=line[l].nip;

      if( (px[j] = (double *)realloc( (double *)px[j], (nip/3+1)*sizeof(double ) ))==NULL )
      { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
        return(-1); }
      if( (py[j] = (double *)realloc( (double *)py[j], (nip/3+1)*sizeof(double ) ))==NULL )
      { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
        return(-1); }
      if( (pz[j] = (double *)realloc( (double *)pz[j], (nip/3+1)*sizeof(double ) ))==NULL )
      { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
        return(-1); }

      if(surf[nr].o[j]=='+')
      {
        n=0; flag=line[l].nip; 
        do
        {
          px[j][p]=line[l].ip[n++];
          py[j][p]=line[l].ip[n++];
          pz[j][p]=line[l].ip[n++];
          p++;
        }while(n<flag);
      }
      else
      {             
        n=line[l].nip; flag=0;
        while(n>flag)
        {
          pz[j][p]=line[l].ip[--n];
          py[j][p]=line[l].ip[--n];
          px[j][p]=line[l].ip[--n];
          p++;
        }
      }
    }
    else
    {
      // printf("lcmb:%s\n", lcmb[surf[nr].l[j]].name);
      if(surf[nr].o[j]=='+')
      {
       for(cl=0; cl<lcmb[surf[nr].l[j]].nl; cl++)
       {
        l=lcmb[surf[nr].l[j]].l[cl];

        nip+=line[l].nip;
        if( (px[j] = (double *)realloc( (double *)px[j], (nip/3+1)*sizeof(double ) ))==NULL )
        { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
          return(-1); }
        if( (py[j] = (double *)realloc( (double *)py[j], (nip/3+1)*sizeof(double ) ))==NULL )
        { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
          return(-1); }
        if( (pz[j] = (double *)realloc( (double *)pz[j], (nip/3+1)*sizeof(double ) ))==NULL )
        { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
          return(-1); }

        if(lcmb[surf[nr].l[j]].o[cl]=='-') flag=-1;
        else flag=1;

        if(flag==1)
        {
                       
          if(cl==0)                     { n=0; flag=line[l].nip-3; }
          else if(cl==lcmb[surf[nr].l[j]].nl-1)    { n=0; flag=line[l].nip; }
          else                         { n=0; flag=line[l].nip-3; }
          
          do
          {
            px[j][p]=line[l].ip[n++];
            py[j][p]=line[l].ip[n++];
            pz[j][p]=line[l].ip[n++];
            p++;
          }while(n<flag);
        }
        else
        {
                      
          if(cl==0)                     { n=line[l].nip; flag=3; }
          else if(cl==lcmb[surf[nr].l[j]].nl-1)    { n=line[l].nip; flag=0; }
          else                         { n=line[l].nip; flag=3; }

          while(n>flag)
          {
            pz[j][p]=line[l].ip[--n];
            py[j][p]=line[l].ip[--n];
            px[j][p]=line[l].ip[--n];
            p++;
          }
        }
       }
      }
      else
      {
       for(cl=lcmb[surf[nr].l[j]].nl-1; cl>=0; cl--)
       {
        l=lcmb[surf[nr].l[j]].l[cl];

        nip+=line[l].nip;
        if( (px[j] = (double *)realloc( (double *)px[j], (nip/3+1)*sizeof(double ) ))==NULL )
        { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
          return(-1); }
        if( (py[j] = (double *)realloc( (double *)py[j], (nip/3+1)*sizeof(double ) ))==NULL )
        { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
          return(-1); }
        if( (pz[j] = (double *)realloc( (double *)pz[j], (nip/3+1)*sizeof(double ) ))==NULL )
        { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
          return(-1); }

        if(lcmb[surf[nr].l[j]].o[cl]=='-') flag=1;
        else flag=-1;

        if(flag==1)
        {
                       
          if(cl==0)                     { n=0; flag=line[l].nip; }
          else if(cl==lcmb[surf[nr].l[j]].nl-1)    { n=0; flag=line[l].nip-3; }
          else                         { n=0; flag=line[l].nip-3; }
          
          do
          {
            px[j][p]=line[l].ip[n++];
            py[j][p]=line[l].ip[n++];
            pz[j][p]=line[l].ip[n++];
            p++;
          }while(n<flag);
        }
        else
        {
                      
          if(cl==0)                     { n=line[l].nip; flag=0; }
          else if(cl==lcmb[surf[nr].l[j]].nl-1)    { n=line[l].nip; flag=3; }
          else                         { n=line[l].nip; flag=3; }

          while(n>flag)
          {
            pz[j][p]=line[l].ip[--n];
            py[j][p]=line[l].ip[--n];
            px[j][p]=line[l].ip[--n];
            p++;
          }
        }
       }
      }
    }
    np[j]=p;
  }

  /* each edge must have at least 4 points */
  for (e=0; e<4; e++) if(np[e]<4)
  {
    if( (px[e] = (double *)realloc( (double *)px[e], (5)*sizeof(double ) ))==NULL )
    { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
      return(-1); }
    if( (py[e] = (double *)realloc( (double *)py[e], (5)*sizeof(double ) ))==NULL )
    { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
      return(-1); }
    if( (pz[e] = (double *)realloc( (double *)pz[e], (5)*sizeof(double ) ))==NULL )
    { printf(" ERROR: realloc failure in createBlendedNurbs()\n\n");
      return(-1); }
    p0[0]=px[e][np[e]-2];
    p0[1]=py[e][np[e]-2];
    p0[2]=pz[e][np[e]-2];
    p1[0]=px[e][np[e]-1];
    p1[1]=py[e][np[e]-1];
    p1[2]=pz[e][np[e]-1];
    vl_result(p0,p1,p0p1);
    vl_norm(p0p1,ep0p1); 
    vl=vl_betrag(p0p1);
    nip=4-np[e];
    dvl=vl/(nip+1);
    vl=0.;
    for(i=0; i<=nip; i++)
    {
      vl+=dvl;
      vl_scal(&vl,ep0p1, p0pn);
      vl_add(p0,p0pn,pn);
      // printf("PNT ! %f %f %f\n",  pn[0],pn[1],pn[2]);
      px[e][np[e]-1+i]=pn[0];
      py[e][np[e]-1+i]=pn[1];
      pz[e][np[e]-1+i]=pn[2];
    }
    np[e]+=nip;
  }
  /*
  for(j=0; j<4; j++)
  {
    printf("# pnts:%d\n", np[j]);
    for ( i= 0; i < np[j] ; i ++ ) printf(" PNT ! %f %f %f\n",  px[j][i],  py[j][i],  pz[j][i]); 
  }
  */

  // Create array of points to be passed to snlCurve constructor.
  // printf("  Create array of points\n"); 
  snlPoint* curvePointsEdge1 = new snlPoint [ np[0] ];
  snlPoint* curvePointsEdge2 = new snlPoint [ np[1] ];
  snlPoint* curvePointsEdge3 = new snlPoint [ np[2] ];
  snlPoint* curvePointsEdge4 = new snlPoint [ np[3] ];
  
  j=0; for ( i= 0; i < np[j] ; i ++ ) curvePointsEdge1 [ i ].components ( px[j][i], py[j][i], pz[j][i] );
  j=1; for ( i= 0; i < np[j] ; i ++ ) curvePointsEdge2 [ i ].components ( px[j][i], py[j][i], pz[j][i] );
  j=2; for ( i= np[j]-1; i >= 0 ; i -- ) curvePointsEdge3 [ np[j]-1-i ].components ( px[j][i], py[j][i], pz[j][i] );
  j=3; for ( i= np[j]-1; i >= 0 ; i -- ) curvePointsEdge4 [ np[j]-1-i ].components ( px[j][i], py[j][i], pz[j][i] );
  
  // Create curves. All curves must have the same degree. "degree" is an integer.
  int degree=3;
  snlCurve* curveEdgeU1 = new snlCurve ( 	curvePointsEdge1,
  								np[0],
  								snlCurve::SNL_GLOBAL_INTERP_CENTRIFUGAL,
  								degree 	);
  snlCurve* curveEdgeV2 = new snlCurve ( 	curvePointsEdge2,
  								np[1],
  								snlCurve::SNL_GLOBAL_INTERP_CENTRIFUGAL,
  								degree 	);
  snlCurve* curveEdgeU2 = new snlCurve ( 	curvePointsEdge3,
  								np[2],
  								snlCurve::SNL_GLOBAL_INTERP_CENTRIFUGAL,
  								degree 	);
  snlCurve* curveEdgeV1 = new snlCurve ( 	curvePointsEdge4,
  								np[3],
  								snlCurve::SNL_GLOBAL_INTERP_CENTRIFUGAL,
  								degree 	);
  
  // Create bilinear Coons patch. The orientation of the curves given to this
  // function is very important. There are two curves in the U direction
  // and two in the V direction. They should be oriented as follows:
  //
  //		V1
  //		-------->
  //		|         |
  //	U1	|         |  U2
  //		|         |
  //		V	 V
  //		 -------->
  //		V2
  //
  // The arrow heads are the end of the curve. If you want to reverse
  // the direction of the curve call it's reverseEvalDirection() function.
  
  // You will have to correspond curveEdge1 etc to one of the directions in
  // the following constructor.
  
  // printf(" Create bilinear Coons patch\n"); 
  snlSurface * surface = new snlSurface ( curveEdgeU1,curveEdgeU2,curveEdgeV1,curveEdgeV2 );
  
  // You now have a new NURBS Coons Patch.
  
  // Clean up allocated memory.
  
  delete[] curvePointsEdge1;
  delete[] curvePointsEdge2;
  delete[] curvePointsEdge3;
  delete[] curvePointsEdge4;
  
  delete curveEdgeU1;
  delete curveEdgeU2;
  delete curveEdgeV1;
  delete curveEdgeV2;


  // (5)   create the cgx nurbs
  // printf(" create blended nurbs\n"); 
  buffer[0]='S';
  buffer[1]=(char)NULL;
  getNewName( name, buffer );
  for (i=0; i<MAX_LINE_LENGTH; i++) sname[i]=name[i];
  if(printFlag) printf ("store NURS Nr:%d Name:%s\n", anzGeo->nurs+1, name);

  if ((nurbs = (Nurbs *)realloc( (Nurbs *)nurbs, (anzGeo->nurs+1)*sizeof(Nurbs)) ) == NULL )
  { printf("\n\nERROR: realloc failure in Nurs, nurbs:%s not installed\n\n", name); return(-1); }

  nr=anzGeo->nurs;

  // hashNurs was deactivated but its needed. Could be moved into the calling function
  hashNurs( sumAsci, name, nr );

  anzGeo->nurs++;
  k=strlen(sname);
  if((nurbs[nr].name= (char *)malloc((k+1)*sizeof(char))) == NULL )
  { printf("ERROR: malloc failed\n\n" ); return(-1); }
  for (i=0; i<=k; i++) nurbs[nr].name[i]=name[i];

  /* the nurbs fits exactly, no trimming is necessary */
  nurbs[nr].trimFlag=0;
  nurbs[nr].u_exp=surface->degreeU();
  nurbs[nr].v_exp=surface->degreeV();
  nurbs[nr].u_npnt=surface->sizeU();
  nurbs[nr].v_npnt=surface->sizeV();
  nurbs[nr].u_nknt=1+nurbs[nr].u_npnt+ nurbs[nr].u_exp;
  nurbs[nr].v_nknt=1+nurbs[nr].v_npnt+ nurbs[nr].v_exp;
  nurbs[nr].v_stride=4;
  nurbs[nr].u_stride=4* nurbs[nr].v_npnt;

  nurbs[nr].uv=NULL;
  nurbs[nr].xyz=NULL;
  nurbs[nr].np=NULL;
  nurbs[nr].nc=NULL;
  nurbs[nr].vmax=NULL;
  nurbs[nr].umax=NULL;
  nurbs[nr].vstep=NULL;
  nurbs[nr].ustep=NULL;
  nurbs[nr].sum_ambiguousPnts=NULL;
  nurbs[nr].uvflipped=NULL;

  nurbs[nr].ctlarray=NULL; 

  nurbs[nr].patches=0;
  nurbs[nr].endFlag=1;       
  nurbs[nr].type=GL_MAP2_VERTEX_4;       
  nurbs[nr].Nurb = (GLUnurbsObj *)gluNewNurbsRenderer();

  // knots
  if ( (nurbs[nr].uknt = (float *)malloc((nurbs[nr].u_nknt+1) * sizeof(float))) == NULL )
    printf("\n\n ERROR: malloc failed uknt\n\n");
  if ( (nurbs[nr].vknt = (float *)malloc((nurbs[nr].v_nknt+1) * sizeof(float))) == NULL )
    printf("\n\n ERROR: malloc failed vknt\n\n");
  for(i=0; i<nurbs[nr].u_nknt; i++) { nurbs[nr].uknt[i]=surface->knotsU()[i]; }
  for(i=0; i<nurbs[nr].v_nknt; i++) { nurbs[nr].vknt[i]=surface->knotsV()[i]; }

  // discrete controll points and weights
  if ( (nurbs[nr].ctlpnt = (int **)malloc(  (nurbs[nr].u_npnt+1) * sizeof(int *))) == NULL )
    printf("\n\n ERROR: malloc failed ctlpnt\n\n");
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    if ( (nurbs[nr].ctlpnt[i] = (int *)malloc(  (nurbs[nr].v_npnt+1) * sizeof( int ))) == NULL )
      printf("\n\n ERROR: malloc failed ctlpnt[i]\n\n");
  }
  if ( (nurbs[nr].weight = (GLfloat **)malloc(  (nurbs[nr].u_npnt+1) * sizeof(GLfloat *))) == NULL )
    printf("\n\n ERROR: malloc failed weight\n\n");
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    if ( (nurbs[nr].weight[i] = (GLfloat *)malloc(  (nurbs[nr].v_npnt+1) * sizeof(GLfloat))) == NULL )
      printf("\n\n ERROR: malloc failed weight[i]\n\n");
  }

  index=0;
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      buffer[0]='p';
      buffer[1]=(char)NULL;
      getNewName( name, buffer );
      nurbs[nr].ctlpnt[i][j]  = pnt( name, surface->controlPoints()[ index ].x(), surface->controlPoints()[ index ].y(), surface->controlPoints()[ index ].z(), 0);
      point=*pntpntr;  /* update pointer */
#if TEST
      printf("p:%s\n", point[nurbs[nr].ctlpnt[i][j]].name);
#endif
      nurbs[nr].weight[i][j]  = surface->controlPoints()[ index ].w();
      index++;
    }
  }

#if TEST
  printf("out: degUV %d %d sizeUV %u %u\n", surface->degreeU(), surface->degreeV(), surface->sizeU(), surface->sizeV());
  printf("knotu:%d\n", nurbs[nr].u_nknt);
  printf("knotv:%d\n", nurbs[nr].v_nknt);
  for(i=0; i<nurbs[nr].u_nknt; i++) { printf("knu:%f\n", nurbs[nr].uknt[i]); }
  for(i=0; i<nurbs[nr].v_nknt; i++) { printf("knv:%f\n", nurbs[nr].vknt[i]); }
  index=0;
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      printf("pnt: %f %f %f %f \n", surface->controlPoints()[ index ].x(),surface->controlPoints()[ index ].y(),surface->controlPoints()[ index ].z(),surface->controlPoints()[ index ].w());
      index++;
    }
  }
#endif

  delete surface;   // Release surface object.
  *pntpntr=point;
  return(nr);
}



// uses double reduceDegree( int dir, unsigned numDeg, double tolerance ) 
// returns the error during reduction. "dir" is the direction you wish to degree
// reduce, it is one of the constants:
//
//        snlSurface::SNL_U_DIR
//        snlSurface::SNL_V_DIR
//
// for the U and V directions respectively. "numDeg" is the number of degrees you w
// ish the surface to be reduced by and "tolerance" is the maximum error allowed du
// ring processing. If tolerance is exceeded then the surface is restored to origin
// al. If you don't want a tolerance and will take any error then use a value of 0.
// This function will increase the size of the data describing the surface consider
// ably. I am yet to implement a function that remedies that, but in the mean time
// it will work just the same.
//
// WARNING: The size will change in certain situations (180deg sector of a cylinder)
// in this case cgx is not able to render the new nurbs because the master surf
// might be bigger than the remaining nurbs. The program will crash.
//  
double repairNurbs( Nurbs *nurbs, int nr, int deg, int dir )
{
  int i,j,index=0;
  int numberOfControlPoints =nurbs[nr].u_npnt*nurbs[nr].v_npnt;
  snlCtrlPoint* controlPoints = new snlCtrlPoint [ numberOfControlPoints ];
  double error;


  // (1)      Create an array of control points from your cgx control points:
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      controlPoints [ index++ ].components (
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3] );
    }
  }

  // (2)      Create an array of knots for each knot vector in your cgx surface:
  knot* knotsU = new knot [ nurbs[nr].u_nknt ];
  knot* knotsV = new knot [ nurbs[nr].v_nknt ];

  for(i=0; i<nurbs[nr].u_nknt; i++)  knotsU[i]=nurbs[nr].uknt[i];
  for(i=0; i<nurbs[nr].v_nknt; i++)  knotsV[i]=nurbs[nr].vknt[i];

  // (3)      Create an snlSurface:
  // You need to supply degreeU, degreeV, sizeU and sizeV.
  snlSurface* surface = new snlSurface ( nurbs[nr].u_exp, nurbs[nr].v_exp, nurbs[nr].u_npnt, nurbs[nr].v_npnt, controlPoints, knotsU, knotsV );

#if TEST
  // print the nurbs-parameter before reduction 
  printf("in: degUV %d %d sizeUV %u %u\n", surface->degreeU(), surface->degreeV(), surface->sizeU(), surface->sizeV());
  for(i=0; i<nurbs[nr].u_nknt; i++) {  printf("knu:%f\n", nurbs[nr].uknt[i]); }
  for(i=0; i<nurbs[nr].v_nknt; i++) {  printf("knv:%f\n", nurbs[nr].vknt[i]); }
  index=0;
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      printf("pnt: %f %f %f %f \n", controlPoints [ index ].x(),controlPoints [ index ].y(),controlPoints [ index ].z(),controlPoints [ index ].w());
      index++;
    }
  }
#endif

  // (4) reduce degree
  if(dir==0) error=surface -> reduceDegree ( snlSurface::SNL_U_DIR, deg, 0.0 );
  else error=surface -> reduceDegree ( snlSurface::SNL_V_DIR, deg, 0.0 );

  // (5)   change the cgx nurbs
  nurbs[nr].u_exp=surface->degreeU();
  nurbs[nr].v_exp=surface->degreeV();
  nurbs[nr].u_npnt=surface->sizeU();
  nurbs[nr].v_npnt=surface->sizeV();
  nurbs[nr].u_nknt=1+nurbs[nr].u_npnt+ nurbs[nr].u_exp;
  nurbs[nr].v_nknt=1+nurbs[nr].v_npnt+ nurbs[nr].v_exp;
  nurbs[nr].u_stride=4* nurbs[nr].v_npnt;

  // knots
  if ( (nurbs[nr].uknt = (float *)realloc(  (float *)nurbs[nr].uknt, (nurbs[nr].u_nknt+1) * sizeof(float))) == NULL )
    printf("\n\n ERROR: realloc failed uknt\n\n");
  if ( (nurbs[nr].vknt = (float *)realloc(  (float *)nurbs[nr].vknt, (nurbs[nr].v_nknt+1) * sizeof(float))) == NULL )
    printf("\n\n ERROR: realloc failed vknt\n\n");
  for(i=0; i<nurbs[nr].u_nknt; i++) { nurbs[nr].uknt[i]=surface->knotsU()[i]; }
  for(i=0; i<nurbs[nr].v_nknt; i++) { nurbs[nr].vknt[i]=surface->knotsV()[i]; }

  // controll-array, discrete controll points and weights
  if( (nurbs[nr].ctlarray = (GLfloat *)realloc( (GLfloat *)nurbs[nr].ctlarray, (nurbs[nr].u_npnt*nurbs[nr].v_npnt*nurbs[nr].v_stride+5)*sizeof(GLfloat) )) == NULL )
  { printf(" ERROR: realloc failure in repairNurbs(), nurbs:%s can not be shaped\n\n", nurbs[nr].name);
      return(-1); }
  index=0;
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]=surface->controlPoints()[ index ].x();
      nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]=surface->controlPoints()[ index ].y();
      nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]=surface->controlPoints()[ index ].z();
      nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]=surface->controlPoints()[ index ].w();
      index++;
    }
  }

#if TEST
  // print 
  printf("out: degUV %d %d sizeUV %u %u\n", surface->degreeU(), surface->degreeV(), surface->sizeU(), surface->sizeV());
  printf("knotu:%d\n", nurbs[nr].u_nknt);
  printf("knotv:%d\n", nurbs[nr].v_nknt);
  for(i=0; i<nurbs[nr].u_nknt; i++) { printf("knu:%f\n", nurbs[nr].uknt[i]); }
  for(i=0; i<nurbs[nr].v_nknt; i++) { printf("knv:%f\n", nurbs[nr].vknt[i]); }
  index=0;
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      printf("pnt: %f %f %f %f \n", surface->controlPoints()[ index ].x(),surface->controlPoints()[ index ].y(),surface->controlPoints()[ index ].z(),surface->controlPoints()[ index ].w());
      index++;
    }
  }
#endif

  delete surface;   // Release surface object.

  // The rest of the objects are deleted by libSNL.

  return(error);
}


int evalNurbs( Nurbs *nurbs, int nr, int sum_p, double *pnt_u, double *pnt_v, Points *pnt)
{ 
  int i,j,index=0;
  snlPoint snlPnt;
  int numberOfControlPoints =nurbs[nr].u_npnt*nurbs[nr].v_npnt;
  snlCtrlPoint* controlPoints = new snlCtrlPoint [ numberOfControlPoints ];


  // (1)      Create an array of control points from your cgx control points:
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      controlPoints [ index++ ].components (
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3] );
    }
  }

  // (2)      Create an array of knots for each knot vector in your cgx surface:
  knot* knotsU = new knot [ nurbs[nr].u_nknt ];
  knot* knotsV = new knot [ nurbs[nr].v_nknt ];

  for(i=0; i<nurbs[nr].u_nknt; i++)  knotsU[i]=(double)nurbs[nr].uknt[i];
  for(i=0; i<nurbs[nr].v_nknt; i++)  knotsV[i]=(double)nurbs[nr].vknt[i];

  // (3)      Create an snlSurface:
  // You need to supply degreeU, degreeV, sizeU and sizeV.
  snlSurface* surface = new snlSurface ( nurbs[nr].u_exp, nurbs[nr].v_exp, nurbs[nr].u_npnt, nurbs[nr].v_npnt, controlPoints, knotsU, knotsV );

  // (4) get coordinates
  for(i=0; i<sum_p; i++)
  {
    // negative values lead to a segfault
    if(pnt_u[i]<0.) pnt_u[i]=0.;
    if(pnt_v[i]<0.) pnt_v[i]=0.;
    snlPnt= surface -> eval ( pnt_u[i], pnt_v[i] );
    pnt[i].px=snlPnt.x();
    pnt[i].py=snlPnt.y();
    pnt[i].pz=snlPnt.z();
  }

  delete surface;   // Release surface object.

  // The rest of the objects are deleted by libSNL.
  return(1);
}


int evalNurbsWithNormalVector( Nurbs *nurbs, int nr, int sum_p, double *pnt_u, double *pnt_v, Points *pnt, Points *nv)
{ 
  int i,j,index=0;
  snlPoint snlPnt;
  int numberOfControlPoints =nurbs[nr].u_npnt*nurbs[nr].v_npnt;
  snlCtrlPoint* controlPoints = new snlCtrlPoint [ numberOfControlPoints ];


  // (1)      Create an array of control points from your cgx control points:
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      controlPoints [ index++ ].components (
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3] );
    }
  }

  // (2)      Create an array of knots for each knot vector in your cgx surface:
  knot* knotsU = new knot [ nurbs[nr].u_nknt ];
  knot* knotsV = new knot [ nurbs[nr].v_nknt ];

  for(i=0; i<nurbs[nr].u_nknt; i++)  knotsU[i]=nurbs[nr].uknt[i];
  for(i=0; i<nurbs[nr].v_nknt; i++)  knotsV[i]=nurbs[nr].vknt[i];

  // (3)      Create an snlSurface:
  // You need to supply degreeU, degreeV, sizeU and sizeV.
  snlSurface* surface = new snlSurface ( nurbs[nr].u_exp, nurbs[nr].v_exp, nurbs[nr].u_npnt, nurbs[nr].v_npnt, controlPoints, knotsU, knotsV );

  snlVector refNorm;
  // (4) get coordinates
  for(i=0; i<sum_p; i++)
  {
    // negative values lead to a segfault
    if(pnt_u[i]<0.) pnt_u[i]=0.;
    if(pnt_v[i]<0.) pnt_v[i]=0.;
    snlPnt= surface -> eval ( pnt_u[i], pnt_v[i] );
    refNorm = surface -> calcNormal ( pnt_u[i], pnt_v[i] );
    nv[i].px=refNorm.x();
    nv[i].py=refNorm.y();
    nv[i].pz=refNorm.z();
    pnt[i].px=snlPnt.x();
    pnt[i].py=snlPnt.y();
    pnt[i].pz=snlPnt.z();
  }

  delete surface;   // Release surface object.

  // The rest of the objects are deleted by libSNL.
  return(1);
}



void projSurfToNurbs( Nurbs *nurbs, int nr, Gsur *surf, int snr, Nodes *node )
{
  int i,j,n,k,index=0;
  int sum_p=0, sum_inverted=0, returnedPntsPerPnt;

  double convergTol,  normTol;
  int maxPass; 
  snlSurfLocn* inverted;
  int *isort=NULL;
  int numberOfControlPoints =nurbs[nr].u_npnt*nurbs[nr].v_npnt;
  snlCtrlPoint* controlPoints = new snlCtrlPoint [ numberOfControlPoints ];

  normTol=COS_TOL;
  convergTol=ITER_TOL;
  maxPass=MAX_PASS;

  if(node) sum_p=surf[snr].nn;
  else     sum_p=surf[snr].npgn;

  if(sum_p<=0) return;

  // (1)      Create an array of control points from your cgx control points:
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      controlPoints [ index++ ].components (
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3] );
    }
  }

  // (2)      Create an array of knots for each knot vector in your cgx surface:
  knot* knotsU = new knot [ nurbs[nr].u_nknt ];
  knot* knotsV = new knot [ nurbs[nr].v_nknt ];

  for(i=0; i<nurbs[nr].u_nknt; i++)  knotsU[i]=nurbs[nr].uknt[i];
  for(i=0; i<nurbs[nr].v_nknt; i++)  knotsV[i]=nurbs[nr].vknt[i];

  // (3)      Create an snlSurface:
  // You need to supply degreeU, degreeV, sizeU and sizeV.
  snlSurface* surface = new snlSurface ( nurbs[nr].u_exp, nurbs[nr].v_exp, nurbs[nr].u_npnt, nurbs[nr].v_npnt,
                                               controlPoints, knotsU, knotsV );
  // (4)      Project some points to the surface:
  // Create an array of snlPoints to project onto the surface.
  snlPoint* toProject = new snlPoint [ sum_p ]; // You supply numPointsToProject.

  // Fill the snlPoint array with data from cgx
  if(node)
  {
    for(i=0; i<sum_p; i++)
    {
      toProject[i].components (node[surf[snr].nod[i]].nx, node[surf[snr].nod[i]].ny, node[surf[snr].nod[i]].nz);
    }
  }
  else
  {
    sum_p=n=0;
    while((surf[snr].npgn-n))
    {
      /* create temporary nodes */
      n++;
      j=(int)surf[snr].pgn[n++];
      n+=3;
      for(k=0; k<j; k++)
      {
        toProject[sum_p].components (surf[snr].pgn[n], surf[snr].pgn[n+1], surf[snr].pgn[n+2]);
        sum_p++;
        n+=3;
      }
    }
  }

  // Project points to surface.
  returnedPntsPerPnt =MAX_RETURNED_PNTS_PER_PNT/5;
  isort = new int [ sum_p ];

 finerProjection:;
  inverted =surface -> fastProject( toProject, sum_p, &sum_inverted, convergTol, normTol, maxPass, PROJ_SENSITIVITY, returnedPntsPerPnt );

  //printf("sum_inverted:%d\n", sum_inverted);
  //for(i=0; i<sum_inverted; i++) printf("%d node:%d dist:%f\n", inverted[i].origPtIndex, surf[snr].nod[inverted[i].origPtIndex],  inverted[i].dist);

  /* store the index of the clostest entity */
  for(i=0; i<sum_p; i++) isort[i]=-1;
  for(i=0; i<sum_inverted; i++)
  {
    n=inverted[i].origPtIndex;
    if(isort[n]==-1) isort[n]=i;
    else if(inverted[i].dist<inverted[isort[n]].dist) isort[n]=i;
  }
  //printf("sum:%d\n", sum_p);
  //for(i=0; i<sum_p; i++) printf("%d node:%d dist:%f\n", isort[i], surf[snr].nod[inverted[isort[i]].origPtIndex],  inverted[isort[i]].dist);

  if(node)
  {
    for(i=0; i<sum_p; i++)
    {
      // check if the distance is "close"
      if((inverted[isort[i]].dist>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
      { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }

      node[surf[snr].nod[i]].nx = inverted[isort[i]].pt.x();
      node[surf[snr].nod[i]].ny = inverted[isort[i]].pt.y();
      node[surf[snr].nod[i]].nz = inverted[isort[i]].pt.z();
    }
  }
  else
  {
    i=n=0;
    while((surf[snr].npgn-n))
    {
      /* create temporary nodes */
      n++;
      j=(int)surf[snr].pgn[n++];
      n+=3;
      for(k=0; k<j; k++)
      {
        // check if the distance is "close"
        if((inverted[isort[i]].dist>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
        { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }

        surf[snr].pgn[n]   = inverted [ isort[i]].pt.x();
        surf[snr].pgn[n+1] = inverted [ isort[i]].pt.y();
        surf[snr].pgn[n+2] = inverted [ isort[i]].pt.z();
        i++;
        n+=3;
      }
    }
  }
  delete[] isort; 
  delete surface;  // Release surface object.
  delete[] inverted;  // Release snlVertex array returned from projection function.
  delete[] toProject;  // Release points that were projected onto surface.

  // The rest of the objects are deleted by libSNL.
}


void projSetToNurbs( Nurbs *nurbs, int nr, Sets *set, int setNr, Points *pnt)
{
  int i,j,n,index=0;

  double convergTol,  normTol;
  int maxPass,sum_p, sum_inverted=0, returnedPntsPerPnt; 
  snlSurfLocn* inverted;
  int numberOfControlPoints =nurbs[nr].u_npnt*nurbs[nr].v_npnt;
  snlCtrlPoint* controlPoints = new snlCtrlPoint [ numberOfControlPoints ];

  normTol=COS_TOL;
  convergTol=ITER_TOL;
  maxPass=MAX_PASS;

 
  // (1)      Create an array of control points from your cgx control points:
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      controlPoints [ index++ ].components (
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3] );
    }
  }

  // (2)      Create an array of knots for each knot vector in your cgx surface:

  knot* knotsU = new knot [ nurbs[nr].u_nknt ];
  knot* knotsV = new knot [ nurbs[nr].v_nknt ];

  for(i=0; i<nurbs[nr].u_nknt; i++)  knotsU[i]=nurbs[nr].uknt[i];
  for(i=0; i<nurbs[nr].v_nknt; i++)  knotsV[i]=nurbs[nr].vknt[i];

  // (3)      Create an snlSurface:
  // You need to supply degreeU, degreeV, sizeU and sizeV.
  snlSurface* surface = new snlSurface ( nurbs[nr].u_exp, nurbs[nr].v_exp, nurbs[nr].u_npnt, nurbs[nr].v_npnt,
                                               controlPoints, knotsU, knotsV );
  // (4)      Project some points to the surface:
  // Create an array of snlPoints to project onto the surface.
  snlPoint* toProject = new snlPoint [ set[setNr].anz_p ]; // You supply numPointsToProject.

  // Fill the snlPoint array with data from cgx
  for(i=0; i<set[setNr].anz_p; i++)
  {
    toProject[i].components (pnt[set[setNr].pnt[i]].px, pnt[set[setNr].pnt[i]].py, pnt[set[setNr].pnt[i]].pz);
#if TEST
    printf("1xyz: %f %f %f\n",pnt[set[setNr].pnt[i]].px, pnt[set[setNr].pnt[i]].py, pnt[set[setNr].pnt[i]].pz);
#endif
  }

  // Project points to surface.
  sum_p=set[setNr].anz_p;
  returnedPntsPerPnt =MAX_RETURNED_PNTS_PER_PNT/5;
  int* isort = new int [ sum_p ];

 finerProjection:;
  inverted =surface -> fastProject( toProject, sum_p, &sum_inverted, convergTol, normTol, maxPass, PROJ_SENSITIVITY, returnedPntsPerPnt );

  //printf("sum_inverted:%d\n", sum_inverted);
  //for(i=0; i<sum_inverted; i++) printf("%d node:%d dist:%f\n", inverted[i].origPtIndex, surf[snr].nod[inverted[i].origPtIndex],  inverted[i].dist);

  /* store the index of the clostest entity */
  for(i=0; i<sum_p; i++) isort[i]=-1;
  for(i=0; i<sum_inverted; i++)
  {
    n=inverted[i].origPtIndex;
    if(isort[n]==-1) isort[n]=i;
    else if(inverted[i].dist<inverted[isort[n]].dist) isort[n]=i;
  }
  //printf("sum:%d\n", sum_p);
  //for(i=0; i<sum_p; i++) printf("%d node:%d dist:%f\n", isort[i], surf[snr].nod[inverted[isort[i]].origPtIndex],  inverted[isort[i]].dist);

  for(i=0; i<set[setNr].anz_p; i++)
  {
    // check if the distance is "close"
    if((inverted[isort[i]].dist>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
    { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }

    pnt[set[setNr].pnt[i]].px = inverted [ isort[i]].pt.x();
    pnt[set[setNr].pnt[i]].py = inverted [ isort[i]].pt.y();
    pnt[set[setNr].pnt[i]].pz = inverted [ isort[i]].pt.z();

#if TEST
    printf("pnt P%d %f %f %f\n", i, pnt[set[setNr].pnt[i]].px, pnt[set[setNr].pnt[i]].py, pnt[set[setNr].pnt[i]].pz);
#endif
  }

  delete[]  isort; 
  delete surface;  // Release surface object.
  delete[] inverted;  // Release snlVertex array returned from projection function.
  delete[] toProject;  // Release points that were projected onto surface.

  // The rest of the objects are deleted by libSNL.
}



double *projPntsToNurbs( Nurbs *nurbs, int nr, int anz_p, Points *pnt)
{
  int i,j,n,index=0;

  double convergTol,  normTol;
  int maxPass,sum_p, sum_inverted=0, returnedPntsPerPnt; 
  snlSurfLocn* inverted;
  int numberOfControlPoints =nurbs[nr].u_npnt*nurbs[nr].v_npnt;
  snlCtrlPoint* controlPoints = new snlCtrlPoint [ numberOfControlPoints ];

  normTol=COS_TOL;
  convergTol=ITER_TOL;
  maxPass=MAX_PASS;

 
  // (1)      Create an array of control points from your cgx control points:
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      controlPoints [ index++ ].components (
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3] );
    }
  }

  // (2)      Create an array of knots for each knot vector in your cgx surface:

  knot* knotsU = new knot [ nurbs[nr].u_nknt ];
  knot* knotsV = new knot [ nurbs[nr].v_nknt ];

  for(i=0; i<nurbs[nr].u_nknt; i++)  knotsU[i]=nurbs[nr].uknt[i];
  for(i=0; i<nurbs[nr].v_nknt; i++)  knotsV[i]=nurbs[nr].vknt[i];

  // (3)      Create an snlSurface:
  // You need to supply degreeU, degreeV, sizeU and sizeV.
  snlSurface* surface = new snlSurface ( nurbs[nr].u_exp, nurbs[nr].v_exp, nurbs[nr].u_npnt, nurbs[nr].v_npnt,
                                               controlPoints, knotsU, knotsV );
  // (4)      Project some points to the surface:
  // Create an array of snlPoints to project onto the surface.
  snlPoint* toProject = new snlPoint [ anz_p ]; // You supply numPointsToProject.

  // Fill the snlPoint array with data from cgx
  for(i=0; i<anz_p; i++)
  {
    toProject[i].components (pnt[i].px, pnt[i].py, pnt[i].pz);
#if TEST
    printf("1xyz: %f %f %f\n",pnt[i].px, pnt[i].py, pnt[i].pz);
#endif
  }

  // Project points to surface.
  sum_p=anz_p;
  returnedPntsPerPnt =MAX_RETURNED_PNTS_PER_PNT/5;
  int* isort = new int [ sum_p ];
  double* dist = new double [ sum_p ];

 finerProjection:;
  //printf("surface -> fastProject\n");
  inverted =surface -> fastProject( toProject, sum_p, &sum_inverted, convergTol, normTol, maxPass, PROJ_SENSITIVITY, returnedPntsPerPnt );

  //printf("sum_inverted:%d\n", sum_inverted);
  //for(i=0; i<sum_inverted; i++) printf("%d node:%d dist:%f\n", inverted[i].origPtIndex, surf[snr].nod[inverted[i].origPtIndex],  inverted[i].dist);

  /* store the index of the clostest entity */
  for(i=0; i<sum_p; i++) isort[i]=-1;
  for(i=0; i<sum_inverted; i++)
  {
    n=inverted[i].origPtIndex;
    if(isort[n]==-1) isort[n]=i;
    else if(inverted[i].dist<inverted[isort[n]].dist) isort[n]=i;
  }
  //printf("sum:%d\n", sum_p);
  //for(i=0; i<sum_p; i++) printf("%d node:%d dist:%f\n", isort[i], surf[snr].nod[inverted[isort[i]].origPtIndex],  inverted[isort[i]].dist);

  for(i=0; i<anz_p; i++)
  {
    // check if the distance is "close"
    if((inverted[isort[i]].dist>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
    { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }

    pnt[i].px = inverted [ isort[i]].pt.x();
    pnt[i].py = inverted [ isort[i]].pt.y();
    pnt[i].pz = inverted [ isort[i]].pt.z();
    dist[i]   = inverted [ isort[i]].dist;

#if TEST
    printf("2xyz: %f %f %f\n",pnt[i].px, pnt[i].py, pnt[i].pz);
#endif
  }

  delete[]  isort; 
  delete surface;  // Release surface object.
  delete[] inverted;  // Release snlVertex array returned from projection function.
  delete[] toProject;  // Release points that were projected onto surface.

  // The rest of the objects are deleted by libSNL.
  return(dist);
}


double proj1PntToNurbs( Nurbs *nurbs, int nr, double *pnt)
{
  int i,j,n,index=0;

  double convergTol,  normTol, result=0.;
  int maxPass,sum_p, sum_inverted=0, returnedPntsPerPnt; 
  snlSurfLocn* inverted;
  int numberOfControlPoints =nurbs[nr].u_npnt*nurbs[nr].v_npnt;
  snlCtrlPoint* controlPoints = new snlCtrlPoint [ numberOfControlPoints ];

  normTol=COS_TOL;
  convergTol=ITER_TOL;
  maxPass=MAX_PASS;

 
  // (1)      Create an array of control points from your cgx control points:
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      controlPoints [ index++ ].components (
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3] );
    }
  }

  // (2)      Create an array of knots for each knot vector in your cgx surface:

  knot* knotsU = new knot [ nurbs[nr].u_nknt ];
  knot* knotsV = new knot [ nurbs[nr].v_nknt ];

  for(i=0; i<nurbs[nr].u_nknt; i++)  knotsU[i]=nurbs[nr].uknt[i];
  for(i=0; i<nurbs[nr].v_nknt; i++)  knotsV[i]=nurbs[nr].vknt[i];

  // (3)      Create an snlSurface:
  // You need to supply degreeU, degreeV, sizeU and sizeV.
  snlSurface* surface = new snlSurface ( nurbs[nr].u_exp, nurbs[nr].v_exp, nurbs[nr].u_npnt, nurbs[nr].v_npnt,
                                               controlPoints, knotsU, knotsV );
  // (4)      Project some points to the surface:
  // Create an array of snlPoints to project onto the surface.
  snlPoint* toProject = new snlPoint [ 1 ]; // You supply numPointsToProject.

  // Fill the snlPoint array with data from cgx
  toProject[0].components (pnt[0], pnt[1], pnt[2]);

  // Project points to surface.
  sum_p=1;
  returnedPntsPerPnt =MAX_RETURNED_PNTS_PER_PNT/5;
  int* isort = new int [ sum_p ];

 finerProjection:;
  inverted =surface -> fastProject( toProject, sum_p, &sum_inverted, convergTol, normTol, maxPass, PROJ_SENSITIVITY, returnedPntsPerPnt );
  //
  //printf("sum_inverted:%d\n", sum_inverted);
  //for(i=0; i<sum_inverted; i++) printf("%d dist:%f\n", inverted[i].origPtIndex, inverted[i].dist);

  /* store the index of the clostest entity */
  isort[0]=-1;
  for(i=0; i<sum_inverted; i++)
  {
    n=inverted[i].origPtIndex;
    if(isort[n]==-1) isort[n]=i;
    else if(inverted[i].dist<inverted[isort[n]].dist) isort[n]=i;
  }
  //
  //printf("sum:%d\n", sum_p);
  //for(i=0; i<sum_p; i++) printf("%d node:%d dist:%f\n", isort[i],inverted[isort[i]].origPtIndex,  inverted[isort[i]].dist);

  i=0;
  {
    // check if the distance is "close"
    if((inverted[isort[i]].dist>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
    { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }

    pnt[0] = inverted [ isort[i]].pt.x();
    pnt[1] = inverted [ isort[i]].pt.y();
    pnt[2] = inverted [ isort[i]].pt.z();
  }

  result=inverted[isort[i]].dist;
  delete[]  isort; 
  delete surface;  // Release surface object.
  delete[] inverted;  // Release snlVertex array returned from projection function.
  delete[] toProject;  // Release points that were projected onto surface.

  return(result);
  // The rest of the objects are deleted by libSNL.
}



// The "ball" type is difficult if the trimming loop extend in the ambiguous zone. To avoid this the nurbs is rotated in space.
// return 1 if the nurbs was rotated in space
int rotateBall( Nurbs *nurbs, int nr, int axis, int patch, double utol_ambig, double vtol_ambig)
{
  int i=0,j=0,k=0,n=0, p;
  int ipax1=0, ipax2=0, pntset=0, curve=0, atPole=0;
  double u=0.5, angle[2]={0.,0.};
  double pax1[3]={0.,0.,0.}, pax2[3]={0.,0.,0.}, p1p2[3]={0.,0.,0.}, cg_sphere[3]={0.,0.,0.}, cg_patch[3]={0.,0.,0.}, vax1[3]={0.,0.,0.}, vax2[3]={0.,0.,0.}, vcgp[3]={0.,0.,0.};
  char buffer[MAX_LINE_LENGTH];
  int foundPntAtPole[2]={0,0}, foundPntAtMeridian[2]={0,0};
  double umin, umax,vmin,vmax, du,dv;
  double pumin, pumax,pvmin,pvmax;
  int *ptmp=NULL;
  extern Points    *point;


  // determine the axis cross

  // 1.axis defined by the points at the poles
  if(axis==1)  //u is axis, pax1 at top
  {
    i=nurbs[nr].u_npnt-1;j=0;
    pax1[0]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0];
    pax1[1]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1];
    pax1[2]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2];
    i=0;
    pax2[0]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0];
    pax2[1]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1];
    pax2[2]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2];
  }
  else if(axis==2)  //v is axis, pax1 at top
  {
    j=nurbs[nr].v_npnt-1;i=0;
    pax1[0]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0];
    pax1[1]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1];
    pax1[2]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2];
    j=0;
    pax2[0]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0];
    pax2[1]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1];
    pax2[2]= nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2];
  }
  else return(0);

  v_result( pax1, pax2, p1p2 );
  v_scal( &u, p1p2, vax1);
  v_add( pax1, vax1, cg_sphere);

  // 2.axis defined by the cross-product of v_1.axis and v_CGsurf

  // CG-surf (trimming loop)
  // over all points of the outer curve
  // scip the last point
  j=0;
  for(k=0; k<nurbs[nr].np[patch][0]-1; k++)
  {
    for(n=0;n<3; n++)
    {
      cg_patch[n]+=nurbs[nr].xyz[patch][0][j++];
    }
  }
  for(n=0;n<3; n++) cg_patch[n]/=j/3;

  v_result( cg_sphere, cg_patch, vcgp );
  v_prod( vcgp, vax1, vax2);
  v_add(cg_sphere, vax2, pax2);

  // determine the nurbs-u,v range  by looking into the knots (to detect ambiguous points) 
  umin=nurbs[nr].uknt[0];
  umax=nurbs[nr].uknt[nurbs[nr].u_nknt-1];
  vmin=nurbs[nr].vknt[0];
  vmax=nurbs[nr].vknt[nurbs[nr].v_nknt-1];

  // determine the rotations. "0" around axis through the poles (1.axis) 
  i=0;
  pumax=pvmax=-MAXVALUE;
  pumin=pvmin=MAXVALUE;
  if(axis==1)
  {
  for(curve=0; curve<nurbs[nr].nc[patch]; curve++)
    for(j=0; j<nurbs[nr].np[patch][curve]; j++)
    {
      if( nurbs[nr].uv[patch][curve][i] < pumin ) pumin=nurbs[nr].uv[patch][curve][i];
      if( nurbs[nr].uv[patch][curve][i] > pumax ) pumax=nurbs[nr].uv[patch][curve][i];
      atPole=0;
      if( (nurbs[nr].uv[patch][curve][i]- utol_ambig ) < umin ) { foundPntAtPole[0]=1; atPole=1; }
      if( (nurbs[nr].uv[patch][curve][i]+ utol_ambig ) > umax ) { foundPntAtPole[1]=1; atPole=1; }
      i++;

      if( nurbs[nr].uv[patch][curve][i] < pvmin ) pvmin=nurbs[nr].uv[patch][curve][i];
      if( nurbs[nr].uv[patch][curve][i] > pvmax ) pvmax=nurbs[nr].uv[patch][curve][i];
      if(!atPole)
      {
        if( (nurbs[nr].uv[patch][curve][i]- vtol_ambig ) < vmin ) foundPntAtMeridian[0]=1;
        if( (nurbs[nr].uv[patch][curve][i]+ vtol_ambig ) > vmax ) foundPntAtMeridian[1]=1;
      }
      i++;
    }
  }
  if(axis==2)
  {
  for(curve=0; curve<nurbs[nr].nc[patch]; curve++)
    for(j=0; j<nurbs[nr].np[patch][curve]; j++)
    {
      if( nurbs[nr].uv[patch][curve][i] < pumin ) pumin=nurbs[nr].uv[patch][curve][i];
      if( nurbs[nr].uv[patch][curve][i] > pumax ) pumax=nurbs[nr].uv[patch][curve][i];
      if( nurbs[nr].uv[patch][curve][i+1] < pvmin ) pvmin=nurbs[nr].uv[patch][curve][i+1];
      if( nurbs[nr].uv[patch][curve][i+1] > pvmax ) pvmax=nurbs[nr].uv[patch][curve][i+1];
      atPole=0;
      if( (nurbs[nr].uv[patch][curve][i+1]- vtol_ambig ) < vmin ) { foundPntAtPole[0]=1; atPole=1; }
      if( (nurbs[nr].uv[patch][curve][i+1]+ vtol_ambig ) > vmax ) { foundPntAtPole[1]=1; atPole=1; }
      if(!atPole)
      {
        if( (nurbs[nr].uv[patch][curve][i]- utol_ambig ) < umin ) foundPntAtMeridian[0]=1;
        if( (nurbs[nr].uv[patch][curve][i]+ utol_ambig ) > umax ) foundPntAtMeridian[1]=1;
      }
      i+=2;
    }
  }

  // determine the uv displacement necessary to move the patch in the middle of the nurbs
  du=0.5*((umax-umin) - (pumax-pumin)) + (umin-pumin);
  dv=0.5*((vmax-vmin) - (pvmax-pvmin)) + (vmin-pvmin);
  //du=(0.5*(pumin-umin) + (umax-pumax)) - (pumin-umin);
  //dv=(0.5*(pvmin-vmin) + (vmax-pvmax)) - (pvmin-vmin);


  // convert in rotation (deg):
  if(axis==1) //axis is u
  {
    angle[0]=dv*360./(vmax-vmin);
    // check if it is likely that v=0 is in the surf (the surf extends nearly around the aequator)
    if( ((pvmax-pvmin)/(vmax-vmin)) > 0.9) angle[0]+=180.;
    angle[1]=du*180./(umax-umin);
  }
  else //axis is v
  {
    angle[1]=dv*180./(vmax-vmin);
    angle[0]=du*360./(umax-umin);
    // check if it is likely that u=0 is in the surf (the surf extends nearly around the aequator)
    if( ((pumax-pumin)/(umax-umin)) > 0.9) angle[0]+=180.;
  }
  
#if TEST
  printf(" axis:%d uvtol_ambig:%f %f pu: %f %f pv: %f %f ball rotated by angle: %f %f duv:%f %f\n", axis, utol_ambig, vtol_ambig, pumin,pumax, pvmin,pvmax, angle[0],angle[1], du,dv); 
#endif

  if((abs(angle[0])<1.)&&(abs(angle[1])<1.)) return(0);

  // rotate the control-points. Do not rotate the original points because a substitute nurbs could be referenced and they uses modified coordinates
  if ( (ptmp = (int *)realloc((int *)ptmp,  nurbs[nr].u_npnt*nurbs[nr].v_npnt* sizeof(int))) == NULL )
   printf("\n\n ERROR: realloc failed\n\n") ;

  delSet("-tmp");
  pntset=pre_seta("-tmp1","i",0);
  k=0;
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      getNewName( buffer, "p" );
      p=pnt( buffer, nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	     nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	     nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2], 0 );
      seta(pntset,"p",p);
      ptmp[k++]=p;
    }
  }

  if(angle[0]!=0.)
  {
    /* generate points on rotation-axis */
    ipax1=pnt( "+pax11",cg_sphere[0],cg_sphere[1],cg_sphere[2], 0 );
    ipax2=pnt( "+pax12",pax1[0],pax1[1],pax1[2], 0 );
    sprintf(buffer,"-tmp1 rot +pax11 +pax12 %f", angle[0] );
    pre_move_pfast(buffer);
    delPnt( 1, &ipax1 );
    delPnt( 1, &ipax2 );
  }
  if(angle[1]!=0.)
  {
    /* generate points on rotation-axis */
    ipax1=pnt( "+pax21",cg_sphere[0],cg_sphere[1],cg_sphere[2], 0 );
    ipax2=pnt( "+pax22",pax2[0],pax2[1],pax2[2], 0 );
    sprintf(buffer,"-tmp1 rot +pax21 +pax22 %f", angle[1] );
    pre_move_pfast(buffer);
    delPnt( 1, &ipax1 );
    delPnt( 1, &ipax2 );
  }

  // adapt the nurbs definition
  k=0;
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]=point[ptmp[k]].px;
      nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]=point[ptmp[k]].py;
      nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]=point[ptmp[k++]].pz;
    }
  }

  delPnt( k, ptmp );
  delSet("-tmp1");
  return(1);
}



/* returns the type ( -1error, 0plate, 1cyl, 2torus, 3ball, 4half-ball-bottom, 5half-ball-top */
/* ( half-ball-top: closed at top)  */
/* axis=1 ->u (ambig point has either vmax or vmin) */
/* axis=2 ->v (ambig point has either umax or umin) */
/* axis=3 ->uv (ambig point has either umax or umin and either vmax or vmin) */

/* ambig=1 if the end-points of one axis are closer than ITER_TOL in the same xyz location (ie a cylinder) */
/* ambig[0]==1 u-ambig at vmin  axis=+2 */
/* ambig[1]==1 u-ambig at vmax  axis=+2 */
/* ambig[2]==1 v-ambig at umin  axis=1 */
/* ambig[3]==1 v-ambig at umax  axis=1 */

/* collapsed=1 if one ambiguous edge is collapsed (a point, top of a ball or cone) */
/* this is the case if all control points on that edge are closer than ITER_TOL in the same xyz location */
/* collapsed[0]==1  u-collapsed at vmin  axis=2 */
/* collapsed[1]==1  u-collapsed at vmax  axis=2 */
/* collapsed[2]==1  v-collapsed at umin  axis=1 */
/* collapsed[3]==1  v-collapsed at umax  axis=1 */
int getNurbsType( Nurbs *nurbs, int nr, int *axis, int *sector)
{
  int i,j,k,n;
  int collapsed[6], ambig[6];
#if TEST2
  extern Points    *point;
#endif

  double diff1;

  *axis=0;
  *sector=0;
  for(k=0; k<6; k++)  collapsed[k]=ambig[k]=0;


  /* check if one edge is ambiguous */
  /* this is the case if control points meet each other */

  /* check v at umin and umax */
  for(k=2; k<4; k++)
  {
    if(k==2) i=0; else i=nurbs[nr].u_npnt-1;
#if TEST2
      printf("check at umin and umax\n");
      j=0;
      printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
      printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
      printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
      printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      printf(" i(u):%d j(v):%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif

    /* check the first and last only */
    /* if they are equal its an ambiguous edge */ 
    /* difference to the first point */
    ambig[k]=1;
    j=nurbs[nr].v_npnt-1;
#if TEST2
      printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
      printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
      printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
      printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      printf(" i:%d j:%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif
    for(n=0; n<3; n++)
    {
      diff1=( nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+3] -
              nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      if( diff1*diff1 >ITER_TOL*ITER_TOL ) ambig[k]=0;
#if TEST2
      printf("diff1:%f ambig:%d\n",diff1,ambig[k]);
#endif
    }

    /* check if one ambiguous edge is collapsed (a point!) */
    /* this is the case if all control points on that edge are on the same spot */
    if(ambig[k])
    {
      collapsed[k]=1;
      for(j=1; j<nurbs[nr].v_npnt; j++)
      {
#if TEST2
        printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
        printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
        printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
        printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
        printf(" %s\n", point[nurbs[nr].ctlpnt[i][j]].name);
#endif

        /* difference to the first point */
        for(n=0; n<3; n++)
        {
          //diff1=( nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+n] -
          //        nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n]);
          diff1=( nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+3] -
                  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
          if( diff1*diff1 >ITER_TOL*ITER_TOL ) collapsed[k]=0;
        }
      }
    }
  }

  /* check v at u in the middle */
  if(nurbs[nr].u_npnt>2)
  {
    k=4;
    i=nurbs[nr].u_npnt/2;
#if TEST2
      printf("check at umean\n");
      j=0;
      printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
      printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
      printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
      printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      printf(" i:%d j:%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif

    /* check the first and last only */
    /* if they are equal its an ambiguous edge */ 
    /* difference to the first point */
    ambig[k]=1;
    j=nurbs[nr].v_npnt-1;
#if TEST2
      printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
      printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
      printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
      printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      printf(" i:%d j:%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif
    for(n=0; n<3; n++)
    {
      //diff1=( nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+n] -
      //        nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n]);
      diff1=( nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+3] -
              nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      if( diff1*diff1 >ITER_TOL*ITER_TOL ) ambig[k]=0;
#if TEST2
      printf("diff1:%f ambig:%d\n",diff1,ambig[k]);
#endif
    }
  }

  /* check u at vmin and vmax */
  for(k=0; k<2; k++)
  {
    if(k==0) j=0; else j=nurbs[nr].v_npnt-1;

#if TEST2
      printf("check at vmin and vmax\n");
    i=0;
      printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
      printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
      printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
      printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      printf(" i:%d j:%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif
    /* check the first and last only */
    /* if they are equal its an ambiguous edge */ 
    /* difference to the first point */
    ambig[k]=1;
    i=nurbs[nr].u_npnt-1;
#if TEST2
      printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
      printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
      printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
      printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      printf(" i:%d j:%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif
    for(n=0; n<3; n++)
    {
      //diff1=( nurbs[nr].ctlarray[j*(nurbs[nr].v_stride)+n] - 
      //        nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n]);
      diff1=( nurbs[nr].ctlarray[j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[j*(nurbs[nr].v_stride)+3] -
              nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      if( diff1*diff1 >ITER_TOL*ITER_TOL ) ambig[k]=0;
#if TEST2
      printf("diff1:%f ambig:%d\n",diff1,ambig[k]);
#endif
    }

    /* check if one ambiguous edge is collapsed (a point!) */
    /* this is the case if all control points on that edge are on the same spot */
    if(ambig[k])
    {
      collapsed[k]=1;
      for(i=1; i<nurbs[nr].u_npnt; i++)
      {
#if TEST2
        printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
        printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
        printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
        printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
        printf(" i:%d j:%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif

        /* difference to the first point */
        for(n=0; n<3; n++)
        {
          diff1=(nurbs[nr].ctlarray[j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[j*(nurbs[nr].v_stride)+3] -
                 nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
          if( diff1*diff1 >ITER_TOL*ITER_TOL ) collapsed[k]=0;
        }
      }
    }
  }

  /* check u at v in the middle */
  if(nurbs[nr].v_npnt>2)
  {
    k=5;
    i=nurbs[nr].v_npnt/2;
#if TEST2
      printf("check at vmean\n");
    i=0;
      printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
      printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
      printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
      printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      printf(" i:%d j:%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif
    /* check the first and last only */
    /* if they are equal its an ambiguous edge */ 
    /* difference to the first point */
    ambig[k]=1;
    i=nurbs[nr].u_npnt-1;
#if TEST2
      printf("x:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0]);
      printf("y:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1]);
      printf("z:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2]);
      printf("w:%f ", nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      printf(" i:%d j:%d %s\n", i, j, point[nurbs[nr].ctlpnt[i][j]].name);
#endif
    for(n=0; n<4; n++)
    {
      //diff1=(nurbs[nr].ctlarray[j*(nurbs[nr].v_stride)+n] -
      //       nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n]);
      diff1=(nurbs[nr].ctlarray[j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[j*(nurbs[nr].v_stride)+3] -
             nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n] / nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3]);
      if( diff1*diff1 >ITER_TOL*ITER_TOL ) ambig[k]=0;
#if TEST2
      printf("diff1:%f ambig:%d\n",diff1,ambig[k]);
#endif
    }
  }


  if((ambig[2]==1)||(ambig[3]==1)) *axis=1;
  if((ambig[0]==1)||(ambig[1]==1)) *axis+=2;
#if TEST2
  i=0; for(k=0; k<4; k++)
  {
    printf("%d collapsed=%d (1:yes)\n",k, collapsed[k]);
    printf("   ambig=%d (1:yes)\n", ambig[k]);
    printf("   axis=%d (1:2)\n", *axis);
  }
#endif
  /* determine the case */
  /* -1error, 0plate, 1cyl, 2torus, 3ball, 4half-ball-bot, 5half-ball-top */
  /* ( half-ball-top: closed at top)  */
  i=0; for(k=0; k<4; k++)
  {
    i+=collapsed[k];
  }
  if(i==0)
  {
    if((*axis==1)||(*axis==2)) return(1);
    else if(*axis==3) return(2);
    else return(0);
  }
  else
  {
    if((collapsed[0]==1)&&(collapsed[1]==1)) { *axis=2; if(ambig[5]==0) *sector=1; return(3); } 
    else if((collapsed[2]==1)&&(collapsed[3]==1)) { *axis=1; if(ambig[4]==0) *sector=1; return(3); } 
    else if((collapsed[0]==1)&&(collapsed[1]==0)) { *axis=2; if(ambig[1]==0) *sector=1; return(4); } 
    else if((collapsed[2]==1)&&(collapsed[3]==0)) { *axis=1; if(ambig[3]==0) *sector=1; return(4); } 
    else if((collapsed[0]==0)&&(collapsed[1]==1)) { *axis=2; if(ambig[0]==0) *sector=1; return(5); } 
    else if((collapsed[2]==0)&&(collapsed[3]==1)) { *axis=1; if(ambig[2]==0) *sector=1; return(5); }
    else return(-1);
  }
}



int trimNurbs( Nurbs *nurbs, int nr, int patch, double ini_tol_ambig)
{
  int i,j,k,n,p;
  int curve, sum_p=0, sum_inverted=0, nurbsType=0,index=0, axis=0, sectorFlag=0, buf=0, returnedPntsPerPnt  ;
  int     pcollapsed, collapsedFlag=0, moveFlag=1 ;

  double convergTol, normTol;
  int maxPass; 
  int numberOfControlPoints =nurbs[nr].u_npnt*nurbs[nr].v_npnt;
  snlCtrlPoint* controlPoints=NULL;
  knot* knotsU=NULL;
  knot* knotsV=NULL;
  snlSurface* surface=NULL;

  double v[3],maxv[3], minv[3];

  int *nppc;
  int disti=0, startFlag, pskip, nextUflag=0, nextVflag=0, lastUflag=-1, lastVflag=-1;
  double umin, umax,vmin,vmax, du, dv, distp;
  double dist1=0., dist2=0., tol_ambig=0., utol_ambig, vtol_ambig;
  double av_local_elength,p0[3],p1[3],p0p1[3];

  PntProj *pntproj=NULL;
  snlSurfLocn* inverted=NULL;

  normTol=COS_TOL;
  convergTol=ITER_TOL;
  maxPass=MAX_PASS;

  if(nurbs[nr].ctlarray==NULL)
  {
    printf(" programmers error: nurbs ctlarray was not initialized\n");
    exit(0);
  }

  for(i=0; i<nurbs[nr].nc[patch]; i++) sum_p+=nurbs[nr].np[patch][i];

  // (4) Create an array of snlPoints to project onto the surface.
  snlPoint* toProject = new snlPoint [ sum_p ]; // You supply numPointsToProject.

  // determine the nurbs-u,v range  by looking into the knots (to detect ambiguous points) 
  umin=nurbs[nr].uknt[0];
  umax=nurbs[nr].uknt[nurbs[nr].u_nknt-1];
  vmin=nurbs[nr].vknt[0];
  vmax=nurbs[nr].vknt[nurbs[nr].v_nknt-1];
  du=umax-umin;
  dv=vmax-vmin;
#if TEST
  printf(" u_nknt:%d v_nknt:%d\n",nurbs[nr].u_nknt,nurbs[nr].v_nknt ); 
  printf(" umax:%f umin:%f vmax:%f vmin:%f\n", umax,umin,vmax,vmin); 
#endif

  // Fill the snlPoint array with data from cgx
  // and determine the patch-x,y,z range  by looking into the points (tolerance of ambig)
  for(n=0; n<3; n++) { maxv[n]=-MAXVALUE; minv[n]=MAXVALUE; }
  p=0;
  for(curve=0; curve<nurbs[nr].nc[patch]; curve++)
  {
    j=0;
    for(i=0; i<nurbs[nr].np[patch][curve]; i++)
    {
      for(n=0;n<3; n++)
      {
	v[n]=nurbs[nr].xyz[patch][curve][j++];
        if(maxv[n]<v[n]) maxv[n]=v[n];
        if(minv[n]>v[n]) minv[n]=v[n];
      }
      toProject[p++].components (v[0],v[1],v[2]);
#if TEST
      printf("PNT P%d %f %f %f\n",p,v[0],v[1],v[2]);
#endif
    }
  }
  // else
  //for(n=0; n<3; n++) dist1+=maxv[n]-minv[n];
  // or
  dist1=-MAXVALUE;
  for(n=0; n<3; n++)
  {
    maxv[n]-=minv[n];
    if(dist1<maxv[n]) dist1=maxv[n];
  }

#if PRINT_SNL
  surface ->print_cpp();
  exit(-1);
#endif

  // determine the nurbs-x,y,z range  by looking into the points (tolerance of ambig)
  for(n=0; n<3; n++) { maxv[n]=-MAXVALUE; minv[n]=MAXVALUE; }
  for(i=0; i<nurbs[nr].u_npnt; i++)
  {
    for(j=1; j<nurbs[nr].v_npnt; j++)
    {
      for(n=0; n<3; n++)
      {
        if(maxv[n]<nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n]) maxv[n]=nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n];
        if(minv[n]>nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n]) minv[n]=nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+n];
      }
    }
  }
  // else
  //for(n=0; n<3; n++) dist2+=maxv[n]-minv[n];
  // or
  dist2=-MAXVALUE;
  for(n=0; n<3; n++)
  {
    maxv[n]-=minv[n];
    if(dist2<maxv[n]) dist2=maxv[n];
  }
  tol_ambig=dist1/dist2*ini_tol_ambig;
  utol_ambig=tol_ambig*du;
  vtol_ambig=tol_ambig*dv;

#if TEST
  printf("extension of region (xyz) dist1:%f\n", dist1);
  printf("extension of nurbs (xyz) dist2:%f\n", dist2);
  printf("dist1/dist2*ini_tol_ambig = tol_ambig:%f\n", tol_ambig);
#endif

  // temporary buffer for the projected points
  pntproj= new PntProj [sum_p];

  // additional number of points per curve object
  nppc= new int [nurbs[nr].nc[patch]+1];

  // (5) Project points to surface.
  if(printFlag) printf("inversion of %d points to nurbs:%s\n", sum_p, nurbs[nr].name);
  returnedPntsPerPnt =MAX_RETURNED_PNTS_PER_PNT/5;


  // determine the type of the nurbs 
  // (0plate, 1cyl, 2torus, 3ball, 4half-ball-bot, 5half-ball-top) 
  // ( half-ball-top: closed at top)  
  // axis=1 v is ambiguous (point has either vmax or vmin)
  // axis=2 u is ambiguous (point has either umax or umin)
  // axis=3 u and v are ambiguous (torus)
  if((nurbsType=getNurbsType( nurbs, nr, &axis, &sectorFlag ))<0)
  { printf("ERROR: type of nurbs:%d not known\n",nurbsType); exit(-1); }
#if TEST
  printf("nurbsType:%d (0plate, 1cyl, 2torus, 3ball, 4half-ball-bot, 5half-ball-top) axis:%d (1:u v:2 uv:3) tol_ambig:%f\n",nurbsType,axis,tol_ambig);
#endif
  nurbs[nr].nurbsType=nurbsType;

 finerProjection:;
  pcollapsed=-1;
  if(surface) delete surface;      // Release surface object.

  // (1)      Create an array of control points from your cgx control points:
  controlPoints = new snlCtrlPoint [ numberOfControlPoints ];
  index=0;
  for (i=0; i<nurbs[nr].u_npnt; i++)
  {
    for (j=0; j<nurbs[nr].v_npnt; j++)
    {
      controlPoints [ index++ ].components (
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+0],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+1],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+2],
	  nurbs[nr].ctlarray[i*(nurbs[nr].v_stride*nurbs[nr].v_npnt)+j*(nurbs[nr].v_stride)+3] );
    }
  }

  // (2)      Create an array of knots for each knot vector in your cgx surface:
  knotsU = new knot [ nurbs[nr].u_nknt ];
  knotsV = new knot [ nurbs[nr].v_nknt ];

  for(i=0; i<nurbs[nr].u_nknt; i++)  knotsU[i]=nurbs[nr].uknt[i];
  for(i=0; i<nurbs[nr].v_nknt; i++)  knotsV[i]=nurbs[nr].vknt[i];

  // (3)      Create an snlSurface:
  // You need to supply degreeU, degreeV, sizeU and sizeV.
#if TEST
  printf("proj attempt\n");
#endif
  surface = new snlSurface ( nurbs[nr].u_exp, nurbs[nr].v_exp, nurbs[nr].u_npnt, nurbs[nr].v_npnt, controlPoints, knotsU, knotsV );

  for(i=0; i<sum_p; i++) pntproj[i].n=0;
  inverted =  surface -> fastProject( toProject, sum_p, &sum_inverted, convergTol, normTol, maxPass, PROJ_SENSITIVITY, returnedPntsPerPnt );

  // sort all projections according to the point indexes in "toProject" 
  for(j=0; j<sum_inverted; j++)
  {
    pntproj[inverted[j].origPtIndex].paramU[pntproj[inverted[j].origPtIndex].n]
     =inverted[j].paramU;
    pntproj[inverted[j].origPtIndex].paramV[pntproj[inverted[j].origPtIndex].n]
     =inverted[j].paramV;
    pntproj[inverted[j].origPtIndex].dist[pntproj[inverted[j].origPtIndex].n]
     =inverted[j].dist;
    pntproj[inverted[j].origPtIndex].n++;
  }

 tryThePlate:;
  // plate
  if(nurbsType==0)
  {
    n=0;
    for(curve=0; curve<nurbs[nr].nc[patch]; curve++)
    {
      j=0;
      // over all points of that curve
      // scip the last point to avoid differences between 1st and last
      for(k=0; k<nurbs[nr].np[patch][curve]-1; k++)
      {
        distp=MAXVALUE;
        for(i=0; i<pntproj[n].n; i++)
        {
          if(pntproj[n].dist[i]<distp)
          { distp=pntproj[n].dist[i]; disti=i; }
        }
        // check if the distance is "close"
        if((distp>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
        { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }

        nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramU[disti];
        nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramV[disti];

#if TEST
        printf("pnt:%d dist:%f disti:%d uv:%e %e  orig-xyz: %f %f %f \n",n, distp,disti, pntproj[n].paramU[disti],pntproj[n].paramV[disti], toProject[n].x(), toProject[n].y(), toProject[n].z() );
#endif
        n++;
      }
  
      /* last point == 1st point */
      nurbs[nr].uv[patch][curve][j++]=nurbs[nr].uv[patch][curve][0];
      nurbs[nr].uv[patch][curve][j++]=nurbs[nr].uv[patch][curve][1];
      n++;
    }
  }
  else
  {
   // go to the first not ambiguous point and store the skipped ones 
   // go over all points and chose the closest one if ambiguous
   // finally care about the skipped leading points

   // if ball look if the max or min direction of u or v is collapsed
   if(nurbsType>2)
   {
     if(axis==1) // u is axis
     {
       if(nurbsType==3) collapsedFlag=3;  // collapsed at umin and umax
       if(nurbsType==4) collapsedFlag=1;  // collapsed at umin
       if(nurbsType==5) collapsedFlag=2;  // collapsed at umax
     }
     if(axis==2)    // v is axis
     {
       if(nurbsType==3) collapsedFlag=3;  // collapsed at vmin and vmax
       if(nurbsType==4) collapsedFlag=1;  // collapsed at vmin
       if(nurbsType==5) collapsedFlag=2;  // collapsed at vmax
     }
     if(axis==3)
     {
       collapsedFlag=3;
     }
#if TEST
     printf("  axis:%d collapsedFlag:%d\n", axis, collapsedFlag);
#endif
   }

   n=0;
   for(curve=0; curve<nurbs[nr].nc[patch]; curve++)
   {
    nppc[curve]=0;
    startFlag=1;
    pskip=0; 
    j=0;

    // over all points of that curve
    // scip the last point to avoid differences between 1st and last
    for(k=0; k<nurbs[nr].np[patch][curve]-1; k++)
    {
      // search the closest one to the nurbs (we have several possible points per xyz-location)
      distp=MAXVALUE;
      for(i=0; i<pntproj[n].n; i++) if(pntproj[n].dist[i]<distp)
      {
        distp=pntproj[n].dist[i]; disti=i;
      }

      // check if the distance is "close"
      if((distp>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
      { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }
#if TEST
        printf("pnt:%d dist:%f disti:%d uv:%f %f  orig-xyz: %f %f %f \n",n, distp,disti, pntproj[n].paramU[disti],pntproj[n].paramV[disti], toProject[n].x(), toProject[n].y(), toProject[n].z() );
#endif

      // look if one edge is collapsed and set coords of close points to ideal values 
      if((nurbsType>2)&&(collapsedFlag))
      {
        if(axis==1)
	{
          if(((collapsedFlag==1)||(collapsedFlag==3))&&((pntproj[n].paramU[disti]- utol_ambig )<umin)) { pntproj[n].paramV[disti]=vmin; } 
          if((collapsedFlag>1)&&((pntproj[n].paramU[disti]+ utol_ambig )>umax)) {  pntproj[n].paramV[disti]=vmin; }
	}
        else if(axis==2)
	{
          if(((collapsedFlag==1)||(collapsedFlag==3))&&((pntproj[n].paramV[disti]- vtol_ambig )<vmin)) { pntproj[n].paramU[disti]=umin; } 
          if((collapsedFlag>1)&&((pntproj[n].paramV[disti]+ vtol_ambig )>vmax)) { pntproj[n].paramU[disti]=umin; }
	}
        else if(axis==3)
	{
          if((pntproj[n].paramV[disti]- vtol_ambig )<vmin) { pntproj[n].paramU[disti]=umin; } 
          if((pntproj[n].paramV[disti]+ vtol_ambig )>vmax) { pntproj[n].paramU[disti]=umin; }
          if((pntproj[n].paramU[disti]- utol_ambig )<umin) { pntproj[n].paramV[disti]=vmin; } 
          if((pntproj[n].paramU[disti]+ utol_ambig )>umax) { pntproj[n].paramV[disti]=vmin; }
	}
      }

      // look for leading ambiguous points (at the poles) and skip the leading ones but only until the first valid was found
      if(startFlag)
      {
        if((axis==2)||(axis==3))     // in udir
	{
            if(((pntproj[n].paramU[disti]- utol_ambig )<umin)||((pntproj[n].paramU[disti]+ utol_ambig )>umax))
            {
              pskip++;
#if TEST
	      printf("pnt:%d scipped\n",n); 
#endif
              goto skip;
            }
	}
        if((axis==1)||(axis==3))     // in vdir 
	{
            if(((pntproj[n].paramV[disti]- vtol_ambig )<vmin)||((pntproj[n].paramV[disti]+ vtol_ambig )>vmax))
            {
              pskip++;
#if TEST
	      printf("pnt:%d scipped\n",n); 
#endif
              goto skip;
            }
	}
      }
      startFlag=0;

      if(!sectorFlag)
      {
        // if the point is ambig look which coord would give the shortest dist to the prev point
        if((axis==2)||(axis==3))     // in udir
        {
          if(((pntproj[n].paramU[disti]-utol_ambig)<umin)||((pntproj[n].paramU[disti]+utol_ambig)>umax))
          {
  	    dist1=nurbs[nr].uv[patch][curve][j-2]-umin;
            dist2=nurbs[nr].uv[patch][curve][j-2]-umax;
            if((dist1*dist1)<(dist2*dist2))
            {
              /* pntproj is at umin. Take the value of umin if pntproj is close to umax, else do nothing */
              if((pntproj[n].paramU[disti]+utol_ambig)>umax) pntproj[n].paramU[disti]=umin;
            }
            else
            {
              /* pntproj is at umax. Take the value of umax if pntproj is close to umin, else do nothing */
              if((pntproj[n].paramU[disti]-utol_ambig)<umin) pntproj[n].paramU[disti]=umax;
            }
#if TEST
        printf("dist1:%f dist2:%f corr.u:%f\n", dist1, dist2, pntproj[n].paramU[disti]);
#endif
          }
        }
        if((axis==1)||(axis==3))     // in vdir 
        {
          if(((pntproj[n].paramV[disti]-vtol_ambig)<vmin)||((pntproj[n].paramV[disti]+vtol_ambig)>vmax))
          {
            dist1=nurbs[nr].uv[patch][curve][j-1]-vmin;
            dist2=nurbs[nr].uv[patch][curve][j-1]-vmax;
            if((dist1*dist1)<(dist2*dist2))
            {
              /* pntproj is at vmin. Take the value of vmin if pntproj is close to vmax, else do nothing */
              if((pntproj[n].paramV[disti]+vtol_ambig)>vmax) pntproj[n].paramV[disti]=vmin;
            }
            else
            {
              /* pntproj is at vmax. Take the value of vmax if pntproj is close to vmin, else do nothing */
              if((pntproj[n].paramV[disti]-vtol_ambig)<vmin) pntproj[n].paramV[disti]=vmax;
            }
#if TEST
        printf("dist1:%f dist2:%f corr.v:%f\n", dist1, dist2, pntproj[n].paramV[disti]);
#endif
          }
        }
      }

      // if the coordinates of this point have to be used for the previously duplicated one
#if TEST
      if((nextUflag>0)||(nextVflag>0))
      printf(" nextu:%d nextv:%d  prev pnt:%d changed to uv:%f %f \n",nextUflag,nextVflag,n-1, pntproj[n].paramU[disti],pntproj[n].paramV[disti] );
#endif
      if(nextUflag>0)
      { nurbs[nr].uv[patch][curve][nextUflag]=pntproj[n].paramU[disti]; nextUflag=0; } 
      if(nextVflag>0)
      { nurbs[nr].uv[patch][curve][nextVflag]=pntproj[n].paramV[disti]; nextVflag=0; }

      // if collapsed edges exist add an additional point
      if(nurbsType>2)
      {
        if(axis==1)     // in udir
        {
	  if(nurbsType==3)
          { if(((pntproj[n].paramU[disti]-utol_ambig)<umin)||((pntproj[n].paramU[disti]+utol_ambig)>umax)) buf=1; else buf=0; }
	  if(nurbsType==4)
          { if((pntproj[n].paramU[disti]-utol_ambig)<umin) buf=1; else buf=0; }
          if(nurbsType==5)
          { if((pntproj[n].paramU[disti]+utol_ambig)>umax) buf=1; else buf=0; }
          if(buf)  // on the collapsed edge
          {
            // additional point required       
            // use the v value from the previous point
#if TEST
            if(j>2) printf("use v-val from prev point:%f\n", nurbs[nr].uv[patch][curve][j-1] );
            else    printf("use v-val from prev point: (wait till end)\n" );
#endif
            nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramU[disti];
            if(j>2) { nurbs[nr].uv[patch][curve][j]=nurbs[nr].uv[patch][curve][j-2];j++; }
            else lastVflag=j++;

            // include a new point
            nppc[curve]++;
            if( (nurbs[nr].uv[patch][curve] = (GLfloat *)realloc( (GLfloat *)nurbs[nr].uv[patch][curve], ((nurbs[nr].np[patch][curve]+1+nppc[curve])*2)*sizeof(GLfloat) ))==NULL )
	      { printf(" ERROR: realloc failure in trimNurbs(), nurbs:%s can not be shaped\n\n", nurbs[nr].name);  return(-1); }

            // use the v value from the next point
            // the value is not really known now, save a flag for later correction
            nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramU[disti];
            distp=MAXVALUE; for(i=0; i<pntproj[n+1].n; i++) if(pntproj[n+1].dist[i]<distp)
            { distp=pntproj[n+1].dist[i]; nurbs[nr].uv[patch][curve][j]=pntproj[n+1].paramV[i]; }

            // check if the distance is "close"
            if((distp>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
            { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }
            nextVflag=j++;
            pcollapsed=j/2;
#if TEST
            printf("collapsed at u:%f add pnt:%d\n", pntproj[n].paramU[disti],pcollapsed );
#endif
          }
          else // not on the collapsed edge
          {
            nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramU[disti];
            nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramV[disti];
#if TEST
            printf(" ori:%d uv:%f %f\n", n, nurbs[nr].uv[patch][curve][j-2], nurbs[nr].uv[patch][curve][j-1] );
#endif
          }
        }
        else     // in vdir
        {
	  if(nurbsType==3)
          { if(((pntproj[n].paramV[disti]-vtol_ambig)<vmin)||((pntproj[n].paramV[disti]+vtol_ambig)>vmax)) buf=1; else buf=0; }
	  if(nurbsType==4)
          { if((pntproj[n].paramV[disti]-vtol_ambig)<vmin) buf=1; else buf=0; }
          if(nurbsType==5)
          { if((pntproj[n].paramV[disti]+vtol_ambig)>vmax) buf=1; else buf=0; }
          if(buf)  // on the collapsed edge
          {
            // additional point required       
            // use the u value from the previous point
#if TEST
            if(j>2) printf("use u-val from prev point:%f\n", nurbs[nr].uv[patch][curve][j-2] );
            else    printf("use u-val from prev point: (wait till end)\n" );
#endif
            if(j>2) { nurbs[nr].uv[patch][curve][j]=nurbs[nr].uv[patch][curve][j-2];j++; }
            else lastUflag=j++;
            nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramV[disti];

            // include a new point
            nppc[curve]++;
            if( (nurbs[nr].uv[patch][curve] = (GLfloat *)realloc( (GLfloat *)nurbs[nr].uv[patch][curve], ((nurbs[nr].np[patch][curve]+1+nppc[curve])*2)*sizeof(GLfloat) ))==NULL )
	      { printf(" ERROR: realloc failure in trimNurbs(), nurbs:%s can not be shaped\n\n", nurbs[nr].name);  return(-1); }

            // use the u value from the next point
            // the value is not really known now, save a flag for later correction
            distp=MAXVALUE; for(i=0; i<pntproj[n+1].n; i++) if(pntproj[n+1].dist[i]<distp)
            { distp=pntproj[n+1].dist[i]; nurbs[nr].uv[patch][curve][j]=pntproj[n+1].paramU[i]; }

            // check if the distance is "close"
            if((distp>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
            { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }
            nextUflag=j++;
            nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramV[disti];
            pcollapsed=j/2;
#if TEST
            printf("collapsed at v:%f add pnt:%d\n", pntproj[n].paramV[disti], pcollapsed  );
#endif
          }
          else // not on the collapsed edge
          {
            nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramU[disti];
            nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramV[disti]; 
#if TEST
            printf(" ori:%d uv:%f %f\n", n, nurbs[nr].uv[patch][curve][j-2], nurbs[nr].uv[patch][curve][j-1] );
#endif
          }
        }
      }
      // no collapsed edges
      else
      {
        nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramU[disti];
        nurbs[nr].uv[patch][curve][j++]=pntproj[n].paramV[disti];
      }
      
    skip:;
      n++;
    }
    if(j<2) { nurbsType=0; goto tryThePlate; } //return(1);  // no regular point was found, all points are on a collapsed edge

#if TEST
    printf("\nLEADING ambiguous points:%d, not ambiguous:%d\n\n",pskip, j/2);
#endif

    // store leading ambiguous points 
    for(k=n-nurbs[nr].np[patch][curve]+1; k<n-nurbs[nr].np[patch][curve]+1+pskip; k++)
    {
      // search the closest one to the nurbs
      distp=MAXVALUE;
      for(i=0; i<pntproj[k].n; i++) if(pntproj[k].dist[i]<distp)
      { distp=pntproj[k].dist[i]; disti=i; }

      // check if the distance is "close"
      if((distp>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
      { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }
#if TEST
        printf("pnt:%d dist:%f disti:%d uv:%f %f  orig-xyz: %f %f %f \n", k, distp,disti, pntproj[k].paramU[disti],pntproj[k].paramV[disti], toProject[k].x(), toProject[k].y(), toProject[k].z() );
#endif


      //  look which coord would give the shortest dist to the last-1 point of the curve
      if((axis==2)||(axis==3))     // in udir
      {
       if(((pntproj[k].paramU[disti]-utol_ambig)<umin)||((pntproj[k].paramU[disti]+utol_ambig)>umax))
       {
	 dist1=nurbs[nr].uv[patch][curve][j-2]-umin;  // valgrind: invalid read, Address 0x1e90f608 is 8 bytes before a block of size 600 alloc'd
	 dist2=nurbs[nr].uv[patch][curve][j-2]-umax;  // valgrind: invalid read, Address 0x1e90f608 is 8 bytes before a block of size 600 alloc'd
	 //if((dist1*dist1)<(dist2*dist2)) pntproj[k].paramU[disti]=umin;
         //else  pntproj[k].paramU[disti]=umax;
	 if((dist1*dist1)<(dist2*dist2))
	 {
           /* pntproj is at umin. Take the value of umin if pntproj is close to umax, else do nothing */
	   if((pntproj[k].paramU[disti]+utol_ambig)>umax) pntproj[k].paramU[disti]=umin;
	 }
         else
	 {
           /* pntproj is at umax. Take the value of umax if pntproj is close to umin, else do nothing */
           if((pntproj[k].paramU[disti]-utol_ambig)<umin) pntproj[k].paramU[disti]=umax;
	 }
#if TEST
         printf("dist1:%f dist2:%f corr.u:%f\n", dist1, dist2, pntproj[k].paramU[disti]);
#endif
       }
      }
      if((axis==1)||(axis==3))     // in vdir 
      {
       if(((pntproj[k].paramV[disti]-vtol_ambig)<vmin)||((pntproj[k].paramV[disti]+vtol_ambig)>vmax))
       {
	dist1=nurbs[nr].uv[patch][curve][j-1]-vmin;  // valgrind: invalid read, Address 0x18ff28bc is 4 bytes before a block of size 4,440 alloc'd
	dist2=nurbs[nr].uv[patch][curve][j-1]-vmax;  // valgrind: invalid read, Address 0x18ff28bc is 4 bytes before a block of size 4,440 alloc'd
	//if((dist1*dist1)<(dist2*dist2)) pntproj[k].paramV[disti]=vmin;
        //else  pntproj[k].paramV[disti]=vmax;
	if((dist1*dist1)<(dist2*dist2))
	{
          /* pntproj is at vmin. Take the value of vmin if pntproj is close to vmax, else do nothing */
          if((pntproj[k].paramV[disti]+vtol_ambig)>vmax) pntproj[k].paramV[disti]=vmin;
	}
        else
	{
          /* pntproj is at vmax. Take the value of vmax if pntproj is close to vmin, else do nothing */
          if((pntproj[k].paramV[disti]-vtol_ambig)<vmin) pntproj[k].paramV[disti]=vmax;
	}
#if TEST
        printf("dist1:%f dist2:%f corr.v:%f\n", dist1, dist2, pntproj[k].paramV[disti]);
#endif
       }
      }


      // if the coordinates of this point have to be used for the previously duplicated one
      if(nextUflag>0)
      { nurbs[nr].uv[patch][curve][nextUflag]=pntproj[k].paramU[disti]; nextUflag=0; } 
      if(nextVflag>0)
      { nurbs[nr].uv[patch][curve][nextVflag]=pntproj[k].paramV[disti]; nextVflag=0; }

      // if collapsed edges exist add an additional point
      if(nurbsType>2)
      {
        if(axis==1)     // in udir
        {
	  if(nurbsType==3)
          { if(((pntproj[k].paramU[disti]-utol_ambig)<umin)||((pntproj[k].paramU[disti]+utol_ambig)>umax)) buf=1; else buf=0; }
	  if(nurbsType==4)
          { if((pntproj[k].paramU[disti]-utol_ambig)<umin) buf=1; else buf=0; }
          if(nurbsType==5)
          { if((pntproj[k].paramU[disti]+utol_ambig)>umax) buf=1; else buf=0; }
          if(buf)  // on the collapsed edge
          {
            // additional point required       
            // use the v value from the previous point
#if TEST
            if(j>2) printf("use v-val from prev point:%f\n", nurbs[nr].uv[patch][curve][j-1] );
            else    printf("use v-val from prev point: (wait till end)\n" );
#endif
            nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramU[disti];
            //nurbs[nr].uv[patch][curve][j]=nurbs[nr].uv[patch][curve][j-2]; j++;
            if(j>2) { nurbs[nr].uv[patch][curve][j]=nurbs[nr].uv[patch][curve][j-2];j++; }
            else lastVflag=j++;

            // include a new point
            nppc[curve]++;
            if( (nurbs[nr].uv[patch][curve] = (GLfloat *)realloc( (GLfloat *)nurbs[nr].uv[patch][curve], ((nurbs[nr].np[patch][curve]+1+nppc[curve])*2)*sizeof(GLfloat) ))==NULL )
	      { printf(" ERROR: realloc failure in trimNurbs(), nurbs:%s can not be shaped\n\n", nurbs[nr].name);  return(-1); }

            // use the v value from the next point
            // the value is not really known now, save a flag for later correction
            nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramU[disti];
            distp=MAXVALUE; for(i=0; i<pntproj[k+1].n; i++) if(pntproj[k+1].dist[i]<distp)
            { distp=pntproj[k+1].dist[i]; nurbs[nr].uv[patch][curve][j]=pntproj[k+1].paramV[i]; }
            // check if the distance is "close"
            if((distp>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
            { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }
            nextVflag=j++;
            pcollapsed=j/2;
#if TEST
            printf("collapsed at u:%f add pnt:%d\n", pntproj[k].paramU[disti],pcollapsed );
#endif
          }
          else // not on the collapsed edge
          {
            nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramU[disti];
            nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramV[disti];
#if TEST
            printf(" ori:%d uv:%f %f\n", k, nurbs[nr].uv[patch][curve][j-2], nurbs[nr].uv[patch][curve][j-1] );
#endif
          }
        }
        else     // in vdir
        {
	  if(nurbsType==3)
	    /* this is the original line but its suspicious. changed from u to v:
	  { if(((pntproj[k].paramU[disti]-utol_ambig)<umin)||((pntproj[k].paramU[disti]+utol_ambig)>umax)) buf=1; else buf=0; }
	    */
          { if(((pntproj[k].paramV[disti]-vtol_ambig)<vmin)||((pntproj[k].paramV[disti]+vtol_ambig)>vmax)) buf=1; else buf=0; }
	  if(nurbsType==4)
          { if((pntproj[k].paramV[disti]-vtol_ambig)<vmin) buf=1; else buf=0; }
          if(nurbsType==5)
          { if((pntproj[k].paramV[disti]+vtol_ambig)>vmax) buf=1; else buf=0; }
          if(buf)  // on the collapsed edge
          {
            // additional point required       
            // use the u value from the previous point
#if TEST
            if(j>2) printf("use u-val from prev point:%f\n", nurbs[nr].uv[patch][curve][j-2] );
            else    printf("use u-val from prev point: (wait till end)\n" );
#endif
            if(j>2) { nurbs[nr].uv[patch][curve][j]=nurbs[nr].uv[patch][curve][j-2];j++; }
            else lastUflag=j++;
            nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramV[disti];

            // include a new point
            nppc[curve]++;
            if( (nurbs[nr].uv[patch][curve] = (GLfloat *)realloc( (GLfloat *)nurbs[nr].uv[patch][curve], ((nurbs[nr].np[patch][curve]+1+nppc[curve])*2)*sizeof(GLfloat) ))==NULL )
	      { printf(" ERROR: realloc failure in trimNurbs(), nurbs:%s can not be shaped\n\n", nurbs[nr].name);  return(-1); }

            // use the u value from the next point
            // the value is not really known now, save a flag for later correction
            distp=MAXVALUE; for(i=0; i<pntproj[k+1].n; i++) if(pntproj[k+1].dist[i]<distp)
            { distp=pntproj[k+1].dist[i]; nurbs[nr].uv[patch][curve][j]=pntproj[k+1].paramU[i]; }

            // check if the distance is "close"
            if((distp>MIN_DIST)&&(returnedPntsPerPnt<MAX_RETURNED_PNTS_PER_PNT))
            { returnedPntsPerPnt=MAX_RETURNED_PNTS_PER_PNT; delete[] inverted; goto finerProjection; }
            nextUflag=j++;
            nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramV[disti];
            pcollapsed=j/2;
#if TEST
            printf("collapsed at v:%f add pnt:%d\n", pntproj[k].paramV[disti], j/2 );
#endif
          }
          else // not on the collapsed edge
          {
            nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramU[disti];
            nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramV[disti]; 
#if TEST
            printf(" ori:%d uv:%f %f\n", k, nurbs[nr].uv[patch][curve][j-2], nurbs[nr].uv[patch][curve][j-1] );
#endif
          }
        }
      }
      else
      {
        nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramU[disti];
        nurbs[nr].uv[patch][curve][j++]=pntproj[k].paramV[disti];
	printf("add pnt:%d uv:%f %f  %d\n", j/2, pntproj[k].paramU[disti],pntproj[k].paramV[disti], nurbs[nr].np[patch][curve]);
      }
    }

    // if the coordinates of this point have to be used
    if(lastUflag>-1)
    { nurbs[nr].uv[patch][curve][lastUflag]=nurbs[nr].uv[patch][curve][j-2]; lastUflag=-1; } 
    if(lastVflag>-1)
    { nurbs[nr].uv[patch][curve][lastVflag]=nurbs[nr].uv[patch][curve][j-1]; lastVflag=-1; }

    // if a collapsed point exists, start the loop at that point
    if(pcollapsed!=-1)
    {
      nurbs[nr].sum_ambiguousPnts[patch][curve]=nppc[curve];
      n=0;
      double* uvbuf = new double [ (nurbs[nr].np[patch][curve]+nppc[curve])*2 +1 ];

#if TEST
      i=0;
      for(k=0; k<nurbs[nr].np[patch][curve]+nppc[curve]-1; k++)
      {
        printf(" pnt P%d %f",k, nurbs[nr].uv[patch][curve][i++] );
        printf("  %f\n", nurbs[nr].uv[patch][curve][i++] );
      }
#endif

      i=pcollapsed*2-2;
      for(k=0; k<nurbs[nr].np[patch][curve]+nppc[curve]-pcollapsed; k++)
      {
        //printf(" pnt P%d %f",k, nurbs[nr].uv[patch][curve][i++] );
        //printf("  %f\n", nurbs[nr].uv[patch][curve][i++] ); i--; i--;
        uvbuf[n++]=nurbs[nr].uv[patch][curve][i++];
        uvbuf[n++]=nurbs[nr].uv[patch][curve][i++];
      }
      i=0;
      for(k=0; k<pcollapsed-1; k++)
      {
        //printf(" pnt ! %f", nurbs[nr].uv[patch][curve][i++] );
        //printf("  %f\n", nurbs[nr].uv[patch][curve][i++] );
        uvbuf[n++]=nurbs[nr].uv[patch][curve][i++];
        uvbuf[n++]=nurbs[nr].uv[patch][curve][i++];
      }
      i=0;
      for(k=0; k<nurbs[nr].np[patch][curve]+nppc[curve]-1; k++)
      {
        //printf(" pnt ! %f", nurbs[nr].uv[patch][curve][i++] );
        //printf("  %f\n", nurbs[nr].uv[patch][curve][i++] );
        nurbs[nr].uv[patch][curve][i]=uvbuf[i]; i++;
        nurbs[nr].uv[patch][curve][i]=uvbuf[i]; i++;
      }
      delete[] uvbuf;
    }

    // else if points were skipped, start the loop at the fist skipped point
    else if(pskip)
    {
      double* uvbuf = new double [ (nurbs[nr].np[patch][curve])*2 +1 ];
      i=0;
      for(k=0; k<nurbs[nr].np[patch][curve]-1; k++)
      {
        //printf(" pnt P%d %f",k, nurbs[nr].uv[patch][curve][i++] );
        //printf("  %f\n", nurbs[nr].uv[patch][curve][i++] );
        uvbuf[i]=nurbs[nr].uv[patch][curve][i]; i++;
        uvbuf[i]=nurbs[nr].uv[patch][curve][i]; i++;
      }
      
      i=0;
      n=(nurbs[nr].np[patch][curve]-1-pskip)*2;
      for(k=0; k<pskip; k++)
      {
        //printf(" pnt ! %f", nurbs[nr].uv[patch][curve][i++] );
        //printf("  %f\n", nurbs[nr].uv[patch][curve][i++] );
        nurbs[nr].uv[patch][curve][i++]=uvbuf[n++];
        nurbs[nr].uv[patch][curve][i++]=uvbuf[n++];
      }
      n=0;
      for(k=0; k<nurbs[nr].np[patch][curve]-pskip-1; k++)
      {
        //printf(" pnt ! %f", nurbs[nr].uv[patch][curve][i++] );
        //printf("  %f\n", nurbs[nr].uv[patch][curve][i++] );
        nurbs[nr].uv[patch][curve][i++]=uvbuf[n++];
        nurbs[nr].uv[patch][curve][i++]=uvbuf[n++];
      }
      
      delete[] uvbuf;
    }

    // last point == 1st point
    nurbs[nr].uv[patch][curve][j++]=nurbs[nr].uv[patch][curve][0];
    nurbs[nr].uv[patch][curve][j++]=nurbs[nr].uv[patch][curve][1];

    // if the coordinates of this point have to be used for the previously duplicated one
    if(nextUflag>0)
    { nurbs[nr].uv[patch][curve][nextUflag]=nurbs[nr].uv[patch][curve][j-2]; } 
    if(nextVflag>0)
    { nurbs[nr].uv[patch][curve][nextVflag]=nurbs[nr].uv[patch][curve][j-1]; }

#if TEST
    // check if we have too many points now
    if(((nurbs[nr].np[patch][curve]+nppc[curve])*2)!=j)
    {
      printf(" ERROR too many points:%d/2 (!=%d) in curve)\n", j,nurbs[nr].np[patch][curve]);
      exit(-1);
    }
#endif
    n++;
   }

   // The "ball" type is difficult if the trimming loop extend in the ambiguous zone. To avoid this the nurbs is rotated in space.
   if((nurbsType==3)&&(moveFlag))
   {
     moveFlag=0;
     if( rotateBall(nurbs, nr, axis, patch, utol_ambig, vtol_ambig) ) goto finerProjection;
   }

   // adapt the nurbs-parameter
   for(curve=0; curve<nurbs[nr].nc[patch]; curve++) nurbs[nr].np[patch][curve]+=nppc[curve];

#if TEST
   i=0;
   for(curve=0; curve<nurbs[nr].nc[patch]; curve++)
    for(j=0; j<nurbs[nr].np[patch][curve]; j++)
    {
      printf(" pnt ! %e",  nurbs[nr].uv[patch][curve][i++] );
      printf("  %e\n", nurbs[nr].uv[patch][curve][i++] );
    }
#endif

 }

#if FAST_SHADING
  // for the moment use the coarsest resolution:
  nurbs[nr].ustep[patch]=nurbs[nr].u_exp*4./(umax-umin);
  nurbs[nr].vstep[patch]=nurbs[nr].v_exp*4./(vmax-vmin);
#else
  /* calc the average local spacing between the trimming points (only outer loop) */
  av_local_elength=0.;
  i=0;
  p0[0]=nurbs[nr].xyz[patch][0][i++];
  p0[1]=nurbs[nr].xyz[patch][0][i++];
  p0[2]=nurbs[nr].xyz[patch][0][i++];
  for(j=0; j<nurbs[nr].np[patch][0]-1; j++)
  {
    p1[0]=nurbs[nr].xyz[patch][0][i++];
    p1[1]=nurbs[nr].xyz[patch][0][i++];
    p1[2]=nurbs[nr].xyz[patch][0][i++];
    v_result(p0,p1,p0p1);
    av_local_elength+=v_betrag(p0p1);
    //printf("pnt ! %f %f %f  av_local_elength %f\n", p1[0],p1[1],p1[2], av_local_elength);
    p0[0]=p1[0];
    p0[1]=p1[1];
    p0[2]=p1[2];
  }
  av_local_elength/=nurbs[nr].np[patch][0]-1;

  // average resolution based on gtol which is related to the width of the model and was used in calcNurbsResolution()
  nurbs[nr].ustep[patch]=nurbs[nr].ures/av_local_elength/4;
  nurbs[nr].vstep[patch]=nurbs[nr].vres/av_local_elength/4;
  //printf("uvres %f %f elength %f uvstep %d %d\n", nurbs[nr].ures,nurbs[nr].vres,av_local_elength,nurbs[nr].ustep[patch],nurbs[nr].vstep[patch]);

  // avoid bad values
  if(nurbs[nr].ustep[patch]<(nurbs[nr].u_exp*2./(umax-umin))) nurbs[nr].ustep[patch]=nurbs[nr].u_exp*2./(umax-umin);
  if(nurbs[nr].vstep[patch]<(nurbs[nr].u_exp*2./(vmax-vmin))) nurbs[nr].vstep[patch]=nurbs[nr].v_exp*2./(vmax-vmin);
  if(nurbs[nr].ustep[patch]>CGX_GLU_MAX_STEPS) nurbs[nr].ustep[patch]=CGX_GLU_MAX_STEPS;
  if(nurbs[nr].vstep[patch]>CGX_GLU_MAX_STEPS) nurbs[nr].vstep[patch]=CGX_GLU_MAX_STEPS;

  if(nurbs[nr].ustep[patch]<nurbs[nr].u_exp*4./(umax-umin)) nurbs[nr].ustep[patch]=nurbs[nr].u_exp*4./(umax-umin);
  if(nurbs[nr].vstep[patch]<nurbs[nr].v_exp*4./(vmax-vmin)) nurbs[nr].vstep[patch]=nurbs[nr].v_exp*4./(vmax-vmin);

#endif

  delete[] pntproj;
  delete[] nppc;         // Release number of points per curve object
  delete surface;      // Release surface object.
  delete[] inverted;   // Release snlVertex array returned from projection function.
  delete[] toProject;  // Release points that were inverted onto surface.

  // The rest of the objects are deleted by libSNL.
  return(0);
}
