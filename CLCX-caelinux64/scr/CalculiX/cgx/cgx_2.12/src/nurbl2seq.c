#include <cgx.h>
#include <assert.h>

extern Scale     scale[1];
extern Points *point;

/**/
typedef struct {double x, y, z, w;} wpoint;

typedef struct 
{
	int    n;
        int    degree;
	wpoint  *polygon;
	double *knots;
}
nurbs;

wpoint *pointAt(const nurbs *curve, double u)
{
        static wpoint p;
	wpoint old, new;
	double alpha, knoti, knotikj;
	wpoint *olda, *newa;
	wpoint *oldd, *newd, *tmp;

	wpoint *polygon;
	double *knots;
	int n;

	int r, i, j, k, l;

	polygon = curve->polygon;
	knots = curve->knots;
	n = curve->n;
	k = curve->degree+1;

	if( (olda = (wpoint *) malloc((int)(k+1) * sizeof(wpoint)))==NULL)
        { printf("ERROR: malloc failed in pointAt()\n"); return(NULL); }
	if((newa  = (wpoint *) malloc((int)(k+1) * sizeof(wpoint)))==NULL)
        { printf("ERROR: malloc failed in pointAt()\n"); return(NULL); }

        if(knots[0]>0.)
        {
          for (i=n+k-1; i>=0; i--)
            knots[i] -= knots[0];
          for (i=0; i<n+k; i++)
            knots[i] /= knots[n+k-1];
          /*          assert(fabs(knots[0]) <= DBL_EPSILON);
          assert(fabs(knots[n+k-1]-1.) <= DBL_EPSILON);
          */
        }

	oldd = olda;
	newd = newa;
	/* find span r */
	for (i = curve->degree; i < curve->n; i++)
		if (u < knots[i]) break;
	r = i - 1;

	l = r-k+1;
	if (l<0)       oldd[0] = polygon[l+n];
	else if (l>=n) oldd[0] = polygon[l-n];
	else           oldd[0] = polygon[l];
	for (i = r-k+1+1; i<=r; i++)
	{
		if (i<0)       newd[0] = polygon[i+n];
		else if (i>=n) newd[0] = polygon[i-n];
		else           newd[0] = polygon[i];
		if (i<0)           knoti = knots[i+n-1] - 1.;
		else if (i>=n)	   knoti = 1. + knots[i-n+1];
		else               knoti = knots[i];
		for (j=1; j<i-r+k; j++)
		{
			l = i+k-j;
			if (l<0)           knotikj = knots[l+n-1] - 1.;
			else if (l>=n) knotikj = 1. + knots[l-n+1];
			else               knotikj = knots[l];
                        if((knotikj - knoti)==0.) return(NULL);
			else alpha = (u-knoti) / (knotikj - knoti);
			/* newd[j] = (1-alpha)*oldd[j-1] + alpha*newd[j-1]; */
			old = oldd[j-1];
			new = newd[j-1];
			p.x = (1-alpha)*old.x + alpha*new.x;
			p.y = (1-alpha)*old.y + alpha*new.y;
			p.z = (1-alpha)*old.z + alpha*new.z;
			p.w = (1-alpha)*old.w + alpha*new.w;
			newd[j] = p;
		}
		/* swap newd oldd */
		tmp = newd;
		newd = oldd;
		oldd = tmp;
	}
	p = oldd[k-1];
        free(olda);
        free(newa);
        return(&p);
}

int nurbl2seq(int nurlNr, const Nurbl *nurbl)
{
  int   setNr=-1;
  double u, du, du_ini, v[2][3], e[2][3], cos_a, p0[3]={0.,0.,0.}, p1[3];
  wpoint *p2=NULL, *cp=NULL;
  int i, pnr=0, loop=0;
  nurbs nurbsi;
  char name[MAX_LINE_LENGTH];

  /* printf("nurlname:%s p:%d k:%d e:%d\n",nurbl[nurlNr].name,nurbl[nurlNr].u_npnt,nurbl[nurlNr].u_nknt,nurbl[nurlNr].u_exp); */

  nurbsi.n = nurbl[nurlNr].u_npnt;
  nurbsi.degree = nurbl[nurlNr].u_exp;
  nurbsi.polygon=NULL;
  nurbsi.knots=NULL;

  getNewName( name, "se" );
  if( (setNr=pre_seta( name, "is", 0)) <0 ) return(-1);

  if( (cp = (wpoint *)malloc((nurbl[nurlNr].u_npnt+1)*sizeof(wpoint) ) )==NULL) 
  { printf(" ERROR: malloc failure\n"); goto mfail; }
  if( (nurbsi.knots = (double *)malloc((nurbl[nurlNr].u_nknt+1)*sizeof(double) ) )==NULL) 
  { printf(" ERROR: malloc failure\n"); goto mfail; }

  for(i=0; i<nurbl[nurlNr].u_npnt; i++)
  {
    cp[i].x=(double)nurbl[nurlNr].weight[i]*point[nurbl[nurlNr].ctlpnt[i]].px; 
    cp[i].y=(double)nurbl[nurlNr].weight[i]*point[nurbl[nurlNr].ctlpnt[i]].py; 
    cp[i].z=(double)nurbl[nurlNr].weight[i]*point[nurbl[nurlNr].ctlpnt[i]].pz; 
    cp[i].w=(double)nurbl[nurlNr].weight[i];
    /*
    cp[i].x-=scale->x; 
    cp[i].y-=scale->y; 
    cp[i].z-=scale->z; 
    cp[i].x/=scale->w; 
    cp[i].y/=scale->w; 
    cp[i].z/=scale->w;
    */ 
  } 
  nurbsi.polygon=cp;
  for(i=0; i<nurbl[nurlNr].u_nknt; i++)
  {
    nurbsi.knots[i]=(double)nurbl[nurlNr].uknt[i]/nurbl[nurlNr].uknt[nurbl[nurlNr].u_nknt-1];
  } 


  seta( setNr, "p", nurbl[nurlNr].ctlpnt[0]  );
  u=du=du_ini=.01/nurbl[nurlNr].u_npnt;
  p1[0]=point[nurbl[nurlNr].ctlpnt[0]].px;
  p1[1]=point[nurbl[nurlNr].ctlpnt[0]].py;
  p1[2]=point[nurbl[nurlNr].ctlpnt[0]].pz;
  /*
  p1[0]-=scale->x; 
  p1[1]-=scale->y; 
  p1[2]-=scale->z; 
  p1[0]/=scale->w; 
  p1[1]/=scale->w; 
  p1[2]/=scale->w;
  */ 
  i=0;
  while(u<1.)
  {
    /* check the angle between last 3 points */
    /* if to big or too small create new points */
    p2=pointAt(&nurbsi, (double)u);
    if(p2)
    {
      p2->x/=p2->w;
      p2->y/=p2->w;
      p2->z/=p2->w;
      v_result(p1,(double *)&p2->x, v[i]);
      v_norm(v[i], e[i]);
      i++;
      if(i>1)
      {
        i=0;
        cos_a= v_sprod(e[0], e[1]);
        /* printf("du_ini:%f cos_a:%f du:%f p2:%f %f %f  v0:%f %f %f  v1:%f %f %f\n",du_ini, cos_a,du, p2->x,p2->y,p2->z, v[0][0], v[0][1], v[0][2], v[1][0], v[1][1], v[1][2] ); */
        if((cos_a<GTOL_COS_A)&&(loop<10))
        {
          loop++;
          u=u-2.*du;
          du=du*0.75;
          p1[0]=p0[0];
          p1[1]=p0[1];
          p1[2]=p0[2];
        }
        else if((cos_a>0.999)&&(loop<10))
        {
          loop++;
          u=u-2.*du;
          du=du*1.5;
          p1[0]=p0[0];
          p1[1]=p0[1];
          p1[2]=p0[2];
        }
        else
        {
          loop=0;
          du=du_ini;
          getNewName( name, "p" );
          pnr= pnt( name, p2->x, p2->y, p2->z, 0 );
          seta( setNr, "p", pnr );
        }
      }
      else
      {
        p0[0]=p1[0];
        p0[1]=p1[1];
        p0[2]=p1[2];
        p1[0]=p2->x;
        p1[1]=p2->y;
        p1[2]=p2->z;
        /*  printf("%d  p0:%f %f %f \n",i,p0[0],p0[1],p0[2] ); 
            printf("%d  p1:%f %f %f \n",i,p1[0],p1[1],p1[2] ); */
      }
    }
    else
    {
      printf(" WARNING: nurbl:%s is evaluated with errors at u:%lf\n", nurbl[nurlNr].name, u); 
      goto mfail;
    }
    u+=du;
  }
  seta( setNr, "p", nurbl[nurlNr].ctlpnt[nurbl[nurlNr].u_npnt-1]  );
  

 mfail:;
  free(nurbsi.polygon);
  free(nurbsi.knots);
  return(setNr);
}
