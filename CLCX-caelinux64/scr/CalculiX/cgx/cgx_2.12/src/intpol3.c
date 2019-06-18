/* ---------------------------------------------------------------- */
/* interpoliert zwischen x(i) und x(i-1)                            */
/* es wird auch  extrapoliert.                                      */
/* method=0 lineare interpol, method=1 spline interpol              */
/* x0: Stelle auf die interpoliert wird                             */
/* y0: Rueckgabewert                                                */
/* wenn extrapol. wurde ist method nach der ausfuehrung=1, sonst 0  */
/* ---------------------------------------------------------------- */
#include <extUtil.h>



int splev_( double *t, int *n, double *c__, int *k, double *x, double *y, int *m, int *ier);

int curfit_( int *iopt,int *m, double *x,double *y,double *w,double *xb,double *xe,int *k,double *s,int *nest,int *n,double *t, double*c__,double *fp,double *wrk,int *lwrk,int *iwrk,int *ier);

int fpback_(double *a,double *z__, int *n,int *k, double *c__, int *nest);
int fpbspl_(double *t,int *n,int *k,double *x,int *l,double *h__);

int fpchec_(double *x,int *m,double *t,int *n,int *k,int *ier);

int fpcurf_(int *iopt,double *x,double *y,double *w,int *m,double *xb,double *xe,int *k,double *s,int *nest,double *tol,int *maxit,int *k1,int *k2,int *n,double *t,double *c__,double *fp,double *fpint,double *z__,double *a,double *b,double *g,double *q,int *nrdata,int *ier);

int fpdisc_(double *t,int *n,int *k2,double *b,int *nest);

int fpgivs_(double *piv,double *ww,double *cos__,double *sin__);

int fpknot_(double *x,int *m,double *t,int *n,double *fpint,int *nrdata,int *nrint,int *nest,int *istart);

int fprota_(double *cos__,double *sin__,double *a,double *b);

double fprati_(double *p1,double *f1,double *p2,double *f2,double *p3,double *f3);






/* splev.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/* Subroutine */ int splev_(t, n, c__, k, x, y, m, ier)
double *t;
int *n;
double *c__;
int *k;
double *x, *y;
int *m, *ier;
{
    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static double h__[6];
    static int i__, j, l, k1, l1;
    static double tb;
    static int ll;
    static double te, sp;
    static int nk1;
    static double arg;

/*  subroutine splev evaluates in a number of points x(i),i=1,2,...,m */
/*  a spline s(x) of degree k, given in its b-spline representation. */

/*  calling sequence: */
/*     call splev(t,n,c,k,x,y,m,ier) */

/*  input parameters: */
/*    t    : array,length n, which contains the position of the knots. */
/*    n    : int, giving the total number of knots of s(x). */
/*    c    : array,length n, which contains the b-spline coefficients. */
/*    k    : int, giving the degree of s(x). */
/*    x    : array,length m, which contains the points where s(x) must */
/*           be evaluated. */
/*    m    : int, giving the number of points where s(x) must be */
/*           evaluated. */

/*  output parameter: */
/*    y    : array,length m, giving the value of s(x) at the different */
/*           points. */
/*    ier  : error flag */
/*      ier = 0 : normal return */
/*      ier =10 : invalid input data (see restrictions) */

/*  restrictions: */
/*    m >= 1 */
/*    t(k+1) <= x(i) <= x(i+1) <= t(n-k) , i=1,2,...,m-1. */

/*  other subroutines required: fpbspl. */

/*  references : */
/*    de boor c  : on calculating with b-splines, j. approximation theory */
/*                 6 (1972) 50-62. */
/*    cox m.g.   : the numerical evaluation of b-splines, j. inst. maths */
/*                 applics 10 (1972) 134-149. */
/*    dierckx p. : curve and surface fitting with splines, monographs on */
/*                 numerical analysis, oxford university press, 1993. */

/*  author : */
/*    p.dierckx */
/*    dept. computer science, k.u.leuven */
/*    celestijnenlaan 200a, b-3001 heverlee, belgium. */
/*    e-mail : Paul.Dierckx@cs.kuleuven.ac.be */

/*  latest update : march 1987 */

/*  ..scalar arguments.. */
/*  ..array arguments.. */
/*  ..local scalars.. */
/*  ..local array.. */
/*  .. */
/*  before starting computations a data check is made. if the input data */
/*  are invalid control is immediately repassed to the calling program. */
    /* Parameter adjustments */
    --c__;
    --t;
    --y;
    --x;

    /* Function Body */
    *ier = 10;
    if ((i__1 = *m - 1) < 0) {
	goto L100;
    } else if (i__1 == 0) {
	goto L30;
    } else {
	goto L10;
    }
L10:
    i__1 = *m;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__] < x[i__ - 1]) {
	    goto L100;
	}
/* L20: */
    }
L30:
    *ier = 0;
/*  fetch tb and te, the boundaries of the approximation interval. */
    k1 = *k + 1;
    nk1 = *n - k1;
    tb = t[k1];
    te = t[nk1 + 1];
    l = k1;
    l1 = l + 1;
/*  main loop for the different points. */
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*  fetch a new x-value arg. */
	arg = x[i__];
	if (arg < tb) {
	    arg = tb;
	}
	if (arg > te) {
	    arg = te;
	}
/*  search for knot interval t(l) <= arg < t(l+1) */
L40:
	if (arg < t[l1] || l == nk1) {
	    goto L50;
	}
	l = l1;
	l1 = l + 1;
	goto L40;
/*  evaluate the non-zero b-splines at arg. */
L50:
	fpbspl_(&t[1], n, k, &arg, &l, h__);
/*  find the value of s(x) at x=arg. */
	sp = (double)0.;
	ll = l - k1;
	i__2 = k1;
	for (j = 1; j <= i__2; ++j) {
	    ++ll;
	    sp += c__[ll] * h__[j - 1];
/* L60: */
	}
	y[i__] = sp;
/* L80: */
    }
L100:
    return 0;
} /* splev_ */

/* curfit.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/* Table of constant values */

static int c__1 = 1;

/* Subroutine */ int curfit_(iopt, m, x, y, w, xb, xe, k, s, nest, n, t, c__, 
	fp, wrk, lwrk, iwrk, ier)
int *iopt, *m;
double *x, *y, *w, *xb, *xe;
int *k;
double *s;
int *nest, *n;
double *t, *c__, *fp, *wrk;
int *lwrk, *iwrk, *ier;
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static int nmin, i__, j, maxit, k1, k2, lwest, ia, ib, ig;
    static int iq, iz;
    static int ifp;
    static double tol;

/*  given the set of data points (x(i),y(i)) and the set of positive */
/*  numbers w(i),i=1,2,...,m,subroutine curfit determines a smooth spline */
/*  approximation of degree k on the interval xb <= x <= xe. */
/*  if iopt=-1 curfit calculates the weighted least-squares spline */
/*  according to a given set of knots. */
/*  if iopt>=0 the number of knots of the spline s(x) and the position */
/*  t(j),j=1,2,...,n is chosen automatically by the routine. the smooth- */
/*  ness of s(x) is then achieved by minimalizing the discontinuity */
/*  jumps of the k-th derivative of s(x) at the knots t(j),j=k+2,k+3,..., */
/*  n-k-1. the amount of smoothness is determined by the condition that */
/*  f(p)=sum((w(i)*(y(i)-s(x(i))))**2) be <= s, with s a given non- */
/*  negative constant, called the smoothing factor. */
/*  the fit s(x) is given in the b-spline representation (b-spline coef- */
/*  ficients c(j),j=1,2,...,n-k-1) and can be evaluated by means of */
/*  subroutine splev. */

/*  calling sequence: */
/*     call curfit(iopt,m,x,y,w,xb,xe,k,s,nest,n,t,c,fp,wrk, */
/*    * lwrk,iwrk,ier) */

/*  parameters: */
/*   iopt  : int flag. on entry iopt must specify whether a weighted */
/*           least-squares spline (iopt=-1) or a smoothing spline (iopt= */
/*           0 or 1) must be determined. if iopt=0 the routine will start */
/*           with an initial set of knots t(i)=xb, t(i+k+1)=xe, i=1,2,... */
/*           k+1. if iopt=1 the routine will continue with the knots */
/*           found at the last call of the routine. */
/*           attention: a call with iopt=1 must always be immediately */
/*           preceded by another call with iopt=1 or iopt=0. */
/*           unchanged on exit. */
/*   m     : int. on entry m must specify the number of data points. */
/*           m > k. unchanged on exit. */
/*   x     : real array of dimension at least (m). before entry, x(i) */
/*           must be set to the i-th value of the independent variable x, */
/*           for i=1,2,...,m. these values must be supplied in strictly */
/*           ascending order. unchanged on exit. */
/*   y     : real array of dimension at least (m). before entry, y(i) */
/*           must be set to the i-th value of the dependent variable y, */
/*           for i=1,2,...,m. unchanged on exit. */
/*   w     : real array of dimension at least (m). before entry, w(i) */
/*           must be set to the i-th value in the set of weights. the */
/*           w(i) must be strictly positive. unchanged on exit. */
/*           see also further comments. */
/*   xb,xe : real values. on entry xb and xe must specify the boundaries */
/*           of the approximation interval. xb<=x(1), xe>=x(m). */
/*           unchanged on exit. */
/*   k     : int. on entry k must specify the degree of the spline. */
/*           1<=k<=5. it is recommended to use cubic splines (k=3). */
/*           the user is strongly dissuaded from choosing k even,together */
/*           with a small s-value. unchanged on exit. */
/*   s     : real.on entry (in case iopt>=0) s must specify the smoothing */
/*           factor. s >=0. unchanged on exit. */
/*           for advice on the choice of s see further comments. */
/*   nest  : int. on entry nest must contain an over-estimate of the */
/*           total number of knots of the spline returned, to indicate */
/*           the storage space available to the routine. nest >=2*k+2. */
/*           in most practical situation nest=m/2 will be sufficient. */
/*           always large enough is  nest=m+k+1, the number of knots */
/*           needed for interpolation (s=0). unchanged on exit. */
/*   n     : int. */
/*           unless ier =10 (in case iopt >=0), n will contain the */
/*           total number of knots of the spline approximation returned. */
/*           if the computation mode iopt=1 is used this value of n */
/*           should be left unchanged between subsequent calls. */
/*           in case iopt=-1, the value of n must be specified on entry. */
/*   t     : real array of dimension at least (nest). */
/*           on succesful exit, this array will contain the knots of the */
/*           spline,i.e. the position of the interior knots t(k+2),t(k+3) */
/*           ...,t(n-k-1) as well as the position of the additional knots */
/*           t(1)=t(2)=...=t(k+1)=xb and t(n-k)=...=t(n)=xe needed for */
/*           the b-spline representation. */
/*           if the computation mode iopt=1 is used, the values of t(1), */
/*           t(2),...,t(n) should be left unchanged between subsequent */
/*           calls. if the computation mode iopt=-1 is used, the values */
/*           t(k+2),...,t(n-k-1) must be supplied by the user, before */
/*           entry. see also the restrictions (ier=10). */
/*   c     : real array of dimension at least (nest). */
/*           on succesful exit, this array will contain the coefficients */
/*           c(1),c(2),..,c(n-k-1) in the b-spline representation of s(x) */
/*   fp    : real. unless ier=10, fp contains the weighted sum of */
/*           squared residuals of the spline approximation returned. */
/*   wrk   : real array of dimension at least (m*(k+1)+nest*(7+3*k)). */
/*           used as working space. if the computation mode iopt=1 is */
/*           used, the values wrk(1),...,wrk(n) should be left unchanged */
/*           between subsequent calls. */
/*   lwrk  : int. on entry,lwrk must specify the actual dimension of */
/*           the array wrk as declared in the calling (sub)program.lwrk */
/*           must not be too small (see wrk). unchanged on exit. */
/*   iwrk  : int array of dimension at least (nest). */
/*           used as working space. if the computation mode iopt=1 is */
/*           used,the values iwrk(1),...,iwrk(n) should be left unchanged */
/*           between subsequent calls. */
/*   ier   : int. unless the routine detects an error, ier contains a */
/*           non-positive value on exit, i.e. */
/*    ier=0  : normal return. the spline returned has a residual sum of */
/*             squares fp such that abs(fp-s)/s <= tol with tol a relat- */
/*             ive tolerance set to 0.001 by the program. */
/*    ier=-1 : normal return. the spline returned is an interpolating */
/*             spline (fp=0). */
/*    ier=-2 : normal return. the spline returned is the weighted least- */
/*             squares polynomial of degree k. in this extreme case fp */
/*             gives the upper bound fp0 for the smoothing factor s. */
/*    ier=1  : error. the required storage space exceeds the available */
/*             storage space, as specified by the parameter nest. */
/*             probably causes : nest too small. if nest is already */
/*             large (say nest > m/2), it may also indicate that s is */
/*             too small */
/*             the approximation returned is the weighted least-squares */
/*             spline according to the knots t(1),t(2),...,t(n). (n=nest) */
/*             the parameter fp gives the corresponding weighted sum of */
/*             squared residuals (fp>s). */
/*    ier=2  : error. a theoretically impossible result was found during */
/*             the iteration proces for finding a smoothing spline with */
/*             fp = s. probably causes : s too small. */
/*             there is an approximation returned but the corresponding */
/*             weighted sum of squared residuals does not satisfy the */
/*             condition abs(fp-s)/s < tol. */
/*    ier=3  : error. the maximal number of iterations maxit (set to 20 */
/*             by the program) allowed for finding a smoothing spline */
/*             with fp=s has been reached. probably causes : s too small */
/*             there is an approximation returned but the corresponding */
/*             weighted sum of squared residuals does not satisfy the */
/*             condition abs(fp-s)/s < tol. */
/*    ier=10 : error. on entry, the input data are controlled on validity */
/*             the following restrictions must be satisfied. */
/*             -1<=iopt<=1, 1<=k<=5, m>k, nest>2*k+2, w(i)>0,i=1,2,...,m */
/*             xb<=x(1)<x(2)<...<x(m)<=xe, lwrk>=(k+1)*m+nest*(7+3*k) */
/*             if iopt=-1: 2*k+2<=n<=smin(nest,m+k+1) */
/*                         xb<t(k+2)<t(k+3)<...<t(n-k-1)<xe */
/*                       the schoenberg-whitney conditions, i.e. there */
/*                       must be a subset of data points xx(j) such that */
/*                         t(j) < xx(j) < t(j+k+1), j=1,2,...,n-k-1 */
/*             if iopt>=0: s>=0 */
/*                         if s=0 : nest >= m+k+1 */
/*             if one of these conditions is found to be violated,control */
/*             is immediately repassed to the calling program. in that */
/*             case there is no approximation returned. */

/*  further comments: */
/*   by means of the parameter s, the user can control the tradeoff */
/*   between closeness of fit and smoothness of fit of the approximation. */
/*   if s is too large, the spline will be too smooth and signal will be */
/*   lost ; if s is too small the spline will pick up too much noise. in */
/*   the extreme cases the program will return an interpolating spline if */
/*   s=0 and the weighted least-squares polynomial of degree k if s is */
/*   very large. between these extremes, a properly chosen s will result */
/*   in a good compromise between closeness of fit and smoothness of fit. */
/*   to decide whether an approximation, corresponding to a certain s is */
/*   satisfactory the user is highly recommended to inspect the fits */
/*   graphically. */
/*   recommended values for s depend on the weights w(i). if these are */
/*   taken as 1/d(i) with d(i) an estimate of the standard deviation of */
/*   y(i), a good s-value should be found in the range (m-sqrt(2*m),m+ */
/*   sqrt(2*m)). if nothing is known about the statistical error in y(i) */
/*   each w(i) can be set equal to one and s determined by trial and */
/*   error, taking account of the comments above. the best is then to */
/*   start with a very large value of s ( to determine the least-squares */
/*   polynomial and the corresponding upper bound fp0 for s) and then to */
/*   progressively decrease the value of s ( say by a factor 10 in the */
/*   beginning, i.e. s=fp0/10, fp0/100,...and more carefully as the */
/*   approximation shows more detail) to obtain closer fits. */
/*   to economize the search for a good s-value the program provides with */
/*   different modes of computation. at the first call of the routine, or */
/*   whenever he wants to restart with the initial set of knots the user */
/*   must set iopt=0. */
/*   if iopt=1 the program will continue with the set of knots found at */
/*   the last call of the routine. this will save a lot of computation */
/*   time if curfit is called repeatedly for different values of s. */
/*   the number of knots of the spline returned and their location will */
/*   depend on the value of s and on the complexity of the shape of the */
/*   function underlying the data. but, if the computation mode iopt=1 */
/*   is used, the knots returned may also depend on the s-values at */
/*   previous calls (if these were smaller). therefore, if after a number */
/*   of trials with different s-values and iopt=1, the user can finally */
/*   accept a fit as satisfactory, it may be worthwhile for him to call */
/*   curfit once more with the selected value for s but now with iopt=0. */
/*   indeed, curfit may then return an approximation of the same quality */
/*   of fit but with fewer knots and therefore better if data reduction */
/*   is also an important objective for the user. */

/*  other subroutines required: */
/*    fpback,fpbspl,fpchec,fpcurf,fpdisc,fpgivs,fpknot,fprati,fprota */

/*  references: */
/*   dierckx p. : an algorithm for smoothing, differentiation and integ- */
/*                ration of experimental data using spline functions, */
/*                j.comp.appl.maths 1 (1975) 165-184. */
/*   dierckx p. : a fast algorithm for smoothing data on a rectangular */
/*                grid while using spline functions, siam j.numer.anal. */
/*                19 (1982) 1286-1304. */
/*   dierckx p. : an improved algorithm for curve fitting with spline */
/*                functions, report tw54, dept. computer science,k.u. */
/*                leuven, 1981. */
/*   dierckx p. : curve and surface fitting with splines, monographs on */
/*                numerical analysis, oxford university press, 1993. */

/*  author: */
/*    p.dierckx */
/*    dept. computer science, k.u. leuven */
/*    celestijnenlaan 200a, b-3001 heverlee, belgium. */
/*    e-mail : Paul.Dierckx@cs.kuleuven.ac.be */

/*  creation date : may 1979 */
/*  latest update : march 1987 */

/*  .. */
/*  ..scalar arguments.. */
/*  ..array arguments.. */
/*  ..local scalars.. */
/*  .. */
/*  we set up the parameters tol and maxit */
    /* Parameter adjustments */
    --w;
    --y;
    --x;
    --iwrk;
    --c__;
    --t;
    --wrk;

    /* Function Body */
    maxit = 20;
    tol = (double).001;
/*  before starting computations a data check is made. if the input data */
/*  are invalid, control is immediately repassed to the calling program. */
    *ier = 10;
    if (*k <= 0 || *k > 5) {
	goto L50;
    }
    k1 = *k + 1;
    k2 = k1 + 1;
    if (*iopt < -1 || *iopt > 1) {
	goto L50;
    }
    nmin = k1 << 1;
    if (*m < k1 || *nest < nmin) {
	goto L50;
    }
    lwest = *m * k1 + *nest * (*k * 3 + 7);
    if (*lwrk < lwest) {
	goto L50;
    }
    if (*xb > x[1] || *xe < x[*m] || w[1] <= (double)0.) {
	goto L50;
    }
    i__1 = *m;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (x[i__ - 1] >= x[i__] || w[i__] <= (double)0.) {
	    goto L50;
	}
/* L10: */
    }
    if (*iopt >= 0) {
	goto L30;
    }
    if (*n < nmin || *n > *nest) {
	goto L50;
    }
    j = *n;
    i__1 = k1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	t[i__] = *xb;
	t[j] = *xe;
	--j;
/* L20: */
    }
    fpchec_(&x[1], m, &t[1], n, k, ier);
    if (*ier != 0) {
	goto L50;
    } else {
	goto L40;
    }
L30:
    if (*s < (double)0.) {
	goto L50;
    }
    if (*s == (double)0. && *nest < *m + k1) {
	goto L50;
    }
    *ier = 0;
/* we partition the working space and determine the spline approximation. */
L40:
    ifp = 1;
    iz = ifp + *nest;
    ia = iz + *nest;
    ib = ia + *nest * k1;
    ig = ib + *nest * k2;
    iq = ig + *nest * k2;
    fpcurf_(iopt, &x[1], &y[1], &w[1], m, xb, xe, k, s, nest, &tol, &maxit, &
	    k1, &k2, n, &t[1], &c__[1], fp, &wrk[ifp], &wrk[iz], &wrk[ia], &
	    wrk[ib], &wrk[ig], &wrk[iq], &iwrk[1], ier);
L50:
    return 0;
} /* curfit_ */

/* Subroutine */ int fpback_(a, z__, n, k, c__, nest)
double *a, *z__;
int *n, *k;
double *c__;
int *nest;
{
    /* System generated locals */
    int a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    static int i__, j, l, m, i1;
    static double store;
    static int k1;

/*  subroutine fpback calculates the solution of the system of */
/*  equations a*c = z with a a n x n upper triangular matrix */
/*  of bandwidth k. */
/*  .. */
/*  ..scalar arguments.. */
/*  ..array arguments.. */
/*  ..local scalars.. */
/*  .. */
    /* Parameter adjustments */
    --c__;
    --z__;
    a_dim1 = *nest;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;

    /* Function Body */
    k1 = *k - 1;
    c__[*n] = z__[*n] / a[*n + a_dim1];
    i__ = *n - 1;
    if (i__ == 0) {
	goto L30;
    }
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	store = z__[i__];
	i1 = k1;
	if (j <= k1) {
	    i1 = j - 1;
	}
	m = i__;
	i__2 = i1;
	for (l = 1; l <= i__2; ++l) {
	    ++m;
	    store -= c__[m] * a[i__ + (l + 1) * a_dim1];
/* L10: */
	}
	c__[i__] = store / a[i__ + a_dim1];
	--i__;
/* L20: */
    }
L30:
    return 0;
} /* fpback_ */

/* Subroutine */ int fpbspl_(t, n, k, x, l, h__)
double *t;
int *n, *k;
double *x;
int *l;
double *h__;
{
    /* System generated locals */
    int i__1, i__2;

    /* Local variables */
    static double f;
    static int i__, j;
    static double hh[5];
    static int li, lj;
    static double one;

/*  subroutine fpbspl evaluates the (k+1) non-zero b-splines of */
/*  degree k at t(l) <= x < t(l+1) using the stable recurrence */
/*  relation of de boor and cox. */
/*  .. */
/*  ..scalar arguments.. */
/*  ..array arguments.. */
/*  ..local scalars.. */
/*  ..local arrays.. */
/*  .. */
    /* Parameter adjustments */
    --t;
    --h__;

    /* Function Body */
    one = (double)1.;
    h__[1] = one;
    i__1 = *k;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    hh[i__ - 1] = h__[i__];
/* L10: */
	}
	h__[1] = (double)0.;
	i__2 = j;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    li = *l + i__;
	    lj = li - j;
	    f = hh[i__ - 1] / (t[li] - t[lj]);
	    h__[i__] += f * (t[li] - *x);
	    h__[i__ + 1] = f * (*x - t[lj]);
/* L20: */
	}
    }
    return 0;
} /* fpbspl_ */

/* Subroutine */ int fpchec_(x, m, t, n, k, ier)
double *x;
int *m;
double *t;
int *n, *k, *ier;
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static int i__, j, l, k1, k2;
    static double tj, tl;
    static int nk1, nk2, nk3;

/*  subroutine fpchec verifies the number and the position of the knots */
/*  t(j),j=1,2,...,n of a spline of degree k, in relation to the number */
/*  and the position of the data points x(i),i=1,2,...,m. if all of the */
/*  following conditions are fulfilled, the error parameter ier is set */
/*  to zero. if one of the conditions is violated ier is set to ten. */
/*      1) k+1 <= n-k-1 <= m */
/*      2) t(1) <= t(2) <= ... <= t(k+1) */
/*         t(n-k) <= t(n-k+1) <= ... <= t(n) */
/*      3) t(k+1) < t(k+2) < ... < t(n-k) */
/*      4) t(k+1) <= x(i) <= t(n-k) */
/*      5) the conditions specified by schoenberg and whitney must hold */
/*         for at least one subset of data points, i.e. there must be a */
/*         subset of data points y(j) such that */
/*             t(j) < y(j) < t(j+k+1), j=1,2,...,n-k-1 */
/*  .. */
/*  ..scalar arguments.. */
/*  ..array arguments.. */
/*  ..local scalars.. */
/*  .. */
    /* Parameter adjustments */
    --x;
    --t;

    /* Function Body */
    k1 = *k + 1;
    k2 = k1 + 1;
    nk1 = *n - k1;
    nk2 = nk1 + 1;
    *ier = 10;
/*  check condition no 1 */
    if (nk1 < k1 || nk1 > *m) {
	goto L80;
    }
/*  check condition no 2 */
    j = *n;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (t[i__] > t[i__ + 1]) {
	    goto L80;
	}
	if (t[j] < t[j - 1]) {
	    goto L80;
	}
	--j;
/* L20: */
    }
/*  check condition no 3 */
    i__1 = nk2;
    for (i__ = k2; i__ <= i__1; ++i__) {
	if (t[i__] <= t[i__ - 1]) {
	    goto L80;
	}
/* L30: */
    }
/*  check condition no 4 */
    if (x[1] < t[k1] || x[*m] > t[nk2]) {
	goto L80;
    }
/*  check condition no 5 */
    if (x[1] >= t[k2] || x[*m] <= t[nk1]) {
	goto L80;
    }
    i__ = 1;
    l = k2;
    nk3 = nk1 - 1;
    if (nk3 < 2) {
	goto L70;
    }
    i__1 = nk3;
    for (j = 2; j <= i__1; ++j) {
	tj = t[j];
	++l;
	tl = t[l];
L40:
	++i__;
	if (i__ >= *m) {
	    goto L80;
	}
	if (x[i__] <= tj) {
	    goto L40;
	}
	if (x[i__] >= tl) {
	    goto L80;
	}
/* L60: */
    }
L70:
    *ier = 0;
L80:
    return 0;
} /* fpchec_ */

/* Subroutine */ int fpcurf_(iopt, x, y, w, m, xb, xe, k, s, nest, tol, maxit,
	 k1, k2, n, t, c__, fp, fpint, z__, a, b, g, q, nrdata, ier)
int *iopt;
double *x, *y, *w;
int *m;
double *xb, *xe;
int *k;
double *s;
int *nest;
double *tol;
int *maxit, *k1, *k2, *n;
double *t, *c__, *fp, *fpint, *z__, *a, *b, *g, *q;
int *nrdata, *ier;
{
    /* System generated locals */
    int a_dim1, a_offset, b_dim1, b_offset, g_dim1, g_offset, q_dim1, 
	    q_offset, i__1, i__2, i__3, i__4, i__5;
    double d__1;

    /* Local variables */
    static double half;
    static int nmin, iter, nmax;
    static double fpms, term, pinv, h__[7];
    static int i__, j, l;
    static double p, fpold, fpart, f1, f2, f3;
    static int i1, i2;
    static double store;
    static int i3, k3;
    static double p1, p2, p3;
    static int l0, nplus, nrint, n8;
    static int it;
    static double rn, wi, xi, yi;
    static double fp0;
    static int mk1, nk1;
    static double acc, one, cos__, sin__;
    static int new__;
    static double piv;
    static int ich1, ich3;
    static double con1, con4, con9;
    static int npl1;

/*  .. */
/*  ..scalar arguments.. */
/*  ..array arguments.. */
/*  ..local scalars.. */
/*  ..local arrays.. */
/*  ..function references */
/*  ..subroutine references.. */
/*    fpback,fpbspl,fpgivs,fpdisc,fpknot,fprota */
/*  .. */
/*  set constants */
    /* Parameter adjustments */
    --w;
    --y;
    --x;
    --nrdata;
    --z__;
    --fpint;
    --c__;
    --t;
    q_dim1 = *m;
    q_offset = 1 + q_dim1 * 1;
    q -= q_offset;
    a_dim1 = *nest;
    a_offset = 1 + a_dim1 * 1;
    a -= a_offset;
    g_dim1 = *nest;
    g_offset = 1 + g_dim1 * 1;
    g -= g_offset;
    b_dim1 = *nest;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    one = (double)1.;
    con1 = (double).1;
    con9 = (double).9;
    con4 = (double).04;
    half = (double).5;
/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/*  part 1: determination of the number of knots and their position     c */
/*  **************************************************************      c */
/*  given a set of knots we compute the least-squares spline sinf(x),   c */
/*  and the corresponding sum of squared residuals fp=f(p=inf).         c */
/*  if iopt=-1 sinf(x) is the requested approximation.                  c */
/*  if iopt=0 or iopt=1 we check whether we can accept the knots:       c */
/*    if fp <=s we will continue with the current set of knots.         c */
/*    if fp > s we will increase the number of knots and compute the    c */
/*       corresponding least-squares spline until finally fp<=s.        c */
/*    the initial choice of knots depends on the value of s and iopt.   c */
/*    if s=0 we have spline interpolation; in that case the number of   c */
/*    knots equals nmax = m+k+1.                                        c */
/*    if s > 0 and                                                      c */
/*      iopt=0 we first compute the least-squares polynomial of         c */
/*      degree k; n = nmin = 2*k+2                                      c */
/*      iopt=1 we start with the set of knots found at the last         c */
/*      call of the routine, except for the case that s > fp0; then     c */
/*      we compute directly the least-squares polynomial of degree k.   c */
/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/*  determine nmin, the number of knots for polynomial approximation. */
    nmin = *k1 << 1;
    if (*iopt < 0) {
	goto L60;
    }
/*  calculation of acc, the absolute tolerance for the root of f(p)=s. */
    acc = *tol * *s;
/*  determine nmax, the number of knots for spline interpolation. */
    nmax = *m + *k1;
    if (*s > (double)0.) {
	goto L45;
    }
/*  if s=0, s(x) is an interpolating spline. */
/*  test whether the required storage space exceeds the available one. */
    *n = nmax;
    if (nmax > *nest) {
	goto L420;
    }
/*  find the position of the interior knots in case of interpolation. */
L10:
    mk1 = *m - *k1;
    if (mk1 == 0) {
	goto L60;
    }
    k3 = *k / 2;
    i__ = *k2;
    j = k3 + 2;
    if (k3 << 1 == *k) {
	goto L30;
    }
    i__1 = mk1;
    for (l = 1; l <= i__1; ++l) {
	t[i__] = x[j];
	++i__;
	++j;
/* L20: */
    }
    goto L60;
L30:
    i__1 = mk1;
    for (l = 1; l <= i__1; ++l) {
	t[i__] = (x[j] + x[j - 1]) * half;
	++i__;
	++j;
/* L40: */
    }
    goto L60;
/*  if s>0 our initial choice of knots depends on the value of iopt. */
/*  if iopt=0 or iopt=1 and s>=fp0, we start computing the least-squares */
/*  polynomial of degree k which is a spline without interior knots. */
/*  if iopt=1 and fp0>s we start computing the least squares spline */
/*  according to the set of knots found at the last call of the routine. */
L45:
    if (*iopt == 0) {
	goto L50;
    }
    if (*n == nmin) {
	goto L50;
    }
    fp0 = fpint[*n];
    fpold = fpint[*n - 1];
    nplus = nrdata[*n];
    if (fp0 > *s) {
	goto L60;
    }
L50:
    *n = nmin;
    fpold = (double)0.;
    nplus = 0;
    nrdata[1] = *m - 2;
/*  main loop for the different sets of knots. m is a save upper bound */
/*  for the number of trials. */
L60:
    i__1 = *m;
    for (iter = 1; iter <= i__1; ++iter) {
	if (*n == nmin) {
	    *ier = -2;
	}
/*  find nrint, tne number of knot intervals. */
	nrint = *n - nmin + 1;
/*  find the position of the additional knots which are needed for */
/*  the b-spline representation of s(x). */
	nk1 = *n - *k1;
	i__ = *n;
	i__2 = *k1;
	for (j = 1; j <= i__2; ++j) {
	    t[j] = *xb;
	    t[i__] = *xe;
	    --i__;
/* L70: */
	}
/*  compute the b-spline coefficients of the least-squares spline */
/*  sinf(x). the observation matrix a is built up row by row and */
/*  reduced to upper triangular form by givens transformations. */
/*  at the same time fp=f(p=inf) is computed. */
	*fp = (double)0.;
/*  initialize the observation matrix a. */
	i__2 = nk1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    z__[i__] = (double)0.;
	    i__3 = *k1;
	    for (j = 1; j <= i__3; ++j) {
		a[i__ + j * a_dim1] = (double)0.;
/* L80: */
	    }
	}
	l = *k1;
	i__3 = *m;
	for (it = 1; it <= i__3; ++it) {
/*  fetch the current data point x(it),y(it). */
	    xi = x[it];
	    wi = w[it];
	    yi = y[it] * wi;
/*  search for knot interval t(l) <= xi < t(l+1). */
L85:
	    if (xi < t[l + 1] || l == nk1) {
		goto L90;
	    }
	    ++l;
	    goto L85;
/*  evaluate the (k+1) non-zero b-splines at xi and store them in q. */
L90:
	    fpbspl_(&t[1], n, k, &xi, &l, h__);
	    i__2 = *k1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		q[it + i__ * q_dim1] = h__[i__ - 1];
		h__[i__ - 1] *= wi;
/* L95: */
	    }
/*  rotate the new row of the observation matrix into triangle. */
	    j = l - *k1;
	    i__2 = *k1;
	    for (i__ = 1; i__ <= i__2; ++i__) {
		++j;
		piv = h__[i__ - 1];
		if (piv == (double)0.) {
		    goto L110;
		}
/*  calculate the parameters of the givens transformation. */
		fpgivs_(&piv, &a[j + a_dim1], &cos__, &sin__);
/*  transformations to right hand side. */
		fprota_(&cos__, &sin__, &yi, &z__[j]);
		if (i__ == *k1) {
		    goto L120;
		}
		i2 = 1;
		i3 = i__ + 1;
		i__4 = *k1;
		for (i1 = i3; i1 <= i__4; ++i1) {
		    ++i2;
/*  transformations to left hand side. */
		    fprota_(&cos__, &sin__, &h__[i1 - 1], &a[j + i2 * a_dim1])
			    ;
/* L100: */
		}
L110:
		;
	    }
/*  add contribution of this row to the sum of squares of residual */
/*  right hand sides. */
L120:
/* Computing 2nd power */
	    d__1 = yi;
	    *fp += d__1 * d__1;
/* L130: */
	}
	if (*ier == -2) {
	    fp0 = *fp;
	}
	fpint[*n] = fp0;
	fpint[*n - 1] = fpold;
	nrdata[*n] = nplus;
/*  backward substitution to obtain the b-spline coefficients. */
	fpback_(&a[a_offset], &z__[1], &nk1, k1, &c__[1], nest);
/*  test whether the approximation sinf(x) is an acceptable solution. */
	if (*iopt < 0) {
	    goto L440;
	}
	fpms = *fp - *s;
	if (abs(fpms) < acc) {
	    goto L440;
	}
/*  if f(p=inf) < s accept the choice of knots. */
	if (fpms < (double)0.) {
	    goto L250;
	}
/*  if n = nmax, sinf(x) is an interpolating spline. */
	if (*n == nmax) {
	    goto L430;
	}
/*  increase the number of knots. */
/*  if n=nest we cannot increase the number of knots because of */
/*  the storage capacity limitation. */
	if (*n == *nest) {
	    goto L420;
	}
/*  determine the number of knots nplus we are going to add. */
	if (*ier == 0) {
	    goto L140;
	}
	nplus = 1;
	*ier = 0;
	goto L150;
L140:
	npl1 = nplus << 1;
	rn = (double) nplus;
	if (fpold - *fp > acc) {
	    npl1 = (int) (rn * fpms / (fpold - *fp));
	}
/* Computing MIN */
/* Computing MAX */
	i__4 = npl1, i__5 = nplus / 2, i__4 = smax(i__4,i__5);
	i__3 = nplus << 1, i__2 = smax(i__4,1);
	nplus = smin(i__3,i__2);
L150:
	fpold = *fp;
/*  compute the sum((w(i)*(y(i)-s(x(i))))**2) for each knot interval */
/*  t(j+k) <= x(i) <= t(j+k+1) and store it in fpint(j),j=1,2,...nrint. */
	fpart = (double)0.;
	i__ = 1;
	l = *k2;
	new__ = 0;
	i__3 = *m;
	for (it = 1; it <= i__3; ++it) {
	    if (x[it] < t[l] || l > nk1) {
		goto L160;
	    }
	    new__ = 1;
	    ++l;
L160:
	    term = (double)0.;
	    l0 = l - *k2;
	    i__2 = *k1;
	    for (j = 1; j <= i__2; ++j) {
		++l0;
		term += c__[l0] * q[it + j * q_dim1];
/* L170: */
	    }
/* Computing 2nd power */
	    d__1 = w[it] * (term - y[it]);
	    term = d__1 * d__1;
	    fpart += term;
	    if (new__ == 0) {
		goto L180;
	    }
	    store = term * half;
	    fpint[i__] = fpart - store;
	    ++i__;
	    fpart = store;
	    new__ = 0;
L180:
	    ;
	}
	fpint[nrint] = fpart;
	i__3 = nplus;
	for (l = 1; l <= i__3; ++l) {
/*  add a new knot. */
	    fpknot_(&x[1], m, &t[1], n, &fpint[1], &nrdata[1], &nrint, nest, &
		    c__1);
/*  if n=nmax we locate the knots as for interpolation. */
	    if (*n == nmax) {
		goto L10;
	    }
/*  test whether we cannot further increase the number of knots. */
	    if (*n == *nest) {
		goto L200;
	    }
/* L190: */
	}
/*  restart the computations with the new set of knots. */
L200:
	;
    }
/*  test whether the least-squares kth degree polynomial is a solution */
/*  of our approximation problem. */
L250:
    if (*ier == -2) {
	goto L440;
    }
/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/*  part 2: determination of the smoothing spline sp(x).                c */
/*  ***************************************************                 c */
/*  we have determined the number of knots and their position.          c */
/*  we now compute the b-spline coefficients of the smoothing spline    c */
/*  sp(x). the observation matrix a is extended by the rows of matrix   c */
/*  b expressing that the kth derivative discontinuities of sp(x) at    c */
/*  the interior knots t(k+2),...t(n-k-1) must be zero. the corres-     c */
/*  ponding weights of these additional rows are set to 1/p.            c */
/*  iteratively we then have to determine the value of p such that      c */
/*  f(p)=sum((w(i)*(y(i)-sp(x(i))))**2) be = s. we already know that    c */
/*  the least-squares kth degree polynomial corresponds to p=0, and     c */
/*  that the least-squares spline corresponds to p=infinity. the        c */
/*  iteration process which is proposed here, makes use of rational     c */
/*  interpolation. since f(p) is a convex and strictly decreasing       c */
/*  function of p, it can be approximated by a rational function        c */
/*  r(p) = (u*p+v)/(p+w). three values of p(p1,p2,p3) with correspond-  c */
/*  ing values of f(p) (f1=f(p1)-s,f2=f(p2)-s,f3=f(p3)-s) are used      c */
/*  to calculate the new value of p such that r(p)=s. convergence is    c */
/*  guaranteed by taking f1>0 and f3<0.                                 c */
/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc */
/*  evaluate the discontinuity jump of the kth derivative of the */
/*  b-splines at the knots t(l),l=k+2,...n-k-1 and store in b. */
    fpdisc_(&t[1], n, k2, &b[b_offset], nest);
/*  initial value for p. */
    p1 = (double)0.;
    f1 = fp0 - *s;
    p3 = -one;
    f3 = fpms;
    p = (double)0.;
    i__1 = nk1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	p += a[i__ + a_dim1];
/* L255: */
    }
    rn = (double) nk1;
    p = rn / p;
    ich1 = 0;
    ich3 = 0;
    n8 = *n - nmin;
/*  iteration process to find the root of f(p) = s. */
    i__1 = *maxit;
    for (iter = 1; iter <= i__1; ++iter) {
/*  the rows of matrix b with weight 1/p are rotated into the */
/*  triangularised observation matrix a which is stored in g. */
	pinv = one / p;
	i__3 = nk1;
	for (i__ = 1; i__ <= i__3; ++i__) {
	    c__[i__] = z__[i__];
	    g[i__ + *k2 * g_dim1] = (double)0.;
	    i__2 = *k1;
	    for (j = 1; j <= i__2; ++j) {
		g[i__ + j * g_dim1] = a[i__ + j * a_dim1];
/* L260: */
	    }
	}
	i__2 = n8;
	for (it = 1; it <= i__2; ++it) {
/*  the row of matrix b is rotated into triangle by givens transformation */
	    i__3 = *k2;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		h__[i__ - 1] = b[it + i__ * b_dim1] * pinv;
/* L270: */
	    }
	    yi = (double)0.;
	    i__3 = nk1;
	    for (j = it; j <= i__3; ++j) {
		piv = h__[0];
/*  calculate the parameters of the givens transformation. */
		fpgivs_(&piv, &g[j + g_dim1], &cos__, &sin__);
/*  transformations to right hand side. */
		fprota_(&cos__, &sin__, &yi, &c__[j]);
		if (j == nk1) {
		    goto L300;
		}
		i2 = *k1;
		if (j > n8) {
		    i2 = nk1 - j;
		}
		i__4 = i2;
		for (i__ = 1; i__ <= i__4; ++i__) {
/*  transformations to left hand side. */
		    i1 = i__ + 1;
		    fprota_(&cos__, &sin__, &h__[i1 - 1], &g[j + i1 * g_dim1])
			    ;
		    h__[i__ - 1] = h__[i1 - 1];
/* L280: */
		}
		h__[i2] = (double)0.;
/* L290: */
	    }
L300:
	    ;
	}
/*  backward substitution to obtain the b-spline coefficients. */
	fpback_(&g[g_offset], &c__[1], &nk1, k2, &c__[1], nest);
/*  computation of f(p). */
	*fp = (double)0.;
	l = *k2;
	i__2 = *m;
	for (it = 1; it <= i__2; ++it) {
	    if (x[it] < t[l] || l > nk1) {
		goto L310;
	    }
	    ++l;
L310:
	    l0 = l - *k2;
	    term = (double)0.;
	    i__3 = *k1;
	    for (j = 1; j <= i__3; ++j) {
		++l0;
		term += c__[l0] * q[it + j * q_dim1];
/* L320: */
	    }
/* Computing 2nd power */
	    d__1 = w[it] * (term - y[it]);
	    *fp += d__1 * d__1;
/* L330: */
	}
/*  test whether the approximation sp(x) is an acceptable solution. */
	fpms = *fp - *s;
	if (abs(fpms) < acc) {
	    goto L440;
	}
/*  test whether the maximal number of iterations is reached. */
	if (iter == *maxit) {
	    goto L400;
	}
/*  carry out one more step of the iteration process. */
	p2 = p;
	f2 = fpms;
	if (ich3 != 0) {
	    goto L340;
	}
	if (f2 - f3 > acc) {
	    goto L335;
	}
/*  our initial choice of p is too large. */
	p3 = p2;
	f3 = f2;
	p *= con4;
	if (p <= p1) {
	    p = p1 * con9 + p2 * con1;
	}
	goto L360;
L335:
	if (f2 < (double)0.) {
	    ich3 = 1;
	}
L340:
	if (ich1 != 0) {
	    goto L350;
	}
	if (f1 - f2 > acc) {
	    goto L345;
	}
/*  our initial choice of p is too small */
	p1 = p2;
	f1 = f2;
	p /= con4;
	if (p3 < (double)0.) {
	    goto L360;
	}
	if (p >= p3) {
	    p = p2 * con1 + p3 * con9;
	}
	goto L360;
L345:
	if (f2 > (double)0.) {
	    ich1 = 1;
	}
/*  test whether the iteration process proceeds as theoretically */
/*  expected. */
L350:
	if (f2 >= f1 || f2 <= f3) {
	    goto L410;
	}
/*  find the new value for p. */
	p = fprati_(&p1, &f1, &p2, &f2, &p3, &f3);
L360:
	;
    }
/*  error codes and messages. */
L400:
    *ier = 3;
    goto L440;
L410:
    *ier = 2;
    goto L440;
L420:
    *ier = 1;
    goto L440;
L430:
    *ier = -1;
L440:
    return 0;
} /* fpcurf_ */

/* Subroutine */ int fpdisc_(t, n, k2, b, nest)
double *t;
int *n, *k2;
double *b;
int *nest;
{
    /* System generated locals */
    int b_dim1, b_offset, i__1, i__2, i__3;

    /* Local variables */
    static double prod, h__[12];
    static int i__, j, k, l, nrint, k1;
    static double an;
    static int ik, jk, lj, lk, lp, nk1;
    static double fac;
    static int lmk;

/*  subroutine fpdisc calculates the discontinuity jumps of the kth */
/*  derivative of the b-splines of degree k at the knots t(k+2)..t(n-k-1) */
/*  ..scalar arguments.. */
/*  ..array arguments.. */
/*  ..local scalars.. */
/*  ..local array.. */
/*  .. */
    /* Parameter adjustments */
    --t;
    b_dim1 = *nest;
    b_offset = 1 + b_dim1 * 1;
    b -= b_offset;

    /* Function Body */
    k1 = *k2 - 1;
    k = k1 - 1;
    nk1 = *n - k1;
    nrint = nk1 - k;
    an = (double) nrint;
    fac = an / (t[nk1 + 1] - t[k1]);
    i__1 = nk1;
    for (l = *k2; l <= i__1; ++l) {
	lmk = l - k1;
	i__2 = k1;
	for (j = 1; j <= i__2; ++j) {
	    ik = j + k1;
	    lj = l + j;
	    lk = lj - *k2;
	    h__[j - 1] = t[l] - t[lk];
	    h__[ik - 1] = t[l] - t[lj];
/* L10: */
	}
	lp = lmk;
	i__2 = *k2;
	for (j = 1; j <= i__2; ++j) {
	    jk = j;
	    prod = h__[j - 1];
	    i__3 = k;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		++jk;
		prod = prod * h__[jk - 1] * fac;
/* L20: */
	    }
	    lk = lp + k1;
	    b[lmk + j * b_dim1] = (t[lk] - t[lp]) / prod;
	    ++lp;
/* L30: */
	}
/* L40: */
    }
    return 0;
} /* fpdisc_ */

/* Subroutine */ int fpgivs_(piv, ww, cos__, sin__)
double *piv, *ww, *cos__, *sin__;
{
    /* System generated locals */
    double d__1;

    /* Local variables */
    static double store, dd, one;

/*  subroutine fpgivs calculates the parameters of a givens */
/*  transformation . */
/*  .. */
/*  ..scalar arguments.. */
/*  ..local scalars.. */
/*  ..function references.. */
/*  .. */
    one = (double)1.;
    store = abs(*piv);
    if (store >= *ww) {
/* Computing 2nd power */
	d__1 = *ww / *piv;
	dd = store * sqrt(one + d__1 * d__1);
    }
    if (store < *ww) {
/* Computing 2nd power */
	d__1 = *piv / *ww;
	dd = *ww * sqrt(one + d__1 * d__1);
    }
    *cos__ = *ww / dd;
    *sin__ = *piv / dd;
    *ww = dd;
    return 0;
} /* fpgivs_ */

/* Subroutine */ int fpknot_(x, m, t, n, fpint, nrdata, nrint, nest, istart)
double *x;
int *m;
double *t;
int *n;
double *fpint;
int *nrdata, *nrint, *nest, *istart;
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static int next, j, k, ihalf;
    static double fpmax;
    static int maxpt;
    static double am, an;
    static int jj, jk, jbegin, maxbeg, number, jpoint, nrx;

/*  subroutine fpknot locates an additional knot for a spline of degree */
/*  k and adjusts the corresponding parameters,i.e. */
/*    t     : the position of the knots. */
/*    n     : the number of knots. */
/*    nrint : the number of knotintervals. */
/*    fpint : the sum of squares of residual right hand sides */
/*            for each knot interval. */
/*    nrdata: the number of data points inside each knot interval. */
/*  istart indicates that the smallest data point at which the new knot */
/*  may be added is x(istart+1) */
/*  .. */
/*  ..scalar arguments.. */
/*  ..array arguments.. */
/*  ..local scalars.. */
/*  .. */
    /* Parameter adjustments */
    --x;
    --nrdata;
    --fpint;
    --t;

    /* Function Body */
    k = (*n - *nrint - 1) / 2;
/*  search for knot interval t(number+k) <= x <= t(number+k+1) where */
/*  fpint(number) is maximal on the condition that nrdata(number) */
/*  not equals zero. */
    fpmax = (double)0.;
    jbegin = *istart;
    i__1 = *nrint;
    for (j = 1; j <= i__1; ++j) {
	jpoint = nrdata[j];
	if (fpmax >= fpint[j] || jpoint == 0) {
	    goto L10;
	}
	fpmax = fpint[j];
	number = j;
	maxpt = jpoint;
	maxbeg = jbegin;
L10:
	jbegin = jbegin + jpoint + 1;
/* L20: */
    }
/*  let coincide the new knot t(number+k+1) with a data point x(nrx) */
/*  inside the old knot interval t(number+k) <= x <= t(number+k+1). */
    ihalf = maxpt / 2 + 1;
    nrx = maxbeg + ihalf;
    next = number + 1;
    if (next > *nrint) {
	goto L40;
    }
/*  adjust the different parameters. */
    i__1 = *nrint;
    for (j = next; j <= i__1; ++j) {
	jj = next + *nrint - j;
	fpint[jj + 1] = fpint[jj];
	nrdata[jj + 1] = nrdata[jj];
	jk = jj + k;
	t[jk + 1] = t[jk];
/* L30: */
    }
L40:
    nrdata[number] = ihalf - 1;
    nrdata[next] = maxpt - ihalf;
    am = (double) maxpt;
    an = (double) nrdata[number];
    fpint[number] = fpmax * an / am;
    an = (double) nrdata[next];
    fpint[next] = fpmax * an / am;
    jk = next + k;
    t[jk] = x[nrx];
    ++(*n);
    ++(*nrint);
    return 0;
} /* fpknot_ */

double fprati_(p1, f1, p2, f2, p3, f3)
double *p1, *f1, *p2, *f2, *p3, *f3;
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    static double p, h1, h2, h3;

/*  given three points (p1,f1),(p2,f2) and (p3,f3), function fprati */
/*  gives the value of p such that the rational interpolating function */
/*  of the form r(p) = (u*p+v)/(p+w) equals zero at p. */
/*  .. */
/*  ..scalar arguments.. */
/*  ..local scalars.. */
/*  .. */
    if (*p3 > (double)0.) {
	goto L10;
    }
/*  value of p in case p3 = infinity. */
    p = (*p1 * (*f1 - *f3) * *f2 - *p2 * (*f2 - *f3) * *f1) / ((*f1 - *f2) * *
	    f3);
    goto L20;
/*  value of p in case p3 ^= infinity. */
L10:
    h1 = *f1 * (*f2 - *f3);
    h2 = *f2 * (*f3 - *f1);
    h3 = *f3 * (*f1 - *f2);
    p = -(*p1 * *p2 * h3 + *p2 * *p3 * h1 + *p3 * *p1 * h2) / (*p1 * h1 + *p2 
	    * h2 + *p3 * h3);
/*  adjust the value of p1,f1,p3 and f3 such that f1 > 0 and f3 < 0. */
L20:
    if (*f2 < (double)0.) {
	goto L30;
    }
    *p1 = *p2;
    *f1 = *f2;
    goto L40;
L30:
    *p3 = *p2;
    *f3 = *f2;
L40:
    ret_val = p;
    return ret_val;
} /* fprati_ */

/* Subroutine */ int fprota_(cos__, sin__, a, b)
double *cos__, *sin__, *a, *b;
{
    static double stor1, stor2;

/*  subroutine fprota applies a givens rotation to a and b. */
/*  .. */
/*  ..scalar arguments.. */
/* ..local scalars.. */
/*  .. */
    stor1 = *a;
    stor2 = *b;
    *b = *cos__ * stor2 + *sin__ * stor1;
    *a = *cos__ * stor1 - *sin__ * stor2;
    return 0;
} /* fprota_ */



/* if in error method is changed to -1 */
/* for exact interpolation s=0. else smooth interpolation */
/* for first call iopt=0 else 1 (triggers calc of spline coefficients) */
double intpol3(double *x, double *y, int n, double x0, int *method, double s, int iopt )
{
  /* with the following lines an X-Error occures from time to time if 'static' defined */
  int      j, imin, imax;

  int      *isort;
  double   *xsort, *ysort, *buffer;

  double     y0=0., dx,dy,dx0;


  int    ns=1;
  static double *c__; 
  static double *w, x0d, y0d, *t, fp, *wrk;
  static int    k, nest, nn, lwrk, *iwrk, ier;

  /* Daten nach aufsteigendem x sortieren */

  if((xsort =(double *)malloc((int)(n+1) *sizeof(double)))==NULL)
  {  printf("\n\n ERROR in intpol3: malloc xsort failed \n\n") ; }
  if((ysort =(double *)malloc((int)(n+1) *sizeof(double)))==NULL)
  {  printf("\n\n ERROR in intpol3: malloc ysort failed \n\n") ; }
  if((isort =(int *)malloc((int)(n+1) *sizeof(int)))==NULL)
  {  printf("\n\n ERROR in intpol3: malloc isort failed \n\n") ; }
  if((buffer =(double *)malloc((int)(n+1) *sizeof(double)))==NULL)
  {  printf("\n\n ERROR in intpol3: malloc buffer failed \n\n") ; }
  bsort(x, buffer, isort, n );
  imax=n-1;
  for (j=0; j<n; j++)
  {
    xsort[j]=x[isort[imax-j]];
    ysort[j]=y[isort[imax-j]];
  }

  if ((x0 < xsort[0])||(x0 > xsort[n-1])) *method=0;
  if ((*method==0)||(n<3))       /* lineare interpolation */
  {
      n--;
      if (x0 <= xsort[0])
      {
        *method=1;
        imin = 0;
        imax = 1;
      }
      else if (x0 >= xsort[n])
      {
        *method=1;
        imin = n-1;
        imax = n;
      }
      else
      {
        *method=0;
        imax=0;
        while (xsort[imax]<x0) imax++;
        imin=imax-1;
      }

      /*     *** lineare interpolation   ************    */

      dx=xsort[imax]-xsort[imin];
      dy=ysort[imax]-ysort[imin];
      dx0=x0-xsort[imin];
      y0=ysort[imin]+dx0*dy/dx;
  }
  else
  {
    *method=0;

/*             the following restrictions must be satisfied. */
/*             -1<=iopt<=1, 1<=k<=5, m>k, nest>2*k+2, w(i)>0,i=1,2,...,m */
/*             xb<=x(1)<x(2)<...<x(m)<=xe, lwrk>=(k+1)*m+nest*(7+3*k) */
    if(n>3) k=3;
    else k=n-1;
    nest=n+k+1;
    /* dim wrk = lwrk = (m*(k+1)+nest*(7+3*k)) */
    lwrk=n*(k+1)+nest*(7+3*k);
    if((wrk =(double *)malloc((lwrk) *sizeof(double)))==NULL)
    {  printf("\n\n ERROR in intpol3: malloc wrk failed \n\n") ; }
    if((w =(double *)malloc((int)(n+1) *sizeof(double)))==NULL)
    {  printf("\n\n ERROR in intpol3: malloc w failed \n\n") ; }
    for(j=0; j<n; j++) w[j]=1.;
    if((t =(double *)malloc((int)(nest) *sizeof(double)))==NULL)
    {  printf("\n\n ERROR in intpol3: malloc t failed \n\n") ; }
    if((c__ =(double *)malloc((int)(nest) *sizeof(double)))==NULL)
    {  printf("\n\n ERROR in intpol3: malloc c failed \n\n") ; }
    if((iwrk =(int *)malloc((int)(nest) *sizeof(int)))==NULL)
    {  printf("\n\n ERROR in intpol3: malloc iwrk failed \n\n") ; }

    curfit_(&iopt, &n, xsort, ysort, w, &xsort[0], &xsort[n-1], &k, &s, &nest, &nn, t, c__, &fp, wrk, &lwrk, iwrk, &ier);
    if(ier>0)
    {
      *method=-1;
    }
    else
    {
      x0d=x0;
      x0d=x0;
      splev_(t, &nn, c__, &k, &x0d, &y0d, &ns, &ier);
      y0=y0d;
      if(ier>0)
      {
        *method=-1;
      }
    }
    free(wrk);
    free(iwrk);
    free(w);
    free(t);
    free(c__);
  }

  /*
  for (imin=0;imin<n ;imin++ )
  {
    printf("x0:%lf y0:%lf xsort:%lf ysort:%lf\n", x0,y0,xsort[imin],ysort[imin] );
  }
  */

  free(xsort);
  free(ysort);
  free(isort);
  free(buffer);
  return(y0);
}




