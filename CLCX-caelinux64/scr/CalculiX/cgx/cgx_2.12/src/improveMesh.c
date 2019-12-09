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

/* smooth.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/
#include <cgx.h>

#define integer int
#define doublereal double

extern Scale     scale[1];
#define NOD 0


/* Subroutine */ int clasnodes_(co, nk, kon, ne, iptr1, iptr2, nside, ineino, 
	nkind, ieltyp, npec, npet)
doublereal *co;
integer *nk, *kon, *ne, *iptr1, *iptr2, *nside, *ineino, *nkind, *ieltyp, *
	npec, *npet;
{
    /* Initialized data */

    static integer nendnode2d[8]	/* was [2][4] */ = { 2,4,1,3,2,4,1,3 }
	    ;
    static integer nendnode3d[24]	/* was [3][8] */ = { 2,4,5,1,3,6,2,4,
	    7,1,3,8,1,6,8,2,5,7,3,6,8,4,5,7 };

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Local variables */
    static integer node, ndim, nelement, location, i__, j, k, l, nodea, ifree,
	     konloc[20];


/*     determines the elements and element sides connected to a movable node */


/*     nside(i)=# of element edges connected to node i; the information */
/*              on the edges is stored in ineino(1..3,j) where */
/*              j=iptr1(i),j=iptr2(iptr1(i)),j=iptr2(iptr2(iptr1(i))),.., */
/*              in total nside(i) j-values for node i */

/*     ineino(*): neighboring end node */




    /* Parameter adjustments */
    --nkind;
    --ineino;
    --nside;
    --iptr2;
    --iptr1;
    kon -= 21;
    co -= 4;

    /* Function Body */

/*     initialisation */

    i__1 = *nk;
    for (i__ = 1; i__ <= i__1; ++i__) {
	nside[i__] = 0;
	iptr1[i__] = 0;
    }
    i__1 = *nk * 6;
    for (i__ = 1; i__ <= i__1; ++i__) {
	iptr2[i__] = 0;
    }
    ifree = 1;

/*     determining the element edges containing a given node and */
/*     the number of elements connnected to the node */

    i__1 = *ne;
    for (i__ = 1; i__ <= i__1; ++i__) {
	nelement = i__;
	i__2 = *npet;
	for (j = 1; j <= i__2; ++j) {
	    konloc[j - 1] = kon[j + nelement * 20];
	}

/*       focussing on a vertex of the element */

	i__2 = *npec;
	for (j = 1; j <= i__2; ++j) {
	    node = konloc[j - 1];

/*         focussing on the nodes connected to the vertex */

	    if (*ieltyp == 1 || *ieltyp == 2) {
		ndim = 2;
	    } else {
		ndim = 3;
	    }

/* L2: */
	    i__3 = ndim;
	    for (k = 1; k <= i__3; ++k) {
		nodea = konloc[nendnode3d[k + j * 3 - 4] - 1];
		if (nodea == node) {
		    goto L1;
		}
		i__4 = nside[node];
		for (l = 1; l <= i__4; ++l) {
		    if (l == 1) {
			location = iptr1[node];
		    } else {
			location = iptr2[location];
		    }
		    if (ineino[location] == nodea) {
			goto L1;
		    }
		}
		if (nside[node] == 0) {
		    iptr1[node] = ifree;
		} else {
		    iptr2[location] = ifree;
		}
		if (*ieltyp == 3 || *ieltyp == 4) {
		    ineino[ifree] = konloc[nendnode3d[k + j * 3 - 4] - 1];
		} else if (*ieltyp == 1 || *ieltyp == 2) {
		    ineino[ifree] = konloc[nendnode2d[k + (j << 1) - 3] - 1];
		}
		++nside[node];
		++ifree;
/*            if(ifree.gt.6*nk) then */
/*              write(*,*) 'error in clasnodes; increase the dimension' */
/*              write(*,*) 'of ineino' */
/*              stop */
/*            endif */
L1:
		;
	    }
	    ;
	}
/* L3: */
    }

    return 0;
} /* clasnodes_ */

/* attach.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/* distattach.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/


/* Subroutine */ int distattach_(xi, et, pneigh, ndiv, pnode, dist, p)
doublereal *xi, *et, *pneigh;
integer *ndiv;
doublereal *pnode, *dist, *p;
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;


    /* Local variables */
    static integer i__;
    static doublereal a1, a2, a3, a4;
    static integer n1, n2, n3, n4, n5, n6, n7, n8, n9, n10;
    static doublereal eu, ev, xj, xk;


/*     calculates the global coordinates p within an 8-node 2-D element */
/*     (given by global coordinates pneigh)of a point with given local */
/*     coordinates (xi,et) and determines the distance from this point */
/*     (dist)to the point with global coordinates pnode */




    /* Parameter adjustments */
    --p;
    --pnode;
    --ndiv;
    pneigh -= 4;

    /* Function Body */
    xj = (*xi + 1.) / 2.;
    xk = 1. - xj;
    eu = (*et + 1.) / 2.;
    ev = 1. - eu;

    n1 = (integer) (xj * ndiv[1]) + 1;
    a1 = xj * ndiv[1] + 1 - n1;
    n2 = ndiv[1] + 1 + (integer) (eu * ndiv[2]);
    a2 = ndiv[1] + 1 + eu * ndiv[2] - n2;
    n3 = ndiv[1] + ndiv[2] + 1 + (integer) (xk * ndiv[3]);
    a3 = ndiv[1] + ndiv[2] + 1 + xk * ndiv[3] - n3;
    n4 = ndiv[1] + ndiv[2] + ndiv[3] + 1 + (integer) (ev * ndiv[4]);
    a4 = ndiv[1] + ndiv[2] + ndiv[3] + 1 + ev * ndiv[4] - n4;

    n5 = 1;
    n6 = ndiv[1] + 1;
    n7 = n6 + ndiv[2];
    n8 = n7 + ndiv[3];
    n10 = n8 + ndiv[4];

    n9 = n4 + 1;
    if (n4 >= n10) {
	n4 = n4 - n10 + 1;
    }
    if (n9 >= n10) {
	n9 = n9 - n10 + 1;
    }

    for (i__ = 1; i__ <= 3; ++i__) {
	p[i__] = ev * ((1. - a1) * pneigh[i__ + n1 * 3] + a1 * pneigh[i__ + (
		n1 + 1) * 3]) + eu * ((1. - a3) * pneigh[i__ + n3 * 3] + a3 * 
		pneigh[i__ + (n3 + 1) * 3]) + xk * ((1. - a4) * pneigh[i__ + 
		n4 * 3] + a4 * pneigh[i__ + n9 * 3]) + xj * ((1. - a2) * 
		pneigh[i__ + n2 * 3] + a2 * pneigh[i__ + (n2 + 1) * 3]) - xk *
		 ev * pneigh[i__ + n5 * 3] - xj * ev * pneigh[i__ + n6 * 3] - 
		xk * eu * pneigh[i__ + n8 * 3] - xj * eu * pneigh[i__ + n7 * 
		3];
    }

/* Computing 2nd power */
    d__1 = p[1] - pnode[1];
/* Computing 2nd power */
    d__2 = p[2] - pnode[2];
/* Computing 2nd power */
    d__3 = p[3] - pnode[3];
    *dist = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);

    return 0;
} /* distattach_ */

/* Subroutine */ int attach_(co, nk, neigh, pneigh, ndiv, node)
doublereal *co;
integer *nk, *neigh;
doublereal *pneigh;
integer *ndiv, *node;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static doublereal aold[9]	/* was [3][3] */;
    static integer imin, jmin;
    static doublereal a[9]	/* was [3][3] */;
    static integer i__, j;
    static doublereal p[3], pnode[3], etold[9]	/* was [3][3] */, xiold[9]	
	    /* was [3][3] */, d1, d2, d3, d4, etopt, xiopt;
    static integer ii;
    static doublereal et[9]	/* was [3][3] */, xi[9]	/* was [3][3] */;
    extern /* Subroutine */ int distattach_();
    static doublereal distmin;


/*     projects a node ('node') on the face of a twenty-node element. The */
/*     eight face nodes are contained in neigh. */





    /* Parameter adjustments */
    --ndiv;
    pneigh -= 4;
    --neigh;
    co -= 4;

    /* Function Body */
    d1 = .25;
    d2 = .03125;
    d3 = .0039063;
    d4 = .001;

    i__1 = ndiv[1] + ndiv[2] + ndiv[3] + ndiv[4];
    for (i__ = 1; i__ <= i__1; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    pneigh[j + i__ * 3] = co[j + neigh[i__] * 3];
	}
    }

    for (j = 1; j <= 3; ++j) {
	pnode[j - 1] = co[j + *node * 3];
    }

/*     initialisation */

    for (i__ = -1; i__ <= 1; ++i__) {
	for (j = -1; j <= 1; ++j) {
	    xi[i__ + j * 3 + 4] = i__ * d1;
	    et[i__ + j * 3 + 4] = j * d1;
	    distattach_(&xi[i__ + j * 3 + 4], &et[i__ + j * 3 + 4], &pneigh[4]
		    , &ndiv[1], pnode, &a[i__ + j * 3 + 4], p);
	}
    }

/*     minimizing the distance from the face to the node */

    for (ii = 1; ii <= 100000; ++ii) {
	distmin = a[4];
	imin = 0;
	jmin = 0;
	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		if (a[i__ + j * 3 + 4] < distmin) {
		    distmin = a[i__ + j * 3 + 4];
		    imin = i__;
		    jmin = j;
		}
	    }
	}

/*       exit if minimum found */

	if (imin == 0 && jmin == 0) {
	    goto L100;
	}

	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		aold[i__ + j * 3 + 4] = a[i__ + j * 3 + 4];
		xiold[i__ + j * 3 + 4] = xi[i__ + j * 3 + 4];
		etold[i__ + j * 3 + 4] = et[i__ + j * 3 + 4];
	    }
	}

	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		if (i__ + imin >= -1 && i__ + imin <= 1 && j + jmin >= -1 && 
			j + jmin <= 1) {
		    a[i__ + j * 3 + 4] = aold[i__ + imin + (j + jmin) * 3 + 4]
			    ;
		    xi[i__ + j * 3 + 4] = xiold[i__ + imin + (j + jmin) * 3 + 
			    4];
		    et[i__ + j * 3 + 4] = etold[i__ + imin + (j + jmin) * 3 + 
			    4];
		} else {
		    xi[i__ + j * 3 + 4] += imin * d1;
		    et[i__ + j * 3 + 4] += jmin * d1;

/* Computing MIN */
		    d__1 = xi[i__ + j * 3 + 4];
		    xi[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
		    d__1 = xi[i__ + j * 3 + 4];
		    xi[i__ + j * 3 + 4] = smax(d__1,-1.);
/* Computing MIN */
		    d__1 = et[i__ + j * 3 + 4];
		    et[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
		    d__1 = et[i__ + j * 3 + 4];
		    et[i__ + j * 3 + 4] = smax(d__1,-1.);

		    distattach_(&xi[i__ + j * 3 + 4], &et[i__ + j * 3 + 4], &
			    pneigh[4], &ndiv[1], pnode, &a[i__ + j * 3 + 4], 
			    p);
		}
	    }
	}
    }
L100:

/*     2nd run */
/*     initialisation */

    xiopt = xi[4];
    etopt = et[4];
    for (i__ = -1; i__ <= 1; ++i__) {
	for (j = -1; j <= 1; ++j) {
	    xi[i__ + j * 3 + 4] = xiopt + i__ * d2;
	    et[i__ + j * 3 + 4] = etopt + j * d2;
/* Computing MIN */
	    d__1 = xi[i__ + j * 3 + 4];
	    xi[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
	    d__1 = xi[i__ + j * 3 + 4];
	    xi[i__ + j * 3 + 4] = smax(d__1,-1.);
/* Computing MIN */
	    d__1 = et[i__ + j * 3 + 4];
	    et[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
	    d__1 = et[i__ + j * 3 + 4];
	    et[i__ + j * 3 + 4] = smax(d__1,-1.);
	    distattach_(&xi[i__ + j * 3 + 4], &et[i__ + j * 3 + 4], &pneigh[4]
		    , &ndiv[1], pnode, &a[i__ + j * 3 + 4], p);
	}
    }

/*     minimizing the distance from the face to the node */

    for (ii = 1; ii <= 100000; ++ii) {
	distmin = a[4];
	imin = 0;
	jmin = 0;
	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		if (a[i__ + j * 3 + 4] < distmin) {
		    distmin = a[i__ + j * 3 + 4];
		    imin = i__;
		    jmin = j;
		}
	    }
	}

/*       exit if minimum found */

	if (imin == 0 && jmin == 0) {
	    goto L101;
	}

	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		aold[i__ + j * 3 + 4] = a[i__ + j * 3 + 4];
		xiold[i__ + j * 3 + 4] = xi[i__ + j * 3 + 4];
		etold[i__ + j * 3 + 4] = et[i__ + j * 3 + 4];
	    }
	}

	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		if (i__ + imin >= -1 && i__ + imin <= 1 && j + jmin >= -1 && 
			j + jmin <= 1) {
		    a[i__ + j * 3 + 4] = aold[i__ + imin + (j + jmin) * 3 + 4]
			    ;
		    xi[i__ + j * 3 + 4] = xiold[i__ + imin + (j + jmin) * 3 + 
			    4];
		    et[i__ + j * 3 + 4] = etold[i__ + imin + (j + jmin) * 3 + 
			    4];
		} else {
		    xi[i__ + j * 3 + 4] += imin * d2;
		    et[i__ + j * 3 + 4] += jmin * d2;

/* Computing MIN */
		    d__1 = xi[i__ + j * 3 + 4];
		    xi[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
		    d__1 = xi[i__ + j * 3 + 4];
		    xi[i__ + j * 3 + 4] = smax(d__1,-1.);
/* Computing MIN */
		    d__1 = et[i__ + j * 3 + 4];
		    et[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
		    d__1 = et[i__ + j * 3 + 4];
		    et[i__ + j * 3 + 4] = smax(d__1,-1.);

		    distattach_(&xi[i__ + j * 3 + 4], &et[i__ + j * 3 + 4], &
			    pneigh[4], &ndiv[1], pnode, &a[i__ + j * 3 + 4], 
			    p);
		}
	    }
	}
    }
L101:

/*     3rd run */
/*     initialisation */

    xiopt = xi[4];
    etopt = et[4];
    for (i__ = -1; i__ <= 1; ++i__) {
	for (j = -1; j <= 1; ++j) {
	    xi[i__ + j * 3 + 4] = xiopt + i__ * d3;
	    et[i__ + j * 3 + 4] = etopt + j * d3;
/* Computing MIN */
	    d__1 = xi[i__ + j * 3 + 4];
	    xi[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
	    d__1 = xi[i__ + j * 3 + 4];
	    xi[i__ + j * 3 + 4] = smax(d__1,-1.);
/* Computing MIN */
	    d__1 = et[i__ + j * 3 + 4];
	    et[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
	    d__1 = et[i__ + j * 3 + 4];
	    et[i__ + j * 3 + 4] = smax(d__1,-1.);
	    distattach_(&xi[i__ + j * 3 + 4], &et[i__ + j * 3 + 4], &pneigh[4]
		    , &ndiv[1], pnode, &a[i__ + j * 3 + 4], p);
	}
    }

/*     minimizing the distance from the face to the node */

    for (ii = 1; ii <= 100000; ++ii) {
	distmin = a[4];
	imin = 0;
	jmin = 0;
	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		if (a[i__ + j * 3 + 4] < distmin) {
		    distmin = a[i__ + j * 3 + 4];
		    imin = i__;
		    jmin = j;
		}
	    }
	}

/*       exit if minimum found */

	if (imin == 0 && jmin == 0) {
	    goto L102;
	}

	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		aold[i__ + j * 3 + 4] = a[i__ + j * 3 + 4];
		xiold[i__ + j * 3 + 4] = xi[i__ + j * 3 + 4];
		etold[i__ + j * 3 + 4] = et[i__ + j * 3 + 4];
	    }
	}

	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		if (i__ + imin >= -1 && i__ + imin <= 1 && j + jmin >= -1 && 
			j + jmin <= 1) {
		    a[i__ + j * 3 + 4] = aold[i__ + imin + (j + jmin) * 3 + 4]
			    ;
		    xi[i__ + j * 3 + 4] = xiold[i__ + imin + (j + jmin) * 3 + 
			    4];
		    et[i__ + j * 3 + 4] = etold[i__ + imin + (j + jmin) * 3 + 
			    4];
		} else {
		    xi[i__ + j * 3 + 4] += imin * d3;
		    et[i__ + j * 3 + 4] += jmin * d3;

/* Computing MIN */
		    d__1 = xi[i__ + j * 3 + 4];
		    xi[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
		    d__1 = xi[i__ + j * 3 + 4];
		    xi[i__ + j * 3 + 4] = smax(d__1,-1.);
/* Computing MIN */
		    d__1 = et[i__ + j * 3 + 4];
		    et[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
		    d__1 = et[i__ + j * 3 + 4];
		    et[i__ + j * 3 + 4] = smax(d__1,-1.);

		    distattach_(&xi[i__ + j * 3 + 4], &et[i__ + j * 3 + 4], &
			    pneigh[4], &ndiv[1], pnode, &a[i__ + j * 3 + 4], 
			    p);
		}
	    }
	}
    }
L102:

/*     4th run */
/*     initialisation */

    xiopt = xi[4];
    etopt = et[4];
    for (i__ = -1; i__ <= 1; ++i__) {
	for (j = -1; j <= 1; ++j) {
	    xi[i__ + j * 3 + 4] = xiopt + i__ * d4;
	    et[i__ + j * 3 + 4] = etopt + j * d4;
/* Computing MIN */
	    d__1 = xi[i__ + j * 3 + 4];
	    xi[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
	    d__1 = xi[i__ + j * 3 + 4];
	    xi[i__ + j * 3 + 4] = smax(d__1,-1.);
/* Computing MIN */
	    d__1 = et[i__ + j * 3 + 4];
	    et[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
	    d__1 = et[i__ + j * 3 + 4];
	    et[i__ + j * 3 + 4] = smax(d__1,-1.);
	    distattach_(&xi[i__ + j * 3 + 4], &et[i__ + j * 3 + 4], &pneigh[4]
		    , &ndiv[1], pnode, &a[i__ + j * 3 + 4], p);
	}
    }

/*     minimizing the distance from the face to the node */

    for (ii = 1; ii <= 100000; ++ii) {
	distmin = a[4];
	imin = 0;
	jmin = 0;
	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		if (a[i__ + j * 3 + 4] < distmin) {
		    distmin = a[i__ + j * 3 + 4];
		    imin = i__;
		    jmin = j;
		}
	    }
	}

/*       exit if minimum found */

	if (imin == 0 && jmin == 0) {
	    goto L103;
	}

	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		aold[i__ + j * 3 + 4] = a[i__ + j * 3 + 4];
		xiold[i__ + j * 3 + 4] = xi[i__ + j * 3 + 4];
		etold[i__ + j * 3 + 4] = et[i__ + j * 3 + 4];
	    }
	}

	for (i__ = -1; i__ <= 1; ++i__) {
	    for (j = -1; j <= 1; ++j) {
		if (i__ + imin >= -1 && i__ + imin <= 1 && j + jmin >= -1 && 
			j + jmin <= 1) {
		    a[i__ + j * 3 + 4] = aold[i__ + imin + (j + jmin) * 3 + 4]
			    ;
		    xi[i__ + j * 3 + 4] = xiold[i__ + imin + (j + jmin) * 3 + 
			    4];
		    et[i__ + j * 3 + 4] = etold[i__ + imin + (j + jmin) * 3 + 
			    4];
		} else {
		    xi[i__ + j * 3 + 4] += imin * d4;
		    et[i__ + j * 3 + 4] += jmin * d4;

/* Computing MIN */
		    d__1 = xi[i__ + j * 3 + 4];
		    xi[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
		    d__1 = xi[i__ + j * 3 + 4];
		    xi[i__ + j * 3 + 4] = smax(d__1,-1.);
/* Computing MIN */
		    d__1 = et[i__ + j * 3 + 4];
		    et[i__ + j * 3 + 4] = smin(d__1,1.);
/* Computing MAX */
		    d__1 = et[i__ + j * 3 + 4];
		    et[i__ + j * 3 + 4] = smax(d__1,-1.);

		    distattach_(&xi[i__ + j * 3 + 4], &et[i__ + j * 3 + 4], &
			    pneigh[4], &ndiv[1], pnode, &a[i__ + j * 3 + 4], 
			    p);
		}
	    }
	}
    }
L103:

    distattach_(&xi[4], &et[4], &pneigh[4], &ndiv[1], pnode, &a[4], p);
    for (i__ = 1; i__ <= 3; ++i__) {
	co[i__ + *node * 3] = p[i__ - 1];
    }

    return 0;
} /* attach_ */



/* Subroutine */ int smooth_length(co, nk, kon, ne, iptr1, iptr2, nside, ineino, 
	nkind, neigh, ndiv, pneigh, maxsumdiv, ieltyp_frd, n_ori)
doublereal *co;
integer *nk, *kon, *ne, *iptr1, *iptr2, *nside, *ineino, *nkind, *neigh, *
	ndiv;
doublereal *pneigh;
integer *maxsumdiv, *ieltyp_frd, *n_ori;
{
    /* Initialized data */

    static integer nei2d[12]	/* was [3][4] */ = { 5,1,2,6,2,3,7,3,4,8,4,1 }
	    ;
    static integer nei3d[36]	/* was [3][12] */ = { 9,1,2,10,2,3,11,3,4,12,
	    1,4,13,5,6,14,6,7,15,7,8,16,5,8,17,1,5,18,2,6,19,3,7,20,4,8 };

    /* System generated locals */
    integer neigh_dim1, neigh_offset, i__1, i__2, i__3;


    /* Local variables */
    static integer ieltyp;
    static integer npec, node, npet, location, node1, node2, i__, j, l;
    static doublereal x, y, z__;
    static integer niter;
    extern /* Subroutine */ int clasnodes_();
    static doublereal dc, dd;
    static integer ii;
    static doublereal dx, dy, dz;
    static doublereal n1[3], n2[3], nm[3];
    extern /* Subroutine */ int attach_();


/*     smoothes a mesh by means of a length weighted Laplacian */

/*     ieltyp is determined according to frd spec */
/*     C2D4: ieltyp=9 */
/*     C2D8: ieltyp=10 */
/*     C3D8: ieltyp=1 */
/*     C3D20: ieltyp=4 */

    ieltyp=*ieltyp_frd;

/*     npec: # corner nodes (eg. 8 for C3D20) */
/*     npet: total # nodes (eg. 20 for C3D20) */


/*     niter determines the number of smoothing loops */
    niter = 10;




    /* Parameter adjustments */
    co -= 4;
    kon -= 21;
    --iptr1;
    --iptr2;
    --nside;
    --ineino;
    --nkind;
    --n_ori;
    ndiv -= 5;
    pneigh -= 4;
    neigh_dim1 = *maxsumdiv;
    neigh_offset = 1 + neigh_dim1 * 1;
    neigh -= neigh_offset;

    /* Function Body */

/*     C2D4: ieltyp=1 */
/*     C2D8: ieltyp=2 */
/*     C3D8: ieltyp=3 */
/*     C3D20: ieltyp=4 */
/*     change the element_nr frm frd to guidos notation */
    if (ieltyp == 9) {
	ieltyp = 1;
	npec = 4;
	npet = 4;
    } else if (ieltyp == 10) {
	ieltyp = 2;
	npec = 4;
	npet = 8;
    } else if (ieltyp == 1) {
	ieltyp = 3;
	npec = 8;
	npet = 8;
    } else if (ieltyp == 4) {
	ieltyp = 4;
	npec = 8;
	npet = 20;
/*      else */
/*        write(*,*) 'eltype not known' */
/*        stop */
    }

/*     determining the elements and nodes neighboring a given node */

/* 	write(*,*) 'ieltyp', ieltyp */
    clasnodes_(&co[4], nk, &kon[21], ne, &iptr1[1], &iptr2[1], &nside[1], &
	    ineino[1], &nkind[1], &ieltyp, &npec, &npet);

/*     smoothing the position of each node */

    i__1 = niter;
    for (ii = 1; ii <= i__1; ++ii) {

	i__2 = *nk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    j = nkind[i__];
	    if (j < 0 || j > 2) {
		goto L101;
	    }
	    dx = 0.;
	    dy = 0.;
	    dz = 0.;
	    dc = 0.;
	    i__3 = nside[i__];
	    for (l = 1; l <= i__3; ++l) {
		if (l == 1) {
		    location = iptr1[i__];
		} else {
		    location = iptr2[location];
		}
		node = ineino[location];

/*         for the smoothing of surface nodes only surface neighbours */
/*         are taken into account */

		if (j > 0 && nkind[node] != j && nkind[node] > -1) {
		    goto L102;
		}

		x = co[node * 3 + 1] - co[i__ * 3 + 1];
		y = co[node * 3 + 2] - co[i__ * 3 + 2];
		z__ = co[node * 3 + 3] - co[i__ * 3 + 3];
		/* dd = sqrt(x * x + y * y + z__ * z__); */
		/* dd = 1/sqrt(x * x + y * y + z__ * z__);  */ 
                dd=1.;

		dx += dd * x;
		dy += dd * y;
		dz += dd * z__;
		dc += dd;
L102:
		;
	    }

	    if (dx * dx + dy * dy + dz * dz < 1e-14) {
		goto L101;
	    }

	    dx /= dc;
	    dy /= dc;
	    dz /= dc;

	    co[i__ * 3 + 1] += dx;
	    co[i__ * 3 + 2] += dy;
	    co[i__ * 3 + 3] += dz;

	    
	    if (j > 0) {
	       attach_(&co[4], nk, &neigh[j * neigh_dim1 + 1], &pneigh[4], &ndiv[(j << 2) + 1], &i__);
	    }
	    
L101:
	    ;
	}

	if (ii != niter || (ieltyp == 1 || ieltyp == 3)) {
	    goto L103;
	}

/*       adjusting the middle nodes (possible improvement: */
/*       parabolic instead of linear adjustment) */

	i__2 = *ne;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = npet;
	    for (l = npec + 1; l <= i__3; ++l) {
		node = kon[l + i__ * 20];
		j = nkind[node];
  		if (ieltyp == 2) {
		    node1 = kon[nei2d[(l - 4) * 3 - 2] + i__ * 20];
		    node2 = kon[nei2d[(l - 4) * 3 - 1] + i__ * 20];
		} else if (ieltyp == 4) {
		    node1 = kon[nei3d[(l - 8) * 3 - 2] + i__ * 20];
		    node2 = kon[nei3d[(l - 8) * 3 - 1] + i__ * 20];
		}
		if (j <0 || j > 2) {
                  n1[0]=co[node1 * 3 + 1];
                  n1[1]=co[node1 * 3 + 2];
                  n1[2]=co[node1 * 3 + 3];
                  n2[0]=co[node2 * 3 + 1];
                  n2[1]=co[node2 * 3 + 2];
                  n2[2]=co[node2 * 3 + 3];
                  nm[0]=co[node  * 3 + 1];
                  nm[1]=co[node  * 3 + 2];
                  nm[2]=co[node  * 3 + 3];
		  adjustMidsideNode(n1,n2,nm,0);
	  	  co[node * 3 + 1] = nm[0];
		  co[node * 3 + 2] = nm[1];
	  	  co[node * 3 + 3] = nm[2];
                  /*
  		  if (j > 0) {
		    attach_(&co[4], nk, &neigh[j * neigh_dim1 + 1], &pneigh[4]
			    , &ndiv[(j << 2) + 1], &node);
                  }
                  */
		}
                else
                {
		  co[node * 3 + 1] = (co[node1 * 3 + 1] + co[node2 * 3 + 1]) / 
			2.;
		  co[node * 3 + 2] = (co[node1 * 3 + 2] + co[node2 * 3 + 2]) / 
			2.;
		  co[node * 3 + 3] = (co[node1 * 3 + 3] + co[node2 * 3 + 3]) / 
			2.;
                  /*
  		  if (j > 0) {
		    attach_(&co[4], nk, &neigh[j * neigh_dim1 + 1], &pneigh[4]
			    , &ndiv[(j << 2) + 1], &node);
                  }
                  */
		}
		;
	    }
	}

L103:
	;
    }

    return 0;
} /* smooth_ */



/* Subroutine */ int smooth_angle(co, nk, kon, ne, iptr1, iptr2, nside, ineino, 
	nkind, neigh, ndiv, pneigh, maxsumdiv, ieltyp_frd, n_ori)
doublereal *co;
integer *nk, *kon, *ne, *iptr1, *iptr2, *nside, *ineino, *nkind, *neigh, *
	ndiv;
doublereal *pneigh;
integer *maxsumdiv, *ieltyp_frd, *n_ori;
{
    /* Initialized data */

    static integer nei2d[12]	/* was [3][4] */ = { 5,1,2,6,2,3,7,3,4,8,4,1 }
	    ;
    static integer nei3d[36]	/* was [3][12] */ = { 9,1,2,10,2,3,11,3,4,12,
	    1,4,13,5,6,14,6,7,15,7,8,16,5,8,17,1,5,18,2,6,19,3,7,20,4,8 };

    /* System generated locals */
    integer neigh_dim1, neigh_offset, i__1, i__2, i__3;


    /* Local variables */
    static integer ieltyp;
    static integer npec, node, npet, location, node1, node2, i__, j, l, k;
    static doublereal x[30], y[30], z__[30];
    static integer niter,j1,j2,j1max,j2max;
    extern /* Subroutine */ int clasnodes_();
    static doublereal dd[30];
    static integer ii;
    static doublereal dx, dy, dz, prod,prodmax;
    static doublereal n1[3], n2[3], nm[3];
    extern /* Subroutine */ int attach_();

    static integer slave_node[100], iii;



/*     ieltyp is determined according to frd spec */
/*     C2D4: ieltyp=9 */
/*     C2D8: ieltyp=10 */
/*     C3D8: ieltyp=1 */
/*     C3D20: ieltyp=4 */

    ieltyp=*ieltyp_frd;

/*     npec: # corner nodes (eg. 8 for C3D20) */
/*     npet: total # nodes (eg. 20 for C3D20) */


/*     niter determines the number of smoothing loops */
    niter = 40;



    /* Parameter adjustments */
    --n_ori;
    co -= 4;
    kon -= 21;
    --iptr1;
    --iptr2;
    --nside;
    --ineino;
    --nkind;
    ndiv -= 5;
    pneigh -= 4;
    neigh_dim1 = *maxsumdiv;
    neigh_offset = 1 + neigh_dim1 * 1;
    neigh -= neigh_offset;

    /* Function Body */

/*     C2D4: ieltyp=1 */
/*     C2D8: ieltyp=2 */
/*     C3D8: ieltyp=3 */
/*     C3D20: ieltyp=4 */
/*     change the element_nr frm frd to guidos notation */
    if (ieltyp == 9) {
	ieltyp = 1;
	npec = 4;
	npet = 4;
    } else if (ieltyp == 10) {
	ieltyp = 2;
	npec = 4;
	npet = 8;
    } else if (ieltyp == 1) {
	ieltyp = 3;
	npec = 8;
	npet = 8;
    } else if (ieltyp == 4) {
	ieltyp = 4;
	npec = 8;
	npet = 20;
/*      else */
/*        write(*,*) 'eltype not known' */
/*        stop */
    }

/*     determining the elements and nodes neighboring a given node */

/* 	write(*,*) 'ieltyp', ieltyp */
    clasnodes_(&co[4], nk, &kon[21], ne, &iptr1[1], &iptr2[1], &nside[1], &
	    ineino[1], &nkind[1], &ieltyp, &npec, &npet);

/*     smoothing the position of each node */

    i__1 = niter;
    for (ii = 1; ii <= i__1; ++ii) {

	i__2 = *nk;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    j = nkind[i__];

	    dx = 0.;
	    dy = 0.;
	    dz = 0.;
            k=0;
	    i__3 = nside[i__];

            if(n_ori[i__]==NOD)
              printf("n1:%d %lf %lf %lf\n", n_ori[i__], co[i__ * 3 + 1]* scale->w+scale->x,co[i__ * 3 + 2]* scale->w+scale->y, co[i__ * 3 + 3]* scale->w+scale->z);

	    for (l = 1; l <= i__3; ++l) {
		if (l == 1) {
		    location = iptr1[i__];
		} else {
		    location = iptr2[location];
		}
		node = ineino[location];


/*
                j=0: Bulknode
                 -1: Edge
              -2,-3: edge on top or bottom face
                1,2: Top Bottom
                3-6: other sides
*/

/*         for the smoothing of surface nodes only surface neighbours */
/*         are taken into account */
	        if ((j < 3 && j > 0)  && ( (nkind[node] != j) && (nkind[node] !=-2) )) goto L102;
                if ((j<-1) && ((nkind[node] != j) && (nkind[node] != 1) && (nkind[node] != 2) ))  goto L102;

                if(n_ori[i__]==NOD)
                  printf("n2:%d k:%d %lf %lf %lf\n", n_ori[node],k, co[node * 3 + 1]* scale->w+scale->x,co[node * 3 + 2]* scale->w+scale->y, co[node * 3 + 3]* scale->w+scale->z);

 		x[k] = co[node * 3 + 1] - co[i__ * 3 + 1];
		y[k] = co[node * 3 + 2] - co[i__ * 3 + 2];
		z__[k] = co[node * 3 + 3] - co[i__ * 3 + 3];

                /* normieren auf laenge 1 */
		dd[k] = sqrt(x[k] * x[k] + y[k] * y[k] + z__[k] * z__[k]);
		x[k] = x[k]/dd[k];
		y[k] =  y[k]/dd[k];
		z__[k] = z__[k]/dd[k];

                slave_node[k]=node;
                k++;
L102:
		;
	    }

            j1max=-1;
            prodmax=0;
            for(j1=0; j1<k; j1++)
            {
              for(j2=j1+1; j2<k; j2++)
              {
                  prod=x[j1]*x[j2]+y[j1]*y[j2]+z__[j1]*z__[j2];
                  if(j1max==-1)
                  {
                    prodmax=prod;
                    j1max=j1;
                    j2max=j2;
                  }
                  else if(prodmax<=prod)
                  {
                    prodmax=prod;
                    j1max=j1;
                    j2max=j2;
                }
              }
            }
            /* optimize only if cos is below a certain value (80 deg) */
            if(prodmax < 0.174) goto L101;

            if(n_ori[i__]==NOD)
              printf("prodmax:%lf j1max:%d j2max:%d j:%d nj1:%d nj2:%d nkind:%d %d\n", prodmax,j1max,j2max,j,n_ori[slave_node[j1max]],n_ori[slave_node[j2max]],nkind[slave_node[j1max]],nkind[slave_node[j2max]]);


	    if(j<0 || j >2)
            {
	      if ((nkind[slave_node[j1max]] > -1)&&(nkind[slave_node[j1max]] <3))
	      {
                dx=(dd[j2max]*x[j2max])/(2.0*niter)*prodmax;
                dy=(dd[j2max]*y[j2max])/(2.0*niter)*prodmax;
                dz=(dd[j2max]*z__[j2max])/(2.0*niter)*prodmax;
	        co[slave_node[j1max] * 3 + 1] -= dx;
	        co[slave_node[j1max] * 3 + 2] -= dy;
	        co[slave_node[j1max] * 3 + 3] -= dz;
                if(n_ori[i__]==NOD)  printf("dv1 %f %f %f\n", dx* scale->w,dy* scale->w,dz* scale->w);
	      }
	      if ((nkind[slave_node[j2max]] > -1)&&(nkind[slave_node[j2max]] <3))
	      {
                dx=(dd[j1max]*x[j1max])/(2.0*niter)*prodmax;
                dy=(dd[j1max]*y[j1max])/(2.0*niter)*prodmax;
                dz=(dd[j1max]*z__[j1max])/(2.0*niter)*prodmax;
	        co[slave_node[j2max] * 3 + 1] -= dx;
	        co[slave_node[j2max] * 3 + 2] -= dy;
	        co[slave_node[j2max] * 3 + 3] -= dz;
                if(n_ori[i__]==NOD)  printf("dv2 %f %f %f\n", dx* scale->w,dy* scale->w,dz* scale->w);
	      }
	    }
            else
	    {
              dx=(dd[j1max]*x[j1max]+dd[j2max]*x[j2max])/(2.0*niter)*prodmax;
              dy=(dd[j1max]*y[j1max]+dd[j2max]*y[j2max])/(2.0*niter)*prodmax;
              dz=(dd[j1max]*z__[j1max]+dd[j2max]*z__[j2max])/(2.0*niter)*prodmax;
	      co[i__ * 3 + 1] += dx;
	      co[i__ * 3 + 2] += dy;
	      co[i__ * 3 + 3] += dz;
	    }
L101:
	    ;
	}
    }

    for (iii = 1; iii <=*nk ; ++iii)
    {
        j = nkind[iii];

	if ((j == 1)||(j == 2)) {
	  // attach_(&co[4], nk, &neigh[j * neigh_dim1 + 1], &pneigh[4], &ndiv[(j << 2) + 1], &iii);
	}

	if ( (ieltyp == 1 || ieltyp == 3)) {
	    goto L103;
	}

/*       adjusting the middle nodes (possible improvement: */
/*       parabolic instead of linear adjustment) */

	i__2 = *ne;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = npet;
	    for (l = npec + 1; l <= i__3; ++l) {
		node = kon[l + i__ * 20];
		j = nkind[node];
  		if (ieltyp == 2) {
		    node1 = kon[nei2d[(l - 4) * 3 - 2] + i__ * 20];
		    node2 = kon[nei2d[(l - 4) * 3 - 1] + i__ * 20];
		} else if (ieltyp == 4) {
		    node1 = kon[nei3d[(l - 8) * 3 - 2] + i__ * 20];
		    node2 = kon[nei3d[(l - 8) * 3 - 1] + i__ * 20];
		}
		if (j <0 || j > 2) {
                  n1[0]=co[node1 * 3 + 1];
                  n1[1]=co[node1 * 3 + 2];
                  n1[2]=co[node1 * 3 + 3];
                  n2[0]=co[node2 * 3 + 1];
                  n2[1]=co[node2 * 3 + 2];
                  n2[2]=co[node2 * 3 + 3];
                  nm[0]=co[node  * 3 + 1];
                  nm[1]=co[node  * 3 + 2];
                  nm[2]=co[node  * 3 + 3];
		  adjustMidsideNode(n1,n2,nm,0);
	  	  co[node * 3 + 1] = nm[0];
		  co[node * 3 + 2] = nm[1];
	  	  co[node * 3 + 3] = nm[2];
		}
                else
                {
		  co[node * 3 + 1] = (co[node1 * 3 + 1] + co[node2 * 3 + 1]) / 
			2.;
		  co[node * 3 + 2] = (co[node1 * 3 + 2] + co[node2 * 3 + 2]) / 
			2.;
		  co[node * 3 + 3] = (co[node1 * 3 + 3] + co[node2 * 3 + 3]) / 
			2.;
		}
		;
	    }
	}

L103:
	;
    }

    return 0;
} /* smooth_ */



/* Subroutine */ int smooth_midnodes(co, nk, kon, ne, iptr1, iptr2, nside, ineino, 
	nkind, neigh, ndiv, pneigh, maxsumdiv, ieltyp_frd, n_ori)
doublereal *co;
integer *nk, *kon, *ne, *iptr1, *iptr2, *nside, *ineino, *nkind, *neigh, *
	ndiv;
doublereal *pneigh;
integer *maxsumdiv, *ieltyp_frd, *n_ori;
{
    /* Initialized data */

    static integer nei2d[12]	/* was [3][4] */ = { 5,1,2,6,2,3,7,3,4,8,4,1 }
	    ;
    static integer nei3d[36]	/* was [3][12] */ = { 9,1,2,10,2,3,11,3,4,12,
	    1,4,13,5,6,14,6,7,15,7,8,16,5,8,17,1,5,18,2,6,19,3,7,20,4,8 };

    /* System generated locals */
    integer neigh_dim1, neigh_offset, i__2, i__3;


    /* Local variables */
    static integer ieltyp;
    static integer npec, node, npet, node1, node2, i__, j, l;
    extern /* Subroutine */ int clasnodes_();
    static doublereal n1[3], n2[3], nm[3];
    extern /* Subroutine */ int attach_();

    static integer iii;



/*     ieltyp is determined according to frd spec */
/*     C2D4: ieltyp=9 */
/*     C2D8: ieltyp=10 */
/*     C3D8: ieltyp=1 */
/*     C3D20: ieltyp=4 */

    ieltyp=*ieltyp_frd;

/*     npec: # corner nodes (eg. 8 for C3D20) */
/*     npet: total # nodes (eg. 20 for C3D20) */

    /* Parameter adjustments */
    --n_ori;
    co -= 4;
    kon -= 21;
    --iptr1;
    --iptr2;
    --nside;
    --ineino;
    --nkind;
    ndiv -= 5;
    pneigh -= 4;
    neigh_dim1 = *maxsumdiv;
    neigh_offset = 1 + neigh_dim1 * 1;
    neigh -= neigh_offset;

    /* Function Body */

/*     C2D4: ieltyp=1 */
/*     C2D8: ieltyp=2 */
/*     C3D8: ieltyp=3 */
/*     C3D20: ieltyp=4 */
/*     change the element_nr frm frd to guidos notation */
    if (ieltyp == 9) {
	ieltyp = 1;
	npec = 4;
	npet = 4;
    } else if (ieltyp == 10) {
	ieltyp = 2;
	npec = 4;
	npet = 8;
    } else if (ieltyp == 1) {
	ieltyp = 3;
	npec = 8;
	npet = 8;
    } else if (ieltyp == 4) {
	ieltyp = 4;
	npec = 8;
	npet = 20;
/*      else */
/*        write(*,*) 'eltype not known' */
/*        stop */
    }

/*     determining the elements and nodes neighboring a given node */

/* 	write(*,*) 'ieltyp', ieltyp */
    clasnodes_(&co[4], nk, &kon[21], ne, &iptr1[1], &iptr2[1], &nside[1], &
	    ineino[1], &nkind[1], &ieltyp, &npec, &npet);

    for (iii = 1; iii <=*nk ; ++iii)
    {
        j = nkind[iii];


	if ( (ieltyp == 1 || ieltyp == 3)) {
	    goto L103;
	}

/*       adjusting the middle nodes (possible improvement: */
/*       parabolic instead of linear adjustment) */

	i__2 = *ne;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = npet;
	    for (l = npec + 1; l <= i__3; ++l) {
		node = kon[l + i__ * 20];
		j = nkind[node];
  		if (ieltyp == 2) {
		    node1 = kon[nei2d[(l - 4) * 3 - 2] + i__ * 20];
		    node2 = kon[nei2d[(l - 4) * 3 - 1] + i__ * 20];
		} else if (ieltyp == 4) {
		    node1 = kon[nei3d[(l - 8) * 3 - 2] + i__ * 20];
		    node2 = kon[nei3d[(l - 8) * 3 - 1] + i__ * 20];
		}
		if (j <0 || j > 2) {
                  n1[0]=co[node1 * 3 + 1];
                  n1[1]=co[node1 * 3 + 2];
                  n1[2]=co[node1 * 3 + 3];
                  n2[0]=co[node2 * 3 + 1];
                  n2[1]=co[node2 * 3 + 2];
                  n2[2]=co[node2 * 3 + 3];
                  nm[0]=co[node  * 3 + 1];
                  nm[1]=co[node  * 3 + 2];
                  nm[2]=co[node  * 3 + 3];
		  adjustMidsideNode(n1,n2,nm,0);
	  	  co[node * 3 + 1] = nm[0];
		  co[node * 3 + 2] = nm[1];
	  	  co[node * 3 + 3] = nm[2];
		}
                else
                {
		  co[node * 3 + 1] = (co[node1 * 3 + 1] + co[node2 * 3 + 1]) / 
			2.;
		  co[node * 3 + 2] = (co[node1 * 3 + 2] + co[node2 * 3 + 2]) / 
			2.;
		  co[node * 3 + 3] = (co[node1 * 3 + 3] + co[node2 * 3 + 3]) / 
			2.;
		}
		;
	    }
	}

L103:
	;
    }

    return 0;
} /* smooth_ */

/* clasnodes.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/




