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


#include <cgx.h>

/* Subroutine */ 
int surfMesh(int *imax, int *jmax, double *x, double *y, double *z)
{
    /* System generated locals */
    int  x_dim1, x_offset, y_dim1, y_offset, z_dim1, z_offset, i__1, i__2, 
	    i__3;
    double r__1;

    /* Local variables */
    static double dels, delt;
    static int imin, jmin;
    static double srat, trat, f[4], g[4], h[4];
    static int i, j, k, m;
    static double *s, *t, h1, h2, h3, h4;
    static int ia, ja, id, jd, ii, jj, li, lj, ip, jp, iq, jq;

    if( (s=(double *)malloc( (*imax**jmax)*sizeof(double) ) )==NULL) 
    { printf(" ERROR: malloc failure\n"); return(-1); }
    if( (t=(double *)malloc( (*imax**jmax)*sizeof(double) ) )==NULL) 
    { printf(" ERROR: malloc failure\n"); return(-1); }

/* ERZEUGUNG VON INNEREN KNOTENPUNKTEN IN EINEM QUASI-RECHTECK */
/* BEI GEGEBENEN RANDKNOTENPUNKTEN */
/* QUELLE: W.A. COOK,  BODY ORIENTED (NATURAL) CO-ORDINATES */
/* FOR GENERATING THREE-DIMENSIONAL MESHES */
/* INT.J.NME 8, 27-43, 1974 */

    /* Parameter adjustments */
    z_dim1 = *imax;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    y_dim1 = *imax;
    y_offset = y_dim1 + 1;
    y -= y_offset;
    x_dim1 = *imax;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    imin = 1;
    jmin = 1;

/* L5: */
    i__1 = *imax;
    for (i = imin; i <= i__1; ++i) {
/* L10: */
	t[i + jmin * *imax-(*imax+1)] = (double)0.;
    }
    i__1 = *jmax;
    for (j = jmin; j <= i__1; ++j) {
/* L20: */
	s[imin + j * *imax-(*imax+1)] = (double)0.;
    }
    ip = imin + 1;
    jp = jmin + 1;
    li = *imax - imin;
    lj = *jmax - jmin;
    i__1 = *imax;
    i__2 = li;
    for (k = imin; i__2 < 0 ? k >= i__1 : k <= i__1; k += i__2) {
	i__3 = *jmax;
	for (j = jp; j <= i__3; ++j) {
	    h1 = x[k + j * x_dim1] - x[k + (j - 1) * x_dim1];
	    h2 = y[k + j * y_dim1] - y[k + (j - 1) * y_dim1];
	    h3 = z[k + j * z_dim1] - z[k + (j - 1) * z_dim1];
/* L30: */
	    t[k + j * *imax-(*imax+1)] = t[k + (j - 1) * *imax-(*imax+1)] + sqrt(h1 * h1 
		    + h2 * h2 + h3 * h3);
	}
    }
    i__3 = *jmax;
    i__2 = lj;
    for (k = jmin; i__2 < 0 ? k >= i__3 : k <= i__3; k += i__2) {
	i__1 = *imax;
	for (i = ip; i <= i__1; ++i) {
	    h1 = x[i + k * x_dim1] - x[i - 1 + k * x_dim1];
	    h2 = y[i + k * y_dim1] - y[i - 1 + k * y_dim1];
	    h3 = z[i + k * z_dim1] - z[i - 1 + k * z_dim1];
/* L40: */
	    s[i + k * *imax-(*imax+1)] = s[i - 1 + k * *imax-(*imax+1)] + sqrt(h1 * h1 + 
		    h2 * h2 + h3 * h3);
	}
    }

/*      DO 50 J = JP, JMAX */
/*      T(IMIN,J) = T(IMIN,J) / T(IMIN,JMAX) */
/* 50    T(IMAX,J) = T(IMAX,J) / T(IMAX,JMAX) */
/*      DO 60 I = IP, IMAX */
/*      S(I,JMIN) = S(I,JMIN) / S(IMAX,JMIN) */
/* 60    S(I,JMAX) = S(I,JMAX) / S(IMAX,JMAX) */

/*     the previous lines were replaced by the next ones to cover */
/*     surfaces with collapsed sides */

    if ((r__1 = t[imin + *jmax * *imax-(*imax+1)], dabs(r__1)) < 1e-10) {
	i__1 = *jmax;
	for (j = jp; j <= i__1; ++j) {
	    t[imin + j * *imax-(*imax+1)] = (double) (j-jmin) / (double) (*jmax-jmin);
	}
    } else {
	i__1 = *jmax;
	for (j = jp; j <= i__1; ++j) {
	    t[imin + j * *imax-(*imax+1)] /= t[imin + *jmax * *imax-(*imax+1)];
	}
    }

    if ((r__1 = t[*imax + *jmax * *imax-(*imax+1)], dabs(r__1)) < 1e-10) {
	i__1 = *jmax;
	for (j = jp; j <= i__1; ++j) {
	    t[*imax + j * *imax-(*imax+1)] = (double) (j-jmin) / (double) (*jmax-jmin);
	}
    } else {
	i__1 = *jmax;
	for (j = jp; j <= i__1; ++j) {
	    t[*imax + j * *imax-(*imax+1)] /= t[*imax + *jmax * *imax-(*imax+1)];
	}
    }

    if ((r__1 = s[*imax + jmin * *imax-(*imax+1)], dabs(r__1)) < 1e-10) {
	i__1 = *imax;
	for (i = ip; i <= i__1; ++i) {
	    s[i + jmin * *imax-(*imax+1)] = (double) (i-imin) / (double) (*imax-imin);
	}
    } else {
	i__1 = *imax;
	for (i = ip; i <= i__1; ++i) {
	    s[i + jmin * *imax-(*imax+1)] /= s[*imax + jmin * *imax-(*imax+1)];
	}
    }

    if ((r__1 = s[*imax + *jmax * *imax-(*imax+1)], dabs(r__1)) < 1e-10) {
	i__1 = *imax;
	for (i = ip; i <= i__1; ++i) {
	    s[i + *jmax * *imax-(*imax+1)] = (double) (i-imin) / (double) (*imax-imin);
	}
    } else {
	i__1 = *imax;
	for (i = ip; i <= i__1; ++i) {
	    s[i + *jmax * *imax-(*imax+1)] /= s[*imax + *jmax * *imax-(*imax+1)];
	}
    }

    iq = *imax - 1;
    jq = *jmax - 1;
    i__1 = jq;
    for (j = jp; j <= i__1; ++j) {
/* L70: */
	s[*imax + j * *imax-(*imax+1)] = (double)1.;
    }
    i__1 = iq;
    for (i = ip; i <= i__1; ++i) {
/* L80: */
	t[i + *jmax * *imax-(*imax+1)] = (double)1.;
    }
    i__1 = iq;
    for (i = ip; i <= i__1; ++i) {
	i__2 = jq;
	for (j = jp; j <= i__2; ++j) {
	    delt = t[*imax + j * *imax-(*imax+1)] - t[imin + j * *imax-(*imax+1)];
	    dels = s[i + *jmax * *imax-(*imax+1)] - s[i + jmin * *imax-(*imax+1)];
	    h1 = (double)1. / ((double)1. - dels * delt);
	    t[i + j * *imax-(*imax+1)] = (s[i + jmin * *imax-(*imax+1)] * delt + t[imin + 
		    j * *imax-(*imax+1)]) * h1;
	    s[i + j * *imax-(*imax+1)] = (t[imin + j * *imax-(*imax+1)] * dels + s[i + 
		    jmin * *imax-(*imax+1)]) * h1;
	    for (m = 1; m <= 4; ++m) {
		f[m - 1] = (double)0.;
		g[m - 1] = (double)0.;
		h[m - 1] = (double)0.;
/* L100: */
	    }
	    id = imin;
	    for (m = 3; m <= 4; ++m) {
		i__3 = *jmax;
		for (ja = jp; ja <= i__3; ++ja) {
		    jj = ja;
		    if (t[id + ja * *imax-(*imax+1)] >= t[i + j * *imax-(*imax+1)]) {
			goto L125;
		    }
/* L120: */
		}
L125:
		k = jj - 1;
		trat = (t[i + j * *imax-(*imax+1)] - t[id + k * *imax-(*imax+1)]) / (t[id 
			+ jj * *imax-(*imax+1)] - t[id + k * *imax-(*imax+1)]);
		f[m - 1] = (x[id + jj * x_dim1] - x[id + k * x_dim1]) * trat 
			+ x[id + k * x_dim1];
		g[m - 1] = (y[id + jj * y_dim1] - y[id + k * y_dim1]) * trat 
			+ y[id + k * y_dim1];
		h[m - 1] = (z[id + jj * z_dim1] - z[id + k * z_dim1]) * trat 
			+ z[id + k * z_dim1];
/* L110: */
		id = *imax;
	    }
	    jd = jmin;
	    for (m = 1; m <= 2; ++m) {
		i__3 = *imax;
		for (ia = ip; ia <= i__3; ++ia) {
		    ii = ia;
		    if (s[ia + jd * *imax-(*imax+1)] >= s[i + j * *imax-(*imax+1)]) {
			goto L145;
		    }
/* L140: */
		}
L145:
		k = ii - 1;
		srat = (s[i + j * *imax-(*imax+1)] - s[k + jd * *imax-(*imax+1)]) / (s[ii 
			+ jd * *imax-(*imax+1)] - s[k + jd * *imax-(*imax+1)]);
		f[m - 1] = (x[ii + jd * x_dim1] - x[k + jd * x_dim1]) * srat 
			+ x[k + jd * x_dim1];
		g[m - 1] = (y[ii + jd * y_dim1] - y[k + jd * y_dim1]) * srat 
			+ y[k + jd * y_dim1];
		h[m - 1] = (z[ii + jd * z_dim1] - z[k + jd * z_dim1]) * srat 
			+ z[k + jd * z_dim1];
/* L130: */
		jd = *jmax;
	    }
	    h1 = s[i + j * *imax-(*imax+1)];
	    h2 = t[i + j * *imax-(*imax+1)];
	    h3 = (double)1. - h1;
	    h4 = (double)1. - h2;
	    x[i + j * x_dim1] = h4 * f[0] + h2 * f[1] + h3 * f[2] + h1 * f[3] 
		    - x[imin + jmin * x_dim1] * h4 * h3 - x[imin + *jmax * 
		    x_dim1] * h2 * h3 - x[*imax + jmin * x_dim1] * h1 * h4 - 
		    x[*imax + *jmax * x_dim1] * h1 * h2;
	    y[i + j * y_dim1] = h4 * g[0] + h2 * g[1] + h3 * g[2] + h1 * g[3] 
		    - y[imin + jmin * y_dim1] * h4 * h3 - y[imin + *jmax * 
		    y_dim1] * h2 * h3 - y[*imax + jmin * y_dim1] * h1 * h4 - 
		    y[*imax + *jmax * y_dim1] * h1 * h2;
	    z[i + j * z_dim1] = h4 * h[0] + h2 * h[1] + h3 * h[2] + h1 * h[3] 
		    - z[imin + jmin * z_dim1] * h4 * h3 - z[imin + *jmax * 
		    z_dim1] * h2 * h3 - z[*imax + jmin * z_dim1] * h1 * h4 - 
		    z[*imax + *jmax * z_dim1] * h1 * h2;
/* L90: */
	}
    }
    free(s); free(t);
    return 0;
}

