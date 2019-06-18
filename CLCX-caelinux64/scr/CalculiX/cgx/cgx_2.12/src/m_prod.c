
/* Subroutine */ int m_prod(n, a, b, c)
int *n;
double *a, *b, *c;
{
    /* System generated locals */
    int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2, 
	    i__3;

    /* Local variables */
    static int i, j, k;

/* ********************************************************* */
/* *  Matrizenmultiplikation: C = A x B                    * */
/* ********************************************************* */

    /* Parameter adjustments */
    c_dim1 = *n;
    c_offset = c_dim1 + 1;
    c -= c_offset;
    b_dim1 = *n;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    c[i + j * c_dim1] = (double)0.;
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
		c[i + j * c_dim1] += a[i + k * a_dim1] * b[k + j * b_dim1];
/* L20: */
	    }
	}
    }

/* L9999: */
    return 0;
} /* matprod_ */

