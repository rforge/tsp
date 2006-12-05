
#include <R.h>
#include <Rdefines.h>


/* two_opt implements a greedy heuristic that exchanges two edges 
 * immediately if this improves the tour length and stops if no further
 * improvement (over all combinations of edges) is possible. exchanging
 * edges amounts to reversing subpaths.
 *
 * the time complexity is O(n^2) with n the number of cities.
 *
 * note: the algorithm could easily be extended to a simulated 
 *	 annealing algorithm. the code is slightly optimized.
 *
 * (C) ceeboo 2006
 * License: GPL
 *
 * x ... dist
 * t ... tour as a integer vector
 * returns a new tour as an integer vector
 */


SEXP two_opt(SEXP x, SEXP t) {
    if (TYPEOF(x) != REALSXP)
	error("distance matrix has invalid storage type");
    if (TYPEOF(t) != INTSXP)
	error("tour has invalid storage type");
    int i, n, f = 0;

    // check length
    n = 1 + (int) sqrt(2*LENGTH(x));
    if (LENGTH(x) != n*(n-1)/2)
	error("distance matrix has invalid length");
    if (LENGTH(t) != n)
	error("tour has invalid length");

    for (i = 0; i < n; i++)
	if (INTEGER(t)[i] < 1 || INTEGER(t)[i] > n)
	    error("tour contains invalid values");
    
    PROTECT(t = duplicate(t)); 
    do {
	int i, j, k = 0, l = 0, c1, c2, c3, c4 = n-1;
	double e23, e13, e12, e34, e24, e31, e41;
	
	f = 0;
	c1 = INTEGER(t)[0]-1;
	for (i = 1; i < n-1; i++) {
	    c2 = INTEGER(t)[i]-1;
	    c3 = INTEGER(t)[i+1]-1;
	    if (c2 > c3)
		e23 = REAL(x)[c2+c3*(n-1)-c3*(c3+1)/2-1];
	    else
		e23 = REAL(x)[c3+c2*(n-1)-c2*(c2+1)/2-1];
	    if (c1 > c3)
		e13 = REAL(x)[c1+c3*(n-1)-c3*(c3+1)/2-1];
	    else
		e13 = REAL(x)[c3+c1*(n-1)-c1*(c1+1)/2-1];
	    if (e23 > e13) {
		f++;
		for (k = 0; k < (i+1)/2; k++) {
		    l = INTEGER(t)[i-k];
		    INTEGER(t)[i-k] = INTEGER(t)[k];
		    INTEGER(t)[k] = l;
		}
		c1 = INTEGER(t)[0]-1;
	    }
	}
	for (i = 0; i < n-3; i++) {
	    c1 = INTEGER(t)[i]-1;
	    c2 = INTEGER(t)[i+1]-1;
	    if (c1 > c2)
		e12 = REAL(x)[c1+c2*(n-1)-c2*(c2+1)/2-1];
	    else
		e12 = REAL(x)[c2+c1*(n-1)-c1*(c1+1)/2-1];
	    for (j = i+2; j < n-1; j++) {
		c3 = INTEGER(t)[j]-1;
		c4 = INTEGER(t)[j+1]-1;
		if (c3 > c4)
		    e34 = REAL(x)[c3+c4*(n-1)-c4*(c4+1)/2-1];
		else
		    e34 = REAL(x)[c4+c3*(n-1)-c3*(c3+1)/2-1];
		if (c2 > c4)
		    e24 = REAL(x)[c2+c4*(n-1)-c4*(c4+1)/2-1];
		else
		    e24 = REAL(x)[c4+c2*(n-1)-c2*(c2+1)/2-1];
		if (c3 > c1)
		    e31 = REAL(x)[c3+c1*(n-1)-c1*(c1+1)/2-1];
		else
		    e31 = REAL(x)[c1+c3*(n-1)-c3*(c3+1)/2-1];

		if (e12+e34 > e24+e31) {
		    f++;
		    for (k = 0; k < (j-i)/2; k++) {
			l = INTEGER(t)[j-k];
			INTEGER(t)[j-k] = INTEGER(t)[i+1+k];
			INTEGER(t)[i+1+k] = l;
		    }
		    c2 = INTEGER(t)[i+1]-1;
		    if (c1 > c2)
			e12 = REAL(x)[c1+c2*(n-1)-c2*(c2+1)/2-1];
		    else
			e12 = REAL(x)[c2+c1*(n-1)-c1*(c1+1)/2-1];
		}
	    }
	    if (c4 > c1)
		e41 = REAL(x)[c4+c1*(n-1)-c1*(c1+1)/2-1];
	    else
		e41 = REAL(x)[c1+c4*(n-1)-c4*(c4+1)/2-1];
	    if (e12 > e41) {
		f++;
		for (k = 0; k < (j-i)/2; k++) {
		    l = INTEGER(t)[j-k];
		    INTEGER(t)[j-k] = INTEGER(t)[i+1+k];
		    INTEGER(t)[i+1+k] = l;
		}
	    }
	    R_CheckUserInterrupt();
	}
    } while (f);
   
    UNPROTECT(1);
    return t;
}

/**/
