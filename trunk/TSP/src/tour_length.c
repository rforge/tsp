#include <R.h>
#include <Rdefines.h>

#include "matrix_pos.h"


/*
 * Calculate the tour length given a distance matrix and a permuation vector
 */

SEXP tour_length_dist(SEXP R_dist, SEXP R_order) {

    double tour_length = 0.0;
    SEXP R_tour_length;
    double segment;
    
    int *order = INTEGER(R_order);
    int n = INTEGER(getAttrib(R_dist, install("Size")))[0];

    if (n != LENGTH(R_order)) 
        error("length of distance matrix and tour do not match");

    for (int i = 0; i < (n-1); i++) {
        segment = REAL(R_dist)[LT_POS(n, order[i]-1, order[i+1]-1)];
        // check inf
        if(segment == R_PosInf) {
            tour_length = R_PosInf;
            PROTECT(R_tour_length = NEW_NUMERIC(1));
            REAL(R_tour_length)[0] = R_PosInf;
            UNPROTECT(1);
            return R_tour_length;
        }
        
        tour_length += segment; 
    }

    // close tour
    segment = REAL(R_dist)[LT_POS(n, order[n-1]-1, order[0]-1)];
    
    // check inf
    if(segment == R_PosInf) tour_length = R_PosInf;
    else tour_length += segment; 

    PROTECT(R_tour_length = NEW_NUMERIC(1));
    REAL(R_tour_length)[0] = tour_length;
    UNPROTECT(1);

    return R_tour_length;
}

/*
 * Calculate tour length form a matrix
 */

SEXP tour_length_matrix(SEXP R_matrix, SEXP R_order) {

    double tour_length = 0.0;
    SEXP R_tour_length;
    double segment;
    
    int *order = INTEGER(R_order);
    int n = INTEGER(GET_DIM(R_matrix))[0];

    if (n != LENGTH(R_order)) 
        error("length of distance matrix and tour do not match");

    for (int i = 0; i < (n-1); i++) {
        segment = REAL(R_matrix)[M_POS(n, order[i]-1, order[i+1]-1)]; 
        
        // check inf
        if(segment == R_PosInf) {
            tour_length = R_PosInf;
            PROTECT(R_tour_length = NEW_NUMERIC(1));
            REAL(R_tour_length)[0] = R_PosInf;
            UNPROTECT(1);
            return R_tour_length;
        }
        tour_length += segment;
    }
    
    // close tour
    segment = REAL(R_matrix)[M_POS(n, order[n-1]-1, order[0]-1)]; 

    // check inf
    if(segment == R_PosInf) tour_length = R_PosInf;
    else tour_length += segment; 
    
    PROTECT(R_tour_length = NEW_NUMERIC(1));
    REAL(R_tour_length)[0] = tour_length;
    UNPROTECT(1);
    
    return R_tour_length;
}
