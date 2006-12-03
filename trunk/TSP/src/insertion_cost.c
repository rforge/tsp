#include <R.h>
#include <Rdefines.h>

#include "matrix_pos.h"


/*
 * Calculate insertion cost for the insertion algorithms
 */

SEXP insertion_cost(SEXP R_matrix, SEXP R_order, SEXP R_k) {

    int n = INTEGER(GET_DIM(R_matrix))[0];
    int length = LENGTH(R_order);
    int *order = INTEGER(R_order);
    int k = INTEGER(R_k)[0]-1;
    SEXP R_cost;
    PROTECT(R_cost = NEW_NUMERIC(length));

    if (length == 1) {
        REAL(R_cost)[0] = REAL(R_matrix)[M_POS(n, order[0]-1, k)];
    }else{
        for (int i = 0; i < (length-1); i++) {
            REAL(R_cost)[i] = 
                REAL(R_matrix)[M_POS(n, order[i]-1, k)] +
                REAL(R_matrix)[M_POS(n, k, order[i+1]-1)] - 
                REAL(R_matrix)[M_POS(n, order[i]-1, order[i+1]-1)];
        }

        REAL(R_cost)[length-1] = 
            REAL(R_matrix)[M_POS(n, order[length-1]-1, k)] +
            REAL(R_matrix)[M_POS(n, k, order[0]-1)] -
            REAL(R_matrix)[M_POS(n, order[length-1]-1, order[0]-1)];

    }

    UNPROTECT(1);
    return R_cost;
}

