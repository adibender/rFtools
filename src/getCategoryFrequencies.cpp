#include <Rcpp.h>

RcppExport SEXP getCategoryFrequencies(SEXP matrix, SEXP splitVar) {
BEGIN_RCPP

    Rcpp::NumericMatrix SNPmatrix (matrix);
    int n_snp = SNPmatrix.ncol(), n_obs = SNPmatrix.nrow();
    Rcpp::NumericMatrix nCatMatrix(5, n_snp);
    Rcpp::NumericVector splitInd (splitVar);
    int splitIndex = splitInd[0] - 1;

    for (int i = 0; i <= n_snp - 1; i++) {
			int count0 = 0, count1 = 0, nlevels = 0;

			for(int j = 0; j <= n_obs - 1; j++) {
				if(SNPmatrix(j, i) == 0) count0++;
				else if (SNPmatrix(j, i) == 1) count1++;
			}

			nCatMatrix(1, i) = count0; nCatMatrix(2, i) = count1;
			nCatMatrix(3, i) = n_obs - count0 - count1;
			for(int k = 1; k <= 3; k++)
				if (nCatMatrix(k, i) != 0) nlevels++;
			nCatMatrix(4, i) = nlevels;
    }
    nCatMatrix(0, splitIndex) = 1;

    return nCatMatrix;

END_RCPP
}
