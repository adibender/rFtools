#include <Rcpp.h>

RcppExport SEXP tableSplitVarLevelsVsLayer(SEXP nlevels, SEXP layervec,
															SEXP uniqueLayers) {
BEGIN_RCPP

		Rcpp::NumericVector nlevs (nlevels);
		Rcpp::NumericVector layer (layervec);
		int vec_length = layer.size() - 1;
		Rcpp::NumericVector uniqueLayer (uniqueLayers);
		int ul_length = uniqueLayer.size() - 1;
		Rcpp::NumericMatrix table (2, ul_length + 1);

		for (int i = 0; i <= vec_length; i++) {
			for (int j = 0; j <= ul_length; j++) {
				if (layer(i) == j + 1) {
					if (nlevs(i) == 2) {
						table(0,j)++;
					}
					else table(1,j)++;
				}
			}
		}
		return table;

END_RCPP
}
