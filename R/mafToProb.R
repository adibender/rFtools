mafToProb <- function(maf) c({1-maf}^2, 2*{1-maf}*{maf}, maf^2)

