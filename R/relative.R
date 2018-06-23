relative <-function(layerList = list(), row.num = 1, 
		col.nom = 1, row.denom = 3, col.denom = 3) {
	
	sapply(layerList, function(z) z[row.num, col.nom] / z[row.denom, col.denom])

}

