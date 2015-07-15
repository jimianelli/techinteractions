set.seed(777)
# OM random seed
	val <- matrix(sample(1:1e5, size=10000, replace=FALSE), 100, 100) 
	write(t(val), "Random_seed_OM.dat", ncolumns=100)
# Em random seed
	val1 <- val
	val1[,2:100] = val[,1:99]
	write(t(val1), "Random_seed_EM.dat", ncolumns=100)
