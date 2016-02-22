##############################################################################	
##############################################################################	
#
#	Objective: 
#	To calculate profit
#	
#	Author:
#	Kotaro Ono
#
#	Version:
#	V1: 
#
#	TO DO LISTS:
#   - Creates the data that goes into the ADMB code
#
##############################################################################	
##############################################################################	

	### 
	Weight_strategy <- scan("coeff.dat")
	N_Strategy <- length(Weight_strategy)
	price <- scan("main_code.dat", skip=5, nlines=1)
	Catch_proportion <- matrix(scan("main_code.dat", skip=7, nlines=4), nrow=4, byrow=T)
	Sim_info <-  matrix(scan("TruExp_history.dat"), ncol=5, byrow=T)
	Sim_info <- Sim_info[nrow(Sim_info),c(1,2)]
	
	profit <- t(Weight_strategy)%*%t(Catch_proportion)*price
	output <- c(Sim_info, profit)
	
	write(output, file="profit.dat", append=T)
	



