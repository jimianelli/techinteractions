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

	Nspec = as.numeric(scan("OM.dat", skip=11, nlines=1, what = "character")[4])
	### how much they fished in specific metier
	Weight_strategy <- scan("coeff.dat")

	N_Strategy <- length(Weight_strategy)
	price <- scan("main_code.dat", skip=5, nlines=1)
	Catch_proportion <- matrix(scan("main_code.dat", skip=7, nlines=(Nspec+1)), nrow=(Nspec+1), byrow=T)
	
	### Info on the simulation number
	Sim_info <-  matrix(scan("TruExp_history.dat"), ncol=(Nspec+2), byrow=T)
	Sim_info <- Sim_info[nrow(Sim_info),c(1,2)]
	
	### Metier-sector attribute
	load("Metier_sector.Rdata")
	
	### Catch by metier
	Catch_sector <- sapply(1:4, function(x) t(Weight_strategy[Metier_sector[[x]]])%*%t(Catch_proportion[,Metier_sector[[x]]]))
	Catch_sector_sum <- apply(Catch_sector, 2, function(x) sum(x[-4]))
	
	### Profit by metier
	Profit_sector <- sapply(1:4, function(x) t(Weight_strategy[Metier_sector[[x]]])%*%t(Catch_proportion[,Metier_sector[[x]]])*price)
	Profit_sector_sum <- apply(Profit_sector, 2, function(x) sum(x[-4]))
	
	### Save output (on catch and profit)
	ID <- c("Isim", "Year", "A80", "Longline", "CDQ", "TLA") 
	
	## Catch info
		# for cod
			output_catch_cod <- c(Sim_info, Catch_sector[1,])
			if(!file.exists("Metier_catch_cod.dat"))  write(ID, file="Metier_catch_cod.dat", append=F, ncolumns=length(output_catch_cod))
			write(output_catch_cod, file="Metier_catch_cod.dat", append=T, ncolumns=length(output_catch_cod))
		# for pollock
			output_catch_pollock <- c(Sim_info, Catch_sector[2,])
			if(!file.exists("Metier_catch_pollock.dat"))  write(ID, file="Metier_catch_pollock.dat", append=F, ncolumns=length(output_catch_pollock))
			write(output_catch_pollock, file="Metier_catch_pollock.dat", append=T, ncolumns=length(output_catch_pollock))
		# for yellowfin
			output_catch_yellowfin <- c(Sim_info, Catch_sector[3,])
			if(!file.exists("Metier_catch_yellowfin.dat"))  write(ID, file="Metier_catch_yellowfin.dat", append=F, ncolumns=length(output_catch_yellowfin))
			write(output_catch_yellowfin, file="Metier_catch_yellowfin.dat", append=T, ncolumns=length(output_catch_yellowfin))
		# for halibut
			output_catch_halibut <- c(Sim_info, Catch_sector[4,])
			if(!file.exists("Metier_catch_halibut.dat"))  write(ID, file="Metier_catch_halibut.dat", append=F, ncolumns=length(output_catch_halibut))
			write(output_catch_halibut, file="Metier_catch_halibut.dat", append=T, ncolumns=length(output_catch_halibut))
		# for ALL
			output_catch_all <- c(Sim_info, Catch_sector_sum)
			if(!file.exists("Metier_catch_all.dat"))  write(ID, file="Metier_catch_all.dat", append=F, ncolumns=length(output_catch_all))
			write(output_catch_all, file="Metier_catch_all.dat", append=T, ncolumns=length(output_catch_all))
	
	## Profit info
		# for cod
			output_profit_cod <- c(Sim_info, Profit_sector[1,])
			if(!file.exists("Metier_profit_cod.dat"))  write(ID, file="Metier_profit_cod.dat", append=F, ncolumns=length(output_profit_cod))
			write(output_profit_cod, file="Metier_profit_cod.dat", append=T, ncolumns=length(output_profit_cod))
		# for pollock
			output_profit_pollock <- c(Sim_info, Profit_sector[2,])
			if(!file.exists("Metier_profit_pollock.dat"))  write(ID, file="Metier_profit_pollock.dat", append=F, ncolumns=length(output_profit_pollock))
			write(output_profit_pollock, file="Metier_profit_pollock.dat", append=T, ncolumns=length(output_profit_pollock))
		# for yellowfin
			output_profit_yellowfin <- c(Sim_info, Profit_sector[3,])
			if(!file.exists("Metier_profit_yellowfin.dat"))  write(ID, file="Metier_profit_yellowfin.dat", append=F, ncolumns=length(output_profit_yellowfin))
			write(output_profit_yellowfin, file="Metier_profit_yellowfin.dat", append=T, ncolumns=length(output_profit_yellowfin))
		# for halibut
			output_profit_halibut <- c(Sim_info, Profit_sector[4,])
			if(!file.exists("Metier_profit_halibut.dat"))  write(ID, file="Metier_profit_halibut.dat", append=F, ncolumns=length(output_profit_halibut))
			write(output_profit_halibut, file="Metier_profit_halibut.dat", append=T, ncolumns=length(output_profit_halibut))
		# for ALL
			output_profit_all <- c(Sim_info, Profit_sector_sum)
			if(!file.exists("Metier_profit_all.dat"))  write(ID, file="Metier_profit_all.dat", append=F, ncolumns=length(output_profit_all))
			write(output_profit_all, file="Metier_profit_all.dat", append=T, ncolumns=length(output_profit_all))
	
