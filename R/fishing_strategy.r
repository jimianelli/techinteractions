##############################################################################	
##############################################################################	
#
#	Objective: 
#	To detect fishing strategy from the observer data
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

##############################################################################	
#
#		directory and libraries	
#
##############################################################################	

	rm(list=ls())
	gc()
	library(reshape2)

#	source("C:\\R functions\\All_functions.r")
#	source("C:\\R functions\\WestCoastMap.R")

##############################################################################	
#
#		Choose how to model the fishing strategies 	
#
##############################################################################	

	Choose_fish_strategies = "Average"    # Option of "Average" or "Year" 		
	Bounds_strategy <- 1	
	Flex_adj <- 1
		
##############################################################################	
#
#		Get the data ready to be written into a .dat file to be read by ADMB 	
#
##############################################################################	

	YEARS <- 2011:2014 #1991:2014
	
	### LOAD data for the analysis
	load(paste0("../R/All_data_Cluster_simple_", min(YEARS), "_", max(YEARS), "_.Rdata"))	# species comp data for each cluster
	load(paste0("../R/Cluster_simple_", min(YEARS), "_", max(YEARS), "_.Rdata"))
	
	NMFS_area <- c(508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524, 530, 541, 542, 543, 550, 610, 620, 630, 640, 650)
	BSAI <- c(508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524, 530, 541, 542, 543, 550)#, 610, 620, 630, 640, 650)
	GOA <- c(610, 620, 630, 640, 650)
	Species_interest <- c("POLLOCK", "PACIFIC.COD", "PACIFIC.HALIBUT", "YELLOWFIN.SOLE")

	for (i in 3:115)
	{
		ALL_clust[,i] <- as.numeric(as.vector(ALL_clust[,i]))
	}
	
	#### Filter the data based so that the cluster examined catch at least one of the species of interest (but not only halibut)
	clust_keep <- apply((ALL_clust[,which(colnames(ALL_clust) %in% Species_interest[-3]==TRUE)]),1,sum)>0
	Data_to_use <- Data_to_use[Data_to_use$Clust %in% ALL_clust$Cluster[clust_keep], ]
	ALL_clust <- ALL_clust[clust_keep,]
	
	### Sectors to save
		A80 <- grep("A80", ALL_clust$Cluster)
		Longline <- grep("Longline", ALL_clust$Cluster)
		CDQ <- grep("CDQ", ALL_clust$Cluster)
		TLA <- grep("BSAI", ALL_clust$Cluster)
		Metier_sector <- list(A80, Longline, CDQ, TLA)
		if(!file.exists("Metier_sector.Rdata")) save(Metier_sector, file="Metier_sector.Rdata") 
	
	#### Organise the data
	# ALL_clust$Gear <- factor(ALL_clust$Gear, labels=c("non pelagic trawl", "pelagic trawl", "trap/pot", "longline"))
	if (Choose_fish_strategies == "Average") BSAI_data <- ALL_clust
	BSAI_data$Cluster <- as.factor(BSAI_data$Cluster)

	if (Choose_fish_strategies == "Average") BSAI_data_partial <- as.data.frame(cbind(BSAI_data[,c(1:2,116)], t(apply(BSAI_data[,-c(1:2, 116)], 1, function(x) x/sum(x)))))	
	BSAI_data_partial[is.na(BSAI_data_partial)] <- 0

	if (Choose_fish_strategies == "Average") 
	{
		BSAI_data <- BSAI_data[,c(1:2,116,which(colnames(BSAI_data) %in% Species_interest == TRUE))]
		BSAI_data_partial <- BSAI_data_partial[,c(1:3,which(colnames(BSAI_data_partial) %in% Species_interest==TRUE))]	
		BSAI_data_partial[,-c(1:3)] <- t(apply(BSAI_data_partial[,-c(1:3)],1,function(x) x/sum(x)))
	}		
	
	#### Filter the data based so that the cluster examined catch at least one of the species of interest
	Data_to_use$Total_catch <- apply(Data_to_use[,which(names(Data_to_use) %in% c("PACIFIC.COD", "POLLOCK", "YELLOWFIN.SOLE"))], 1, sum, na.rm=T)
	
#### Species catch variation within each defined CLUSTER	
		
	Total_catch_variation <- tapply(Data_to_use$Total_catch, list(Data_to_use$YEAR, Data_to_use$Clust), sum, na.rm=T)
	Total_catch_variation <- Total_catch_variation[,order(match(colnames(Total_catch_variation), BSAI_data$Cluster))]
	Total_catch_variation <- 1700000*Total_catch_variation/apply(Total_catch_variation,1,sum,na.rm=T)
			
	### If we decide to put a bound on variability based on cluster
		Weight_metier <- list()
		Weight_metier[[1]] <- apply(Total_catch_variation,2,mean,na.rm=T)
		Weight_metier[[1]] <- 1700000*apply(Total_catch_variation,2,mean,na.rm=T)/sum(apply(Total_catch_variation,2,mean,na.rm=T))
		vals <- apply(Total_catch_variation, 2, function(x) x/mean(x, na.rm=T))
		max_dk_clust <- apply(Total_catch_variation, 2, function(x) quantile(x/mean(x, na.rm=T),Bounds_strategy,na.rm=T))
		min_dk_clust <- apply(Total_catch_variation, 2, function(x) quantile(x/mean(x, na.rm=T),(1-Bounds_strategy),na.rm=T))
		max_dk_clust <- replace(max_dk_clust, max_dk_clust==1, mean(max_dk_clust))
		min_dk_clust <- replace(min_dk_clust, min_dk_clust==1, mean(min_dk_clust))
		max_dk_clust <- Flex_adj*max_dk_clust
		min_dk_clust <- 1/Flex_adj*min_dk_clust
				
################### Case 1: without the gear constraints:
	Yr=NULL; 					## Whether fishing strategies are year based or based on the average of 2010-2014; default = NULL (average)
	Bounds_base = "cluster";	## whether the bounds are based on total catch variation within a cluster ("cluster") or among_cluster based on gear type ("gear")
	Change_strategy=TRUE;		## YES or NO
	seed=1;						## Random seed for reproducibility	
	price_min=0.2; 				## The minimum net price for a fish	
	price_factor=0.5			## The slope of price change (which is a function of stock biomass)
	price_change=FALSE 		## Whether net price changes over time		

	Without_gear_constraints <- function(Yr, Bounds_base = "cluster", CV_strategy=NULL, seed=777, price_min=0.2, price_factor=0.5, price_change=TRUE, ...)
	{	
		##### Begin writing the file into a .dat file (not slack variables)

		set.seed(seed)
		if (Choose_fish_strategies == "Average") 
		{
			Data_weigthing <- as.numeric(Weight_metier[[1]])
			Data_input <- BSAI_data_partial
		}
		select_non_pelagic <- which(Data_input$Gear == "non pelagic trawl")
		select_pelagic <- which(Data_input$Gear == "pelagic trawl")
		select_longline <- which(Data_input$Gear == "longline")
		select_trap_pot <- which(Data_input$Gear == "trap/pot")
		
		if (Choose_fish_strategies == "Average") 
		{
			Data_input <- as.matrix(Data_input[,-c(1:3)])
			Data_input_true <- Data_input[,c(1,3,4,2)]
		}
						
		## If Net price is changing with the abundance of a stock
		True_exploitable <- read.table("TruExp_history.dat")
		True_exploitable <- True_exploitable[,-c(1,2)]
		start_year_exp_biomass <- True_exploitable[1,]
		DoOMEM <- read.table("DoOMEM.dat")
		price <- c(1.0,0.76,0.70,0)
		if(price_change == TRUE) 
		{
			if (DoOMEM == "OM") price <- c(sapply(1:ncol(True_exploitable), function(x) max(price_min, as.numeric(price[x]+price_factor*(1-True_exploitable[nrow(True_exploitable),x]/start_year_exp_biomass[x])))),0)
			if (DoOMEM == "EM") price <- c(sapply(1:ncol(True_exploitable), function(x) max(price_min, as.numeric(price[x]+price_factor*(1-True_exploitable[max(1,(nrow(True_exploitable)-1)),x]/start_year_exp_biomass[x])))),0)
		}

		## Choosing the data to use
		if(Change_strategy == FALSE) Data_input <- Data_input_true
		if(Change_strategy == TRUE) 
		{
			## adding some random error 
			Data_input_fake <- cbind(t(sapply(1:nrow(Data_input_true), function(x) Data_input_true[x,-4]*True_exploitable[nrow(True_exploitable),]/start_year_exp_biomass)),Data_input_true[,4])
			Data_input <- matrix(unlist(Data_input_fake),ncol=length(YEARS),byrow=F) 
			Data_input <- t(apply(Data_input,1,function(x) x/sum(x))) 
		}
		
		## See what is the initial solution
		test <- Data_weigthing%*%Data_input
		
		Nb_strategy <- nrow(Data_input)
		Nb_species <- ncol(Data_input)
		D_upper <- diag(1, nrow=Nb_strategy, ncol=Nb_strategy)
		D_lower <- diag(1, nrow=Nb_strategy, ncol=Nb_strategy)
		
		### Creating the main_code.dat matrix (the input file for the linear programming)
		# Without the double constraint on the bounds (dk,t=1, and dkt-1)
			Nb_constraints_b1 <- c(Nb_species,1,Nb_strategy)
			Nb_constraints_b2 <- c(Nb_strategy)
		# TACs
			TACs <- scan("TAC.dat",skip=1)
		# Without the double constraint on the bounds (dk,t=1, and dkt-1)
			if (Bounds_base == "cluster") 
			{
				max_dk <- max_dk_clust
				min_dk <- min_dk_clust
			}
			Bounds_b1 <- c(rep(0,Nb_species), 1700000, max_dk*Data_weigthing)		# Need to figure this out
			Bounds_b2 <- c(min_dk*Data_weigthing)
			
		# Obj func
			obj_fun <- as.vector(Data_input%*%price)
			
		##### Now write the file
			file_save <- "main_code.dat"
			file_exe <- "main_code.exe"
		
		# Number fishing strategies 
		write("# Number of fishing strategy", file=file_save)
		write(Nb_strategy, file=file_save, append=T)
		# Number of species
		write("# Number of species (including bycatch sp)", file=file_save, append=T)
		write(Nb_species, file=file_save, append=T)
		# Price of species
		write("# Price species (including bycatch sp)", file=file_save, append=T)
		write(price, file=file_save, append=T)
		# Catch proportion matrix 
		write("# Catch proportion by fishing strategy", file=file_save, append=T)
		write(Data_input, file=file_save, ncolumns = Nb_strategy, append=T)
		# Total Number of b1 constaints
		write("# Total number of b1 constraints", file=file_save, append=T)
		write(sum(Nb_constraints_b1), file=file_save, append=T)
		# Total Number of b2 constaints
		write("# Total number of b2 constraints", file=file_save, append=T)
		write(sum(Nb_constraints_b2), file=file_save, append=T)
		# Number of types of b1 constraints 
		write("# Number of types of b1 constraints", file=file_save, append=T)
		write(length(Nb_constraints_b1), file=file_save, append=T)
		# Number of each type of b1 constraints 
		write("# Number of each type of b1 constraints", file=file_save, append=T)
		write(Nb_constraints_b1, file=file_save, append=T)
		# Number of types of b2 constraints 
		write("# Number of types of b2 constraints", file=file_save, append=T)
		write(length(Nb_constraints_b2), file=file_save, append=T)
		# Number of each type of b2 constraints 
		write("# Number of each type of b2 constraints", file=file_save, append=T)
		write(Nb_constraints_b2, file=file_save, append=T)
		# The a0 vector = the objective function
		write("# the a0 vector = objective function", file=file_save, append=T)
		write(obj_fun, file=file_save, ncolumns = Nb_strategy, append=T)
		# the a1 matrix
		write("# The A1 matrix", file=file_save, append=T)
		write((Data_input), file=file_save, ncolumns = Nb_strategy, append=T)			# constraints about the ABC
		write(apply(Data_input,1,function(x) sum(x[-4])), file=file_save, ncolumns = Nb_strategy, append=T)		# constraint about OY i.e. sum(catch)<1.7 million)
		write(D_upper, file=file_save, ncolumns = Nb_strategy, append=T)				# constraint on the "dK' upper bound
		# the a2 matrix
		write("# The A2 matrix", file=file_save, append=T)
		write((D_lower), file=file_save, ncolumns = Nb_strategy, append=T)			# constraint on the "dK' lower bound
		# the initial weight = "dk" values
		write("# The initial weight = dk values", file=file_save, append=T)
		write(Data_weigthing, file=file_save, ncolumns = Nb_strategy, append=T)			# constraint on the "dK' lower bound
		# the b1 bounds (the upper bounds value of each b1 constraints)
		write("# The b1 bounds ", file=file_save, append=T)
		write(Bounds_b1, file=file_save, ncolumns = Nb_strategy, append=T)			# constraint on the "dK' lower bound
		# the b2 bounds (the upper bounds value of each b2 constraints)
		write("# The b2 bounds ", file=file_save, append=T)
		write(Bounds_b2, file=file_save, ncolumns = Nb_strategy, append=T)			# constraint on the "dK' lower bound
		# the multiplier for the b1 bounds i.e max_dk
		write("# max_dk ", file=file_save, append=T)
		write(max_dk, file=file_save, ncolumns = Nb_strategy, append=T)			# constraint on the "dK' lower bound
		# the multiplier for the b2 bounds i.e min_dk
		write("# min_dk ", file=file_save, append=T)
		write(min_dk, file=file_save, ncolumns = Nb_strategy, append=T)			# constraint on the "dK' lower bound
				
		# Now run the admb code and copy it to a different name
			#shell("admb main_code")
			shell("main_code")
				
	}	
		
	seed_val <- scan("seed.dat")
	
	Without_gear_constraints(Yr=NULL, Bounds_base = "cluster", Change_strategy=TRUE, seed=seed_val, price_change = FALSE, price_min=0.2, price_factor=0.1)
	
