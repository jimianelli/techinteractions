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
		
##############################################################################	
#
#		Get the data ready to be written into a .dat file to be read by ADMB 	
#
##############################################################################	

	### LOAD data for the anlaysis
	load("../R/All_data_Cluster_simple_2010_2014_pca_FALSE_.Rdata")	# species comp data for each cluster
	
	if (Choose_fish_strategies == "Year") 
	{
		YEARS <- 2000:2014 #1991:2014
		load("../R/Cluster_2000_2014_pca_FALSE_.Rdata")
	}
	if (Choose_fish_strategies == "Average") 
	{
		YEARS <- 2010:2014 #1991:2014
		load("../R/Cluster_simple_2010_2014_pca_FALSE_.Rdata")
	}
	NMFS_area <- c(508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524, 530, 541, 542, 543, 550, 610, 620, 630, 640, 650)
	BSAI <- c(508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524, 530, 541, 542, 543, 550)#, 610, 620, 630, 640, 650)
	GOA <- c(610, 620, 630, 640, 650)
	Species_interest <- c("POLLOCK", "PACIFIC.COD", "PACIFIC.HALIBUT", "YELLOWFIN.SOLE")

	for (i in 3:115)
	{
		ALL_clust[,i] <- as.numeric(as.vector(ALL_clust[,i]))
	}
	
	#### Filter the data based so that the cluster examined catch at least one of the species of interest
	clust_keep <- apply((ALL_clust[,which(colnames(ALL_clust) %in% Species_interest==TRUE)]),1,sum)>0
	Data_to_use <- Data_to_use[Data_to_use$Clust %in% ALL_clust$Cluster[clust_keep], ]
	ALL_clust <- ALL_clust[clust_keep,]
	
	#### Organise the data
	ALL_clust$Gear <- factor(ALL_clust$Gear, labels=c("non pelagic trawl", "pelagic trawl", "trap/pot", "longline"))
	if (Choose_fish_strategies == "Year") BSAI_data <- subset(ALL_clust, Area %in% BSAI & Yr %in% YEARS)
	if (Choose_fish_strategies == "Average") BSAI_data <- ALL_clust
	BSAI_data$Cluster <- as.factor(BSAI_data$Cluster)

	if (Choose_fish_strategies == "Year") BSAI_data_partial <- as.data.frame(cbind(BSAI_data[,c(1:4,117)], t(apply(BSAI_data[,-c(1:4)], 1, function(x) x/sum(x)))))
	if (Choose_fish_strategies == "Average") BSAI_data_partial <- as.data.frame(cbind(BSAI_data[,c(1:2,116)], t(apply(BSAI_data[,-c(1:2, 116)], 1, function(x) x/sum(x)))))	
	BSAI_data_partial[is.na(BSAI_data_partial)] <- 0

	if (Choose_fish_strategies == "Year") 
	{
		BSAI_data <- BSAI_data[,c(1:3,117,which(colnames(BSAI_data) %in% Species_interest==TRUE))]
		BSAI_data_partial <- BSAI_data_partial[,c(1:3,which(colnames(BSAI_data_partial) %in% Species_interest==TRUE))]
	}
	if (Choose_fish_strategies == "Average") 
	{
		BSAI_data <- BSAI_data[,c(1:2,116,which(colnames(BSAI_data) %in% Species_interest == TRUE))]
		BSAI_data_partial <- BSAI_data_partial[,c(1:3,which(colnames(BSAI_data_partial) %in% Species_interest==TRUE))]	
	}		
	
	### Determine the weight to associate to each of the metier = cluster (based on the data)
	### The weight is the sum of catch of all species of interest based on this specific cluster for a specific Sector

	Withgear_constraint = FALSE
	Weight_metier <- list()

	if (Choose_fish_strategies == "Average")
	{
		new <-rep(0, nrow(BSAI_data))

		tapply(Data_to_use$POLLOCK, Data_to_use$YEAR, sum, na.rm=T)/1000		
		tapply(Data_to_use$PACIFIC.COD, Data_to_use$YEAR, sum, na.rm=T)/1000		
		tapply(Data_to_use$YELLOWFIN.SOLE, Data_to_use$YEAR, sum, na.rm=T)/1000		
		tapply(Data_to_use$Total_catch, Data_to_use$YEAR, sum, na.rm=T)/1000		

		for (clust in seq_along(BSAI_data$Cluster))
		{
			datdat <- Data_to_use[which(Data_to_use$Clust == BSAI_data$Cluster[clust]),]
			val <- tapply(datdat$Total_catch, list(datdat$YEAR), sum)/1000
			new[clust] <- median(val)
		}
		
		#### and now adjust this value so that the sum of the target species catch is the amount specified = 1.7 million ton in this study
		Adj_factor <- 1700000/sum((new%*%as.matrix(BSAI_data_partial[,-c(1:3)]))[1,c(1,3,4)])
		Weight_metier[[1]] <- new*Adj_factor
	}
	
	if (Choose_fish_strategies == "Year")
	{
		for (yr in seq_along(YEARS))
		{
			new <-rep(0, nrow(BSAI_data))

			for (clust in seq_along(BSAI_data$Cluster))
			{
				datdat <- Data_to_use[which(Data_to_use$YEAR == YEARS[yr] & Data_to_use$Clust == BSAI_data$Cluster[clust]),]
				new[clust] <- sum(datdat$Total_catch)/1000
			}
			
			new <- new/sum(new)*1.7*1e6
			Weight_metier[[yr]] <- new

		}
	}


	#### Filter the data based so that the cluster examined catch at least one of the species of interest
	Data_to_use$Total_catch <- apply(Data_to_use[,which(names(Data_to_use) %in% c("PACIFIC.COD", "POLLOCK", "YELLOWFIN.SOLE"))], 1, sum, na.rm=T)
	
#### Species catch variation within each defined CLUSTER	
		
	Total_catch_variation <- tapply(Data_to_use$Total_catch, list(Data_to_use$YEAR, Data_to_use$Clust), sum, na.rm=T)
			
	### If we decide to put a bound on variability based on cluster
		vals <- apply(Total_catch_variation, 2, function(x) x/median(x, na.rm=T))
		max_dk_clust <- apply(Total_catch_variation, 2, function(x) quantile(x/median(x, na.rm=T),0.75,na.rm=T))
		min_dk_clust <- apply(Total_catch_variation, 2, function(x) quantile(x/median(x, na.rm=T),0.25,na.rm=T))
		max_dk_clust <- replace(max_dk_clust, max_dk_clust==1, median(max_dk_clust))
		min_dk_clust <- replace(min_dk_clust, min_dk_clust==1, median(min_dk_clust))
				
	### If we decide to put a bound on variability based on GEAR type (broader)
		gear_clust <- lapply(c("non pelagic trawl", "pelagic trawl", "trap/pot", "longline"), function(x) ALL_clust$Clust[which(ALL_clust$Gear == x)])
		ALL_var <- lapply(1:4, function(x) c(Total_catch_variation[,which(colnames(Total_catch_variation) %in% gear_clust[[x]])]))
		ALL_vars <- sapply(ALL_var, function(x) x/median(x, na.rm=T))
		boxplot(ALL_vars, ylim=c(0,10))
		max_val <- sapply(ALL_vars, function(x) quantile(x, 0.75, na.rm=T))
		min_val <- sapply(ALL_vars, function(x) quantile(x, 0.25, na.rm=T))
		max_dk_gear <- rep(0,nrow(ALL_clust))
		min_dk_gear <- rep(0,nrow(ALL_clust))
		for (i in 1:4) 
		{ 
			max_dk_gear <- replace(max_dk_gear, which(colnames(Total_catch_variation) %in% gear_clust[[i]]), max_val[i])
			min_dk_gear <- replace(min_dk_gear, which(colnames(Total_catch_variation) %in% gear_clust[[i]]), min_val[i])
		}
		
##################### Case 1: without the gear constraints:
	Yr=NULL; 					## Whether fishing strategies are year based or based on the average of 2010-2014; default = NULL (average)
	Bounds_base = "cluster";	## whether the bounds are based on total catch variation within a cluster ("cluster") or among_cluster based on gear type ("gear")
	CV_strategy=0.1;			## How variable are the catch composition between years
	seed=1;						## Random seed for reproducibility	
	price_min=0.2; 				## The minimum net price for a fish	
	price_factor=0.5			## The slope of price change (which is a function of stock biomass)
		

	Without_gear_constraints <- function(Yr, Bounds_base = "cluster", CV_strategy=NULL, seed=777, price_min=0.2, price_factor=0.5, price_change=TRUE, ...)
	{	
		##### Begin writing the file into a .dat file (not slack variables)

		set.seed(seed)
		if (Choose_fish_strategies == "Year") 
		{
			Yr_select <- Yr
			# select <- c(1:10)
			Data_weigthing <- as.numeric(Weight_metier[[which(YEARS == Yr_select)]])
			Data_input <- subset(BSAI_data_partial, Yr == Yr_select)
		}
		if (Choose_fish_strategies == "Average") 
		{
			Data_weigthing <- as.numeric(Weight_metier[[1]])
			Data_input <- BSAI_data_partial
		}
		select_non_pelagic <- which(Data_input$Gear == "non pelagic trawl")
		select_pelagic <- which(Data_input$Gear == "pelagic trawl")
		select_longline <- which(Data_input$Gear == "longline")
		select_trap_pot <- which(Data_input$Gear == "trap/pot")
		
		if (Choose_fish_strategies == "Years") 
		{
			Data_input <- as.matrix(Data_input[,-c(1:4)])
			Data_input_true <- Data_input[,c(1,3,4,2)]
		}
		if (Choose_fish_strategies == "Average") 
		{
			Data_input <- as.matrix(Data_input[,-c(1:3)])
			Data_input_true <- Data_input[,c(1,3,4,2)]
		}

		## Choosing the data to use
		if(is.null(CV_strategy)) Data_input <- Data_input_true
		if(!is.null(CV_strategy)) 
		{
			## adding some random error 
			Data_input_fake <- t(apply(Data_input_true, 1, function(x) { aaa = rnorm(4, x, CV_strategy*x); aaa <- replace(aaa,which(aaa<0),0); ifelse(sum(aaa)>1, bbb <- aaa/sum(aaa), bbb <- aaa); return(bbb)}))
			#Data_input_fake <- t(apply(Data_input_true, 1, function(x) { aaa = x+runif(4,-CV_strategy,CV_strategy); aaa <- replace(aaa,which(aaa<0),0); bbb = aaa/sum(aaa); return(bbb)}))
			#Data_input_fake <- t(apply(Data_input_true, 1, function(x) { aaa = rnorm(4, x, CV_strategy); aaa <- replace(aaa,which(aaa<0),0); bbb = aaa/sum(aaa); return(bbb)}))
			Data_input <- Data_input_fake
		}
		
		## See what is the initial solution
		test <- Data_weigthing%*%Data_input
		duplicated(test)
		
		## Price of the species
		# price <- c(0.57,0.63,0.63,0)		# cod, pollock, yellowfin
		# price <- c(rep(1,3),0)
		
		## If Net price is changing with the abundance of a stock
		True_exploitable <- read.table("TruExp_history.dat")
		start_year_exp_biomass <- True_exploitable[1,]
		DoOMEM <- read.table("DoOMEM.dat")
		if(price_change == TRUE) 
		{
			if (DoOMEM == "OM") price <- c(sapply(1:ncol(True_exploitable), function(x) max(price_min, as.numeric(1+price_factor*(1-True_exploitable[nrow(True_exploitable),x]/start_year_exp_biomass[x])))),0)
			if (DoOMEM == "EM") price <- c(sapply(1:ncol(True_exploitable), function(x) max(price_min, as.numeric(1+price_factor*(1-True_exploitable[(nrow(True_exploitable)-1),x]/start_year_exp_biomass[x])))),0)
		}
		if(price_change == FALSE) price <- c(1,1,1,0)
		
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
			if (Bounds_base == "gear") 
			{
				max_dk <- max_dk_gear
				min_dk <- min_dk_gear
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
		write(rep(1,Nb_strategy), file=file_save, ncolumns = Nb_strategy, append=T)	# constraint about OY
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
				
		# Now run the admb code and copy it to a different name
			#shell("admb main_code")
			shell("main_code")
				
	}	
		
	seed_val <- scan("seed.dat")
	
	Without_gear_constraints(Yr=NULL, Bounds_base = "cluster", CV_strategy=0.1, seed=seed_val, price_change = FALSE, price_min=0.2, price_factor=0.5)
	
