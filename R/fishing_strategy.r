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
	Best_cluster <- lapply(1:length(YEARS), function(x) lapply(1:length(NMFS_area), function(y) vector("list", 4)))
	
	Select_data <- ALL_clust[,(colnames(ALL_clust) %in% c("POLLOCK", "PACIFIC.COD", "PACIFIC.HALIBUT", "YELLOWFIN.SOLE"))]
	if (Choose_fish_strategies == "Year") Select_data <- as.data.frame(cbind(ALL_clust[,c(1:3, ncol(ALL_clust))], Select_data))
	if (Choose_fish_strategies == "Average") Select_data <- as.data.frame(cbind(ALL_clust[,c(1:2, ncol(ALL_clust))], Select_data))
	Select_data$Gear <- factor(Select_data$Gear, labels=c("non pelagic trawl", "pelagic trawl", "trap/pot", "longline"))
	if (Choose_fish_strategies == "Year") BSAI_data <- subset(Select_data, Area %in% BSAI & Yr %in% YEARS)
	if (Choose_fish_strategies == "Average") BSAI_data <- Select_data
	BSAI_data$PACIFIC.COD <- as.numeric(as.vector(BSAI_data$PACIFIC.COD))
	BSAI_data$PACIFIC.HALIBUT <- as.numeric(as.vector(BSAI_data$PACIFIC.HALIBUT))
	BSAI_data$POLLOCK <- as.numeric(as.vector(BSAI_data$POLLOCK))
	BSAI_data$YELLOWFIN.SOLE <- as.numeric(as.vector(BSAI_data$YELLOWFIN.SOLE))
	BSAI_data$Cluster <- as.factor(BSAI_data$Cluster)
	BSAI_data[is.na(BSAI_data)] <- 0
	BSAI_data <- BSAI_data[which(apply(BSAI_data[,-c(1:4)], 1, sum)>0),]

	if (Choose_fish_strategies == "Year") BSAI_data_partial <- as.data.frame(cbind(BSAI_data[,1:4], t(apply(BSAI_data[,-c(1:4)], 1, function(x) x/sum(x)))))
	if (Choose_fish_strategies == "Average") BSAI_data_partial <- as.data.frame(cbind(BSAI_data[,1:3], t(apply(BSAI_data[,-c(1:3)], 1, function(x) x/sum(x)))))
	BSAI_data_partial[is.na(BSAI_data_partial)] <- 0

	### remove from start strategies in which halibut is the main catch (proportion > 0.5)
	if (Choose_fish_strategies == "Years") to_keep <- which(BSAI_data_partial$PACIFIC.HALIBUT<0.5)
	if (Choose_fish_strategies == "Average") to_keep <- which(BSAI_data_partial$PACIFIC.HALIBUT<0.5)
	
	BSAI_data_partial <- BSAI_data_partial[to_keep,]
	BSAI_data <- BSAI_data[to_keep,]

	Catch_data <- reshape2::melt(BSAI_data)
	Catch_data$variable <- factor(Catch_data$variable, levels=c("PACIFIC.COD", "POLLOCK", "YELLOWFIN.SOLE", "PACIFIC.HALIBUT"))
	if (Choose_fish_strategies == "Year")
	{
		Catch_ys <- tapply(Catch_data$value, list(Catch_data$Yr, Catch_data$variable), sum)
		Catch_data <- subset(Catch_data, Yr%in%YEARS)
	}
	Weight_gear_species <- array(NA, dim=c(4,4,length(YEARS)))

	
	if (Choose_fish_strategies == "Year")
	{
		for (yr in seq_along(YEARS))
		{
			Data_subset <- subset(Catch_data, Yr == YEARS[yr])
			new <- tapply(Data_subset$value, list(Data_subset$Gear, Data_subset$variable), sum, na.rm=T)
			Weight_gear_species[,,yr] <- (apply(new, 2, function(x) x/sum(x)))
		}
	}
	if (Choose_fish_strategies == "Average")
	{
		Data_subset <- Catch_data
		new <- tapply(Data_subset$value, list(Data_subset$Gear, Data_subset$variable), sum, na.rm=T)
		Weight_gear_species[,,1] <- (apply(new, 2, function(x) x/sum(x)))
	}
	
####### The initial weight for each strategy should be based on the original observer data
####### 1. aggregate each catch observation by fishing strategy
####### 2. expand the above so that the total catch is 1.700.000 t.
####### --> but this causes problems BECAUSE the observer data does not sample ALL the catches
	
	Catch_strategy <- c()
	Weight_strategy <- list()
	
	Withgear_constraint = FALSE
	if (Choose_fish_strategies == "Year")
	{
		for (yr in seq_along(YEARS))
		{
			Data_subset <- subset(Catch_data, Yr == YEARS[yr])
			Data_input1 <- subset(BSAI_data_partial, Yr == YEARS[yr])
			Sp = c("Cod","Halibut","Pollock","Yellowfin")
			new <- tapply(Data_subset$value, list(Data_subset$Cluster), sum, na.rm=T)
			Catch_strategy <- rbind(Catch_strategy, new)
			weight_new <- as.numeric(new[which(!is.na(new) & new!=0)])
			# print(sum(weight_new)/(1.7*1e9)*100)			# This is less than 1% of the  
			weight_new1 <- weight_new/sum(weight_new)*1.7*1e6		# this is the weight "dk" which shows the importance of each fishing strategy
			if (Withgear_constraint == FALSE) Weight_strategy[[yr]] <- weight_new1		
			
			test <- data.frame(Data_input1[,1:4],weight_new1*Data_input1[,-c(1:4)])
			essai <- reshape2::melt(test)
			print(tapply(essai$value, list(essai$variable), sum))
			
			### to make things reasonable
			if (Withgear_constraint == TRUE) 
			{
				test <- data.frame(Data_input1[,1:4],as.numeric(weight_new1)*as.matrix(Data_input1[,-c(1:4)]))
				test1 <- data.frame(Data_input1[,1:4],as.matrix(Data_input1[,-c(1:4)]))
				weight_new1[which(test[,6] >400 & Data_input1[,6]<0.5)] <- weight_new1[which(test[,6] >400 & Data_input1[,6]<0.5)]/10
				weight_new1[which(test1[,3]=="trap/pot" & test1[,5]>0.9)] <- weight_new1[which(test1[,3]=="trap/pot" & test1[,5]>0.9)]/10
				weight_new1[which(test1[,3]=="longline" & test1[,5]>0.9)] <- weight_new1[which(test1[,3]=="longline" & test1[,5]>0.9)]/10
				Weight_strategy[[yr]] <- weight_new1
			}
			# par(ask=TRUE)
		}
	}
	if (Choose_fish_strategies == "Average")
	{
		Data_subset <- Catch_data
		Data_input1 <- BSAI_data_partial
		new <- tapply(Data_subset$value, list(Data_subset$Cluster), sum, na.rm=T)
		Catch_strategy <- rbind(Catch_strategy, new)
		weight_new <- as.numeric(new[which(!is.na(new) & new!=0)])
		weight_new1 <- weight_new/sum(weight_new)*1.7*1e6		# this is the weight "dk" which shows the importance of each fishing strategy
		if (Withgear_constraint == FALSE) Weight_strategy[[1]] <- weight_new1		
		
		### to make things reasonable
		test <- data.frame(Data_input1[,1:3],as.numeric(weight_new1)*as.matrix(Data_input1[,-c(1:3)]))
		test1 <- data.frame(Data_input1[,1:3],as.matrix(Data_input1[,-c(1:3)]))
		weight_new1[which(test[,6] >400 & Data_input1[,6]<0.5)] <- weight_new1[which(test[,6] >400 & Data_input1[,6]<0.5)]/10
		weight_new1[which(test1[,3]=="trap/pot" & test1[,5]>0.9)] <- weight_new1[which(test1[,3]=="trap/pot" & test1[,5]>0.9)]/10
		weight_new1[which(test1[,3]=="longline" & test1[,5]>0.9)] <- weight_new1[which(test1[,3]=="longline" & test1[,5]>0.9)]/10
		if (Withgear_constraint == TRUE) Weight_strategy[[1]] <- weight_new1		
		
	}
	
			
##################### Case 1: without the gear constraints:
	Without_gear_constraints <- function(Yr, max_dk=3, min_dk=0.3, CV_strategy=NULL, seed=777)
	{	
		##### Begin writing the file into a .dat file (not slack variables)

		set.seed(seed)
		if (Choose_fish_strategies == "Year") 
		{
			Yr_select <- Yr
			# select <- c(1:10)
			Data_weigthing <- as.numeric(Weight_strategy[[which(YEARS == Yr_select)]])
			Data_input <- subset(BSAI_data_partial, Yr == Yr_select)
		}
		if (Choose_fish_strategies == "Average") 
		{
			Data_weigthing <- as.numeric(Weight_strategy[[1]])
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
			Data_input_fake <- t(apply(Data_input_true, 1, function(x) { aaa = rnorm(4, x, CV_strategy*x); aaa <- replace(aaa,which(aaa<0),0); bbb = aaa/sum(aaa); return(bbb)}))
			#Data_input_fake <- t(apply(Data_input_true, 1, function(x) { aaa = x+runif(4,-CV_strategy,CV_strategy); aaa <- replace(aaa,which(aaa<0),0); bbb = aaa/sum(aaa); return(bbb)}))
			#Data_input_fake <- t(apply(Data_input_true, 1, function(x) { aaa = rnorm(4, x, CV_strategy); aaa <- replace(aaa,which(aaa<0),0); bbb = aaa/sum(aaa); return(bbb)}))
			Data_input <- Data_input_fake
		}
		
		## See what is the initial solution
		test <- Data_weigthing%*%Data_input
		duplicated(test)
		
		## Price of the species
		price <- c(0.57,0.53,0.63,0)
		price <- c(0.57,0.63,0.63,0)		# cod, pollock, yellowfin
		price <- c(0.57,0.63,0.63,0)		# cod, pollock, yellowfin
		price <- c(rep(1,3),0)

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
	
	Without_gear_constraints(Yr=NULL,max_dk=5, min_dk=0.2, CV_strategy=0.1, seed=seed_val)
	
