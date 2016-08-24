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

	YEARS <- 2010:2014 #1991:2014
	
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
	if(length(grep("2014", Data_to_use$Cluster))>0) Data_to_use$Clust <- substr(Data_to_use$Cluster, start=6, nchar(Data_to_use$Cluster))
	if(length(grep("2014", Data_to_use$Cluster))==0) Data_to_use$Clust <- Data_to_use$Cluster
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
		max_dk_clust1 <- replace(max_dk_clust, max_dk_clust==1, mean(max_dk_clust))
		min_dk_clust1 <- replace(min_dk_clust, min_dk_clust==1, mean(min_dk_clust))
		
		max_dk_clust <- replace(max_dk_clust, max_dk_clust==1, NA)
		min_dk_clust <- replace(min_dk_clust, min_dk_clust==1, NA)
		fake_max <- data.frame(ALL_clust[,c(1:2,116)], max_dk_clust)
		fake_min <- data.frame(ALL_clust[,c(1:2,116)], min_dk_clust)
		Average_dk_clust_max <- tapply(fake_max$max_dk_clust, list(fake_max$Sector), mean, na.rm=T)
		Average_dk_clust_min <- tapply(fake_min$min_dk_clust, list(fake_min$Sector), mean, na.rm=T)
		for (i in seq_along(unique(fake_max$Sect)))
		{
			to_replace <- which(fake_max$Sector == unique(fake_max$Sect)[i] & is.na(fake_max$max_dk_clust))
			if(length(to_replace)>0) fake_max[to_replace, 'max_dk_clust'] <- Average_dk_clust_max[i]
			to_replace <- which(fake_min$Sector == unique(fake_min$Sect)[i] & is.na(fake_min$min_dk_clust))
			if(length(to_replace)>0) fake_min[to_replace, 'min_dk_clust'] <- Average_dk_clust_min[i]
		}

		max_dk_clust <- Flex_adj*fake_max[,'max_dk_clust']
		min_dk_clust <- 1/Flex_adj*fake_min[,'min_dk_clust']
				
################### Case 1: without the gear constraints:
	Yr=NULL; 					## Whether fishing strategies are year based or based on the average of 2010-2014; default = NULL (average)
	Bounds_base = "cluster";	## whether the bounds are based on total catch variation within a cluster ("cluster") or among_cluster based on gear type ("gear")
	Change_strategy=TRUE;		## YES or NO
	seed=1;						## Random seed for reproducibility	
	price_min=0.2; 				## The minimum net price for a fish	
	price_factor=0.5			## The slope of price change (which is a function of stock biomass)
	price_change=TRUE 			## Whether net price changes over time		
	price_sd = FALSE
	
	Without_gear_constraints <- function(Yr, Bounds_base = "cluster", Change_strategy=TRUE, seed=777, price_change=TRUE, price_sd=TRUE, ...)
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
						
		## Changes in net price with 
		# ABC for quota allocation
		# TAC for vessel dynamics
		# Dealing with quota allocation (EM) or vessel dynamics (OM)?
		DoOMEM <- as.character(unlist(read.table("DoOMEM.dat")))
		# Loading results from the net price changes with species ABC and TAC
		load("../R/Vessel_dyn.Rdata"); Vessel_dyn <- RES
		load("../R/Quota_dyn.Rdata"); Quota_dyn <- RES
		# TACs
		TACs <- scan("TAC.dat")
		# price <- c(1.0,0.80,0.76,0)
		if(price_change == TRUE) 
		{
			if (DoOMEM == "OM") 	# this is the price for the vessel dynamics 
			{
				newdat <- as.data.frame(t(TACs))
				colnames(newdat) <- c("q_cod","q_pol", "q_yel", "q_hal")
				if (price_sd==FALSE) price <- sapply(1:3, function(x) { aaa <- predict(Vessel_dyn[[x]], newdata=newdat,se.fit=TRUE); bbb <- aaa$fit; return(bbb) })
				if (price_sd==TRUE) price <- sapply(1:3, function(x) { aaa <- predict(Vessel_dyn[[x]], newdata=newdat,se.fit=TRUE); bbb <- rnorm(1, aaa$fit, aaa$se.fit); return(bbb) })
				price <- c(10, price)			
			}
			if (DoOMEM == "EM") 	# this is the price for the quota allocation
			{
				newdat <- as.data.frame(t(TACs))
				colnames(newdat) <- c("q_cod","q_pol", "q_yel", "q_hal")
				Logistic <- function(x,XX,YY,x.new,pred=FALSE)
				{
					if(pred == FALSE) 
					{
						y = min(YY)+diff(range(YY))/(1+exp(-x[1]*(XX-x[2])))
						ss <- sum((y - YY)^2)
						plot(XX,YY,type="p", cex=3, xlim=c(0.75*min(XX), 1.5*max(XX)))
						points(XX,y,pch=19,cex=2,col="blue")
						lines(seq(min(XX), 2*max(XX), by=1000), min(YY)+diff(range(YY))/(1+exp(-x[1]*(seq(min(XX), 2*max(XX), by=1000)-x[2]))), col="blue")
						return(ss)
					}	
					if(pred == TRUE) 
					{
						y = min(YY)+diff(range(YY))/(1+exp(-x[1]*(x.new-x[2])))
						return(y)
					}	
				}
				if (price_sd==FALSE) 
				{
					pred_pol = as.numeric(Logistic(Quota_dyn[[2]]$par,XX=Quota_dyn[[1]]$q_pol,YY=Quota_dyn[[1]]$p_pol,x.new=newdat[2], pred=TRUE))
					pred_yel = as.numeric(Logistic(Quota_dyn[[3]]$par,XX=Quota_dyn[[1]]$q_yel,YY=Quota_dyn[[1]]$p_yel,x.new=newdat[3], pred=TRUE))					
					price <- c(pred_pol, pred_yel)
				}
				if (price_sd==TRUE) 
				{
					pred_pol = as.numeric(Logistic(Quota_dyn[[2]]$par,XX=Quota_dyn[[1]]$q_pol,YY=Quota_dyn[[1]]$p_pol,x.new=newdat[2], pred=TRUE))
					pred_yel = as.numeric(Logistic(Quota_dyn[[3]]$par,XX=Quota_dyn[[1]]$q_yel,YY=Quota_dyn[[1]]$p_yel,x.new=newdat[3], pred=TRUE))					
					price <- c(pred_pol, pred_yel)
				}
				price <- c(10, price, 0)			
			}
		}

		## Changes in species catch composition 
		# Exploitable biomass
		SSB_2010 <- c(180952,1913000,713513)
		True_SSB <- read.table("TruSSB.dat")/1000
		if(Change_strategy == FALSE) Data_input <- Data_input_true
		if(Change_strategy == TRUE) 
		{
			## catch composition changes with species abundance 
			Data_input_fake <- cbind(t(sapply(1:nrow(Data_input_true), function(x) Data_input_true[x,-4]*True_SSB/SSB_2010)),Data_input_true[,4])
			Data_input <- matrix(unlist(Data_input_fake),ncol=4,byrow=F) 
			Data_input <- t(apply(Data_input,1,function(x) c(x[-4]/sum(x[-4]),x[4]))) 
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
			price_save <- "price.dat"	

		# Price of species (price.dat)
		if(!file.exists("price.dat")) write("# Price species (including bycatch sp)", file=price_save)
		write(c(DoOMEM, price), file=price_save, append=T)
						
		# Number fishing strategies (main_code.dat)
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
	
	Without_gear_constraints(Yr=NULL, Bounds_base = "cluster", Change_strategy=TRUE, seed=seed_val, price_change = TRUE, price_sd=FALSE)
	
