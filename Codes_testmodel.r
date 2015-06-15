rm(list=ls())
gc()

Scenario = 1

# setwd(paste0("C:\\Users\\kotkot\\Dropbox\\Postdoc_projects\\techinteractions - Copy2\\results"))
# setwd(paste0("C:\\Users\\Kotaro Ono\\Dropbox\\Postdoc_projects\\techinteractions - Copy2\\results"))
# setwd(paste0("F:\\Dropbox\\Postdoc_projects\\techinteractions - Copy2\\results"))

Figures_save <- "C:\\Users\\kotkot\\Dropbox\\Postdoc_projects\\Figures"

###### Source some functions
	for (nm in list.files("C:\\R functions\\")) {
		source(file.path("C:\\R functions\\", nm))
	}
	library(ggplot2)
	library(plyr)

######## Codes for basic plots to understand the fortran code

 Simul = 1

### Qval 
	QQQ <- read.table("SAVE.ALL")
	Q <- as.numeric(QQQ[Simul,-c(1,2)])
 
### SSB
	SSB <- read.table("Summ13.out")
	colnames(SSB) <- c("Sim", "Species", "Year", "SSB")
	SSB$Species <- factor(SSB$Species, labels=c("Cod","Pollock","Yellowfin"))
	SSB <- subset(SSB, subset=c(Sim == Simul))
	
	MSST = matrix(scan("F_output.out", skip=3),ncol=17,byrow=T)
	colnames(MSST) <- c("Isim","IYr","Ispec","Icon","EstFOFL","EstF35","EstB35","EstSPR35","EstF40","EstB40","EstSPR40","TrueF35","TrueB35","TrueSPR35","TrueF40","TrueB40","TruSPR40")
	MSST <- as.data.frame(MSST)
	MSST$Isim <- as.factor(MSST$Isim)
	MSST$Ispec <- as.factor(MSST$Ispec)
	MSST$TrueB35 <- as.numeric(MSST$TrueB35)
	MSSTs <- ddply(MSST, .(Isim,Ispec), summarize, m=mean(TrueB35))
	MSSTs <- ddply(MSST, .(Ispec), summarize, m=mean(TrueB35),std=sd(TrueB35))
	BMSY <- ddply(MSST, .(Isim,Ispec), summarize, m=mean(TrueB40)) 
	BMSY <- ddply(MSST, .(Ispec), summarize, m=mean(TrueB40),std=sd(TrueB40))
	
	g1 <- ggplot(SSB, aes(x=Year, y = SSB/1e6, group = Species, colour=Species)) + theme_bw() + labs(y="SSB (t)") + geom_line() + geom_vline(xintercept = 52, linetype = 2, size=1, colour = "black") + geom_hline(yintercept = 1/2*MSSTs[,2]/1e6, linetype = 2, size=1, colour = c("red","green","blue"))
	# ggplot(SSB, aes(x=Year, y = SSB, group = Species, colour=Species)) + theme_bw() + geom_line() + geom_vline(xintercept = 52, linetype = 2, size=1, colour = "black") +coord_cartesian(ylim=c(0,1.9e09))+geom_hline(yintercept = BMSY[,2], linetype = 2, size=1, colour = c("red","green","blue"))
	g1 <- g1+coord_cartesian(ylim=c(0,1500))
	
### Recruits
	Rec <- read.table("RECRUITS.out")
	colnames(Rec) <- c("Sim", "Species", "Year", "Rec_M", "Rec_F")
	Rec$Species <- factor(Rec$Species, labels=c("Cod","Pollock","Yellowfin"))
	Rec <- subset(Rec, subset=c(Sim == Simul))
	Recs <- ddply(Rec, .(Sim,Species), summarize, m=mean(Rec_F))
	Rec$Rec_std <- (Rec$Rec_F-rep(Recs[,3], nrow(Rec)/3))/rep(Recs[,3], nrow(Rec)/3)

	g2 <- ggplot(Rec, aes(x=Year, y = Rec_F, group = Species, colour=Species)) + theme_bw() + geom_line()+ geom_vline(xintercept = 52, linetype = 2, size=1, colour = "black")
	# ggplot(Rec, aes(x=Year, y = Rec_std, group = Species, colour=Species)) + theme_bw() + geom_line()+ geom_vline(xintercept = 52, linetype = 2, size=1, colour = "black")
	# g2

### Effort
	Effort <- read.table("Summ17.out")
	colnames(Effort) <- c("Sim", "Species", "Year", "Effort")
	Effort$Species <- factor(Effort$Species, labels=c("Cod","Pollock","Yellowfin"))
	Effort <- subset(Effort, subset=c(Sim == Simul))
	Effort$F <- Effort$Effort*rep(Q,nrow(Effort)/3)
	
	g3 <- ggplot(Effort, aes(x=Year, y = F, group = Species, colour=Species))+ geom_line() +coord_cartesian(ylim=c(0,1)) + theme_bw()+ geom_vline(xintercept = 52, linetype = 2, size=1, colour = "black")
	# g3
	
### Catches
	# Catch <- read.table(paste0("Realized_catch.outSim", Simul), header=TRUE)
	# Catch$Year <- 51:80
	# Catch$Total_catch <- apply(Catch, 1, function(x) sum(x[1:3]))
	# Catch <- reshape2::melt(Catch, id.vars=c("Year","Total_catch"), variable.name = "Species")
	# Catch$Catch <- Catch$value

  Catch <- read.table("Summ14.out")
  colnames(Catch) <- c("Sim", "Species", "Year", "Catch")
  Catch$Species <- factor(Catch$Species, labels=c("Cod","Pollock","Yellowfin"))
  Catch <- subset(Catch, subset=c(Sim == Simul))
  Total_catch <- ddply(Catch, .(Year), summarize, Total_catch=sum(Catch))
  Catch$Total_catch <- rep(Total_catch[,2], each=3)
  Catch$Catch <- Catch$Catch/1e3
  Catch$Total_catch <- Catch$Total_catch/1e3
	
	g4 <- ggplot(Catch, aes(x=Year, y = Catch/1e3, group = Species, colour=Species)) + labs(y="Catch (1000t)") + geom_line(aes(x=Year, y=Catch/1e3)) + theme_bw() + geom_line(aes(x=Year, y=Total_catch/1e3), linetype = 2, size=1, colour = "black") + coord_cartesian(ylim=c(0,1.9e03)) #+ geom_vline(xintercept = 52, linetype = 2, size=1, colour = "black") 

### Depletion
	SSBDepletion_OM <- read.table("Summ3.out")
	colnames(SSBDepletion_OM) <- c("Sim", "Species", "Year", "SSBDepletion")
	SSBDepletion_OM$Species <- factor(SSBDepletion_OM$Species, labels=c("Cod","Pollock","Yellowfin"))
	SSBDepletion_OM <- subset(SSBDepletion_OM, subset=c(Sim == Simul))

	g5 <- ggplot(SSBDepletion_OM, aes(x=Year, y = SSBDepletion, group = Species, colour=Species)) + theme_bw() + geom_line()+coord_cartesian(ylim=c(0,130))+ geom_vline(xintercept = 52, linetype = 2, size=1, colour = "black")
	# g5

### The estimated ABC 
	ABC <- read.table("ABC.dat")
	colnames(ABC) <- c("Year", "Cod", "Pollock", "Yellowfin", "bycatch")
	ABC <- as.data.frame(ABC)[,-5]
	ABC <- reshape2::melt(ABC, id.vars=c("Year"), variable.name = "Species")
	Total_ABC <- ddply(ABC, .(Year), summarize, Total_ABC=sum(value))
	ABC$Total_ABC <- rep(Total_ABC[,2], 3)
	ABC_plot <- ggplot(ABC, aes(x=Year, y = value/1e6, group = Species, colour=Species)) + theme_bw() + labs(y="ABC (t)") + geom_line()+coord_cartesian(ylim=c(0,4000))+ geom_line(aes(x=Year, y=Total_ABC/1e6), linetype = 3, size=1, colour = "black")+geom_hline(yintercept = 1700, linetype = 2, size=1, colour = "black")+geom_hline(yintercept = 1500, linetype = 2, size=1, colour = "black")

### Bycatch	
	Bycatch <- read.table(paste0("Realized_catch.outSim", Simul),header=T)
	Bycatch$Year <- 51:80
	Bycatch$Species = "Bycatch"
	g6 <- ggplot(Bycatch, aes(x=Year, y = Bycatch, group=Species, colour=Species)) + geom_line() + theme_bw() +labs(y="Catch(t)")

### Do all plots	
	windows()
	multiplot(g1,ABC_plot,g2,g3,g4,g6, cols=2)	
	

############### Read in the results from CAB
######


	Plot_cab_fit <- function(Which.species,Yr,Sim,fitSSB=TRUE,fitComp=TRUE,fitIA=TRUE)
	{
	
		# Which.species <- 2
		# Yr <- 52
		# Sim = 1
		
		Rep.file = paste0("cab.repSp", Which.species, "Yr", Yr, "Sim", Sim, "Config")
		Par.file = paste0("cab.parSp", Which.species, "Yr", Yr, "Sim", Sim, "Config")
		Dat.file = paste0("cab.datSp", Which.species, "Yr", Yr, "Sim", Sim, "Config")


	############### Read in the results from CAB
	######

		Sp1 <- matrix(scan(Rep.file, skip=15, nlines=52),ncol=6, byrow=T)

		if (fitSSB==TRUE)
		{
			windows()
			par(mfrow=c(1,2), cex.lab=1.3, cex.main=1.4, cex.axis=1.2)
			if(Which.species==1) plot(subset(SSB,Species=="Cod")$Year, subset(SSB,Species=="Cod")$SSB, type="l", ylim=c(0,1.5*max(subset(SSB,Species=="Cod")$SSB)), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB (kg)")
			if(Which.species==2) plot(subset(SSB,Species=="Pollock")$Year, subset(SSB,Species=="Pollock")$SSB, type="l", ylim=c(0,1.5*max(subset(SSB,Species=="Pollock")$SSB)), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB (kg)")
			if(Which.species==3) plot(subset(SSB,Species=="Yellowfin")$Year, subset(SSB,Species=="Yellowfin")$SSB, type="l", ylim=c(0,1.5*max(subset(SSB,Species=="Yellowfin")$SSB)), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB (kg)")
			lines(Sp1[,1], Sp1[,2], lwd=2, lty=2, col="red")
			legend("topright", legend=c("true", "estimated"), col=c(1, "red"), lty=c(1,2), lwd=2, bty="n")
			
			if(Which.species==1) plot(subset(SSBDepletion_OM,Species=="Cod")$Year, subset(SSBDepletion_OM,Species=="Cod")$SSBDepletion/100, type="l", ylim=c(0,1.5), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB depletion")
			if(Which.species==2) plot(subset(SSBDepletion_OM,Species=="Pollock")$Year, subset(SSBDepletion_OM,Species=="Pollock")$SSBDepletion/100, type="l", ylim=c(0,1.5), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB depletion")
			if(Which.species==3) plot(subset(SSBDepletion_OM,Species=="Yellowfin")$Year, subset(SSBDepletion_OM,Species=="Yellowfin")$SSBDepletion/100, type="l", ylim=c(0,1.5), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB depletion")
			lines(Sp1[,1], Sp1[,3], lwd=2, lty=2, col="red")
			abline(h=0.4)
			legend("topright", legend=c("true", "estimated"), col=c(1, "red"), lty=c(1,2), lwd=2, bty="n")
		}

	######################### Plot of the age composition data

	### The data
		DATA <- readLines(con=Dat.file)
		## Age comps
			from = which(DATA == "# The actual data")[2]
			to = which(DATA == "# End of file")-3
			Agecomps <- matrix(scan(Dat.file, skip=from, nlines=(to-from+1)),nrow=(to-from), byrow=T)
		## Index of abundance
			from = which(DATA == "# The abundance data (series, year)")+1
			to = which(DATA == "# Length-frequency data")-2
			IA <- matrix(scan(Dat.file, skip=from, nlines=(to-from+1)),nrow=(to-from), byrow=T)
		## SSB
			SP <- c("Cod","Pollock","Yellowfin")
			SSB_sp <- subset(SSB,Species==SP[Which.species])$SSB

	### The model fit
		REPORT <- readLines(con=Rep.file)
		## Age composition data
		from = which(REPORT == "Age-frequency information")+1
		to = which(REPORT == "Selectivity-at-age")-2
		Agecomp_fit <- matrix(scan(Rep.file, what="character", skip=from, nlines=(to-from)),nrow=(to-from), byrow=T)
		colnames(Agecomp_fit) <- c("Ag","Fleet","F_type","Bla","Year","Nsamp","Neff","Age","Data","Fit")
		Agecomp_fit <- as.data.frame(Agecomp_fit)
		Agecomp_fit$Fleet <- as.numeric(Agecomp_fit$Fleet)
		Agecomp_fit$Nsamp <- as.numeric(as.vector(Agecomp_fit$Nsamp))
		Agecomp_fit$Neff <- as.numeric(as.vector(Agecomp_fit$Neff))
		Agecomp_fit$Age <- as.numeric(as.vector(Agecomp_fit$Age))
		Agecomp_fit$Data <- as.numeric(as.vector(Agecomp_fit$Data))
		Agecomp_fit$Fit <- as.numeric(as.vector(Agecomp_fit$Fit))
		
		Agecomp_fish_fit <- subset(Agecomp_fit, subset=Fleet==1)
		Agecomp_survey_fit <- subset(Agecomp_fit, subset=Fleet==2)
		
		g1 <- ggplot(Agecomp_fish_fit, aes(x=Age, y = Data)) + facet_wrap( ~ Year) + geom_point() +geom_line(aes(y=Fit))+labs(title = "Fishery composition data", y="Proportion")+theme_bw()
		# g1
		g2 <- ggplot(Agecomp_survey_fit, aes(x=Age, y = Data)) + facet_wrap( ~ Year) + geom_point() +geom_line(aes(y=Fit))+labs(title = "Survey composition data", y="Proportion")+theme_bw()
		# g2

		if (fitComp==TRUE)
		{
			windows()
			multiplot(g1,g2, cols=2)
		}

		if (fitIA==TRUE)
		{

			windows()
			par(mfrow=c(1,2), cex.lab=1.3, cex.main=1.4, cex.axis=1.2)
		## Index of abundance
			from = which(REPORT == "Fits to indices")+1
			to = which(REPORT == "length-frequency information")-3
			IA_fit <- matrix(scan(Rep.file, what="character", skip=from, nlines=(to-from)),nrow=(to-from), byrow=T)
			plot(as.numeric(IA_fit[,4]), as.numeric(IA_fit[,5]), ylab="Index of abundance", cex=1.2, xlab="Year", main="Fit to index of abundance"); lines(as.numeric(IA_fit[,4]), IA_fit[,6],lwd=2)
			# lines(1:51, SSB_sp*1.5, col="red")	# this is the true SSB 
		## the recruitment deviation
			PAR <- readLines(con=Par.file)
			recdevs <- scan(Par.file,skip=12,nline=1)
			# plot(1:51, recdevs, ylim=c(-3,3), xlab="Year", type="b", main="Estimated recdevs")
			# abline(h=0)
			from = which(REPORT == "Population 1 sex 1")[1]+1
			to = which(REPORT == "Population 1 sex 2")[1]-1
			Natage <- matrix(scan(Rep.file, skip=from, nlines=(to-from)),nrow=(to-from), byrow=T)
			Rec_fit <- Natage[,2]
			Rec_subset <- subset(Rec,Species==SP[Which.species])
			Rec_true <- Rec_subset$Rec_M
			plot(Natage[,1], Rec_fit, ylab="Number of recruits", xlab="Year", main="Estimated vs true recruitments", cex=1.2); lines(Rec_subset$Year, Rec_true,lwd=2)
		}
		
	}
		
		Plot_cab_fit(Which.species=1,Yr=53,Sim=1,fitComp=TRUE)
		Plot_cab_fit(Which.species=2,Yr=53,Sim=1,fitComp=TRUE)
		Plot_cab_fit(Which.species=3,Yr=53,Sim=1,fitComp=FALSE)
		graphics.off()

