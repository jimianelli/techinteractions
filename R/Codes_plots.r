##############################################################################	
##############################################################################	
#
#	Description: 
#	This file allows you to read in some of the output files produced by the program and produce figures
#
##############################################################################	
##############################################################################	

rm(list=ls())
gc()

wd <- "Techint"	# the folder were you save all of the scenario
End_year = 66

main_folder <- paste0("C:/Users/kotkot/Dropbox/Postdoc_projects/", wd)
Figures_save <- paste0("C:/Users/kotkot/Dropbox/Postdoc_projects/", wd, "/Figures")

###### Source some functions
	for (nm in list.files("C:\\R functions\\")) {
		source(file.path("C:\\R functions\\", nm))
	}
	library(ggplot2)
	library(plyr)
	library(gridExtra)
	library(reshape2)
	library(data.table)
	
	# The palette with grey:	
	cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

	# The palette with black:
	# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

	  
##############################################################################	
#
#		Get the results from each scenario and plot them in a succint way	
#
##############################################################################		
	
	RUN_plot <- function(Scenario, run_history = 3, save_plot=FALSE,...)
	{

	if (run_history == 1) setwd(paste0(main_folder, "\\techinteractions", Scenario, "\\results"))
	if (run_history == 2) setwd(paste0(main_folder, "\\techinteractions", Scenario, "\\results_more_constraints"))
	if (run_history == 3) setwd(paste0(main_folder, "\\techinteractions", Scenario, "\\runs\\results"))
	# setwd(paste0("C:\\Users\\kotkot\\Dropbox\\Postdoc_projects\Techint\\Techinteractions", Scenario, "\\results_more_constraints"))
	# setwd(paste0("C:\\Users\\Kotaro Ono\\Dropbox\\Postdoc_projects\Techint\\Techinteractions", Scenario, "\\results"))
	# setwd(paste0("F:\\Dropbox\\Postdoc_projects\Techint\\Techinteractions", Scenario, "\\results"))

	##############################################################################	
	#
	#		Steps to do 	
	#
	##############################################################################	
					
	######## If there are more than one iteration in a scenario

	Do_summary = FALSE

	if (Do_summary == TRUE)
	{			
		if (!(Scenario %in% c("A","B","C","D","J","K","L","M","R","S","T","U","R1","S1","T1","U1","R2","S2","T2","U2")))
		{
			cols <- RColorBrewer::brewer.pal(8, "Greys")

			### Qval 
				QQQ <- read.table("SAVE.ALL")
				Q <- QQQ[,-c(1,2)]
				Simul1 = which(Q[,2]<1)
				Q1 <- Q[Simul1,]
			 
			### Effort
				Effort <- read.table("Summ17.out")
				colnames(Effort) <- c("Sim", "Species", "Year", "Effort")
				Effort$Species <- factor(Effort$Species, labels=c("Cod","Pollock","Yellowfin"))
				Effort <- subset(Effort, Sim%in%Simul1)
				Effort$Effort <- as.numeric(as.vector(Effort$Effort))
				asd <- ddply(Effort, .(Sim), summarize, max=max(Effort))
				Simul <- asd$Sim[!is.na(asd$max)]
				Effort <- subset(Effort, Sim%in%Simul)
				Q <- Q[Simul,]
				Effort$F <- as.numeric(as.vector(Effort$Effort))*c(apply(Q, 1, function(x) rep(as.numeric(x),End_year)))
				
				quant_dat3 <- ddply(Effort, .(Species, Year), summarize,
					q05 = quantile(F, probs = 0.05),
					q50 = quantile(F, probs = 0.50),
					q95 = quantile(F, probs = 0.95)
				)
				g3 <- ggplot(quant_dat3) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black")+labs(y="F")
				
			### SSB
				SSB <- read.table("Summ13.out")
				colnames(SSB) <- c("Sim", "Species", "Year", "SSB")
				SSB$Species <- factor(SSB$Species, labels=c("Cod","Pollock","Yellowfin"))
				SSB <- subset(SSB, Sim%in%Simul)
				
				quant_dat1 <- ddply(SSB, .(Species, Year), summarize,
					q05 = quantile(SSB/1e6, probs = 0.05),
					q50 = quantile(SSB/1e6, probs = 0.50),
					q95 = quantile(SSB/1e6, probs = 0.95)
				)
				
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
				
				g1 <- ggplot(quant_dat1) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_hline(yintercept = 1/2*MSSTs[,2]/1e6, linetype = 2, size=0.5, colour = c("red","green","blue"))+coord_cartesian(xlim=c(0,End_year))+coord_cartesian(ylim=c(0,5500))+labs(y="SSB (1000t)")
				g1_fake <- ggplot(quant_dat1) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_hline(yintercept = 1/2*MSSTs[,2]/1e6, linetype = 2, size=0.5, colour = c("red","green","blue"))+labs(y="SSB (1000t)")
				
			### Recruits
				Rec <- read.table("RECRUITS.out")
				colnames(Rec) <- c("Sim", "Species", "Year", "Rec_M", "Rec_F")
				Rec$Species <- factor(Rec$Species, labels=c("Cod","Pollock","Yellowfin"))
				Rec <- subset(Rec, Sim%in%Simul)

				quant_dat2 <- ddply(Rec, .(Species, Year), summarize,
					q05 = quantile(Rec_F/1e6, probs = 0.05),
					q50 = quantile(Rec_F/1e6, probs = 0.50),
					q95 = quantile(Rec_F/1e6, probs = 0.95)
				)
				g2 <- ggplot(quant_dat2) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black")+labs(y="Rec (1000t)")
			
			### Catches
				Catch <- read.table("Summ14.out")
				colnames(Catch) <- c("Sim", "Species", "Year", "Catch")
				Catch$Species <- factor(Catch$Species, labels=c("Cod","Pollock","Yellowfin"))
				Catch <- subset(Catch, Sim%in%Simul)
				Total_catch <- ddply(Catch, .(Sim,Year), summarize, Total_catch=sum(Catch))
				Catch$Total_catch <- rep(as.numeric(as.vector(Total_catch[,3])), each=3)
				qwer <- subset(Catch, Year>51)
				qwe <- ddply(qwer, .(Sim), summarize, max=max(Total_catch))
				Simul = qwe[which(qwe$max<=1700000000),1]
				Catch <- subset(Catch, Sim%in%Simul)
						
				quant_dat4 <- ddply(Catch, .(Species, Year), summarize,
					q05 = quantile(Catch/1e6, probs = 0.05),
					q50 = quantile(Catch/1e6, probs = 0.50),
					q95 = quantile(Catch/1e6, probs = 0.95),
					q05_sum = quantile(Total_catch/1e6, probs = 0.05),
					q50_sum = quantile(Total_catch/1e6, probs = 0.50),
					q95_sum = quantile(Total_catch/1e6, probs = 0.95)			
				)
				g4 <- ggplot(quant_dat4) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_ribbon(aes(Year, ymax = q05_sum, ymin = q95_sum), alpha=0.5, fill=grey(0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + geom_line(aes(Year, y=q50_sum), colour="black", linetype=2, size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black")+labs(y="Catch (1000t)")+geom_hline(yintercept = 1700, linetype = 3, colour = "black")
				# g4 <- ggplot(Catch, aes(x=Year, y = Catch/1e6, group = Species, colour=Species))+ geom_line(aes(x=Year, y=Catch/1e6), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_line(aes(x=Year, y=Total_catch/1e6), linetype = 2, size=1, colour = "black") + geom_hline(yintercept = 1.7e3, linetype = 3, colour = "black") +coord_cartesian(xlim=c(0,End_year), ylim=c(0,1900))+ labs(y="Catch (1000t)")

			### Depletion
				SSBDepletion_OM <- read.table("Summ3.out")
				colnames(SSBDepletion_OM) <- c("Sim", "Species", "Year", "SSBDepletion")
				SSBDepletion_OM$Species <- factor(SSBDepletion_OM$Species, labels=c("Cod","Pollock","Yellowfin"))
				SSBDepletion_OM <- subset(SSBDepletion_OM, Sim%in%Simul)

				quant_dat5 <- ddply(SSBDepletion_OM, .(Species, Year), summarize,
					q05 = quantile(SSBDepletion, probs = 0.05),
					q50 = quantile(SSBDepletion, probs = 0.50),
					q95 = quantile(SSBDepletion, probs = 0.95)
				)
				g5 <- ggplot(quant_dat5) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") +labs(y="Depletion")
				# g5 <- ggplot(SSBDepletion_OM, aes(x=Year, y = SSBDepletion, group = Species, colour=Species)) + theme_bw() + geom_line(size=1.5)+coord_cartesian(xlim=c(0,End_year), ylim=c(0,130))+ geom_vline(xintercept = 52, linetype = 2, colour = "black")

			### Bycatch	
				Bycatch <- c()
				for (i in Simul)
				{
					bycatch <- read.table(paste0("Realized_catch.outSim", i),header=T)
					bycatch$Year <- 52:End_year
					bycatch$Species = "Bycatch"
					bycatch$Sim = i
					Bycatch <- rbind(Bycatch, bycatch)
				}
				quant_dat6 <- ddply(Bycatch, .(Species, Year), summarize,
					q05 = quantile(Bycatch, probs = 0.05),
					q50 = quantile(Bycatch, probs = 0.50),
					q95 = quantile(Bycatch, probs = 0.95)
				)
				g6 <- ggplot(quant_dat6) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") +labs(y="Bycatch")+ geom_hline(yintercept = 4575, linetype = 3, colour = "black")
				# g6 <- ggplot(Bycatch, aes(x=Year, y = Bycatch, group=Species, colour=Species)) + geom_line(size=1.5, colour="black") + theme_bw() + coord_cartesian(xlim=c(0,End_year), ylim=c(4000,4600)) + labs(y="ByCatch (t)") + geom_hline(yintercept = 4575, linetype = 3, colour = "black")+ geom_vline(xintercept = 52, linetype = 4, colour = "black")

			### The estimated ABC 
				ABC <- read.table("ABC.dat")
				if(ncol(ABC) == 5) 
				{
					colnames(ABC) <- c("Year", "Cod", "Pollock", "Yellowfin", "bycatch")
					# ABC$Isim <- 1	
					ABC$Isim <- rep(1:50, each=30)	
				}	
				if(ncol(ABC) > 6) {colnames(ABC) <- c("Isim", "Year", "Cod", "Pollock", "Yellowfin", "bycatch")}
				ABC <- as.data.frame(ABC)[,-5]
				ABC <- subset(ABC, Isim%in%Simul)
				ABC <- reshape2::melt(ABC, id.vars=c("Year","Isim"), variable.name = "Species")
				Total_ABC <- ddply(ABC, .(Isim,Year), summarize, Total_ABC=sum(value))
				ABC$Total_ABC <- rep(Total_ABC[,3], 3)

				quant_dat7 <- ddply(ABC, .(Species, Year), summarize,
					q05 = quantile(value/1e6, probs = 0.05),
					q50 = quantile(value/1e6, probs = 0.50),
					q95 = quantile(value/1e6, probs = 0.95),
					q05_sum = quantile(Total_ABC/1e6, probs = 0.05),
					q50_sum = quantile(Total_ABC/1e6, probs = 0.50),
					q95_sum = quantile(Total_ABC/1e6, probs = 0.95)
				)

				g7 <- ggplot(quant_dat7) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_ribbon(aes(Year, ymax = q05_sum, ymin = q95_sum), alpha=0.5, fill=grey(0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + geom_line(aes(Year, y=q50_sum), colour="black", linetype=2, size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black")+labs(y="ABC (1000t)")+geom_hline(yintercept = 1700, linetype = 3, colour = "black")
				# ABC_plot <- ggplot(ABC, aes(x=Year, y = value/1e6, group = Species, colour=Species)) + theme_bw() + labs(y="ABC (1000t)") + geom_line(size=1.5)+coord_cartesian(xlim=c(0,End_year),ylim=c(0,4000))+ geom_line(aes(x=Year, y=Total_ABC/1e6), linetype = 2, size=1, colour = "black")+geom_hline(yintercept = 1700, linetype = 3, colour = "black")+ geom_vline(xintercept = 52, linetype = 4, colour = "black")#+geom_vline(yintercept = 1500, linetype = 2, size=1, colour = "black")
					
			### Do all plots	
				
				g1a <- g1 + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 5500))
				g5a <- g5 + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 120))
				g7a <- g7+ theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 7500))
				g4a <- g4 + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 5500))
				g6a <- g6 + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(2000, 5000))
				# multiplot(g5a,ABC_plota,g4a,g6a, cols=2)	

				library(gridExtra)
				grid_arrange_shared_legend <- function(...) {
					plots <- list(...)
					g <- ggplotGrob(g1_fake + theme(legend.position="bottom"))$grobs
					legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
					lheight <- sum(legend$height)
					grid.arrange(
						do.call(arrangeGrob, lapply(plots, function(x)
							x + theme(legend.position="none"))),
						legend,
						ncol = 1,
						heights = unit.c(unit(1, "npc") - lheight, lheight))
				}	
				
				if (Scenario == "E") file_name = "Base_noest_IND_SQ"		
				if (Scenario == "F") file_name = "Base_noest_IND_LP"		
				if (Scenario == "G") file_name = "Base_noest_COR_SQ"		
				if (Scenario == "H") file_name = "Base_noest_COR_LP"		
				if (Scenario == "N") file_name = "TV_noest_IND_SQ"		
				if (Scenario == "O") file_name = "TV_noest_IND_LP"		
				if (Scenario == "P") file_name = "TV_noest_COR_SQ"		
				if (Scenario == "Q") file_name = "TV_noest_COR_LP"		
				if (Scenario == "V") file_name = "Bycatch_noest_IND_SQ_25"		
				if (Scenario == "W") file_name = "Bycatch_noest_IND_LP_25"		
				if (Scenario == "X") file_name = "Bycatch_noest_COR_SQ_25"		
				if (Scenario == "Y") file_name = "Bycatch_noest_COR_LP_25"		
				if (Scenario == 1)  file_name = "Base_est_IND_SQ"		
				if (Scenario == 2)  file_name = "Base_est_IND_LP"		
				if (Scenario == 3)  file_name = "TV_est_IND_SQ"		
				if (Scenario == 4)  file_name = "TV_est_IND_LP"		
				if (Scenario == 5)  file_name = "COR_est_SQ"		
				if (Scenario == 6)  file_name = "COR_est_LP"		
				if (Scenario == 7)  file_name = "Constraint_est_IND_SQ"		
				if (Scenario == 8)  file_name = "Constraint_est_IND_LP"		
				if (Scenario == 9)  file_name = "Bycatch_est_IND_SQ_25"		
				if (Scenario == 10) file_name = "Bycatch_est_IND_LP_25"		
				
				if (save_plot == TRUE) 
				{
					png(filename = paste0(Figures_save, "/", file_name, ".png"), width = 250, height=200, units="mm", res=200)
					grid_arrange_shared_legend(g1a, g7a, g4a, g6a)
					dev.off()
				}
		}
	}	

	if (Do_summary == FALSE)
	{				
		if (!(Scenario %in% c("A","B","C","D","J","K","L","M","R","S","T","U","R1","S1","T1","U1","R2","S2","T2","U2")))
		{
			cols <- RColorBrewer::brewer.pal(8, "Greys")

			### Qval 
				QQQ <- read.table("SAVE.ALL")
				Q <- QQQ[,-c(1,2)]
				Simul1 = which(Q[,2]<1)
				Q1 <- Q[Simul1,]
			 
			### Effort
				Effort <- read.table("Summ17.out")
				colnames(Effort) <- c("Iteration", "Species", "Year", "Effort")
				Effort$Species <- factor(Effort$Species, labels=c("Cod","Pollock","Yellowfin"))
				Effort <- subset(Effort, Iteration%in%Simul1)
				Effort$Effort <- as.numeric(as.vector(Effort$Effort))
				asd <- ddply(Effort, .(Iteration), summarize, max=max(Effort))
				Simul <- asd$Iteration[!is.na(asd$max)]
				Effort <- subset(Effort, Iteration%in%Simul)
				Q <- Q[Simul,]
				Effort$F <- as.numeric(as.vector(Effort$Effort))*c(apply(Q, 1, function(x) rep(as.numeric(x),End_year)))
				
				Effort$ID <- factor(Effort$Iteration, labels=1:length(unique(Effort$Iteration))) 
				g3 <- ggplot(Effort, aes(x=Year, y=F, colour=ID, group=ID))+facet_grid(. ~ Species) + geom_line() + theme_bw() + geom_vline(xintercept = 51, linetype = 4, colour = "black")+labs(y="F") + scale_colour_grey() +theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.4)))
				if (save_plot == TRUE) ggsave(filename=paste0(Figures_save, "/F_trajectory.png"), width=30, height=18, units="cm", dpi=200)			
				
			### SSB
				SSB <- read.table("Summ13.out")
				colnames(SSB) <- c("Sim", "Species", "Year", "SSB")
				SSB$Species <- factor(SSB$Species, labels=c("Cod","Pollock","Yellowfin"))
				SSB <- subset(SSB, Sim%in%Simul)
				SSB$ID <- paste(SSB$Sim, SSB$Species, sep="_")
				SSB$Isim <- as.numeric(SSB$Sim/50)
			
				MSST = matrix(scan("F_output.out", skip=3),ncol=17,byrow=T)
				colnames(MSST) <- c("Isim","IYr","Ispec","Icon","EstFOFL","EstF35","EstB35","EstSPR35","EstF40","EstB40","EstSPR40","TrueF35","TrueB35","TrueSPR35","TrueF40","TrueB40","TruSPR40")
				MSST <- as.data.frame(MSST)
				MSST$Isim <- as.factor(MSST$Isim)
				MSST$Ispec <- as.factor(MSST$Ispec)
				MSST$TrueB35 <- as.numeric(MSST$TrueB35)
				MSSTs <- ddply(MSST, .(Isim,Ispec), summarize, m=mean(TrueB35))
				MSSTs <- ddply(MSST, .(Ispec), summarize, m=mean(TrueB35),std=sd(TrueB35))
				MSSTs$Species = c("Cod", "Pollock", "Yellowfin")
				BMSY <- ddply(MSST, .(Isim,Ispec), summarize, m=mean(TrueB40)) 
				BMSY <- ddply(MSST, .(Ispec), summarize, m=mean(TrueB40),std=sd(TrueB40))

				quant_dat1 <- ddply(SSB, .(Species, Year), summarize,
					q05 = quantile(SSB/1e6, probs = 0.05),
					q50 = quantile(SSB/1e6, probs = 0.50),
					q95 = quantile(SSB/1e6, probs = 0.95)
				)
				
				# g1 <- ggplot(SSB, aes(x=Year, y=SSB/1e6, colour=Sim, group=Sim)) + facet_grid(Species ~ ., scales="free_y") + geom_line() + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") +coord_cartesian(xlim=c(0,End_year))+labs(y="SSB (1000t)")
				# g1 <- g1 + geom_hline(aes(yintercept = 1/2*m/1e6), MSSTs, linetype = 2, size=1.2)
				g1 <- ggplot(SSB, aes(x=Year, y = SSB/1e6, group = ID, colour=Species, alpha=Sim)) + geom_line() + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_hline(yintercept = 1/2*MSSTs[,2]/1e6, linetype = 2, size=0.5, colour = c("red","green","blue")) + coord_cartesian(xlim=c(0,End_year))+ labs(y="SSB (1000t)")# + geom_line(aes(x=Year, y=Total_catch/1e6), linetype=2, size=0.8, 
				g1_fake <- ggplot(SSB, aes(x=Year, y = SSB/1e6, group = ID, colour=Species, alpha=Sim)) + geom_line(size=1.2) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_hline(yintercept = 1/2*MSSTs[,2]/1e6, linetype = 2, size=0.5, colour = c("red","green","blue")) + coord_cartesian(xlim=c(0,End_year))+ labs(y="SSB (1000t)") + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3)))
				
				# sp1_col =  colorRampPalette(c("#e7e1ef", "#dd1c77"))(length(Simul))
				# sp2_col =  colorRampPalette(c("#e5f5f9", "#2ca25f"))(length(Simul))
				# sp3_col =  colorRampPalette(c("#deebf7", "#3182bd"))(length(Simul))
				# plot(0,0, type="n", xlim=c(0,End_year), ylim=c(0,7500), xlab="Year", ylab="SSB (1000t)")
				# for (i in Simul)
				# { 
					# sp1 <- subset(SSB, Species == "Cod" & Sim == i) 
					# sp2 <- subset(SSB, Species == "Pollock" & Sim == i) 
					# sp3 <- subset(SSB, Species == "Yellowfin" & Sim == i) 
					# lines(sp1$Year, sp1$SSB/1e6, col=sp1_col[i], lty=1, lwd=2)
					# lines(sp2$Year, sp2$SSB/1e6, col=sp2_col[i], lty=1, lwd=2)
					# lines(sp2$Year, sp3$SSB/1e6, col=sp3_col[i], lty=1, lwd=2)
				# }
				# abline(v=52, lty=2)
				
			### Recruits
				Rec <- read.table("RECRUITS.out")
				colnames(Rec) <- c("Sim", "Species", "Year", "Rec_M", "Rec_F")
				Rec$Species <- factor(Rec$Species, labels=c("Cod","Pollock","Yellowfin"))
				Rec <- subset(Rec, Sim%in%Simul)

				g2 <- ggplot(Rec, aes(x=Year, y=Rec_F/1e6, colour=Sim, group=Sim)) + facet_grid(Species ~ ., scales="free_y") + geom_line() + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + labs(y="Rec (1000t)")
				
			### Catches
				Catch <- read.table("Summ14.out")
				colnames(Catch) <- c("Sim", "Species", "Year", "Catch")
				Catch$Species <- factor(Catch$Species, labels=c("Cod","Pollock","Yellowfin"))
				Catch <- subset(Catch, Sim%in%Simul)
				Total_catch <- ddply(Catch, .(Sim,Year), summarize, Total_catch=sum(Catch))
				Catch$Total_catch <- rep(as.numeric(as.vector(Total_catch[,3])), each=3)
				qwer <- subset(Catch, Year>51)
				qwe <- ddply(qwer, .(Sim), summarize, max=max(Total_catch))
				Simul = qwe[which(qwe$max<=1700000000),1]
				Catch <- subset(Catch, Sim%in%Simul)
				Catch$ID <- paste(Catch$Sim, Catch$Species, sep="_")
						
				# g4 <- ggplot(Catch, aes(x=Year, y = Catch/1e6, group = Sim, colour=Sim)) + facet_grid(Species ~ ., scales="free_y") + geom_line() + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_hline(yintercept = 1.7e3, linetype = 3, colour = "black") + coord_cartesian(xlim=c(0,End_year))+ labs(y="Catch (1000t)")
				g4 <- ggplot(Catch, aes(x=Year, y = Catch/1e6, group = ID, colour=Species, alpha=Sim)) + geom_line() + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_hline(yintercept = 1.7e3, linetype = 3, colour = "black") + coord_cartesian(xlim=c(0,End_year))+ labs(y="Catch (1000t)")# + geom_line(aes(x=Year, y=Total_catch/1e6), linetype=2, size=0.8, colour = "black")
				
			### Depletion
				SSBDepletion_OM <- read.table("Summ3.out")
				colnames(SSBDepletion_OM) <- c("Sim", "Species", "Year", "SSBDepletion")
				SSBDepletion_OM$Species <- factor(SSBDepletion_OM$Species, labels=c("Cod","Pollock","Yellowfin"))
				SSBDepletion_OM <- subset(SSBDepletion_OM, Sim%in%Simul)

				g5 <- ggplot(SSBDepletion_OM, aes(x=Year, y = SSBDepletion, group = Sim, colour=Sim)) + facet_grid(Species ~ ., scales="free_y") + theme_bw() + geom_line(size=1)+coord_cartesian(xlim=c(0,End_year), ylim=c(0,150))+ geom_vline(xintercept = 52, linetype = 2, colour = "black")

			### Bycatch	
				Bycatch <- c()
				for (i in Simul)
				{
					bycatch <- read.table(paste0("Realized_catch.outSim", i),header=T)
					bycatch$Year <- 52:End_year
					bycatch$Species = "Bycatch"
					bycatch$Sim = i
					Bycatch <- rbind(Bycatch, bycatch)
				}
				Simul <- Simul[! Simul%in% Bycatch$Sim[which(Bycatch$Bycatch>4575)]]
				Bycatch <- c()
				for (i in Simul)
				{
					bycatch <- read.table(paste0("Realized_catch.outSim", i),header=T)
					bycatch$Year <- 52:End_year
					bycatch$Species = "Bycatch"
					bycatch$Sim = i
					Bycatch <- rbind(Bycatch, bycatch)
				}		
				
				g6 <- ggplot(Bycatch, aes(x=Year, y = Bycatch, group=Sim, colour=Sim)) + scale_colour_gradient(low=grey(0.9), high=grey(0)) + geom_line() + theme_bw() + coord_cartesian(xlim=c(0,End_year), ylim=c(0,5000)) + labs(y="ByCatch (t)") + geom_hline(yintercept = 4575, linetype = 3, colour = "black")+ geom_vline(xintercept = 52, linetype = 4, colour = "black")

			### The estimated ABC 
				ABC <- read.table("ABC.dat")
				if(ncol(ABC) == 5) 
				{
					colnames(ABC) <- c("Year", "Cod", "Pollock", "Yellowfin", "bycatch")
					# ABC$Sim <- 1	
					ABC$Sim <- rep(1:(nrow(ABC)/30), each=30)	
				}	
				if(ncol(ABC) > 6) {colnames(ABC) <- c("Sim", "Year", "Cod", "Pollock", "Yellowfin", "bycatch")}
				ABC <- as.data.frame(ABC)[,-5]
				ABC <- subset(ABC, Sim%in%Simul)
				ABC <- reshape2::melt(ABC, id.vars=c("Year","Sim"), variable.name = "Species")
				Total_ABC <- ddply(ABC, .(Sim,Year), summarize, Total_ABC=sum(value))
				ABC$Total_ABC <- rep(Total_ABC[,3], 3)
				ABC$ID <- paste(ABC$Sim, ABC$Species, sep="_")

				# g7 <- ggplot(ABC, aes(x=Year, y = value/1e6, group = Sim, colour=Sim)) + theme_bw() + labs(y="ABC (1000t)") + geom_line()+coord_cartesian(xlim=c(0,End_year),ylim=c(0,4000))+ geom_line(aes(x=Year, y=Total_ABC/1e6), linetype = 2, size=1, colour = "black")+geom_hline(yintercept = 1700, linetype = 3, colour = "black")+ geom_vline(xintercept = 52, linetype = 4, colour = "black")#+geom_vline(yintercept = 1500, linetype = 2, size=1, colour = "black")
				g7 <- ggplot(ABC, aes(x=Year, y = value/1e6, group = ID, colour=Species, alpha=Sim)) + geom_line() + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_hline(yintercept = 1.7e3, linetype = 3, colour = "black") + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 7500))+ labs(y="ABC (1000t)")# + geom_line(aes(x=Year, y=Total_catch/1e6), linetype=2, size=0.8, colour = "black")
					
			### Do all plots	
				
				g1a <- g1 + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 5500))
				g5a <- g5 + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 120))
				g7a <- g7+ theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 7500))
				g4a <- g4 + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(0, 5500))
				g6a <- g6 + theme(axis.title = element_text(size = rel(1.4)), axis.text = element_text(size = rel(1.3)), legend.title = element_text(size = rel(1.4)), legend.text = element_text(size = rel(1.3))) + coord_cartesian(xlim=c(0,End_year),ylim=c(2000, 5000))
				# multiplot(g5a,ABC_plota,g4a,g6a, cols=2)	

				library(gridExtra); library(grid)
				grid_arrange_shared_legend <- function(...) {
					plots <- list(...)
					g <- ggplotGrob(g1_fake + theme(legend.position="bottom"))$grobs
					legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
					lheight <- sum(legend$height)
					grid.arrange(
						do.call(arrangeGrob, lapply(plots, function(x)
							x + theme(legend.position="none"))),
						legend,
						ncol = 1,
						heights = unit.c(unit(1, "npc") - lheight, lheight))
				}	
				
				# if (run_history != 2) 
				# {
					# if (Scenario == "E") file_name = "Base_noest_IND_SQ"		
					# if (Scenario == "F") file_name = "Base_noest_IND_LP"		
					# if (Scenario == "G") file_name = "Base_noest_COR_SQ"		
					# if (Scenario == "H") file_name = "Base_noest_COR_LP"		
				# }
				# if (run_history == 2) 
				# {
					# if (Scenario == "E") file_name = "Constraint_noest_IND_SQ"		
					# if (Scenario == "F") file_name = "Constraint_noest_IND_LP"		
					# if (Scenario == "G") file_name = "Constraint_noest_COR_SQ"		
					# if (Scenario == "H") file_name = "Constraint_noest_COR_LP"		
				# }
				# if (Scenario == "N") file_name = "TV_noest_IND_SQ"		
				# if (Scenario == "O") file_name = "TV_noest_IND_LP"		
				# if (Scenario == "P") file_name = "TV_noest_COR_SQ"		
				# if (Scenario == "Q") file_name = "TV_noest_COR_LP"		
				# if (Scenario == "V") file_name = "Bycatch_noest_IND_SQ_25"		
				# if (Scenario == "W") file_name = "Bycatch_noest_IND_LP_25"		
				# if (Scenario == "X") file_name = "Bycatch_noest_COR_SQ_25"		
				# if (Scenario == "Y") file_name = "Bycatch_noest_COR_LP_25"		
				# if (Scenario == 1)  file_name = "Base_est_IND_SQ"		
				# if (Scenario == 2)  file_name = "Base_est_IND_LP"		
				# if (Scenario == 3)  file_name = "TV_est_IND_SQ"		
				# if (Scenario == 4)  file_name = "TV_est_IND_LP"		
				# if (Scenario == 5)  file_name = "COR_est_SQ"		
				# if (Scenario == 6)  file_name = "COR_est_LP"		
				# if (Scenario == 7)  file_name = "Constraint_est_IND_SQ"		
				# if (Scenario == 8)  file_name = "Constraint_est_IND_LP"		
				# if (Scenario == 9)  file_name = "Bycatch_est_IND_SQ_25"		
				# if (Scenario == 10) file_name = "Bycatch_est_IND_LP_25"
				# if (Scenario == 11) file_name = "Bycatch_est_IND_SQ_15"		
				# if (Scenario == 12) file_name = "Bycatch_est_IND_LP_15"		
				# if (Scenario == 13) file_name = "Bycatch_est_IND_SQ_25_less"		
				# if (Scenario == 14) file_name = "Bycatch_est_IND_LP_25_less"		
				# if (Scenario == 15) file_name = "TV_est_COR_SQ"		
				# if (Scenario == 16) file_name = "TV_est_COR_LP"		
				# if (Scenario == 17) file_name = "TV_est_IND_SQ_more"		
				# if (Scenario == 18) file_name = "TV_est_IND_LP_more"	
				file_name <- paste0("Scenario_",Scenario)

				if (save_plot == TRUE) 
				{
					png(filename = paste0(Figures_save, "/model_check/", file_name, "a.png"), width = 250, height=200, units="mm", res=200)
					grid_arrange_shared_legend(g1a, g7a, g4a, g6a)
					dev.off()
					png(filename = paste0(Figures_save, "/model_check/", file_name, "b.png"), width = 250, height=200, units="mm", res=200)
					grid_arrange_shared_legend(g2, g3, g5)
					dev.off()
				}		
				if (save_plot == FALSE) grid_arrange_shared_legend(g1a, g7a, g4a, g6a)

		}
	}

	}


	RUN_plot(Scenario=3, run_history = 3, save_plot=FALSE)

	Scenario_list <- c(3:4)

	for (i in seq_along(Scenario_list))
	{
		RUN_plot(Scenario=Scenario_list[i], run_history = 3, save_plot=FALSE)
	}

	
##############################################################################	
#
#		Read in the results from CAB to check performance
#
##############################################################################	

	Scenario=4; Which.species=2;Yr=60;Sim=1;fitComp=FALSE;fitSSB=TRUE;fitIA=TRUE
		
	Plot_cab_fit <- function(Scenario, Which.species,Yr,Sim,fitSSB=TRUE,fitComp=TRUE,fitIA=TRUE)
	{
	
		if(file.exists(paste0(main_folder, "\\Techinteractions", Scenario, "\\results"))) 
		{ setwd(paste0(main_folder, "\\Techinteractions", Scenario, "\\results")) } else {
		setwd(paste0(main_folder, "\\Techinteractions", Scenario, "\\runs\\results"))
		}
		
		Rep.file = paste0("cab.repSp", Which.species, "Yr", Yr, "Sim", Sim, "Config")
		Par.file = paste0("cab.parSp", Which.species, "Yr", Yr, "Sim", Sim, "Config")
		Dat.file = paste0("cab.datSp", Which.species, "Yr", Yr, "Sim", Sim, "Config")


		cols <- RColorBrewer::brewer.pal(8, "Greys")

		Simul <- Sim
		### Qval 
			QQQ <- read.table("SAVE.ALL")
			Q <- QQQ[,-c(1,2)]
			Q1 <- Q[Simul,]
		 
		### Effort
			Effort <- read.table("Summ17.out")
			colnames(Effort) <- c("Sim", "Species", "Year", "Effort")
			Effort$Species <- factor(Effort$Species, labels=c("Cod","Pollock","Yellowfin"))
			Effort <- subset(Effort, Sim%in%Simul)
			Effort$Effort <- as.numeric(as.vector(Effort$Effort))
			asd <- ddply(Effort, .(Sim), summarize, max=max(Effort))
			Effort <- subset(Effort, Sim%in%Simul)
			Q <- Q[Simul,]
			Effort$F <- as.numeric(as.vector(Effort$Effort))*c(apply(Q, 1, function(x) rep(as.numeric(x),End_year)))
			
			quant_dat3 <- ddply(Effort, .(Species, Year), summarize,
				q05 = quantile(F, probs = 0.05),
				q50 = quantile(F, probs = 0.50),
				q95 = quantile(F, probs = 0.95)
			)
			g3 <- ggplot(quant_dat3) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black")+labs(y="F")
			
		### SSB
			SSB <- read.table("Summ13.out")
			colnames(SSB) <- c("Sim", "Species", "Year", "SSB")
			SSB$Species <- factor(SSB$Species, labels=c("Cod","Pollock","Yellowfin"))
			SSB <- subset(SSB, Sim%in%Simul)
			
			quant_dat1 <- ddply(SSB, .(Species, Year), summarize,
				q05 = quantile(SSB/1e6, probs = 0.05),
				q50 = quantile(SSB/1e6, probs = 0.50),
				q95 = quantile(SSB/1e6, probs = 0.95)
			)
			
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
			
			g1 <- ggplot(quant_dat1) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") + geom_hline(yintercept = 1/2*MSSTs[,2]/1e6, linetype = 2, size=0.5, colour = c("red","green","blue"))+coord_cartesian(xlim=c(0,End_year))+coord_cartesian(ylim=c(0,5500))+labs(y="SSB (1000t)")
					
		### Recruits
			Rec <- read.table("RECRUITS.out")
			colnames(Rec) <- c("Sim", "Species", "Year", "Rec_M", "Rec_F")
			Rec$Species <- factor(Rec$Species, labels=c("Cod","Pollock","Yellowfin"))
			Rec <- subset(Rec, Sim%in%Simul)

			quant_dat2 <- ddply(Rec, .(Species, Year), summarize,
				q05 = quantile(Rec_F/1e6, probs = 0.05),
				q50 = quantile(Rec_F/1e6, probs = 0.50),
				q95 = quantile(Rec_F/1e6, probs = 0.95)
			)
			g2 <- ggplot(quant_dat2) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black")+labs(y="Rec (1000t)")	
		
		### Match and mismatch of EstB35, TrueB35, TrueFOFL, EstFOFL
			MSST$RE_F40 <- (MSST$EstF40-MSST$TrueF40)/MSST$TrueF40
			MSST$RE_B40 <- (MSST$EstB40-MSST$TrueB40)/MSST$TrueB40
			Datdat <- subset(MSST, Isim==Sim)
			# windows()
			# nf <- layout(matrix(c(1,2,3,3), nrow=2, ncol=2, byrow=T))
			# nf <- layout(matrix(c(1,2), nrow=1, ncol=2, byrow=T))
			# if(Which.species==1) Recruitment <- subset(Rec, subset=c(Sim==Sim & Species=="Cod"))
			# if(Which.species==2) Recruitment <- subset(Rec, subset=c(Sim==Sim & Species=="Pollock"))
			# if(Which.species==3) Recruitment <- subset(Rec, subset=c(Sim==Sim & Species=="Yellowfin"))
			# par(cex.lab=1.3, cex.main=1.4, cex.axis=1.2, mar=c(4,5,1,1), oma=c(1,0,0,0), xaxt="i", yaxt="i")
			# if(Which.species==1) plot(subset(Datdat,Species=="Cod")$IYr, subset(Datdat,Species=="Cod")$RE_F40, type="l", ylim=c(-0.05,0.05), lty=1, lwd=2, col="black", xlab="Year", ylab=expression(paste("RE of ",F["40%"])))
			# if(Which.species==2) plot(subset(Datdat,Species=="Pollock")$IYr, subset(Datdat,Species=="Pollock")$RE_F40, type="l", ylim=c(-0.05,0.05), lty=1, lwd=2, col="black", xlab="Year", ylab=expression(paste("RE of ",F["40%"])))
			# if(Which.species==3) plot(subset(Datdat,Species=="Yellowfin")$IYr, subset(Datdat,Species=="Yellowfin")$RE_F40, type="l", ylim=c(-0.05,0.05), lty=1, lwd=2, col="black", xlab="Year", ylab=expression(paste("RE of ",F["40%"])))
			# lines(Recruitment$Year, (Recruitment$Rec_F-mean(Recruitment$Rec_F))/sd(Recruitment$Rec_F)/100, type="l", lty=2, lwd=1, col="black", xlab="Year", ylab=expression(paste("Recruitment (", 10^9, ")")), new=T)
			# abline(h=0, lwd=1, lty=3, col=1)
			# if(Which.species==1) plot(subset(Datdat,Species=="Cod")$IYr, subset(Datdat,Species=="Cod")$RE_B40, type="l", ylim=c(-0.3,0.4), lty=1, lwd=2, col="black", xlab="Year", ylab=expression(paste("RE of ",B["40%"])))
			# if(Which.species==2) plot(subset(Datdat,Species=="Pollock")$IYr, subset(Datdat,Species=="Pollock")$RE_B40, type="l", ylim=c(-0.3,0.4), lty=1, lwd=2, col="black", xlab="Year", ylab=expression(paste("RE of ",B["40%"])))
			# if(Which.species==3) plot(subset(Datdat,Species=="Yellowfin")$IYr, subset(Datdat,Species=="Yellowfin")$RE_B40, type="l", ylim=c(-0.3,0.4), lty=1, lwd=2, col="black", xlab="Year", ylab=expression(paste("RE of ",B["40%"])))
			# lines(Recruitment$Year, (Recruitment$Rec_F-mean(Recruitment$Rec_F))/sd(Recruitment$Rec_F)/10, type="l", lty=2, lwd=1, col="black", xlab="Year", ylab=expression(paste("Recruitment (", 10^9, ")")), new=T)
			# abline(h=0, lwd=1, lty=3, col=1)
			# # plot(Recruitment$Year, Recruitment$Rec_F/10^9, type="l", ylim=c(0,1.1*max(Recruitment$Rec_F/10^9)), lty=1, lwd=2, col="black", xlab="Year", ylab=expression(paste("Recruitment (", 10^9, ")")))
			# abline(h=mean(Recruitment$Rec_F)/10^9, lwd=1, lty=2)
			# abline(v=51, lwd=1, lty=2, col=1)
			
		### Catches
			Catch <- read.table("Summ14.out")
			colnames(Catch) <- c("Sim", "Species", "Year", "Catch")
			Catch$Species <- factor(Catch$Species, labels=c("Cod","Pollock","Yellowfin"))
			Catch <- subset(Catch, Sim%in%Simul)
			Total_catch <- ddply(Catch, .(Sim,Year), summarize, Total_catch=sum(Catch))
			Catch$Total_catch <- rep(as.numeric(as.vector(Total_catch[,3])), each=3)
			qwer <- subset(Catch, Year>51)
			qwe <- ddply(qwer, .(Sim), summarize, max=max(Total_catch))
			Simul = qwe[which(qwe$max<=1700000000),1]
			Catch <- subset(Catch, Sim%in%Simul)
					
			quant_dat4 <- ddply(Catch, .(Species, Year), summarize,
				q05 = quantile(Catch/1e6, probs = 0.05),
				q50 = quantile(Catch/1e6, probs = 0.50),
				q95 = quantile(Catch/1e6, probs = 0.95),
				q05_sum = quantile(Total_catch/1e6, probs = 0.05),
				q50_sum = quantile(Total_catch/1e6, probs = 0.50),
				q95_sum = quantile(Total_catch/1e6, probs = 0.95)			
			)
			g4 <- ggplot(quant_dat4) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_ribbon(aes(Year, ymax = q05_sum, ymin = q95_sum), alpha=0.5, fill=grey(0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + geom_line(aes(Year, y=q50_sum), colour="black", linetype=2, size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black")+labs(y="Catch (1000t)")+geom_hline(yintercept = 1700, linetype = 3, colour = "black")

		### Depletion
			SSBDepletion_OM <- read.table("Summ3.out")
			colnames(SSBDepletion_OM) <- c("Sim", "Species", "Year", "SSBDepletion")
			SSBDepletion_OM$Species <- factor(SSBDepletion_OM$Species, labels=c("Cod","Pollock","Yellowfin"))
			SSBDepletion_OM <- subset(SSBDepletion_OM, Sim%in%Simul)

			quant_dat5 <- ddply(SSBDepletion_OM, .(Species, Year), summarize,
				q05 = quantile(SSBDepletion, probs = 0.05),
				q50 = quantile(SSBDepletion, probs = 0.50),
				q95 = quantile(SSBDepletion, probs = 0.95)
			)
			g5 <- ggplot(quant_dat5) + geom_ribbon(aes(Year, ymax = q05, ymin = q95, group = Species, fill=Species, alpha=0.5)) + geom_line(aes(Year, y=q50, group = Species, colour=Species), size=1.5) + theme_bw() + geom_vline(xintercept = 52, linetype = 4, colour = "black") +labs(y="Depletion")

		############### Read in the results from CAB


		if (fitSSB==TRUE)
		{
			SSB_est <- matrix(0, nrow=Yr, ncol=6)
			for (iii in 52:Yr)
			{			
				Rep.file = paste0("cab.repSp", Which.species, "Yr", iii, "Sim", Sim, "Config")
				Sp1 <- matrix(scan(Rep.file, skip=15, nlines=iii),ncol=6, byrow=T)
				SSB_est[iii,] <- Sp1[iii,]
			}
				if(Which.species==1) SSB <- subset(SSB, subset=c(Year %in% seq(52,Yr) & Species=="Cod"))
				if(Which.species==2) SSB <- subset(SSB, subset=c(Year %in% seq(52,Yr) & Species=="Pollock"))
				if(Which.species==3) SSB <- subset(SSB, subset=c(Year %in% seq(52,Yr) & Species=="Yellowfin"))
				SSB_est <- SSB_est[52:Yr,]
				SSB$RE <- (SSB_est[,2]-SSB$SSB)/SSB$SSB
				plot(SSB$Year, SSB$RE, type="p", ylim=c(-1,1), lty=1, lwd=2, col="black", xlab="Year", ylab="RE in SSB")
				abline(h=0, lty=2, lwd=1)
				
			# Sp1 <- matrix(scan(Rep.file, skip=15, nlines=Yr),ncol=6, byrow=T)
			# windows()
			# SSB <- subset(SSB, Year>=51)
			# Sp1 <- Sp1[51:nrow(Sp1),]
			# par(mfrow=c(1,2), cex.lab=1.3, cex.main=1.4, cex.axis=1.2)
			# if(Which.species==1) plot(subset(SSB,Species=="Cod")$Year, subset(SSB,Species=="Cod")$SSB, type="l", ylim=c(0,1.5*max(subset(SSB,Species=="Cod")$SSB)), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB (kg)")
			# if(Which.species==2) plot(subset(SSB,Species=="Pollock")$Year, subset(SSB,Species=="Pollock")$SSB, type="l", ylim=c(0,1.5*max(subset(SSB,Species=="Pollock")$SSB)), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB (kg)")
			# if(Which.species==3) plot(subset(SSB,Species=="Yellowfin")$Year, subset(SSB,Species=="Yellowfin")$SSB, type="l", ylim=c(0,1.5*max(subset(SSB,Species=="Yellowfin")$SSB)), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB (kg)")
			# lines(Sp1[,1], Sp1[,2], lwd=2, lty=2, col="red")
			# legend("topright", legend=c("true", "estimated"), col=c(1, "red"), lty=c(1,2), lwd=2, bty="n")
			
			# if(Which.species==1) plot(subset(SSBDepletion_OM,Species=="Cod")$Year, subset(SSBDepletion_OM,Species=="Cod")$SSBDepletion/100, type="l", ylim=c(0,1.5), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB depletion")
			# if(Which.species==2) plot(subset(SSBDepletion_OM,Species=="Pollock")$Year, subset(SSBDepletion_OM,Species=="Pollock")$SSBDepletion/100, type="l", ylim=c(0,1.5), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB depletion")
			# if(Which.species==3) plot(subset(SSBDepletion_OM,Species=="Yellowfin")$Year, subset(SSBDepletion_OM,Species=="Yellowfin")$SSBDepletion/100, type="l", ylim=c(0,1.5), lty=1, lwd=2, col="black", xlab="Year", ylab="SSB depletion")
			# lines(Sp1[,1], Sp1[,3], lwd=2, lty=2, col="red")
			# abline(h=0.4)
			# legend("topright", legend=c("true", "estimated"), col=c(1, "red"), lty=c(1,2), lwd=2, bty="n")
		# }

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
			plot(as.numeric(IA_fit[,4]), as.numeric(IA_fit[,5]), ylab="Index of abundance", cex=1.2, xlab="Year", main="Fit to index of abundance", type="l"); points(as.numeric(IA_fit[,4]), IA_fit[,6],lwd=2)
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
			plot(Natage[,1], Rec_fit, ylab="Number of recruits", xlab="Year", main="Estimated vs true recruitments", cex=1.2, type="l"); points(Rec_subset$Year, Rec_true,lwd=2); abline(v=51, lty=2)
		}
		
	}
}		
		Plot_cab_fit(Scenario=4, Which.species=2,Yr=60,Sim=1,fitComp=TRUE)
		# Plot_cab_fit(Scenario=1, Which.species=2,Yr=53,Sim=1,fitComp=TRUE)
		# Plot_cab_fit(Scenario=1, Which.species=3,Yr=53,Sim=1,fitComp=FALSE)
		# Plot_cab_fit(Scenario=1, Which.species=2,Yr=75,Sim=1,fitComp=FALSE,fitSSB=TRUE,fitIA=TRUE)
		# graphics.off()


##############################################################################	
#
#		Summarize more succintly the results to directly compare model performance 	
#
##############################################################################	
		
	# Scenario comparison
		# scenarios <- c(3:4,9:10)						# Test influence of correlated recruits
		scenarios <- c(3:4,13:16)						# Test influence of flexibility in fishing behavior
		# scenarios <- c(3:8)							# Test influence of bycatch constraints 
		# scenarios <- c(3:6,"7b","8b")					# Test influence of bycatch constraints 
		# scenarios <- c(3:4,17:18)						# Test change in recruitment regimes
		# scenarios <- c(3:4,17:20)						# Test change in recruitment regimes
		scenarios <- c(3:4,9,10,17:20)					# Test change in recruitment (correlation and regimes)
		scenarios <- c(3:4,11:12,1:2)					# Test influence of data poor, assessment accuracy
		# scenarios <- c(3:4,"7b","8b","13a","14a")		# Test influence of bycatch constraints + more flexibility
		scenarios <- c(3:6,"7b","8b","13a","14a")		# Test influence of bycatch constraints + more flexibility
		
	#### Boxplots of simulation results between scenarios
	#### To make it even better, add on top of it the number of times the LP>Naive
	Yr_end = 65; CI=90
	
	DO_PLOTS <- function(Yr_end = 65, CI=90)
	{
	
		CATCHES <- c()
		OVERFISHED <- c()
		OVERFISHED_ts <- c()
		CATCH_QUOTA <- c()
		CATCH_QUOTA_RATIO <- c()
		Simul_keep <- list()
		DATA_CHECK <- c()
		REFS <- c()
		SSBs <- c()
		
		for (Scenario in scenarios)
		{

			if(file.exists(paste0(main_folder, "/Techinteractions", Scenario, "/results"))) 
			setwd(paste0(main_folder, "/Techinteractions", Scenario, "/results")) 
			if(file.exists(paste0(main_folder, "/Techinteractions", Scenario, "/runs/results"))) 
			setwd(paste0(main_folder, "/Techinteractions", Scenario, "/runs/results"))
			# if(file.exists(paste0(main_folder, "/techinteractions", Scenario, "/runs/results"))) 
			# setwd(paste0(main_folder, "/techinteractions", Scenario, "/runs/results"))
						
			# setwd(paste0("C:\\Users\\Kotaro Ono\\Dropbox\\Postdoc_projects\\Techint\\Techinteractions", Scenario, "\\results"))
			# setwd(paste0("F:\\Dropbox\\Postdoc_projects\\Techint\\Techinteractions", Scenario, "\\results"))

			### To determine the catch by species (the TRUE realized catch)
				Catch <- read.table("Summ14.out")
				colnames(Catch) <- c("Sim", "Species", "Year", "Catch")
				Simul <- unique(Catch$Sim)
				
				Catch$Species <- factor(Catch$Species, labels=c("Cod","Pollock","Yellowfin"))
				Catch$Year = as.numeric(Catch$Year)
				Catch <- subset(Catch, subset=c(Year >=52 & Year <= Yr_end))
				Catch$Scenario = Scenario
				Total_catch <- ddply(Catch, .(Sim,Year), summarize, Total_catch=sum(Catch))
				Catch$Total_catch <- rep(Total_catch[,3], each=3)
				asd <- ddply(Catch, .(Sim), summarize, m=max(Total_catch))
				Simul <- asd[which(asd[,2]<1701000000 & asd[,2]>1000000000),1]
				Simul <- as.numeric(as.vector(Simul))[!is.na(as.numeric(as.vector(Simul)))]
				Catch <- subset(Catch, Sim%in%Simul)
				
				Bycatch <- c()
				for (i in Simul)
				{
					if(file.exists(paste0("Realized_catch.outSim", i))) bycatch <- read.table(paste0("Realized_catch.outSim", i),header=T)
					bycatch$Year <- 52:End_year
					bycatch$Sim = i
					Bycatch <- rbind(Bycatch, bycatch)
				}
									
				if(!(Scenario%in%c(19,20,21,22,33,34,43,44))) Simul <- Simul[! Simul%in% Bycatch$Sim[which(Bycatch$Bycatch>4575)]]
				if(Scenario%in%c(19,20,21,22,33,34,43,44)) Simul <- Simul[! Simul%in% Bycatch$Sim[which(Bycatch$Bycatch>3430)]]
				Catch <- subset(Catch, Sim%in%Simul)
				Bycatch <- subset(Bycatch, Sim%in%Simul)
				Bycatch <- subset(Bycatch, subset=c(Year >=52 & Year <= Yr_end))
				
			#### Check differences in realized catch from LP and the one that TRULY goes into OM
				ID <- subset(Catch, Species == "Cod")[,c(1,3,5)]
				Cod <- subset(Catch, Species == "Cod")$Catch/1000
				Pollock <- subset(Catch, Species == "Pollock")$Catch/1000
				Yellowfin <- subset(Catch, Species == "Yellowfin")$Catch/1000
				Data_check <- data.frame(ID, CodTrue=Cod, CodExp=Bycatch$Cod, CodDiff=Cod-Bycatch$Cod, PollockTrue=Pollock, PollockExp=Bycatch$Pollock, PollockDiff=Pollock-Bycatch$Pollock, YellowfinTrue=Yellowfin, YellowfinExp=Bycatch$Yellowfin, YellowfinDiff=Yellowfin-Bycatch$Yellowfin)
				
				### Select only the ones that has similar (absolute difference less than 1.1% from the true value)
				Tot_diff1 <- rowSums(Data_check[,c(6,9,12)]/Data_check[,c(5,8,11)])
				to_remove1 <- which(abs(Tot_diff1)>0.011)			
				tokeep_end <- setdiff(Simul, Data_check[to_remove1, "Sim"])
			
			#### Catch quota balancing
				Quota <- c()
				for (i in Simul)
				{
					# realized catch (see above)
						
					# quota allocation
						if(file.exists(paste0("TAC_techint.outSim", i))) 
						{
							quota <- read.table(paste0("TAC_techint.outSim", i),header=T)
							quota[,1:3] <- quota[,1:3]/1000
						} else {
							quota <- read.table(paste0("TAC_statusquo.outSim", i),header=T)
						}	
						quota$Year <- 52:End_year
						quota$Sim = i				
						Quota <- rbind(Quota, quota)
				}
				Quota$Scenario <- Scenario
				Quota <- subset(Quota, subset=c(Year >=52 & Year <= Yr_end))
				
				Catch_quota <- cbind(Quota[,-c(1:3)],(Quota[,1:3]-Data_check[,c(5,8,11)]))
				Catch_quota_ratio <- cbind(Quota[,-c(1:3)],(Data_check[,c(5,8,11)]/Quota[,1:3]))
				Catch_quota$Total <- apply(Catch_quota[,-c(1:3)],1,sum)
				Catch_quota_ratio$Total <- apply(Data_check[,c(5,8,11)],1,sum)/apply(Quota[,c(1:3)],1,sum)
				
				### The catch CANNOT exceed the quota (here cannot exceed 10kg)
				Tot_diff2 <- apply((Catch_quota[,c(4:6)]),1, function(x) any(x < -10))
				to_remove2 <- which(Tot_diff2 == TRUE)			
				tokeep_end <- setdiff(tokeep_end, Catch_quota[to_remove2, "Sim"])
				
				CATCHES1 <- Catch[order(Catch$Sim,Catch$Species,Catch$Year),]
				
			#### For calculating overfish probability 	
				SSB <- read.table("Summ13.out")
				colnames(SSB) <- c("Sim", "Species", "Year", "SSB")
				SSB$Species <- factor(SSB$Species, labels=c("Cod","Pollock","Yellowfin"))
				SSB$Year <- as.numeric(SSB$Year)
				SSB <- subset(SSB, Year >=52)
				SSB <- subset(SSB, Sim%in%Simul)
				SSB <- subset(SSB, subset=c(Year >=52 & Year <= Yr_end))
				SSB$Scenario <- Scenario
				
				MSST = matrix(scan("F_output.out", skip=2),ncol=17,byrow=T)
				if((min(MSST[,1])==1) & ((MSST[1,2]!=51) | (MSST[1,3]!=1))) MSST = matrix(scan("F_output.out", skip=2),ncol=17,byrow=T)
				if(min(MSST[,1])>1 & ((MSST[1,2]!=51) | (MSST[1,3]!=1))) MSST = matrix(scan("F_output.out"),ncol=17,byrow=T)
				colnames(MSST) <- c("Isim","IYr","Ispec","Icon","EstFOFL","EstF35","EstB35","EstSPR35","EstF40","EstB40","EstSPR40","TrueF35","TrueB35","TrueSPR35","TrueF40","TrueB40","TruSPR40")
				MSST <- as.data.frame(MSST)
				# MSST <- subset(MSST, IYr >=52)
				MSST$Sim <- as.factor(MSST$Isim)
				MSST$Ispec <- as.factor(MSST$Ispec)
				MSST$TrueB35 <- as.numeric(MSST$TrueB35)
				MSST <- subset(MSST, Sim%in%Simul)
				MSST <- subset(MSST, subset=c(IYr >=52 & IYr <= Yr_end))
				MSST$Scenario <- Scenario
				
				SSB$MSST <- 1/2*MSST$TrueB35

				SSB0 = c(800000000, 5200000000, 1000000000)
				
				over <- ddply(SSB, .(Sim, Species), summarize, OF=sum(SSB<MSST))
				over$Scenario = Scenario
				over$OF_freq = as.numeric(over$OF)/length(unique(over$Sim))*100
				
				over_ts <- ddply(SSB, .(Sim, Species, Year), summarize, OF=sum(SSB<MSST), SSB=SSB)
				over_ts$Scenario = Scenario
				over_ts$depl <- over_ts$SSB/rep(rep(SSB0,each=14),nrow(over_ts)/(3*14))
				
			## Which simulation to keep for each scenario
				kept <- which(scenarios%in%Scenario)
				
				Data_check <- subset(Data_check, Sim %in% tokeep_end)
				Catch_quota <- subset(Catch_quota, Sim %in% tokeep_end)
				Catch_quota_ratio <- subset(Catch_quota_ratio, Sim %in% tokeep_end)
				CATCHES1 <- subset(CATCHES1, Sim %in% tokeep_end)
				over <- subset(over, Sim %in% tokeep_end)
				over_ts <- subset(over_ts, Sim %in% tokeep_end)
				MSST <- subset(MSST, Sim %in% tokeep_end)
				SSB <- subset(SSB, Sim %in% tokeep_end)
				
				DATA_CHECK <- rbind(DATA_CHECK, Data_check)				
				CATCH_QUOTA <- rbind(CATCH_QUOTA,Catch_quota)
				CATCH_QUOTA_RATIO <- rbind(CATCH_QUOTA_RATIO,Catch_quota_ratio)
				CATCHES <- rbind(CATCHES, CATCHES1)
				CATCHES <- subset(CATCHES, subset=c(Year >=52 & Year <= Yr_end))
				OVERFISHED <- rbind(OVERFISHED, over)
				OVERFISHED_ts <- rbind(OVERFISHED_ts, over_ts)
				REFS <- rbind(REFS, MSST)
				SSBs <- rbind(SSBs, SSB)
				
				# kept1 <- NA
				# if(Scenario=="1b") kept1 <- which(scenarios%in%"1")
				# if(Scenario=="2b") kept1 <- which(scenarios%in%"2")
				# if(Scenario=="3b") kept1 <- which(scenarios%in%"3")
				# if(Scenario=="4b") kept1 <- which(scenarios%in%"4")
				# if(!is.na(kept1)) Simul_keep[[kept1]] <- c(Simul_keep[[kept1]],Simul)
				# if(is.na(kept1)) Simul_keep[[kept]] <- Simul
				Simul_keep[[kept]] <- tokeep_end
		}	
			
 		print(sapply(Simul_keep, length))

	### Overfishing risk
			OVERFISHED$Scenario		<- as.factor(OVERFISHED$Scenario)
			OVERFISHED$ScenarioName <- as.character(OVERFISHED$Scenario)
					
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "1")] = "Naive_noasses"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "2")] = "Consc_noasses"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "3")] = "Naive_Base"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "4")] = "Consc_Base"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "5")] = "Naive_PSC"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "6")] = "Consc_PSC"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "7b")] = "Naive_1500"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "8b")] = "Consc_1500"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "9")] = "Naive_COR"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "10")] = "Consc_COR"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "11")] = "Naive_poor"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "12")] = "Consc_poor"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "13")] = "Naive_more"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "14")] = "Consc_more"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "13a")] = "Naive_1500_flex"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "14a")] = "Consc_1500_flex"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "15")] = "Naive_less"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "16")] = "Consc_less"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "17")] = "Naive_rec_up"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "18")] = "Consc_rec_up"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "19")] = "Naive_rec_down"
			OVERFISHED$ScenarioName[which(OVERFISHED$ScenarioName == "20")] = "Consc_rec_down"
		
			OVERFISHED$ScenarioName <- factor(OVERFISHED$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex", "Naive_COR", "Consc_COR","Naive_more", "Consc_more", "Naive_less", "Consc_less","Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
		
			OVERFISHED$COL 			<- as.numeric(OVERFISHED$Scenario)
			OVERFISHED$COL 			<- replace(OVERFISHED$COL, which(OVERFISHED$COL%in%seq(1,15,by=2)), 1)
			OVERFISHED$COL 			<- replace(OVERFISHED$COL, which(OVERFISHED$COL%in%seq(2,15,by=2)), 2)
			OVERFISHED$COL 			<- as.factor(OVERFISHED$COL)
		
		# naming
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10))))) 	titre <- "Correlated recruitment"
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,13:16))))) 	titre <- "Flexibility in fishing activities"
			if (isTRUE(all.equal(as.character(scenarios),as.character(c(3:6,"7b","8b")))))		titre <- "Bycatch constraints 1500"
			if (isTRUE(all.equal(as.character(scenarios),as.character(c(3:8)))))		titre <- "Bycatch constraints"
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,17:18)))))		titre <- "Recruitment regimes"
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,17:20)))))		titre <- "Recruitment regimes"
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10,17:20)))))		titre <- "Recruitment changes"
			if (isTRUE(all.equal(as.character(scenarios), as.character((c(3:4,11,12,1:2))))))		titre <- "Data and assessment accuracy"
			if (isTRUE(all.equal(as.character(scenarios), as.character((c(3:4,"7b","8b","13a","14a"))))))		titre <- "Bycatch + flexibility old"
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:6,"7b","8b","13a","14a")))))		titre <- "Bycatch + flexibility"
			
		# Subset some results
			set.seed(777)
			dat.new <- c()
			dat.new.ts <- c()
			for (i in 1:length(scenarios))
			{
				dat <- c()
				# qwe <- c(unlist(Simul_keep[(i-1)*2+1]),unlist(Simul_keep[(i-1)*2+2]))[duplicated(c(unlist(Simul_keep[(i-1)*2+1]),unlist(Simul_keep[(i-1)*2+2])))]
				qwe <- c(unlist(Simul_keep[i]))
				qwe <- sort(qwe)
				qwe <- qwe[1:min(50,length(qwe))]
				dat <- subset(OVERFISHED, subset=c(Scenario %in% scenarios[i] & Sim %in% qwe))
				dat.ts <- subset(OVERFISHED_ts, subset=c(Scenario %in% scenarios[i] & Sim %in% qwe))			
				dat.new <- rbind(dat.new, dat)
				dat.new.ts <- rbind(dat.new.ts, dat.ts)
			}
			dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex", "Naive_COR", "Consc_COR","Naive_more", "Consc_more", "Naive_less", "Consc_less","Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			dat.new$Scenario <- factor(dat.new$Scenario, levels=scenarios)		
			dat.new$ScenarioName <- as.character(dat.new$Scenario)		
			dat.new$ScenarioName[which(dat.new$ScenarioName == "1")] = "Naive_noasses"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "2")] = "Consc_noasses"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "3")] = "Naive_Base"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "4")] = "Consc_Base"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "5")] = "Naive_PSC"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "6")] = "Consc_PSC"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "7b")] = "Naive_1500"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "8b")] = "Consc_1500"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "9")] = "Naive_COR"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "10")] = "Consc_COR"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "11")] = "Naive_poor"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "12")] = "Consc_poor"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "13")] = "Naive_more"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "14")] = "Consc_more"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "13a")] = "Naive_1500_flex"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "14a")] = "Consc_1500_flex"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "15")] = "Naive_less"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "16")] = "Consc_less"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "17")] = "Naive_rec_up"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "18")] = "Consc_rec_up"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "19")] = "Naive_rec_down"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "20")] = "Consc_rec_down"

			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,17:20))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,13:16))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_more", "Consc_more", "Naive_less", "Consc_less"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,11:12,1:2))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_poor", "Consc_poor", "Naive_noasses", "Consc_noasses"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:6,"7b","8b","13a","14a"))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_COR", "Consc_COR"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10,17:20))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_COR", "Consc_COR", "Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			
		# Choose which data to use for the final graph. All results or just the ones that were common to both
			OVERFISHED_plot <- dat.new #OVERFISHED
			OVERFISHED_ts_plot <- dat.new.ts #OVERFISHED
						
			datdat <- ddply(OVERFISHED_ts_plot, c("Scenario", "Species", "Year"), summarize, .drop=FALSE,
				q10 = quantile(depl, probs = 0.05),
				q25 = quantile(depl, probs = 0.25),
				q50 = quantile(depl, probs = 0.50),
				avg = mean(depl),
				q75 = quantile(depl, probs = 0.75),
				q90 = quantile(depl, probs = 0.95)
			)
			
			datdat <- ddply(OVERFISHED_ts_plot, c("Scenario", "Species", "Year"), summarize, .drop=FALSE,
				q10 = quantile(SSB/10^3, probs = 0.05),
				q25 = quantile(SSB/10^3, probs = 0.25),
				q50 = quantile(SSB/10^3, probs = 0.50),
				avg = mean(SSB/10^3),
				q75 = quantile(SSB/10^3, probs = 0.75),
				q90 = quantile(SSB/10^3, probs = 0.95)
			)
			dummy <- data.frame(Species=c("Cod", "Pollock", "Yellowfin"), Z= c(283310, 1800000,350000))
			plot_SSB <- ggplot(datdat, aes(x=Year,y=avg))+geom_line()+facet_grid(Species~Scenario, drop = FALSE, scales = "free_y") + geom_ribbon(aes(ymax = q90, ymin = q10), fill = 1, alpha = 0.2)+ theme_bw()
			plot_SSB + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.44))) + labs(x="Time", y=expression(paste(Depletion, " (%)")))  + scale_fill_brewer(palette="Greys") + geom_hline(data=dummy, aes(yintercept=Z),col="red")
			
			plot_depl <- ggplot(datdat, aes(x=Year,y=avg))+geom_line()+facet_grid(Scenario~Species, drop = FALSE) + geom_ribbon(aes(ymax = q75, ymin = q25), fill = 1, alpha = 0.2)+ theme_bw()
			plot_depl + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.44))) + labs(x="Time", y=expression(paste(Depletion, " (%)")))  + scale_fill_brewer(palette="Greys")+coord_cartesian(ylim=c(0,0.8))
			
			plot_overfished <- ggplot(OVERFISHED_plot) + geom_boxplot(aes(x=ScenarioName,y=OF_freq,fill=COL)) + facet_grid(.~Species, drop = FALSE)  + theme_bw()
			plot_overfished + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.44)))+labs(x="Scenario", y=expression(paste(P[overfished], " (%)")))  + scale_fill_brewer(palette="Greys")
			ggsave(filename=paste0(Figures_save, "/P_overfished_", titre, Yr_end, ".png"), width=30, height=18, units="cm", dpi=200)			
			
	### SSBs 
			head(SSBs)
			SSBs <- melt(SSBs, .(Sim,Year,Scenario,Species), value.name = "SSB")
			SSBs$Scenario <- as.factor(SSBs$Scenario)
			SSBs$ScenarioName <- as.character(SSBs$Scenario)

			SSBs$ScenarioName[which(SSBs$ScenarioName == "1")] = "Naive_noasses"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "2")] = "Consc_noasses"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "3")] = "Naive_Base"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "4")] = "Consc_Base"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "5")] = "Naive_PSC"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "6")] = "Consc_PSC"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "7b")] = "Naive_1500"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "8b")] = "Consc_1500"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "9")] = "Naive_COR"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "10")] = "Consc_COR"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "11")] = "Naive_poor"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "12")] = "Consc_poor"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "13")] = "Naive_more"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "14")] = "Consc_more"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "13a")] = "Naive_1500_flex"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "14a")] = "Consc_1500_flex"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "15")] = "Naive_less"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "16")] = "Consc_less"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "17")] = "Naive_rec_up"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "18")] = "Consc_rec_up"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "19")] = "Naive_rec_down"
			SSBs$ScenarioName[which(SSBs$ScenarioName == "20")] = "Consc_rec_down"

		
			SSBs$ScenarioName <- factor(SSBs$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex", "Naive_COR", "Consc_COR","Naive_more", "Consc_more", "Naive_less", "Consc_less","Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
		
			SSBs$COL 			<- as.numeric(SSBs$Scenario)
			SSBs$COL 			<- replace(SSBs$COL, which(SSBs$COL%in%seq(1,15,by=2)), 1)
			SSBs$COL 			<- replace(SSBs$COL, which(SSBs$COL%in%seq(2,15,by=2)), 2)
			SSBs$COL 			<- as.factor(SSBs$COL)
				
		# Subset some results
			set.seed(777)
			set.seed(777)
			dat.new <- c()
			dat.new.ts <- c()
			for (i in 1:length(scenarios))
			{
				dat <- c()
				# qwe <- c(unlist(Simul_keep[(i-1)*2+1]),unlist(Simul_keep[(i-1)*2+2]))[duplicated(c(unlist(Simul_keep[(i-1)*2+1]),unlist(Simul_keep[(i-1)*2+2])))]
				qwe <- c(unlist(Simul_keep[i]))
				qwe <- sort(qwe)
				qwe <- qwe[1:min(50,length(qwe))]
				dat <- subset(SSBs, subset=c(Scenario %in% scenarios[i] & Sim %in% qwe))
				dat.new <- rbind(dat.new, dat)
			}
			dat.new$Scenario <- factor(dat.new$Scenario, levels=scenarios)		
			dat.new$ScenarioName <- as.character(dat.new$Scenario)		
			dat.new$ScenarioName[which(dat.new$ScenarioName == "1")] = "Naive_noasses"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "2")] = "Consc_noasses"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "3")] = "Naive_Base"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "4")] = "Consc_Base"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "5")] = "Naive_PSC"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "6")] = "Consc_PSC"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "7b")] = "Naive_1500"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "8b")] = "Consc_1500"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "9")] = "Naive_COR"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "10")] = "Consc_COR"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "11")] = "Naive_poor"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "12")] = "Consc_poor"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "13")] = "Naive_more"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "14")] = "Consc_more"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "13a")] = "Naive_1500_flex"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "14a")] = "Consc_1500_flex"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "15")] = "Naive_less"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "16")] = "Consc_less"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "17")] = "Naive_rec_up"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "18")] = "Consc_rec_up"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "19")] = "Naive_rec_down"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "20")] = "Consc_rec_down"

			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,17:20))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,13:16))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_more", "Consc_more", "Naive_less", "Consc_less"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,11:12,1:2))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_poor", "Consc_poor", "Naive_noasses", "Consc_noasses"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:6,"7b","8b","13a","14a"))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_COR", "Consc_COR"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10,17:20))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_COR", "Consc_COR", "Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			
			# Choose which data to use for the final graph. All results or just the ones that were common to both
			SSBs_plot1 <- dat.new #SSBs
			SSBs_plot <- as.data.table(SSBs_plot1)			

		# Plot by species for each trajectory
			SSBs_plot2 <- subset(SSBs_plot1, subset=c(variable == "SSB"))
			SSBs_plot2$SSB <- SSBs_plot2$SSB/1e6
			SSBs_plot2$ID <- "Conscientious"
			SSBs_plot2$ID <- replace(SSBs_plot2$ID, grep("Naive", SSBs_plot2$ScenarioName), "Naive")
			SSBs_plot2$ID <- factor(SSBs_plot2$ID, levels=c("Naive", "Conscientious"))
			SSBs_plot2$SCN <- substring(SSBs_plot2$ScenarioName, 7)
			SSBs_plot2$SCN <- factor(SSBs_plot2$SCN, levels=c("Base", "PSC", "1500", "1500_flex", "COR", "rec_up", "rec_down", "more", "less", "poor", "noassess"))
			# library(plyr)
			# SSBs_plot2$SCN <- mapvalues(SSBs_plot2$SCN, from = c("Base", "PSC", "1500", "1500_flex", "COR", "rec_up", "rec_down", "more", "less", "poor", "noassess"), to = c("Base", "Min(PSC limit)", "1500t", "1500t+flex", "Correlated recruitment", "Increased recruitment", "Decreased recruitment", "More flexible", "Less flexible", "Data poor", "More accurate assessment"))	
		
			plot_ssb <- ggplot(SSBs_plot2, aes(x=Year,y=SSB)) + geom_line(aes(x = Year, y = SSB, group = Sim, colour=ID), alpha = 1/2, size = 1/2)+ facet_grid(Species ~ ScenarioName, drop = FALSE, scales = "free") +  theme_bw() + labs(y="SSB (1000t)")
			plot_ssb + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.4)))+labs(x="Scenario") + stat_smooth(color="black")+ scale_colour_manual(values=c(grey(0.7),grey(0.35)))
		
			ggsave(filename=paste0(Figures_save, "/SSB", titre, Yr_end, ".png"), width=45, height=18, units="cm", dpi=300)			
			
	### Catch 
			head(CATCHES)
			CATCHES <- melt(CATCHES, .(Sim,Year,Scenario,Species), value.name = "Catch")
			CATCHES$ID <- paste0(CATCHES$Species, CATCHES$variable)
			CATCHES$ID <- replace(CATCHES$ID, which(CATCHES$ID == "CodTotal_catch"), "Total_catch")
			CATCHES$ID <- replace(CATCHES$ID, which(CATCHES$ID == "PollockTotal_catch"), "Total_catch")
			CATCHES$ID <- replace(CATCHES$ID, which(CATCHES$ID == "YellowfinTotal_catch"), "Total_catch")
			CATCHES$ID <- as.factor(CATCHES$ID)
			CATCHES$ID <- factor(CATCHES$ID, labels=c("Cod","Pollock","Total_catch","Yellowfin"))
			CATCHES$ID <- factor(CATCHES$ID, levels=c("Cod","Pollock","Yellowfin","Total_catch"))

			CATCHES$Scenario <- as.factor(CATCHES$Scenario)
			CATCHES$ScenarioName <- as.character(CATCHES$Scenario)

			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "1")] = "Naive_noasses"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "2")] = "Consc_noasses"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "3")] = "Naive_Base"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "4")] = "Consc_Base"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "5")] = "Naive_PSC"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "6")] = "Consc_PSC"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "7b")] = "Naive_1500"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "8b")] = "Consc_1500"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "9")] = "Naive_COR"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "10")] = "Consc_COR"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "11")] = "Naive_poor"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "12")] = "Consc_poor"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "13")] = "Naive_more"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "14")] = "Consc_more"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "13a")] = "Naive_1500_flex"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "14a")] = "Consc_1500_flex"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "15")] = "Naive_less"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "16")] = "Consc_less"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "17")] = "Naive_rec_up"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "18")] = "Consc_rec_up"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "19")] = "Naive_rec_down"
			CATCHES$ScenarioName[which(CATCHES$ScenarioName == "20")] = "Consc_rec_down"
		
			CATCHES$ScenarioName <- factor(CATCHES$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex", "Naive_COR", "Consc_COR","Naive_more", "Consc_more", "Naive_less", "Consc_less","Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
		
			CATCHES$Species = as.factor(CATCHES$ID)
			CATCHES$COL 			<- as.numeric(CATCHES$Scenario)
			CATCHES$COL 			<- replace(CATCHES$COL, which(CATCHES$COL%in%seq(1,15,by=2)), 1)
			CATCHES$COL 			<- replace(CATCHES$COL, which(CATCHES$COL%in%seq(2,15,by=2)), 2)
			CATCHES$COL 			<- as.factor(CATCHES$COL)
				
		# Subset some results
			set.seed(777)
			set.seed(777)
			dat.new <- c()
			dat.new.ts <- c()
			for (i in 1:length(scenarios))
			{
				dat <- c()
				# qwe <- c(unlist(Simul_keep[(i-1)*2+1]),unlist(Simul_keep[(i-1)*2+2]))[duplicated(c(unlist(Simul_keep[(i-1)*2+1]),unlist(Simul_keep[(i-1)*2+2])))]
				qwe <- c(unlist(Simul_keep[i]))
				qwe <- sort(qwe)
				qwe <- qwe[1:min(50,length(qwe))]
				dat <- subset(CATCHES, subset=c(Scenario %in% scenarios[i] & Sim %in% qwe))
				dat.new <- rbind(dat.new, dat)
			}
			dat.new$Scenario <- factor(dat.new$Scenario, levels=scenarios)		
			dat.new$ScenarioName <- as.character(dat.new$Scenario)		
			dat.new$ScenarioName[which(dat.new$ScenarioName == "1")] = "Naive_noasses"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "2")] = "Consc_noasses"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "3")] = "Naive_Base"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "4")] = "Consc_Base"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "5")] = "Naive_PSC"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "6")] = "Consc_PSC"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "7b")] = "Naive_1500"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "8b")] = "Consc_1500"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "9")] = "Naive_COR"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "10")] = "Consc_COR"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "11")] = "Naive_poor"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "12")] = "Consc_poor"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "13")] = "Naive_more"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "14")] = "Consc_more"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "13a")] = "Naive_1500_flex"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "14a")] = "Consc_1500_flex"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "15")] = "Naive_less"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "16")] = "Consc_less"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "17")] = "Naive_rec_up"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "18")] = "Consc_rec_up"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "19")] = "Naive_rec_down"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "20")] = "Consc_rec_down"

			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,17:20))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,13:16))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_more", "Consc_more", "Naive_less", "Consc_less"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,11:12,1:2))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_poor", "Consc_poor", "Naive_noasses", "Consc_noasses"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:6,"7b","8b","13a","14a"))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_COR", "Consc_COR"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10,17:20))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_COR", "Consc_COR", "Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			
			# Choose which data to use for the final graph. All results or just the ones that were common to both
			CATCHES_plot1 <- dat.new #CATCHES
			CATCHES_plot <- as.data.table(CATCHES_plot1)		

		# Plot by species for each trajectory
			plot_catch <- ggplot(CATCHES_plot1, aes(x=Year,y=Catch/1e6)) + geom_line(aes(x = Year, y = Catch/1e6, group = Sim, fill=Sim), alpha = 1/2, size = 1/2)+ facet_grid(Species ~ ScenarioName, drop = FALSE, scales = "free") +  theme_bw() +coord_cartesian(ylim=c(0,1750)) + labs(y="Catch (1000t)") 
			plot_catch + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.4)))+labs(x="Scenario") + scale_color_grey(start=0.6, end=0.95)
		
		# Plot of total catch by year
			# plot_catch <- ggplot(subset(CATCHES_plot, subset=c(Species == "Total_catch"))) + geom_boxplot(aes(x=ScenarioName,y=Catch/1e6, fill=COL))+ facet_grid(.~Species) + theme_bw() + labs(y="Catch (1000t)") # +coord_cartesian(ylim=c(1000,2000)) + facet_grid(Species~., drop = FALSE, scales = "free") 
			# plot_catch + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=1), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.4)))+labs(x="Scenario") + scale_fill_brewer(palette="Greys")
			# ggsave(filename=paste0(Figures_save, "/Catch_history_", titre, Yr_end, ".png"), width=35, height=18, units="cm", dpi=200)			
			datdat <- ddply(CATCHES_plot, c("ScenarioName", "Species", "Sim"), summarize, .drop=FALSE, Catch = mean(Catch, na.rm=T))		
			datdat$COL 			<- as.numeric(as.factor(datdat$ScenarioName))
			datdat$COL 			<- replace(datdat$COL, which(datdat$COL%in%seq(1,15,by=2)), 1)
			datdat$COL 			<- replace(datdat$COL, which(datdat$COL%in%seq(2,15,by=2)), 2)
			datdat$COL 			<- as.factor(datdat$COL)
			datdat <- as.data.table(datdat)
			plot_catch <- ggplot(datdat) + geom_boxplot(aes(x=ScenarioName,y=Catch/1e6, fill=COL))+  theme_bw() +coord_cartesian(ylim=c(0,1750)) + labs(y="Catch (1000t)") + facet_grid(.~Species, drop = FALSE, scales = "free") 
			plot_catch + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.4)))+labs(x="Scenario") + scale_fill_brewer(palette="Greys")
			ggsave(filename=paste0(Figures_save, "/Avg_catch_", titre, Yr_end, ".png"), width=35, height=18, units="cm", dpi=200)			
		# Plot of CV of catch within the 15 year period
			datdat <- ddply(CATCHES_plot, c("ScenarioName", "Species", "Sim"), summarize, .drop=FALSE, CV.Catch = sd(Catch, na.rm=T)/mean(Catch, na.rm=T))		
			datdat$COL 			<- as.numeric(as.factor(datdat$ScenarioName))
			datdat$COL 			<- replace(datdat$COL, which(datdat$COL%in%seq(1,15,by=2)), 1)
			datdat$COL 			<- replace(datdat$COL, which(datdat$COL%in%seq(2,15,by=2)), 2)
			datdat$COL 			<- as.factor(datdat$COL)
			datdat <- as.data.table(datdat)
			plot_catch <- ggplot(datdat) + geom_boxplot(aes(x=ScenarioName,y=CV.Catch, fill=COL))+  theme_bw() +coord_cartesian(ylim=c(0,0.65)) + labs(y=expression(CV[catch])) + facet_grid(.~Species, drop = FALSE, scales = "free") 
			plot_catch + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.4))) +labs(x="Scenario") + scale_fill_brewer(palette="Greys")
			ggsave(filename=paste0(Figures_save, "/CV_catch_", titre, Yr_end, ".png"), width=35, height=18, units="cm", dpi=200)			
			
	### Catch quota balancing
			head(CATCH_QUOTA_RATIO)
			CATCH_QUOTA_RATIO <- melt(CATCH_QUOTA_RATIO, .(Sim,Year,Scenario), value.name = "Catchquota")
			CATCH_QUOTA_RATIO$ID <- paste0(CATCH_QUOTA_RATIO$Species, CATCH_QUOTA_RATIO$variable)
			CATCH_QUOTA_RATIO$ID <- as.factor(CATCH_QUOTA_RATIO$ID)
			CATCH_QUOTA_RATIO$ID <- factor(CATCH_QUOTA_RATIO$ID, labels=c("Cod","Pollock","Total_catch","Yellowfin"))
			CATCH_QUOTA_RATIO$ID <- factor(CATCH_QUOTA_RATIO$ID, levels=c("Cod","Pollock","Yellowfin","Total_catch"))
			# CATCH_QUOTA_RATIO$Scenario <- replace(CATCH_QUOTA_RATIO$Scenario, CATCH_QUOTA_RATIO$Scenario=="1b",1)
			# CATCH_QUOTA_RATIO$Scenario <- replace(CATCH_QUOTA_RATIO$Scenario, CATCH_QUOTA_RATIO$Scenario=="2b",2)
			# CATCH_QUOTA_RATIO$Scenario <- replace(CATCH_QUOTA_RATIO$Scenario, CATCH_QUOTA_RATIO$Scenario=="3b",3)
			# CATCH_QUOTA_RATIO$Scenario <- replace(CATCH_QUOTA_RATIO$Scenario, CATCH_QUOTA_RATIO$Scenario=="4b",4)

			CATCH_QUOTA_RATIO$Scenario <- as.factor(CATCH_QUOTA_RATIO$Scenario)
			CATCH_QUOTA_RATIO$ScenarioName <- as.character(CATCH_QUOTA_RATIO$Scenario)

			CATCHES$Scenario <- as.factor(CATCHES$Scenario)
			CATCHES$ScenarioName <- as.factor(CATCHES$Scenario)

			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "1")] = "Naive_noasses"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "2")] = "Consc_noasses"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "3")] = "Naive_Base"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "4")] = "Consc_Base"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "5")] = "Naive_PSC"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "6")] = "Consc_PSC"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "7b")] = "Naive_1500"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "8b")] = "Consc_1500"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "9")] = "Naive_COR"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "10")] = "Consc_COR"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "11")] = "Naive_poor"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "12")] = "Consc_poor"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "13")] = "Naive_more"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "14")] = "Consc_more"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "13a")] = "Naive_1500_flex"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "14a")] = "Consc_1500_flex"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "15")] = "Naive_less"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "16")] = "Consc_less"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "17")] = "Naive_rec_up"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "18")] = "Consc_rec_up"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "19")] = "Naive_rec_down"
			CATCH_QUOTA_RATIO$ScenarioName[which(CATCH_QUOTA_RATIO$ScenarioName == "20")] = "Consc_rec_down"

			CATCH_QUOTA_RATIO$ScenarioName <- factor(CATCH_QUOTA_RATIO$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex", "Naive_COR", "Consc_COR","Naive_more", "Consc_more", "Naive_less", "Consc_less","Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
		
			CATCH_QUOTA_RATIO$Species = as.factor(CATCH_QUOTA_RATIO$ID)
			CATCH_QUOTA_RATIO$COL 			<- as.numeric(CATCH_QUOTA_RATIO$Scenario)
			CATCH_QUOTA_RATIO$COL 			<- replace(CATCH_QUOTA_RATIO$COL, which(CATCH_QUOTA_RATIO$COL%in%seq(1,15,by=2)), 1)
			CATCH_QUOTA_RATIO$COL 			<- replace(CATCH_QUOTA_RATIO$COL, which(CATCH_QUOTA_RATIO$COL%in%seq(2,15,by=2)), 2)
			CATCH_QUOTA_RATIO$COL 			<- as.factor(CATCH_QUOTA_RATIO$COL)
			
		# Subset some results
			set.seed(777)
			dat.new <- c()
			for (i in 1:length(scenarios))
			{
				dat <- c()
				# qwe <- c(unlist(Simul_keep[(i-1)*2+1]),unlist(Simul_keep[(i-1)*2+2]))[duplicated(c(unlist(Simul_keep[(i-1)*2+1]),unlist(Simul_keep[(i-1)*2+2])))]
				qwe <- c(unlist(Simul_keep[i]))
				qwe <- sort(qwe)
				qwe <- qwe[1:min(50,length(qwe))]
				dat <- subset(CATCH_QUOTA_RATIO, subset=c(Scenario %in% scenarios[i] & Sim %in% qwe))
				dat.new <- rbind(dat.new, dat)
			}
			dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex", "Naive_COR", "Consc_COR","Naive_more", "Consc_more", "Naive_less", "Consc_less","Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			dat.new$Scenario <- factor(dat.new$Scenario, levels=scenarios)		
			dat.new$ScenarioName <- as.character(dat.new$Scenario)		
			dat.new$ScenarioName[which(dat.new$ScenarioName == "1")] = "Naive_noasses"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "2")] = "Consc_noasses"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "3")] = "Naive_Base"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "4")] = "Consc_Base"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "5")] = "Naive_PSC"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "6")] = "Consc_PSC"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "7b")] = "Naive_1500"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "8b")] = "Consc_1500"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "9")] = "Naive_COR"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "10")] = "Consc_COR"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "11")] = "Naive_poor"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "12")] = "Consc_poor"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "13")] = "Naive_more"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "14")] = "Consc_more"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "13a")] = "Naive_1500_flex"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "14a")] = "Consc_1500_flex"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "15")] = "Naive_less"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "16")] = "Consc_less"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "17")] = "Naive_rec_up"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "18")] = "Consc_rec_up"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "19")] = "Naive_rec_down"
			dat.new$ScenarioName[which(dat.new$ScenarioName == "20")] = "Consc_rec_down"

			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,17:20))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,13:16))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_more", "Consc_more", "Naive_less", "Consc_less"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,11:12,1:2))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_poor", "Consc_poor", "Naive_noasses", "Consc_noasses"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:6,"7b","8b","13a","14a"))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_PSC", "Consc_PSC", "Naive_1500", "Consc_1500", "Naive_1500_flex", "Consc_1500_flex"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_COR", "Consc_COR"))
			if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10,17:20))))) dat.new$ScenarioName <- factor(dat.new$ScenarioName, levels=c("Naive_Base", "Consc_Base", "Naive_COR", "Consc_COR", "Naive_rec_up", "Consc_rec_up", "Naive_rec_down", "Consc_rec_down"))
			
		
		# Choose which data to use for the final graph. All results or just the ones that were common to both
			CATCH_QUOTA_RATIO_plot1 <- dat.new #CATCH_QUOTA_RATIO
			CATCH_QUOTA_RATIO_plot <- as.data.table(CATCH_QUOTA_RATIO_plot1)
						
			datdat <- ddply(CATCH_QUOTA_RATIO_plot, c("ScenarioName", "Species", "Sim"), summarize, .drop=FALSE, Catchquota = mean(Catchquota, na.rm=T))		
			datdat$COL 			<- as.numeric(as.factor(datdat$ScenarioName))
			datdat$COL 			<- replace(datdat$COL, which(datdat$COL%in%seq(1,15,by=2)), 1)
			datdat$COL 			<- replace(datdat$COL, which(datdat$COL%in%seq(2,15,by=2)), 2)
			datdat$COL 			<- as.factor(datdat$COL)
			datdat <- as.data.table(datdat)

			plot_catch_quota <- ggplot(datdat) + geom_boxplot(aes(x=ScenarioName,y=Catchquota, fill=COL))+ facet_grid(.~Species) + theme_bw() +coord_cartesian(ylim=c(0,1)) + labs(y="Catch to quota ratio") #  + facet_grid(Species~., drop = FALSE, scales = "free") 
			plot_catch_quota + theme(axis.title = element_text(size = rel(1.5)), axis.text = element_text(size = rel(1.3)), axis.text.x = element_text(angle=90, vjust=0.25), legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.4)), legend.position='none', strip.text = element_text(size = rel(1.4)))+labs(x="Scenario") + scale_fill_manual(values=c(grey(0.85), grey(0.5)))# scale_fill_brewer(palette="Greys")
			ggsave(filename=paste0(Figures_save, "/Catch_quota_balance_", titre, Yr_end, ".png"), width=35, height=18, units="cm", dpi=200)			

		### Another type of plot
		Data=CATCHES_plot1; Var="Catch"; Summary_var = "MEAN"; plot_name="Summary_results"			
		
		Time_series_plot <- function(Data=CATCHES_plot, Var="Catch", Summary_var = "MEAN", plot_name="Summary_results")
		{
			cols <- RColorBrewer::brewer.pal(8, "Greys")

			if (Var=="Catch")
			{
				datdat <- ddply(Data, c("Scenario", "Species", "Year"), summarize, .drop=FALSE,
					q10 = quantile(Catch, probs = 0.05)/10^6,
					q25 = quantile(Catch, probs = 0.25)/10^6,
					q50 = quantile(Catch, probs = 0.50)/10^6,
					avg = mean(Catch)/10^6,
					q75 = quantile(Catch, probs = 0.75)/10^6,
					q90 = quantile(Catch, probs = 0.95)/10^6
				)
				datdat$MEDIAN <- rep(c(t(tapply(Data$Catch, list(Data$Scenario, Data$Species), median))), each=(Yr_end-51))/10^6
				datdat$MEAN <- rep(c(t(tapply(Data$Catch, list(Data$Scenario, Data$Species), mean))), each=(Yr_end-51))/10^6
				if(Summary_var=="MEDIAN") datdat$Summary <- datdat$MEDIAN
				if(Summary_var=="MEAN") datdat$Summary <- datdat$MEAN
				Catch_sd <- ddply(Data, c("Scenario", "Species"), summarize, SD=sd(Catch/10^6))
				Catch_cv <- ddply(Data, c("Scenario", "Species"), summarize, CV=sd(Catch/10^6)/mean(Catch/10^6))
			}
			
			if (Var=="Catchquota")
			{
				datdat <- ddply(Data, c("Scenario", "Species", "Year"), summarize, .drop=FALSE,
					q10 = quantile(Catchquota, probs = 0.05)/10^3,
					q25 = quantile(Catchquota, probs = 0.25)/10^3,
					q50 = quantile(Catchquota, probs = 0.50)/10^3,
					avg = mean(Catchquota)/10^3,
					q75 = quantile(Catchquota, probs = 0.75)/10^3,
					q90 = quantile(Catchquota, probs = 0.95)/10^3
				)
				datdat$MEDIAN <- rep(c(t(tapply(Data$Catchquota, list(Data$Scenario, Data$Species), median))), each=(Yr_end-51))/10^3
				datdat$MEAN <- rep(c(t(tapply(Data$Catchquota, list(Data$Scenario, Data$Species), mean))), each=(Yr_end-51))/10^3
				if(Summary_var=="MEDIAN") datdat$Summary <- datdat$MEDIAN
				if(Summary_var=="MEAN") datdat$Summary <- datdat$MEAN
				Catch_sd <- ddply(Data, c("Scenario", "Species"), summarize, SD=sd(Catchquota/10^3))
				Catch_cv <- ddply(Data, c("Scenario", "Species"), summarize, CV=sd(Catchquota/10^3)/mean(Catchquota/10^3))
			}
			
						
			# Practice with ggplot
				# trial1 <- ggplot(datdat) + geom_ribbon(aes(Year, ymax = q10, ymin = q90), fill = cols[3]) +
				# geom_ribbon(aes(Year, ymax = q25, ymin = q75), fill = cols[4]) +
				# geom_line(aes(Year, q50), colour = cols[8]) +
				# geom_line(aes(Year, MEDIAN), linetype=3, size=1.3, colour="red") + 
				# facet_grid(Species~Scenario, scale="free_y") + 
				# theme_bw() +
				# theme(panel.grid.major = element_blank(),
					# panel.grid.minor = element_blank(),
					# strip.background = element_rect(fill = NA, linetype = 0),
					# axis.text = element_text(colour = "grey50"),
					# axis.title = element_text(colour = "grey30"),
					# axis.ticks = element_line(colour = "grey50"),
					# strip.text = element_text(colour = "grey30")
					# ) + labs(x = "", y = "", title="") 
				
				# trial2 <- ggplot(datdat[datdat$Species == "Pollock",]) + geom_ribbon(aes(Year, ymax = q10, ymin = q90), fill = cols[3]) +
				# geom_ribbon(aes(Year, ymax = q25, ymin = q75), fill = cols[4]) +
				# geom_line(aes(Year, q50), colour = cols[8]) +
				# geom_line(aes(Year, MEDIAN), linetype=3, size=1.3, colour="red") + 
				# facet_grid(Species~Scenario) + coord_cartesian(ylim=c(0,1800)) + 
				# theme_bw() +
				# theme(panel.grid.major = element_blank(),
					# panel.grid.minor = element_blank(),
					# strip.background = element_rect(fill = NA, linetype = 0),
					# axis.text = element_text(colour = "grey50"),
					# axis.title = element_text(colour = "grey30"),
					# axis.ticks = element_line(colour = "grey50"),
					# strip.text.x = element_blank(),
					# strip.text.y = element_text(colour = "grey30")
					# ) + labs(x = "", y = "", title="") 

				# library(gridExtra)
				# gA <- ggplotGrob(trial1)
				# gB <- ggplotGrob(trial2)
				# maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
				# gA$widths[2:5] <- as.list(maxWidth)
				# gB$widths[2:5] <- as.list(maxWidth)

				# p4 <- arrangeGrob(gA, gB, nrow = 2, heights = c(1,1))
				# p4
				
				# multiplot(trial1,trial2, cols=1)	
					
			# Using my own method
				Species = c("Cod","Pollock","Yellowfin","Total_catch")
				if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9:10))))) 	Names <- c("Base", "Correlated recruitment")
				if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,13:16)))))		Names <- c("Base", "More flexible", "Less flexible")
				if (isTRUE(all.equal(as.character(scenarios),as.character(c(3:6,"7b","8b")))))		Names <- c("Base", "Min(PSC limit)", "1500t")
				if (isTRUE(all.equal(as.character(scenarios),as.character(c(3:8)))))		Names <- c("Base", "Min(PSC limit)", "Min(bycatch mortality)")
				if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,17:18)))))		Names <- c("Base", "Goes up")
				if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,17:20)))))		Names <- c("Base", "Increased recruitment", "Decreased recruitment")
				if (isTRUE(all.equal(as.character(scenarios), as.character(c(3:4,9,10,17:20)))))		Names <- c("Base", "Correlated recruitment", "Increased recruitment", "Decreased recruitment")
				if (isTRUE(all.equal(as.character(scenarios), as.character((c(3:4,11:12,1:2))))))		Names <- c("Base", "Data poor", "More accurate assessment")
				if (isTRUE(all.equal(as.character(scenarios), as.character((c(3:4,"7b","8b","13a","14a"))))))		Names <- c("Base", "1500t", "1500t+flex")
				if (isTRUE(all.equal(as.character(scenarios), as.character((c(3:6,"7b","8b","13a","14a"))))))		Names <- c("Base", "Min(PSC limit)", "1500t", "1500t+flex")
					
				if (Var=="Catch") ylims <- c(675, 2070, 270, 2250)
				if (Var=="Catchquota") ylims <- c(150, 600, 150, 600)
				orange_gradient <- grey(seq(0.95,0.75,length.out=9)) #RColorBrewer::brewer.pal(8, "YlOrRd")
				purple_gradient <- grey(seq(0.8,0.4,length.out=9)) #RColorBrewer::brewer.pal(8, "Purples")
				
				Fig_purpose <- "PPT" #  or "MS"
				CV_or_SD <- "SD"
				if(length(grep("bycatch", Names))==0) Bycatch <- "FALSE"
				if(length(grep("bycatch", Names))>0) Bycatch <- "TRUE"
				
				if(length(Names)==2)
				{
					if (Fig_purpose == "PPT") png(filename = paste0(Figures_save, "/", plot_name, titre, Yr_end, ".png"), width = 275, height=180, units="mm", res=450)
					if (Fig_purpose == "MS") png(filename = paste0(Figures_save, "/", plot_name, titre,Yr_end, ".png"), width = 275, height=180, units="mm", res=450)
				}
				if(length(Names)==3)
				{
					if (Fig_purpose == "PPT") png(filename = paste0(Figures_save, "/", plot_name, titre, Yr_end, ".png"), width = 275, height=180, units="mm", res=450)
					if (Fig_purpose == "MS") png(filename = paste0(Figures_save, "/", plot_name, titre,Yr_end, ".png"), width = 275, height=180, units="mm", res=450)
				}
				if(length(Names)>=4)
				{
					if (Fig_purpose == "PPT") png(filename = paste0(Figures_save, "/", plot_name, titre, Yr_end, ".png"), width = 300, height=180, units="mm", res=450)
					if (Fig_purpose == "MS") png(filename = paste0(Figures_save, "/", plot_name, titre,Yr_end, ".png"), width = 300, height=180, units="mm", res=450)
				}
				
				if(length(Names)==5) { 
				par(mfrow=c(4,10), mar=c(0,0,1,0), oma=c(5,6,5,1), cex.axis=1.4)
				par(mar=c(0,0,1.5,0), oma=c(5.5,6,5,4), cex.axis=1.3)
				nf <- layout(matrix(c(1,2,41,3,4,42,5,6,43,7,8,44,9,10,
									  11,12,41,13,14,42,15,16,43,17,18,44,19,20,
									  21,22,41,23,24,42,25,26,43,27,28,44,29,30,
									  31,32,41,33,34,42,35,36,43,37,38,44,39,40), nrow=4, ncol=14,byrow=T), width=c(1,1,0.1,1,1,0.1,1,1,0.1,1,1,0.1,1,1)) }
				if(length(Names)==4) { 
				par(mfrow=c(4,8), mar=c(0,0,1,0), oma=c(5,6,5,1), cex.axis=1.4)
				par(mar=c(0,0,1.5,0), oma=c(5.5,6,6.5,4), cex.axis=1.3)
				nf <- layout(matrix(c(1,2,33,3,4,34,5,6,35,7,8,
									  9,10,33,11,12,34,13,14,35,15,16,
									  17,18,33,19,20,34,21,22,35,23,24,
									  25,26,33,27,28,34,29,30,35,31,32), nrow=4, ncol=11,byrow=T), width=c(1,1,0.1,1,1,0.1,1,1,0.1,1,1)) }
				if(length(Names)==3) { 
				par(mfrow=c(4,6), mar=c(0,0,1,0), oma=c(5,6,5,1), cex.axis=1.4)
				par(mar=c(0,0,1.5,0), oma=c(5.5,6,5,4), cex.axis=1.3)
				nf <- layout(matrix(c(1,2,25,3,4,26,5,6,
									  7,8,25,9,10,26,11,12,
									  13,14,25,15,16,26,17,18,
									  19,20,25,21,22,26,23,24), nrow=4, ncol=8,byrow=T), width=c(1,1,0.1,1,1,0.1,1,1)) }
				if(length(Names)==2) { 
				par(mfrow=c(4,4), mar=c(0,0,1,0), oma=c(5,6,5,1), cex.axis=1.4)
				par(mar=c(0,0,1.5,0), oma=c(5.5,6,5,4), cex.axis=1.3)
				nf <- layout(matrix(c(1,2,17,3,4,
									  5,6,17,7,8,
									  9,10,17,11,12,
									  13,14,17,15,16), nrow=4, ncol=5,byrow=T), width=c(1,1,0.1,1,1)) }
				SCN <- levels(Data$Scenario)

				for (spec in Species)
				{
					for (scn in seq_along(SCN))
					{
						DD <- subset(datdat, subset=(Scenario == SCN[scn] & Species == spec))
						OO1 <- subset(OVERFISHED_plot, subset=(Scenario == SCN[scn] & Species == spec))
						OO <- subset(OVERFISHED_ts_plot, subset=(Scenario == SCN[scn] & Species == spec))

						if (all(is.na(DD$avg))) plot(seq(52,Yr_end),seq(52,Yr_end),type="n", axes=F, bty="n", xlab="", ylab="")
						if (!all(is.na(DD$avg))) 
						{
							plot(DD$Year, DD$q50, type="n", col=1, lwd=2, ylim=c(0,ylims[which(spec==Species)]), xlab="", xaxt="n", yaxt="n", bty="n")

							if (SCN[scn] == 3) 
							{
								if (Var=="Catch")
								{
									if(spec=="Cod") axis(2, at=c(0,250,500))
									if(spec=="Pollock") axis(2, at=c(0,750,1500))
									if(spec=="Yellowfin") axis(2, at=c(0,100,200))
									if(spec=="Total_catch") axis(2, at=c(0,850,1700))						
								}
								if (Var=="Catchquota")
								{
									if(spec=="Cod") axis(2, at=c(0,60,120))
									if(spec=="Pollock") axis(2, at=c(0,250,500))
									if(spec=="Yellowfin") axis(2, at=c(0,60,120))
									if(spec=="Total_catch") axis(2, at=c(0,250,500))						
								}
							}	
							# if (Yr_end == End_year) if (spec == "Total_catch") axis(1, at=c(52,61,71,End_year), labels=c(1,10,20,30))
							# box(bty="L")
							# if (SCN[scn] %in% seq(1,18,by=2)) box(col="orange", lwd=3)
							# if (SCN[scn] %in% seq(2,18,by=2)) box(col="purple", lwd=3)
							if (length(unlist(sapply(seq(1,50,by=2), function(x) grep(x, SCN[scn]))))>=1) 
							{
								# if (CI==90) polygon(c(DD$Year, rev(DD$Year)), c(DD$q10,rev(DD$q90)), col=orange_gradient[2], border=NA) 
								# if (CI==75) polygon(c(DD$Year, rev(DD$Year)), c(DD$q25,rev(DD$q75)), col=orange_gradient[5], border=NA) 
								# plot the individual trajectories
								fake_dat <- subset(Data, subset=(Scenario == SCN[scn] & Species == spec))
								for (i in seq_along(unique(fake_dat$Sim)))
								{
									lines(52:65, (fake_dat[which(fake_dat$Sim == unique(fake_dat$Sim)[i]),'Catch']/10^6)[1:14], col=orange_gradient[6], lwd=1)
								}
								if(Summary_var=="MEDIAN") lines(DD$Year, DD$q50, col=orange_gradient[9], lwd=3)
								if(Summary_var=="MEAN") lines(DD$Year, DD$avg, col=orange_gradient[9], lwd=3)
							}
							if (length(unlist(sapply(seq(2,50,by=2), function(x) grep(x, SCN[scn]))))>=1)
							{
								# if (CI==90) polygon(c(DD$Year, rev(DD$Year)), c(DD$q10,rev(DD$q90)), col=purple_gradient[2], border=NA) 
								# if (CI==75) polygon(c(DD$Year, rev(DD$Year)), c(DD$q25,rev(DD$q75)), col=purple_gradient[5], border=NA) 
								# plot the individual trajectories
								fake_dat <- subset(Data, subset=(Scenario == SCN[scn] & Species == spec))
								for (i in seq_along(unique(fake_dat$Sim)))
								{
									lines(52:65, (fake_dat[which(fake_dat$Sim == unique(fake_dat$Sim)[i]),'Catch']/10^6)[1:14], col=purple_gradient[6], lwd=1)
								}
								if(Summary_var=="MEDIAN") lines(DD$Year, DD$q50, col=purple_gradient[9], lwd=3)						
								if(Summary_var=="MEAN") lines(DD$Year, DD$avg, col=purple_gradient[9], lwd=3)						
							}
							
							## add the median 
							# if (SCN[scn] %in% seq(1,18,by=2)) lines(DD$Year, DD$MEDIAN, col="blue", lwd=2, lty=2)
							# if (SCN[scn] %in% seq(1,18,by=2)) lines(DD$Year, DD$MEDIAN, col="blue", lwd=2, lty=2)
							# if (SCN[scn] %in% seq(2,18,by=2)) lines(DD$Year, DD$MEDIAN, col="red", lwd=2, lty=2)
							
							## add the Average Catch 
							if (Var=="Catch") theta <- ceiling(DD$Summary[1])
							if (Var=="Catchquota") theta <- round(DD$Summary[1],2)
							# text(75, 1.2*max(DD$q90), bquote(bar(Catch) == .(theta)))
							D1 <- subset(datdat, subset=(Species == spec))$Summary
							if (Fig_purpose == "PPT")
							{
								if (Var=="Catch") 
								{
									if (Summary_var=="MEDIAN") if(scn == 1 & spec=="Cod") text(45+0.24*(End_year-Yr_end), 1.15*ylims[which(spec==Species)], expression(paste(Med(Catch), "")), col=1, cex=1.5, font=2, xpd=NA, adj=c(0,0.5))
									if (Summary_var=="MEAN") if(scn == 1 & spec=="Cod") text(45+0.24*(End_year-Yr_end), 1.15*ylims[which(spec==Species)], expression(paste(bar(Catch), "")), col=1, cex=1.5, font=2, xpd=NA, adj=c(0,0.5))
									text(Yr_end, 1.15*ylims[which(spec==Species)], theta, col=grey(1.1*(max(D1,na.rm=T)-DD$Summary[1])/max(D1,na.rm=T)), cex=1.4, font=2, xpd=NA, adj=c(1,0.5))
								}
								if (Var=="Catchquota") 
								{
									if (Summary_var=="MEDIAN") if(scn == 1 & spec=="Cod") text(45+0.24*(End_year-Yr_end), 1.15*ylims[which(spec==Species)], expression(paste(Med(Unused_quota), "")), col=1, cex=1.5, font=2, xpd=NA, adj=c(0,0.5))
									if (Summary_var=="MEAN") if(scn == 1 & spec=="Cod") text(45+0.24*(End_year-Yr_end), 1.15*ylims[which(spec==Species)], expression(paste(bar(Unused_quota), "")), col=1, cex=1.5, font=2, xpd=NA, adj=c(0,0.5))
									text(Yr_end, 1.15*ylims[which(spec==Species)], theta, cex=1.4, font=2, xpd=NA, adj=c(1,0.5))						
								}
							}
							if (Fig_purpose == "MS")
							{
								if (Summary_var=="MEDIAN") if(scn == 1 & spec=="Cod") text(45, 1.15*ylims[which(spec==Species)], expression(paste(Med(Catch), "")), col=1, cex=1.3, font=2, xpd=NA, adj=c(0,0.5))
								if (Summary_var=="MEAN") if(scn == 1 & spec=="Cod") text(45, 1.15*ylims[which(spec==Species)], expression(paste(bar(Catch), "")), col=1, cex=1.3, font=2, xpd=NA, adj=c(0,0.5))
								if (Var=="Catch") text(End_year, 1.15*ylims[which(spec==Species)], theta, col=grey(1.2*(max(D1,na.rm=T)-DD$Summary[1])/max(D1,na.rm=T)+0.3), cex=1.2, font=2, xpd=NA, adj=c(1,0.5))
								if (Var=="Catchquota") text(End_year, 1.15*ylims[which(spec==Species)], theta, cex=1.4, font=2, xpd=NA, adj=c(1,0.5))
							}
							
							## Add a meaningfull title to the plot
							if (Fig_purpose == "PPT")
							{
								if(scn == 1 & spec=="Cod") 
								{
									# mtext(side=3, Scenario_name[scn], line=3.5)
									# lines(x=c(50,113), y=rep(1050,2), col=1, lwd=2, xpd=NA)
									text(x=Yr_end, y=1.53*ylims[1], Names[1], col=1, lwd=2, xpd=NA, adj=0.5, cex=1.8, font=2)
									text(x=Yr_end + (Yr_end-55)*2.35 + 7, y=1.53*ylims[1], Names[2], col=1, lwd=2, xpd=NA, adj=0.5, cex=1.8, font=2)
									text(x=Yr_end + 4.6*(Yr_end-52), y=1.53*ylims[1], Names[3], col=1, lwd=2, xpd=NA, adj=0.5, cex=1.8, font=2)	
									text(x=Yr_end + 6.8*(Yr_end-52)+1, y=1.53*ylims[1], Names[4], col=1, lwd=2, xpd=NA, adj=0.5, cex=1.8, font=2)	
									abline(v=Yr_end+(Yr_end-52)*1.195, xpd=NA, lty=2)
									if (length(Names)>2) abline(v=Yr_end+3*(Yr_end-52)*1.15, xpd=NA, lty=2)
									if (length(Names)>3) abline(v=Yr_end+5*(Yr_end-52)*1.145, xpd=NA, lty=2)
								}
								if(scn == 1 & spec=="Total_catch") 
								{
									# legend(x=50, y=-650, lty=1, lwd=7, col=c("orange", "purple"), legend=c("Naive", "Conscientous"), bty="n", xpd=NA, cex=1.7, horiz=TRUE)
									segments(x0=51+0.05*(Yr_end-51), x1=51+0.2*(Yr_end-51), y0=-1100, y1=-1100, lwd=7, col=grey(0.84), xpd=NA, cex=1.7)
									segments(x0=51+0.78*(Yr_end-51), x1=51+1/1.1*(Yr_end-51), y0=-1100, y1=-1100, lwd=7, col=grey(0.5), xpd=NA, cex=1.7)
									text(x=c(51+0.25*(Yr_end-51),51+1/1.02*(Yr_end-51)), y=c(-1100,-1100), lty=1, lwd=7, col=c(1,1), labels=c("Naive", "Conscientious"), xpd=NA, cex=1.7, adj=0)
								}
							}
							if (Fig_purpose == "MS")
							{
								if(scn == 1 & spec=="Cod") 
								{
									# mtext(side=3, Scenario_name[scn], line=3.5)
									# lines(x=c(50,113), y=rep(1050,2), col=1, lwd=2, xpd=NA)
									text(x=82, y=1.53*ylims[1], Names[1], col=1, lwd=2, xpd=NA, adj=0.5, cex=1.5, font=2)
									text(x=148, y=1.53*ylims[1], Names[2], col=1, lwd=2, xpd=NA, adj=0.5, cex=1.5, font=2)
									text(x=213, y=1.53*ylims[1], Names[3], col=1, lwd=2, xpd=NA, adj=0.5, cex=1.5, font=2)	
									text(x=278, y=1.63*ylims[1], Names[4], col=1, lwd=2, xpd=NA, adj=0.5, cex=1.8, font=2)	
									abline(v=115.5, xpd=NA, lty=2)
									if (length(Names)>2) abline(v=182, xpd=NA, lty=2)
									if (length(Names)>3) abline(v=248.5, xpd=NA, lty=2)
								}
								if(scn == 1 & spec=="Total_catch") 
								{
									# legend(x=50, y=-600, lty=1, lwd=7, col=c("orange", "purple"), legend=c("Naive", "Conscientous"), bty="n", xpd=NA, cex=1.5, horiz=TRUE)
									segments(x0=55, x1=58, y0=-1100, y1=-1100, lwd=7, col=grey(0.84), xpd=NA, cex=1.7)
									segments(x0=75, x1=78, y0=-1100, y1=-1100, lwd=7, col=grey(0.65), xpd=NA, cex=1.7)
									text(x=c(60,80), y=c(-1100,-1100), lty=1, lwd=7, col=c(1,1), labels=c("Naive", "Conscientious"), xpd=NA, cex=1.7, adj=0)
								}
							}						
							## Add the probability of overfishing for each time step
							if (Fig_purpose == "PPT")
							{
								if(CV_or_SD == "SD") if(scn == 1 & spec=="Cod") text(45+0.24*(End_year-Yr_end), 1.01*ylims[which(spec==Species)], bquote(Std[.(Var)]), col=1, cex=1.5, font=2, xpd=NA, adj=c(0,0.5))
								if(CV_or_SD == "CV") if(scn == 1 & spec=="Cod") text(45+0.24*(End_year-Yr_end), 1.01*ylims[which(spec==Species)], bquote(CV[.(Var)]), col=1, cex=1.5, font=2, xpd=NA, adj=c(0,0.5))
								if (Var=="Catch") if(scn == 1 & spec=="Cod") text(45+0.24*(End_year-Yr_end), 0.9*ylims[which(spec==Species)], expression(paste(P[overfished], " (%)")), col=1, cex=1.5, font=2, xpd=NA, adj=c(0,0.5))
								# OF_ts <- ddply(OO, .(Year), summarize, OF=round(sum(OF)/length(unique(OO$Sim))*100,0))
								# if (sum(OO1$OF)>0) text(OF_ts[OF_ts$OF>0,1], rep(c(-0.03*max(DD$q90),0.03*max(DD$q90)),15)[OF_ts$OF>0]+rep(-0.15*max(DD$q90),sum(OF_ts$OF>0)), OF_ts[OF_ts$OF>0,2], cex=0.7, font=3, xpd=NA)
								if(CV_or_SD == "CV") D2 <- Catch_cv[(Catch_cv$Species == spec),3]
								if(CV_or_SD == "SD") D2 <- Catch_sd[(Catch_sd$Species == spec),3]
								D3 <- ddply(subset(OVERFISHED_plot, subset=(Species == spec)), c("Scenario"), summarize, MEAN=mean(OF_freq))$MEAN
								D3b <- ddply(subset(OVERFISHED_plot, subset=(Species == spec)), c("Scenario"), summarize, Q25=quantile(OF_freq,0.25))$Q25
								D3c <- ddply(subset(OVERFISHED_plot, subset=(Species == spec)), c("Scenario"), summarize, Q75=quantile(OF_freq,0.75))$Q75
								
								if(CV_or_SD == "SD") text(Yr_end, 1.02*ylims[which(spec==Species)], round(Catch_sd[(Catch_sd$Scenario == scenarios[scn] & Catch_sd$Species == spec),3],0), col=grey(0.5*((max(D2)-Catch_sd[(Catch_sd$Scenario == scenarios[scn] & Catch_sd$Species == spec),3])/max(D2))+0.2), cex=1.5, font=2, xpd=NA, adj=c(1,0.5))
								if(CV_or_SD == "CV") text(Yr_end, 1.02*ylims[which(spec==Species)], round(Catch_cv[(Catch_cv$Scenario == scenarios[scn] & Catch_cv$Species == spec),3],2), col=grey(0.5*((max(D2)-Catch_cv[(Catch_cv$Scenario == scenarios[scn] & Catch_cv$Species == spec),3])/max(D2))+0.2), cex=1.5, font=2, xpd=NA, adj=c(1,0.5))
								if (Var=="Catch") if (length(D3)>0 & spec!="Total_catch") text(Yr_end, 0.9*ylims[which(spec==Species)], round(mean(OO1$OF_freq),0), col=grey(max(0.8,(max(D3)-mean(OO1$OF_freq))/max(max(D3),4))), cex=1.5, font=2, xpd=NA, adj=c(1,0.5))
							}
							if (Fig_purpose == "MS")
							{
								if(CV_or_SD == "SD") if(scn == 1 & spec=="Cod") text(45, 1.01*ylims[which(spec==Species)], bquote(Std[.(Var)]), col=1, cex=1.3, font=2, xpd=NA, adj=c(0,0.5))
								if(CV_or_SD == "CV") if(scn == 1 & spec=="Cod") text(45, 1.01*ylims[which(spec==Species)], bquote(CV[.(Var)]), col=1, cex=1.3, font=2, xpd=NA, adj=c(0,0.5))
								if (Var=="Catch") if(scn == 1 & spec=="Cod") text(45, 0.9*ylims[which(spec==Species)], expression(paste(P[overfished], " (%)")), col=1, cex=1.3, font=2, xpd=NA, adj=c(0,0.5))
								# OF_ts <- ddply(OO, .(Year), summarize, OF=round(sum(OF)/length(unique(OO$Sim))*100,0))
								# if (sum(OO1$OF)>0) text(OF_ts[OF_ts$OF>0,1], rep(c(-0.03*max(DD$q90),0.03*max(DD$q90)),15)[OF_ts$OF>0]+rep(-0.15*max(DD$q90),sum(OF_ts$OF>0)), OF_ts[OF_ts$OF>0,2], cex=0.7, font=3, xpd=NA)
								if(CV_or_SD == "CV") D2 <- Catch_cv[(Catch_cv$Species == spec),3]
								if(CV_or_SD == "SD") D2 <- Catch_sd[(Catch_sd$Species == spec),3]
								D3 <- ddply(subset(OVERFISHED_plot, subset=(Species == spec)), c("Scenario"), summarize, MEAN=mean(OF_freq))$MEAN
								D3b <- ddply(subset(OVERFISHED_plot, subset=(Species == spec)), c("Scenario"), summarize, Q25=quantile(OF_freq,0.25))$Q25
								D3c <- ddply(subset(OVERFISHED_plot, subset=(Species == spec)), c("Scenario"), summarize, Q75=quantile(OF_freq,0.75))$Q75
								
								if(CV_or_SD == "SD") text(End_year, 1.02*ylims[which(spec==Species)], round(Catch_sd[(Catch_sd$Scenario == scenarios[scn] & Catch_sd$Species == spec),3],0), col=grey(0.5*((max(D2)-Catch_sd[(Catch_sd$Scenario == scenarios[scn] & Catch_sd$Species == spec),3])/max(D2))+0.2), cex=1.2, font=2, xpd=NA, adj=c(1,0.5))
								if(CV_or_SD == "CV") text(End_year, 1.02*ylims[which(spec==Species)], round(Catch_cv[(Catch_cv$Scenario == scenarios[scn] & Catch_cv$Species == spec),3],2), col=grey(0.5*((max(D2)-Catch_cv[(Catch_cv$Scenario == scenarios[scn] & Catch_cv$Species == spec),3])/max(D2))+0.2), cex=1.2, font=2, xpd=NA, adj=c(1,0.5))
								if (Var=="Catch") if (length(D3)>0 & spec!="Total_catch") text(End_year, 0.9*ylims[which(spec==Species)], round(mean(OO1$OF_freq),0), col=grey(max(0.8,(max(D3)-mean(OO1$OF_freq))/max(max(D3),4)+0.2)), cex=1.2, font=2, xpd=NA, adj=c(1,0.5))
							}
						}
					
						if (spec == "Total_catch") axis(1, at=seq(52,Yr_end, length.out=3), labels=seq(1,(Yr_end-51), length.out=3))
					
					}
				
					## the species name on the margin
						mtext(side=4, spec, cex=1, line=2, adj=0.5)
				}
				## labels
					if (Fig_purpose == "PPT")
					{
						mtext(side=1, "Projection year", line=4, outer=T, cex=1.5)
						mtext(side=2, "Catch (1000t)", line=4, outer=T, cex=1.5)
					}
					if (Fig_purpose == "MS")
					{
						mtext(side=1, "Projection year", line=4, outer=T, cex=1.3)
						mtext(side=2, "Catch (1000t)", line=4, outer=T, cex=1.3)
					}
					
				dev.off()
				# dev.off()
		}

		Time_series_plot(Data=CATCHES_plot1, Var="Catch", Summary_var = "MEAN", plot_name="Summary_results_")
		
		Time_series_plot(Data=CATCH_QUOTA_plot1, Var="Catchquota", Summary_var = "MEAN", plot_name="Catch_quota_balancing_")
		
	}

		DO_PLOTS(Yr_end = 65, CI=90)
		
