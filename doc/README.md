## Documentation for the multispecies technical interaction model ##

A. How to create scenario: file-list to check/modify for creating scenario

	1. The HCR (Naive or Conscientious manager)
		- modify "runs/OM.dat" (L165)
	2. OM fishing strategy (fixed, time varying, or none)
		- modify "runs/OM.dat" (L166)
		- modify "runs/Random_strategy_EM.dat" to match or not "runs/Random_strategy_OM.dat" 
	3. Data richness (length, frequency, noise)
		- modify "runs/OM.dat" (L137-147)
		- modify "runs/EM.dat" (L25, 27)
	4. Optimization (total catch, revenue. These are the only options available for now)
		- modify "R/fishing strategy.r" (L223-226)
	5. 	Constraints
		- copy the "main_code_average.tpl" file from one one of the three folders "runs/LP with base constraints", "runs/LP with less constraints", "runs/LP with stricter constraint" to "src/main_code_average.tpl"
		- copy the "main_code_average.dat" file from one one of the three folders "runs/LP with base constraints", "runs/LP with less constraints", "runs/LP with stricter constraint" to "runs/main_code_average.dat"
		- copy the "main_code.tpl" file from one one of the three folders "runs/LP with base constraints", "runs/LP with less constraints", "runs/LP with stricter constraint" to "src/main_code.tpl"
		- modify "R/fishing_strategy.r" (L308)
	6. Bycatch
		- modify "runs/Bycatch.DAT"
	7. The type of process error (none, random, correlated)	
		- modify "OM.dat" (L25)
	8. Variability of metier over time
		- modify "R/fishing_strategy.r" (L308)
	9. Do assessment or not?
		- modify "runs/EM.dat" (L1)
		
		
B. Description of the quota allocation steps:

	1. Quota allocation steps:
		1.1. fix EM_fish = the seed (from Random_seed_EM.dat)
		1.2. Run DoEstAll
			1.2.1. run assessment --> QuotaAct=ABC (based on Tier 3 HCR)
			1.2.2. Save True exploitable biomass (TruExp.dat)
			1.2.3. Save ABC.dat
			1.2.4. Determine the TAC
				1.2.4.1. Naive approach: 
					1.2.4.1.1. TAC = QuotaAct/sum(QuotaAct)*min(1.7e6, sum(QuotaAct))
					1.2.4.1.2. TAC_statusquo.out produced
					1.2.4.1.3. Save proposed quota in TAC.dat
				1.2.4.1. Conciecious approach: 
					1.2.4.2.1. Save ABC value in TAC.dat
					1.2.4.2.2. Run technical interaction model with the above ABC limits
					1.2.4.2.3. Read in values from the technical interaction run and save in QuotaAct
					1.2.4.2.4. Save this results in TAC_techint.out = Proposed quota
					1.2.4.2.5. Do the same and save to TAC.dat = Proposed quota
					
	2. Operating model update
		2.1. Read in true exploitable biomass = TruExp.dat
		2.2. Read in the proposed quota = TAC.dat
		2.3. IF proposed quota > exploitable biomass, then proposed quota = exploitable biomass
		2.4. Save the final proposed quota for the OM in TAC.dat
		2.5. Calculate OM realized catch
			2.5.1. Run technical interaction model (using seed from Random_seed_OM.dat)
			2.5.2. Read in values from the technical interaction run and save in QuotaAct
			2.5.3. This becomes the "realized catch" 
			2.5.4. The TRUE REALIZED CATCH: this updates the population dynamics by finding effort level that match catch... some optimization error can happen
			
			
		
	
		
