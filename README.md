This is the initial commit of the multispecies technical interaction model
1. The model is parametrized for 3 species: cod, pollock and yellowfin sole from BSAI (forced to have BH SR curve though steepness is 0.99 for cod and yellowfin)
2. There are many few files associated with the implementation of technical interaction:
    2.1. bycatch.dat --> this is the input file for the bycatch limit (in t)
    2.2. coeff.dat/main_code.dat --> this contains catch proportion by species and by fishing strategies determined from the analysis of observer data and other information needed to create the framework to run the linear optimization code
    2.3. TAC.dat --> this contains the information on ABC used to run the techint model
    2.4. simple.dat --> this is a fake data to perform estimation in ADMB (could have not used it)
3. The code inside "TechInteraction.for" has been changed to accomodate the multispecies nature of the MSE and to have a different behavior depending on whether it is using the P* approach or the simple Alaska Tier 3 control rule.

