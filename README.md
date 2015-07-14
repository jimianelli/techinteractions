## This version of the technical interaction model has the capability of implementing a HCR with multispecies catch constraints with a simpler definition of fishing strategies. This is the updated version of the "Multsp_techint_KO_LP" branch

## To compile 'techinteractions.for' using gfortran use the following code
gfortran -ffixed-line-length-none TechInteractions.for -o TechInteractions.exe

## Information about the branch:
1. OM.dat file controls the population dynamics of the Operating Model + the specifics of the data generation process (used in the Estimation Model) + how the fishing strategies changes over time in the OM
2. EM.dat file controls the setting the the assessment model (i.e CAB) 
3. CR.dat mostly controls whether to use the P* approach or the AFSC tier 3 HCR rule

## Main changes compared to the "Multsp_techint_KO_LP" branch
1. the fishing strategy is based on the last 5 years.
2. any deviation from it is controlled by the CV of fishing strategies
3. for scenarios with changing fishing strategies, the fortran code needs to run "fishing_strategy.Rexec"

