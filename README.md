## This version of the technical interaction model has the capability of implementing a HCR with multispecies catch constraints with a simpler definition of fishing strategies. This is the updated version of the "Multsp_techint_KO_LP" branch

## Information about the branch:
1. OM.dat file controls the population dynamics of the Operating Model + the specifics of the data generation process (used in the Estimation Model) + how the fishing strategies changes over time in the OM
2. EM.dat file controls the setting the the assessment model (i.e CAB) 
3. CR.dat mostly controls whether to use the P* approach or the AFSC tier 3 HCR rule

## Main changes compared to the "Multsp_techint_KO_LP" branch
1. the fishing strategy is based on the last 5 years.
2. any deviation from it is controlled by the CV of fishing strategies
3. for scenarios with changing fishing strategies, the fortran code needs to run "fishing_strategy.Rexec"

## ! Important information to read to be able to run this branch !
Some of the scenarios require running the "fishing_strategy.Rexec" code. But to be able to do so, user needs to do the following 
1. Open the command line "cmd.exe"
2. type in 
`ASSOC .Rexec=RScriptExecutable`
3. then
`FTYPE RScriptExecutable=C:\Program Files\R\R-3.1.2\bin\x64\Rscript.exe  %1 %*`
4. make sure that the above path to Rscript.exe is right