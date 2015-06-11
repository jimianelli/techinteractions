This version of the technical interaction model has the capability of implementing a more complex HCR.
1. updated version of the "Multsp_techint_KO" branch

Information about the branch:
1. OM.dat file controls the population dynamics of the Operating Model + the specifics of the data generation process (used in the Estimation Model) + how the fishing strategies changes over time in the OM
2. EM.dat file controls the setting the the assessment model (i.e CAB) 
3. CR.dat mostly controls whether to use the P* approach or the AFSC tier 3 HCR rule

Main changes: 
1. Depending on the specification in the OM (last two lines in the OM.dat), the model implements either i) a "naive" quota allocation method TAC=ABC/sum(ABC)*1.7 or ii) a quota allocation based on LP 
2. Depending on the specification in the OM (last two lines in the OM.dat), fishing strategies is either i) fixed over time or ii) changing over time
3. the first line in EM.dat specifies whether to run CAB or not