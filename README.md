# techinteractions

## Description
_techinteractions_ is a program that allows implementing a management strategy evaluation (MSE) for a multispecies fishery with catch constraints e.g. bycatch constraints, cap in total species catch, mixed-species cap. The model combines a biological module, a quota allocation module and a vessel dynamics module to mimic the fishery. 
Quota allocation and vessel dynamics modules are both implemented using linear programming (a constrained optimization approach). This simple yet flexible approach is ideal for MSE as it allows including much complexity of both quota and vessel dynamics while saving time for running the MSE (generally takes few seconds to run these modules).
Currently, the code is based on the Bering Sea Aleutian Islands groundfish fishery example. 

## Contents
- [Structure of the _techinteractions_ program](#structure-of-the-techinteractions-program)
- [Computer setup for running the MSE](#computer-setup-for-running-the-mse)
- [_techinteractions_ MSE setup](#techinteractions-mse-setup)

## Structure of the _techinteractions_ program
- `doc` contains documentations for the model and the MS
- `R` contains various `R` codes and data needed to run the model
- `runs` contains all model configuration files (where we specify scenarios). Simulation results will be stored in a subfolder named `results`
- `src` contains all source codes (both `fortran` and `ADMB`)

## Computer setup for running the MSE

### Programs to install
The main body of _techinteractions_ is written in `fortran` but the program calls for `ADMD` and `R` to run various parts of the model. 
Therefore, users are required to have the following software in order to run this program:
* install `ADMB` [here] (http://www.admb-project.org/downloads/) and follow their documentations/instructions to check whether the installation was performed correctly. P.S: Any of the above `ADMB` installation should come with `fortran` compiler to run the program so no need to install a separate `fortran` compiler.
* install `R` [here] (https://www.r-project.org/)

### Setup the path
The next step is to set the path.
* path to `ADMB`:
Follow the instructions written [here] (http://www.admb-project.org/downloads/) to check whether the path was correctly set. If not, please set the path to `ADMB` in your environment variables i.e. it should be the location where you installed `AMDB` e.g. _C:\ADMB\bin_
* path to `fortran`
* path to `R`: 
Go to the directory where you cloned _techinteractions_. _shift + right mouse click_ above the _techinteractions_ folder and select the option _Open command window here_. Run the command script 

```R
rscript R/test.r
```

If it prints the message _path to R is already setup_, you are all good. If not, please set the path to `R` in your environment variables i.e. it should be the location where you installed `R` e.g. _C:\Program Files\R\R-3.2.2\bin_


## _techinteractions_ MSE setup

Below is the illustration of the full MSE cycle
![MSE cycle] (https://github.com/Kotkot/techinteractions/blob/trial/doc/MSE_fig.png) 

### Creating a scenario for the multispecies MSE 
Creating a scenario requires changes to many nodes in the program:
* the operating model: this specifies the underlying truth about the population dynamics (e.g. how they grow, recruit, fished)
* the assessment model: this both defines the data that are collected to perform the assessment and the assessment model configuration itself
* the quota allocation model: this defines/mimics the behavior of quota allocation process by fishery managers
* the vessel dynamics: this defines/mimics fishers' behavior 

**ALL** these configurations are controlled by several important files in the program:
* [`runs/OM.dat`] (/blob/trial/runs/OM.DAT) : This file controls the operating model i.e. the underlying truth of the model 
* [`runs/EM.dat`] (/blob/trial/runs/EM.DAT) : This file controls the estimation/assessment model
* [`runs/CR.dat`] (/blob/trial/runs/CR.DAT) : This file controls the harvest control rule 
* [`runs/Random_seed_OM.dat`] (/blob/trial/runs/Random_seed_OM.dat) :
* [`runs/Random_seed_EM.dat`] (/blob/trial/runs/Random_seed_EM.dat) :
* [`R/fishing strategy.r`] (/blob/trial/R/fishing_strategy.r) :

Step by step detail for setting an MSE scenario are provided in [`specifying-scenario`] file (https://github.com/Kotkot/techinteractions/blob/trial/doc/specifying-scenario.Rmd)
### Compiling and running the scenario
Once you have set-up the scenario, now it is time to run it. This is done in two steps:
  1. Compile the source codes. Open the command line within the `src` folder and type in `make` (this will run the `Makefile`). This will compile all necessary files to run the model
  2. Run the model. Either open the command line within the `runs` folder and type `run` (this will run the `run` batch file) or _double-click_ the `run` batch file. This will run the models
	
## Description of all files present in the _techinteractions_ program
Please refer to [this document] (https://github.com/Kotkot/techinteractions/tree/trial/doc/File-description.Rmd) 
 	
