# This version of the technical interaction model has the capability of implementing a HCR with multispecies catch constraints under uncertainty as mentioned in the MS #

## Structure of the `techinteractions` folder ##
- `doc` contains documentations for the model and the MS
- `R` contains all R codes used in this study
- `runs` contains all files needed to run the model. Simulation results will be stored in a subfolder named `results`
- `src` contains all source codes

## Steps to run the models ##

To begin with, make sure that you have the path to the `R` folder in your environment variables. This should be something like: `C:\Program Files\R\R-3.2.2\bin`

1. Create/specify scenarios. For this, see documentation in the `doc` folder

2. Compile & run 
  1. To compile: the `src` folder contains all the source code. Open the command line within this folder and use the `make` command. This will compile all necessary files to run the model
  2. To Run: move back to the `runs` folder. Then execute the `run` batch file. This will run the models
