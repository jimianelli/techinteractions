## This version of the technical interaction model has the capability of implementing a HCR with multispecies catch constraints under uncertainty as mentioned in the MS

## Structure of the `techinteractions` folder
1. `doc` contains documentations for the model and the MS
2. `R` contains all R codes used in this study
3. `runs` contains all files needed to run the model. Simulation results will be stored in a subfolder named `results`
4. `src` contains all source codes

## Steps to run the models

- First, make sure that you have the path to the `R` folder in your environment variables. This should be something like: `C:\Program Files\R\R-3.2.2\bin`

- Create/specify scenarios. For this, see documentation in the `doc` folder

- Compile & run
- To compile: the `src` folder contains all the source code. Open the command line within this folder and use the `make` command. This will compile all necessary files to run the model
- To Run: move back to the `runs` folder. Then execute the `run` batch file. This will run the models
