@echo off
set exec=..\src\build\debug\cab.exe 
set exec2=..\src\build\debug\main_code_average.exe 
set exec3=..\src\build\debug\TechInteractions.exe 
:: if EXIST %exec% (mklink /D  %exec% gmacs.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
if EXIST %exec% (copy %exec% cab.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
if EXIST %exec2% (copy %exec2% main_code_average.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
if EXIST %exec3% (copy %exec3% TechInteractions.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
TechInteractions 

