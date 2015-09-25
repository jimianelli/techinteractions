@echo off
if EXIST results (rmdir /S /Q results)
set exec=..\src\\cab.exe 
set exec2=..\src\main_code_average.exe 
set exec3=..\src\TechInteractions.exe 
set exec4=..\src\main_code.exe 
:: if EXIST %exec% (mklink /D  %exec% gmacs.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
if EXIST %exec% (copy %exec% cab.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
if EXIST %exec2% (copy %exec2% main_code_average.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
if EXIST %exec3% (copy %exec3% TechInteractions.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
if EXIST %exec4% (copy %exec4% main_code.exe ) ELSE echo "file missing, compile source code in gmacs\src directory "
TechInteractions 

