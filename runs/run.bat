@echo off

if EXIST results (
	goto doall
) else (
	goto dontbatch
)

:doall
echo Do you want to erase previous run. (yes=1/no=0)
set /p input= 
if /i %input%==1 goto dobatch
if /i %input%==0 goto dontbatch
goto commonexit

:dobatch
if EXIST results (rmdir /S /Q results)
if EXIST profit.dat (del profit.dat)
if EXIST TruExp_history.dat (del TruExp_history.dat)
if EXIST Metier_catch_all.dat (del Metier_catch_all.dat)
if EXIST Metier_catch_cod.dat (del Metier_catch_cod.dat)
if EXIST Metier_catch_pollock.dat (del Metier_catch_pollock.dat)
if EXIST Metier_catch_yellowfin.dat (del Metier_catch_yellowfin.dat)
if EXIST Metier_catch_halibut.dat (del Metier_catch_halibut.dat)
if EXIST Metier_profit_all.dat (del Metier_profit_all.dat)
if EXIST Metier_profit_cod.dat (del Metier_profit_cod.dat)
if EXIST Metier_profit_pollock.dat (del Metier_profit_pollock.dat)
if EXIST Metier_profit_yellowfin.dat (del Metier_profit_yellowfin.dat)
if EXIST Metier_profit_halibut.dat (del Metier_profit_halibut.dat)

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
goto commonexit

:dontbatch
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
goto commonexit

:commonexit
pause