if NOT EXIST build mkdir build
if NOT EXIST build\debug mkdir build\debug
if NOT EXIST build\release mkdir build\release
call admb -g cab.tpl 
call admb -g main_code.tpl 
call admb -g main_code_average.tpl 
gfortran -ffixed-line-length-none TechInteractions.FOR -g -o TechInteractions.exe
copy cab.exe build\debug
copy main_code.exe build\debug
copy main_code_average.exe build\debug
copy TechInteractions.exe build\debug

