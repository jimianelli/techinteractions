# The compiler
FC = gfortran
AD = admb
# flags for debugging or for maximum performance, comment as necessary
FCFLAGS = -O2
FCFLAGS = -g -ffixed-line-length-none -L /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/usr/lib 
ADFLAGS = -f 
# flags forall (e.g. look for system .mod files, required in gfortran)
# FCFLAGS += -I/usr/include

# libraries needed for linking, unused in the examples
#LDFLAGS = -li_need_this_lib

# List of executables to be built within the package
PROGRAMS = TechInteractions Cab main_code main_code_average

# "make" builds all
all: $(PROGRAMS)
%: %.FOR
	$(FC) $(FCFLAGS) $@.FOR  -o $@
%: %.tpl
	$(AD) $(ADFLAGS) $@.tpl  
	rm -f *.o *.mod *.MOD *.cpp *.htp *.obj 

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD *.cpp *.htp *.obj 

veryclean: clean
	rm -f *~ $(PROGRAMS)
