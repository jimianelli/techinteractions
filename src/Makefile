# The compiler
FC = gfortran
# flags for debugging or for maximum performance, comment as necessary
FCFLAGS = -O2
FCFLAGS = -g -ffixed-line-length-none 
# flags forall (e.g. look for system .mod files, required in gfortran)
# FCFLAGS += -I/usr/include

# libraries needed for linking, unused in the examples
#LDFLAGS = -li_need_this_lib

# List of executables to be built within the package
PROGRAMS = TechInteractions

# "make" builds all
all: $(PROGRAMS)
%.o: %.FOR
	$(FC) $(FCFLAGS) -o $@  
	#%.o: %.FOR
	#$(FC) $(FCFLAGS) -c $<
	
# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ $(PROGRAMS)

	
