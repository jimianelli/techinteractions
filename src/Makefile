# The compiler
FC = gfortran
ifeq ($(OS),Windows_NT)
    AD = cmd /C admb
    FCFLAGS = -O2 -ffixed-line-length-none 
    ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
    endif
    ifeq ($(PROCESSOR_ARCHITECTURE),x86)
    endif
else
    AD = admb
    FCFLAGS = -O2
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
    endif
    ifeq ($(UNAME_S),Darwin)
        FCFLAGS = -g -ffixed-line-length-none -L /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/usr/lib 
    endif
    UNAME_P := $(shell uname -p)
    ifeq ($(UNAME_P),x86_64)
    endif
    ifneq ($(filter %86,$(UNAME_P)),)
    endif
    ifneq ($(filter arm%,$(UNAME_P)),)
    endif
endif

# flags for debugging or for maximum performance, comment as necessary
ADFLAGS = -f 

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
