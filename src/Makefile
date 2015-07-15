# Name of executable.
OUTPUT:=TechInteractions
LIBINCLUDE = /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.9.sdk/usr/lib 
# Global source files from current dir.
SRCS:=TechInteractions.FOR

# Portable makefiles (http://skramm.blogspot.com/2013/04/writing-portable-makefiles.html)
# Windows users: download http://gnuwin32.sourceforge.net/packages/make.htm
ifdef ComSpec
    RM=del /F /Q
    CXX:=gfortran
else
    RM=rm -rf
    CXX:=gfortran
endif 


# Path to ADMB distribution directory if ADMB_HOME is not declared in your evironment variables
# export ADMB_HOME=/Users/stevenmartell1/admb-trunk/build/dist
# export ADMB_HOME=/Users/jim/admb/build/dist

# Compiler

# Compiler and linker flags.
CXXFLAGS:=-g -ffixed-line-length-none -L $(LIBINCLUDE)                              
LDFLAGS:=-ffixed-line-length-none -L $(LIBINCLUDE)                              

# Release specific flags.
RELEASE_CXXFLAGS:=-O3 

# Debug specific flags.
DEBUG_CXXFLAGS:=-O0 

# Name of text file containing build number.
BUILD_NUMBER_FILE=build-number.txt

# ======================= END OF CONFIGURABLE THINGS ===========================
# Create debug & release list of object files as well as dep files.
BASEFILES:=$(basename $(SRCS))
DEBUG_OBJS:=$(addprefix build/debug/,$(addsuffix .o,$(BASEFILES)))
RELEASE_OBJS:=$(addprefix build/release/,$(addsuffix .o,$(BASEFILES)))
DEPFILES:=$(addprefix build/deps/,$(addsuffix .d,$(BASEFILES)))

# Default to release build.
all: $(OUTPUT).FOR debug release


# Directory targets
build/debug:
	@echo creating debug directory
	@mkdir -p build/debug 

build/release:
	@echo creating release directory
	@mkdir -p build/release 

# Debug route.
.PHONY: debug
debug: CXXFLAGS+= $(DEBUG_CXXFLAGS)
debug: build/debug/$(OUTPUT)

build/debug/%.o : %.FOR
	@echo 'compiling ' $<
	@$(CXX)  $(CXXFLAGS) $< -o $@

# Release route.
.PHONY: release
release: CXXFLAGS+= $(RELEASE_CXXFLAGS)
release: LDFLAGS+= $(RELEASE_LDFLAGS)
release: build/release/$(OUTPUT)

build/release/$(OUTPUT): build/release $(RELEASE_OBJS)
	@echo 'linking ' build/release/$(OUTPUT)
	@$(CXX) -o build/release/$(OUTPUT) $(LDFLAGS)

build/release/%.o : %.FOR
	@echo 'compiling ' $<
	@$(CXX) -c $(CXXFLAGS) $< -o $@

.PHONY: clean
clean:
	@echo 'removing build directory'
	@$(RM) -rf build
