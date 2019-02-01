# Defining variables
FC:=gfortran -Wall -Wextra -Wconversion
# uncomment this FC if you wish to compile without openmp, and comment the one above it out.
#FC:=gfortran -Wall -Wextra -Wconversion
EXE:=boundaryadd
EXT:=.exe

all:			TYPE :=
MPparallel:    TYPE := _MP
debug:    TYPE := _Debug
coverage: TYPE := _Coverage

all:      OPT := -O3
MPparallel: OPT := -O3 -fopenmp
debug:    OPT := -O0 -g
coverage: OPT := -O0 -g --coverage

#-----------------------------------------------------------------------------------
#Add source files as necessary
#If the files are not comiled using the genieric rules, add commands under their
#rule declaration. Add these items to FILTER
#-----------------------------------------------------------------------------------

SRC := \
				main.f95 	      	\
				globalvariables.f95	\
				readinfunctions.f95	\
				boundaryfunctions.f95	\
				printoutfunctions.f95	\
				globalfunctions.f95

OBJ := $(SRC:.f95=.o)
MOD := $(OBJ:.o=.mod)

FILTER := \

OBJ_FILTER := $(FILTER:.f95=.o)
MOD_FILTER := $(FILTER:.f95=.mod)

#-----------------------------------------------------------------------------------
#Phony targets for cleaning and building
#-----------------------------------------------------------------------------------
.PHONY: all clean reset

all: $(EXE)

debug: $(EXE)

MPparallel: $(EXE)

coverage: $(EXE)

#Intended to clean up compilation artifacts but leave executable & coverage
clean:
	rm -f $(OBJ) $(CONT_OBJ) $(LIB_OBJ)
	rm -f $(MOD) $(CONT_MOD) $(LIB_MOD)
	rm -f *$(EXT)
	rm -f *.mod
	rm -f *.o
	rm -f $(COMP_DEP)

#Intended to reset directory to fresh state with no exe or artifacts
reset: clean
	rm -f *.gcno *.gcda
	rm -f -r $(EXE)*.dSYM
	rm -f ../$(EXE)*
	rm -f *$(EXT)
	rm -f *.mod
	rm -f *.o

#-----------------------------------------------------------------------------------
#Generics for source files
#-----------------------------------------------------------------------------------
$(filter-out $(OBJ_FILTER), $(OBJ)): %.o:	%.f95
	$(FC) -c $(OPT) $<

$(filter-out $(MOD_FILTER), $(MOD)):	%.mod:	%.f95
	$(FC) -c $(OPT) $<

$(EXE): $(OBJ) $(CONT_OBJ) $(LIB_OBJ)
	$(FC) -o $@$(TYPE)$(EXT) $(OPT) $(OBJ) $(CONT_OBJ) $(LIB_OBJ)
	mv ./$(EXE)$(TYPE)$(EXT) ../

#-----------------------------------------------------------------------------------
#Dependency List
#Use [gfortran -M -cpp -fopenmp *.f95] repeatedly until clean compile to update rules below
#-----------------------------------------------------------------------------------
boundaryfunctions.o boundaryfunctions.mod: boundaryfunctions.f95
globalfunctions.o globalfunctions.mod: globalfunctions.f95 \
 globalvariables.mod
globalvariables.o globalvariables.mod: globalvariables.f95
main.o: main.f95 globalvariables.mod readinfunctions.mod \
 printoutfunctions.mod globalfunctions.mod boundaryfunctions.mod
printoutfunctions.o printoutfunctions.mod: printoutfunctions.f95 \
 globalvariables.mod globalfunctions.mod
readinfunctions.o readinfunctions.mod: readinfunctions.f95 \
 globalvariables.mod globalfunctions.mod
