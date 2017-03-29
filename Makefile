
FC = gfortran

INCLUDE  = /usr/include
LDFLAGS  = netcdff

OBJDIR   = build
OBJFILES = $(addprefix $(OBJDIR)/, mo_types.o mo_netcdf.o)

TESTDIR      = tests
TESTBUILDIR  = tmp
TESTOBJFILES = $(addprefix $(TESTBUILDIR)/, mo_string.o mo_assert.o mo_testhelper.o)
TESTFILES    = $(patsubst $(TESTDIR)/%.f90, $(TESTBUILDIR)/%, $(wildcard $(TESTDIR)/test_*.f90))

all: $(OBJDIR) $(OBJFILES)

test: all $(TESTBUILDIR) $(TESTOBJFILES) $(TESTFILES)

clean:
	rm -r $(OBJDIR) $(TESTBUILDIR)

# create directory
$(OBJDIR):
	mkdir -p $(OBJDIR)

# build the module
$(OBJDIR)/%.o : %.f90
	$(FC) -c -I$(INCLUDE) -J$(OBJDIR) -o $@ $< -l$(LDFLAGS)

# create directory
$(TESTBUILDIR):
	mkdir -p $(TESTBUILDIR)

# build the test dependencies
$(TESTBUILDIR)/%.o : $(TESTDIR)/%.f90
	$(FC) -c -I$(INCLUDE) -I$(OBJDIR) -J$(TESTBUILDIR) -o $@ $< -l$(LDFLAGS)

# build and run the tests
$(TESTBUILDIR)/% : $(TESTDIR)/%.f90
	$(FC) -I$(INCLUDE) -I$(OBJDIR) -I$(TESTBUILDIR) $(OBJFILES) $(TESTOBJFILES) -o $@ $< -l$(LDFLAGS)
	./$@



