
FC = gfortran
FCFLAGS = -Wall

INCLUDE  = /usr/include
LDFLAGS  = netcdff

OBJDIR   = build
OBJFILES = $(addprefix $(OBJDIR)/, mo_types.o mo_netcdf.o)

TESTDIR      = tests
TMPDIR       = tmp
TESTOBJFILES = $(addprefix $(TMPDIR)/, mo_string.o mo_assert.o mo_testhelper.o)
TESTFILES    = $(patsubst $(TESTDIR)/%.f90, $(TMPDIR)/%, $(wildcard $(TESTDIR)/test_*.f90))

EXAMPLEDIR   = examples
EXAMPLEFILES = $(patsubst $(EXAMPLEDIR)/%.f90, $(TMPDIR)/%, $(wildcard $(EXAMPLEDIR)/*.f90))

all: $(OBJDIR) $(OBJFILES)

test: all $(TMPDIR) $(TESTOBJFILES) $(TESTFILES)

examples: all $(TMPDIR) $(EXAMPLEFILES)

clean:
	rm -r $(OBJDIR) $(TMPDIR)

# create directory
$(OBJDIR):
	mkdir -p $(OBJDIR)

# build the module
$(OBJDIR)/%.o : %.f90
	$(FC) -c $(FCFLAGS) -I$(INCLUDE) -J$(OBJDIR) -o $@ $< -l$(LDFLAGS)

# create directory
$(TMPDIR):
	mkdir -p $(TMPDIR)

# build the test dependencies
$(TMPDIR)/%.o : $(TESTDIR)/%.f90
	$(FC) -c $(FCFLAGS) -I$(INCLUDE) -I$(OBJDIR) -J$(TMPDIR) -o $@ $< -l$(LDFLAGS)

# build and run the tests
$(TMPDIR)/% : $(TESTDIR)/%.f90 
	$(FC) $(FCFLAGS) -I$(INCLUDE) -I$(OBJDIR) -I$(TMPDIR) $(OBJFILES) $(TESTOBJFILES) -o $@ $< -l$(LDFLAGS)
	./$@

# build and run the examples
$(TMPDIR)/% : $(EXAMPLEDIR)/%.f90 
	$(FC) $(FCFLAGS) -I$(INCLUDE) -I$(OBJDIR) -I$(TMPDIR) $(OBJFILES) -o $@ $< -l$(LDFLAGS)
	./$@




