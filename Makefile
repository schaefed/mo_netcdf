
FC = gfortran
FCFLAGS = -Wall

INCLUDE  = /usr/include
LDFLAGS  = netcdff

SRCDIR      = src
OBJDIR      = build
TESTDIR     = tests
EXAMPLEDIR  = examples
TMPDIR      = tmp

OBJFILES = $(patsubst $(SRCDIR)/%.f90, $(OBJDIR)/%.o, $(addprefix $(SRCDIR)/, mo_types.f90 mo_netcdf.f90))

TESTOBJFILES = $(patsubst $(TESTDIR)/%.f90, $(TMPDIR)/%.o, $(addprefix $(TESTDIR)/, mo_string.f90 mo_assert.f90 mo_testhelper.f90)) 
TESTEXE      = $(patsubst $(TESTDIR)/%.f90, $(TMPDIR)/%, $(wildcard $(TESTDIR)/test_*.f90))

EXAMPLEXE   = $(patsubst $(EXAMPLEDIR)/%.f90, $(TMPDIR)/%, $(wildcard $(EXAMPLEDIR)/*.f90))

all: $(OBJDIR) $(OBJFILES)

test: all $(TMPDIR) $(TESTOBJFILES) $(TESTEXE)

examples: all $(TMPDIR) $(EXAMPLEXE)

clean:
	rm -r $(OBJDIR) $(TMPDIR)

# create directory
$(OBJDIR):
	mkdir -p $(OBJDIR)

# build the module
$(OBJDIR)/%.o : $(SRCDIR)/%.f90
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




