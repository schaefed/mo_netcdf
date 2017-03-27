
FC = gfortran

INCLUDE  = /usr/include
LDFLAGS  = netcdff

OBJDIR   = build
OBJFILES = $(addprefix $(OBJDIR)/, mo_types.o mo_netcdf.o)

TESTDIR      = tests
TESTBUILDIR  = tmp
TESTOBJFILES = $(addprefix $(TESTBUILDIR)/, mo_string.o mo_assert.o mo_testhelper.o)
TESTFILES    = $(addprefix $(TESTBUILDIR)/, test_putdata)

all: $(OBJDIR) $(OBJFILES)

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(OBJDIR)/%.o : %.f90
	$(FC) -c -I$(INCLUDE) -J$(OBJDIR) -o $@ $< -l$(LDFLAGS)

$(TESTBUILDIR):
	mkdir -p $(TESTBUILDIR)

$(TESTBUILDIR)/%.o : $(TESTDIR)/%.f90
	$(FC) -c -I$(INCLUDE) -I$(OBJDIR) -J$(TESTBUILDIR) -o $@ $< -l$(LDFLAGS)

$(TESTBUILDIR)/% : $(TESTDIR)/%.f90
	$(FC) -I$(INCLUDE) -I$(OBJDIR) -I$(TESTBUILDIR) $(OBJFILES) $(TESTOBJFILES) -o $@ $< -l$(LDFLAGS)
	./$@

test: all $(TESTBUILDIR) $(TESTOBJFILES) $(TESTFILES)

clean:
	rm -r $(OBJDIR) $(TESTBUILDIR)

