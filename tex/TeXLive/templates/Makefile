.PHONY: $(MAKECMDGOALS) makefile-help

makefile-help:
	@echo Usage:
	@echo '  make [TOOLCHAINS="xelatx pdflatex dvipdfmx"] doc-without-suffix...'
	@echo
	@echo xelatex is the default toolchain.


AVAILABLE_TOOLCHAINS := xelatex pdflatex dvipdfmx


define xelatex
%$1.pdf : %.tex
	xelatex $$< -o $$@
	xelatex $$< -o $$@
endef

define pdflatex
%$1.pdf : %.tex
	pdflatex $$< -o $$@
	pdflatex $$< -o $$@
endef

define dvipdfmx
%$1.pdf : %.tex
	latex $$< -o $$(@:.pdf=.dvi)
	latex $$< -o $$(@:.pdf=.dvi)
	dvipdfmx $$(@:.pdf=.dvi)
endef


TOOLCHAINS := $(strip $(TOOLCHAINS))
ifeq ($(TOOLCHAINS),)
	TOOLCHAINS := xelatex
endif

comma :=,
empty :=
space := $(empty) $(empty)
TOOLCHAINS := $(subst $(comma),$(space),$(TOOLCHAINS))

ifneq ($(filter-out $(AVAILABLE_TOOLCHAINS),$(TOOLCHAINS)),)
$(error Unrecognized toolchains: $(TOOLCHAINS) (available: $(AVAILABLE_TOOLCHAINS)))
endif

SOURCES := $(filter-out clean distclean,$(MAKECMDGOALS))

ifneq ($(SOURCES),)
ifeq ($(words $(TOOLCHAINS)),1)
$(SOURCES): % : %.pdf
$(eval $(call $(TOOLCHAINS)))
else
$(SOURCES): % : $(addprefix %-,$(addsuffix .pdf,$(TOOLCHAINS)))
$(foreach toolchain,$(TOOLCHAINS),$(eval $(call $(toolchain),-$(toolchain))))
endif
endif

clean:
	rm -f *.log *.aux *.toc *.out *.dvi *.xdv

distclean: clean
	rm -f *.pdf

