prefix=/Users/kbisset/emacs/auctex-11.82$(null)
datadir=${prefix}/share$(null)
packagedir=no$(null)
infodir=${prefix}/info$(null)
TEX=/sw/bin/tex
PDFTEX=/sw/bin/pdftex
MAKEINFO=/sw/bin/makeinfo
INSTALL_INFO=/sw/sbin/install-info
INSTALL=/usr/bin/install -c
INSTALL_DATA=${INSTALL} -m 644
DESTDIR=
TEXI2HTML=/sw/bin/texi2html
TEXI2DVI=/sw/bin/texi2dvi
TEXI2PDF=/sw/bin/texi2pdf
MKINSTALLDIRS = ../mkinstalldirs
DVIPS=/sw/bin/dvips
PERL=/usr/bin/perl
AUCTEXTEXIFILES = auctex.texi install.texi wininstall.texi intro.texi \
	changes.texi todo.texi faq.texi macros.texi quickstart.texi
PREVIEWTEXIFILES = copying.texi preview-faq.texi macros.texi \
	preview-dtxdoc.texi preview-latex.texi preview-problems.texi \
	preview-readme.texi preview-todo.texi
DISTTEXTS = ../README ../INSTALL ../TODO ../INSTALL.windows ../FAQ ../CHANGES

all: dist

info: auctex.info preview-latex.info

dvi: auctex.dvi tex-ref.dvi preview-latex.dvi

install: install-auctex install-preview

install-man:
	test x$(packagedir) != xno && \
	{ $(MKINSTALLDIRS) $(DESTDIR)$(packagedir)/man/auctex ; \
	for i in $(AUCTEXTEXIFILES) $(PREVIEWTEXIFILES) tex-ref.tex ; \
	do echo $(INSTALL_DATA) $$i $(DESTDIR)$(packagedir)/man/auctex; \
	   $(INSTALL_DATA) $$i $(DESTDIR)$(packagedir)/man/auctex; \
	done ; \
	}

dist: $(DISTTEXTS) preview-latex.info auctex.info

extradist: html/auctex_toc.html auctex.ps auctex.pdf tex-ref.ps tex-ref.pdf

.PHONY: all info dvi dist install-auctex disttexts clean distclean \
	maintainer-clean install-preview install-man html-docs extradist

# AUCTeX

html/auctex_toc.html: auctex.texi
	rm -rf html
	mkdir html
	cd html && $(TEXI2HTML) -split_node -I .. ../auctex.texi && \
	test ! -d auctex || { mv auctex/* . && rm -rf auctex ; }

tex-ref.dvi: tex-ref.tex
	$(TEX) tex-ref

tex-ref.ps: tex-ref.dvi
	$(DVIPS) tex-ref.dvi -Ppdf -o tex-ref.ps

tex-ref.pdf: tex-ref.tex
	$(PDFTEX) tex-ref.tex

auctex.dvi: $(AUCTEXTEXIFILES)
	$(TEXI2DVI) auctex.texi

auctex.pdf: $(AUCTEXTEXIFILES)
	$(TEXI2PDF) auctex.texi

auctex.ps: auctex.dvi
	$(DVIPS) auctex.dvi -Ppdf -o auctex.ps

auctex.info: $(AUCTEXTEXIFILES)
	$(MAKEINFO) auctex.texi

install-auctex: auctex.info
	-$(MKINSTALLDIRS) $(DESTDIR)$(infodir)
	rm -f $(DESTDIR)$(infodir)/auctex $(DESTDIR)$(infodir)/auctex-*
	$(INSTALL_DATA) auctex.info $(DESTDIR)$(infodir)
	for x in auctex.info-*; do \
	  if [ -r $$x ]; then \
	    $(INSTALL_DATA) $$x $(DESTDIR)$(infodir) ; \
	  fi; \
	done
	-$(INSTALL_INFO)  --info-dir=$(DESTDIR)$(infodir) auctex.info

# preview

preview-latex/index.html: $(PREVIEWTEXIFILES)
	rm -f preview-latex/*
	$(TEXI2HTML) preview-latex.texi

html-docs: preview-latex/index.html

preview-dtxdoc.texi: ../preview/latex/preview.dtx preview-dtxdoc.pl
	-$(PERL) preview-dtxdoc.pl ../preview/latex/preview.dtx \
		preview-dtxdoc.texi

preview-latex.dvi: $(PREVIEWTEXIFILES)
	$(TEXI2DVI) preview-latex.texi

preview-latex.ps: preview-latex.dvi
	$(DVIPS) preview-latex.dvi -Ppdf -o preview-latex.ps

preview-latex.pdf: $(PREVIEWTEXIFILES)
	$(TEXI2PDF) preview-latex.texi

preview-latex.info: $(PREVIEWTEXIFILES)
	$(MAKEINFO) preview-latex.texi

install-preview: preview-latex.info
	-$(MKINSTALLDIRS) $(DESTDIR)$(infodir)
	for x in preview-latex.info* ; do \
		$(INSTALL_DATA) $$x $(DESTDIR)$(infodir) ; \
	done
	-$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) preview-latex.info

# disttexts

../INSTALL: install.texi macros.texi
	$(MAKEINFO) -D rawfile --no-headers  \
		install.texi --output $@

../INSTALL.windows: wininstall.texi macros.texi
	$(MAKEINFO) -D rawfile --no-headers  \
		wininstall.texi --output $@

../README: intro.texi preview-readme.texi macros.texi
	$(MAKEINFO) -D rawfile --no-headers  \
		intro.texi --output $@
	$(MAKEINFO) -D rawfile --no-headers  \
		preview-readme.texi --output - >> $@

../CHANGES: changes.texi macros.texi
	$(MAKEINFO) -D rawfile --no-headers  \
		changes.texi --output $@

../TODO: todo.texi preview-todo.texi macros.texi
	$(MAKEINFO) -D rawfile --no-headers  \
		todo.texi --output $@
	$(MAKEINFO) -D rawfile --no-headers  \
		preview-todo.texi --output - >> $@

../FAQ: faq.texi preview-faq.texi macros.texi
	$(MAKEINFO) -D rawfile --no-headers  \
		faq.texi --output $@
	$(MAKEINFO) -D rawfile --no-headers  \
		--number-sections preview-faq.texi --output - >> $@

../PROBLEMS: preview-problems.texi macros.texi
	$(MAKEINFO) -D rawfile --no-headers  \
		--number-sections preview-problems.texi --output $@

# clean

clean:
	rm -f *.dvi *.ps *.pdf *.aux *.cp *.fn *.ky *.log *~ \#*\# \
		*.tp *.vr *.pg *.toc *.tp *.bak *.cps *.kys *.tps \
		*.fns *.vrs *.pgs *.tmp *.html
	rm -rf html preview-latex

distclean: clean
	rm -f Makefile

maintainer-clean: distclean
	rm -f INSTALL.windows README CHANGES HISTORY TODO FAQ \
		preview-dtxdoc.texi auto.texi $(DISTTEXTS) *.info *.info-*
