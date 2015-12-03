

LATEX=/sw/bin/latex
PDFLATEX=/sw/bin/pdflatex
TEX=/sw/bin/tex
TEXHASH=/sw/bin/texhash

prefix=/Users/kbisset/emacs/auctex-11.82$(null)
datadir=${prefix}/share$(null)
exec_prefix=${prefix}$(null)
libdir=${exec_prefix}/lib$(null)
previewtexmfdir = /sw/share/texmf-local/tex/latex/preview$(null)
previewdocdir = /sw/share/texmf-local/doc/latex/styles$(null)
TEXMFGEN = preview.sty prauctex.def prauctex.cfg prshowbox.def prshowlabels.def prtracingall.def prtightpage.def prlyx.def prcounters.def prfootnotes.def

DESTDIR=
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
MKINSTALLDIRS = ../../mkinstalldirs

all: $(TEXMFGEN) preview.drv preview-mk.ins preview.dvi preview.pdf

install: install-texmf install-texmf-doc

install-texmf: $(TEXMFGEN)
	-$(MKINSTALLDIRS) $(DESTDIR)$(previewtexmfdir)
	for x in $(TEXMFGEN) ; do \
	  echo $(INSTALL_DATA) $$x $(DESTDIR)$(previewtexmfdir) ; \
	  $(INSTALL_DATA) $$x $(DESTDIR)$(previewtexmfdir) ; \
	done
	-$(TEXHASH)

install-texmf-doc: preview.dvi
	-$(MKINSTALLDIRS) $(DESTDIR)$(previewdocdir)
	$(INSTALL_DATA) preview.dvi $(DESTDIR)$(previewdocdir)

preview-mk.ins: preview.dtx bootstrap.ins
	$(TEX) '\nonstopmode \input bootstrap.ins'

#only for standalone preview.sty package:
preview.ins: preview.dtx
	$(TEX) '\nonstopmode\def\jobname{.ins}\input docstrip ' \
	'\generate{\file{preview.ins}{\from{preview.dtx}{installer}}}' \
	'\endbatchfile'

$(TEXMFGEN) preview.drv: preview.dtx preview-mk.ins
	$(TEX) '\nonstopmode \input preview-mk.ins'

preview.dvi: preview.drv preview.dtx preview.sty
	$(LATEX) '\nonstopmode \input preview.drv'
	$(LATEX) '\nonstopmode \input preview.drv'
	$(LATEX) '\nonstopmode \input preview.drv'

preview.pdf: preview.drv preview.dtx preview.sty
	$(PDFLATEX) '\nonstopmode \AtBeginDocument{\OnlyDescription}\input{preview.drv}'
	$(PDFLATEX) '\nonstopmode \AtBeginDocument{\OnlyDescription}\input{preview.drv}'
	$(PDFLATEX) '\nonstopmode \AtBeginDocument{\OnlyDescription}\input{preview.drv}'

clean:
	rm -f *~ *.aux *.dvi *.drv *.log 
	rm -f $(TEXMFGEN) preview.ins preview-mk.ins 
