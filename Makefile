SETUP = ocaml setup.ml

DOCDIR = .gh-pages
DOC_CORE = $(DOCDIR)/redis.docdir
DOC_SYNC = $(DOCDIR)/redis_sync.docdir
DOC_LWT = $(DOCDIR)/redis_lwt.docdir

LWT ?= $(shell if ocamlfind query lwt >/dev/null 2>&1; then echo --enable-lwt; fi)

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS) $(LWT)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS) $(LWT)

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp docs/index.html $(DOCDIR)/index.html
	cp docs/style.css $(DOCDIR)/style.css
	cp -r _build/src/redis.docdir $(DOC_CORE)
	cp -r _build/src_sync/redis_sync.docdir $(DOC_SYNC)
	cp -r _build/src_lwt/redis_lwt.docdir $(DOC_LWT)
	cp docs/style.css $(DOC_CORE)/style.css
	cp docs/style.css $(DOC_SYNC)/style.css
	cp docs/style.css $(DOC_LWT)/style.css
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

.PHONY: build doc test all install uninstall reinstall clean distclean configure
