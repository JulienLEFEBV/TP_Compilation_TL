
# OCAML configuration
OCAML_PREFIX=
OCAMLC=$(OCAML_PREFIX)ocamlc
OCAMLLEX=$(OCAML_PREFIX)ocamllex
OCAMLYACC=$(OCAML_PREFIX)ocamlyacc


# configuration
CFLAGS= -g -I vm -I http-lib/src
LDFLAGS= str.cma vm/vm.cma
OCAMLYACC_FLAGS=--strict -v
OCAMLLEX_FLAGS=-ml


# definitions
SUBDIRS= \
	vm \
	http-lib
APPS=\
	logocc \
	logorun \
	logoapp
CLEAN=

# useful
DATE = $(shell date +"%y%m%d")

# rules
all: all-rec $(APPS)

# logocc
LOGOCC_SOURCES=\
	common.ml \
	ast.ml \
	logo.ml \
	lexer.ml \
	comp.ml \
	parser.ml \
	logocc.ml
LOGOCC_OBJECTS=$(LOGOCC_SOURCES:.ml=.cmo)
CLEAN+= \
	$(LOGOCC_OBJECTS) \
	$(LOGOCC_SOURCES:.ml=.cmi) \
	$(LOGOAPP_SOURCES:.ml=.cmi) \
	$(LOGORUN_SOURCES:.ml=.cmi) \
	logocc lexer.ml parser.ml parser.output

logocc: $(LOGOCC_OBJECTS)
	$(OCAMLC) $(CFLAGS) -o $@ $(LDFLAGS) $^


# logorun
LOGORUN_SOURCES = \
	common.ml \
	logo.ml \
	logorun.ml
LOGORUN_OBJECTS=$(LOGORUN_SOURCES:.ml=.cmo)
CLEAN+= \
	$(LOGORUN_OBJECTS) \
	logorun

logorun: $(LOGORUN_OBJECTS)
	$(OCAMLC) $(CFLAGS) -o $@ $(LDFLAGS) $^


# logoapp
LOGOAPP_SOURCES = \
	common.ml \
	logo.ml \
	logoapp.ml
LOGOAPP_OBJECTS=$(LOGOAPP_SOURCES:.ml=.cmo)
LOGOAPP_LIBADD=http-lib/tiny_httpd.cma
CLEAN+= \
	$(LOGOAPP_OBJECTS) \
	logoapp

logoapp: $(LOGOAPP_OBJECTS)
	$(OCAMLC) $(CFLAGS) -o $@ $(LDFLAGS) $(LOGOAPP_LIBADD) $^


# object dependencies
parser.cmo: ast.cmo common.cmo comp.cmo
parser.cmi: ast.cmo parser.mly common.cmi comp.cmi
lexer.cmo: common.cmo ast.cmo parser.cmi
comp.cmo: vm/vm.cma common.cmi ast.cmo logo.cmo
comp.cmi: common.cmi
logo.cmo: vm/vm.cma common.cmo
logocc.cmo: vm/vm.cma common.cmo ast.cmo lexer.cmo parser.cmo logo.cmo comp.cmo
logorun.cmo: common.cmo logo.cmo
logoapp.cmo: vm/vm.cma $(LOGOAPP_LIBADD) common.cmo logo.cmo


# generic rules
all-rec:
	@for sub in $(SUBDIRS); do \
		cd $$sub; $(MAKE) all || exit 1; cd ..; \
	done

clean-rec:
	@for sub in $(SUBDIRS); do \
		cd $$sub; $(MAKE) clean || exit 1; cd ..; \
	done

%.cmo: %.ml
	$(OCAMLC) $(CFLAGS) -o $@ -c $<

%.cmi: %.mli
	$(OCAMLC) $(CFLAGS) -o $@ -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $(OCAMLYACC_FLAGS) $<

%.ml: %.mll
	$(OCAMLLEX) $(OCAMLLEX_FLAGS) $<

clean: clean-rec
	-rm -rf $(CLEAN) $(tiny_httpd).cma progs/*.s progs/*.exe


# shortcuts
%.s: %.logo
	./logocc $<

%.exe: %.s
	./vm/stack-as $<

run-%: progs/%.exe
	./logoapp $<

code-%: code/%.exe
	./logoapp $<

clean-%:
	rm -f progs/$*.s progs/$*.exe

.PRECIOUS: %.s %.exe


# packing
PACK_NAME = logo
PACK_DIR = $(PACK_NAME)
PACK_ARC = ../logo-$(DATE).tgz
PACK_DATA = \
	Makefile \
	pages \
	doc
PACK_CLEAN = vm http-lib
PACK_FLAGS = --exclude=".git*"
PACK_FILTER=\
	$(filter-out parser.ml lexer.ml,$(LOGOCC_SOURCES)) \
	$(LOGOAPP_SOURCES) \
	$(LOGORUN_SOURCES) \
	$(wildcard code/*.s) \
	parser.mly lexer.mll

pack: pack-dir
	tar cvfz $(PACK_ARC) $(PACK_FLAGS) $(PACK_DIR)

pack-test: pack-dir
	cd $(PACK_DIR); make

pack-dir:
	-rm -rf $(PACK_DIR)
	mkdir $(PACK_DIR)
	for f in $(PACK_DATA); do cp -R $$f $(PACK_DIR); done
	for d in $(PACK_CLEAN); do cp -R $$d $(PACK_DIR); make clean -C $(PACK_DIR)/$$d; done
	mkdir $(PACK_DIR)/code
	mkdir $(PACK_DIR)/progs
	for f in $(PACK_FILTER); do ./filter.py < $$f > $(PACK_DIR)/$$f; done
	cp progs/*.logo $(PACK_DIR)/progs

# distribution
dist:
	-rm -rf $(PACKDIR)
	make clean
	cd ..; tar cvfz logo-dist-$(DATE).tgz $(PACK_FLAGS) logo

# delivery
DELIV_SOURCES = \
	lexer.mll \
	parser.mly \
	comp.ml
tp1:
	tar cvfz tp1-$(DATE).tgz code/*.s

tp2:
	tar cvfz tp2-$(DATE).tgz $(DELIV_SOURCES)

tp3:
	tar cvfz tp3-$(DATE).tgz $(DELIV_SOURCES)

tp4:
	tar cvfz tp4-$(DATE).tgz $(DELIV_SOURCES)
