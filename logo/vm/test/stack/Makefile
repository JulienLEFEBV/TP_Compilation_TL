
OCAMLC_FLAGS = -I ../..

TESTS = \
	count.s \
	meta.s \
	call.s \
	fact.s
EXES = $(TESTS:.s=.exe)

all: mystackvm $(EXES)
CLEAN += $(EXES)

mystackvm: ../../vm.cma mystackvm.cmo
	$(OCAMLC) $^ -o $@
CLEAN += mystackvm mystackvm.cmo mystackvm.cmi

mystackvm.cmo: ../../vm.cma

%.exe: %.s
	../../stack-as $<

include ../../Makefile.std
