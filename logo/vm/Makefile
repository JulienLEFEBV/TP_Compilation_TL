
# Definitions
SOURCES = \
	common.ml \
	stackinst.ml \
	stackvm.ml \
	stackparser.ml \
	stacklexer.ml
OBJECTS = $(SOURCES:.ml=.cmo)
SUBDIRS = test/stack


# Rules
all: vm.cma stack-as stack-disasm
	@for d in $(SUBDIRS); do \
		cd $$d; \
		$(MAKE) all; \
		cd ..; \
	done

vm.cma: $(OBJECTS)
	$(OCAMLC) -a $(OBJECTS) -o $@
CLEAN += $(OBJECTS) vm.cma $(SOURCES:.ml=.cmi) stackparser.output

stackinst.cmo: common.cmo
stackvm.cmo: stackinst.cmo
stackparser.cmo: stackparser.cmi stackinst.cmo
stacklexer.cmo: stackparser.cmo
vm.cma: $(OBJECTS)

stack-as: vm.cma stack_as.cmo
	$(OCAMLC) -o $@ $^
stack_as.cmo: stackinst.cmo stackparser.cmi stacklexer.cmo stackparser.cmo
CLEAN += stack-as stack_as.cmo stack_as.cmi  stackparser.mli

stack-disasm: vm.cma stack_disasm.cmo
	$(OCAMLC) -o $@ $^
stack_disasm.cmo: stackinst.cmo
CLEAN += stack-disasm stack_disasm.cmo stack_disasm.cmi

include Makefile.std
