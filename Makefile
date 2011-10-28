DESTDIR=/users/tomhayas
CIL=cil
ARCH=x86_LINUX
RESULT = cLister.bin
SOURCES = calleeprinter.ml funlister.ml callerprinter.ml funnamelister.ml \
	  dotcallerprinter.ml main.ml
INCDIRS = $(CIL)/obj/$(ARCH)
OCAMLBLDFLAGS = unix.cma str.cma nums.cma $(CIL)/obj/$(ARCH)/cil.cma 
OCAMLNLDFLAGS = unix.cmxa str.cmxa nums.cmxa $(CIL)/obj/$(ARCH)/cil.cmxa
OCAMLMAKEFILE = OCamlMakefile

#install:
#	cp -f cLister.bin $(DESTDIR)/usr/bin/
#	cp -f cLister.sh $(DESTDIR)/usr/bin/cLister

include $(OCAMLMAKEFILE)

