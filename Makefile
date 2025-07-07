all: vm vm_dbg
extra: compiler

vm: wim.c mmap_file.c wim.h
	gcc wim.c mmap_file.c -o vm

vm_dbg: wim.c mmap_file.c wim.h
	gcc -DSTEPDEBUG wim.c mmap_file.c -o vm_dbg

compiler: Autoserialize.hs Compile.hs Parse.hs Compiler.hs
	ghc -dynamic Compiler.hs -o $@

#assembler: assembler.o
#	gcc -o assembler assembler.c

clean:
	rm -f assembler compiler vm vm_dbg $(wildcard *.o) $(wildcard *.hi)
