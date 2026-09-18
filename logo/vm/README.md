# VM

VM is a small OCAML library aiming to implement stack and register _virtual machines_ .
It comes with its own virtual machine, assembler and disassembler.

## Requirements

* [OCAML](https://ocaml.org/)
* [GNU Make](https://www.gnu.org/software/make)

## Building it

Move in the project directory and type:

```sh
$ make
```

## Using it

Let the code below (`first.s`):

```
_start:
	stop
```

You can use `stack-as` to assemble stack VM instructions:

```sh
$ stack-as first.s
```

Or to disassemble:

```sh
$ stack-disasm first.exe
```

You can use VM library to run your own stack machine with, for example, the
following code (`myvm.ml`):

```ocaml

let _ =
	let prog = Stackinst.load "first.exe", in
	let state = Stackvm.new_state prog (fun _ -> ()) () in
	Stackvm.run state
```

And compile it with:

```sh
$ ocaml -I vm vm/vm.cma myvm.ml -o myvm
```

And run it with:
```
$ ./myvm
```






