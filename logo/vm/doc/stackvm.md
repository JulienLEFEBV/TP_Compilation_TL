# Stack Machine

The stack machine executes instructions by storing arguments and results in a stack.


## Machine State

It uses 3 registers :

* **PC** (initial 0 or "_start")-- programm counter (current instruction in the code memory)
* **SP** (initial 0)-- top of the stack in the top memory (ascending empty)
* **FP** (initial 0)-- start of the stack for a function frame (where are found local variables)

Below are presented the instructions. First the instruction is presented with at most 1 argument and the top of the stack. Semantics of the instruction is presented with the effect on the stack and/or the registers.

We denote by mem the memory as an array of integers starting at index 0. The stack is put at the start of the memmory.

The instructions are described below:
* first line: instruction name and argument, initial state of the stack (before SP).
* second line: register modification and new state of the stack.

**ADD** 				|| X::Y::S
	=>	PC <- PC +1		|| (X+Y)::S

**SUB** 				|| X::Y::S
	=>	PC <- PC +1		|| (X-Y)::S

**MUL** 				|| X::Y::S
	=>	PC <- PC +1		|| (X*Y)::S

**DIV** 				|| X::Y::S
	=>	PC <- PC +1		|| (X/Y)::S

**MOD** 				|| X::Y::S
	=>	PC <- PC +1		|| (X%Y)::S

**PUSH** k				|| S
	=> PC <- PC + 1		|| k::S

**GET** k				|| S
	=> PC <- PC + 1		|| mem[FP + k]::S

**SET** k				|| X::S
	=> PC <- PC + 1		|| S
	=> mem[FP + i] <- x

**GOTO** k				|| S
	=> PC <- k			|| S

**GOTO_EQ** k			|| X::Y::S
	=> PC <- if X = Y then k else PC+1	|| S

**GOTO_NE** k			|| X::Y::S
	=> PC <- if X != Y then k else PC+1	|| S

**GOTO_LT** k			|| X::Y::S
	=> PC <- if X < Y then k else PC+1	|| S

**GOTO_LE** k			|| X::Y::S
	=> PC <- if X <= Y then k else PC+1	|| S

**GOTO_GT** k			|| X::Y::S
	=> PC <- if X > Y then k else PC+1	|| S

**GOTO_GE** k			|| X::Y::S
	=> PC <- if X >= Y then k else PC+1	|| S

**INVOKE** k			|| S
	=> system call; PC <- PC + 1

**STOP**				|| S
	=>					|| S

**RESERVE** k			|| S
	=> PC <- PC + 1		|| [0]*::S

**CALL** k					|| S
	=> PC <- k; FP <- SP 	|| PC::FP::S

**RETURN_VOID** k							|| ... ::X::Y::Zk-1 .. Z::0::S
	=> SP <- FP-k-2; PC <- X; FP <- Y		|| S

**RETURN** k								|| X::...::Y::Z::Tk-1:: ... ::T0::S
	=> SP <- FP-k-2; PC <- Y; FP <- Z		|| X::S

**GET_GLOB** k			|| S
	=> PC <- PC + 1		|| mem[k]::S

**SET_GLOB** k				|| X::S
	=> PC <- PC + 1		|| S
	=> mem[i] <- x


## System and subprogram calls

The _system call_ may update the state of the system (FP, PC, SP and stack) as it needs. This allows to provide system functions to be called by the executed program.

Local variables are stored in the stack after a call by a **RESERVE** _k_ with _k_ the number of local variables. Local variables are number from 0 to _k_-1 and accessed with **GET** and **SET** commands.

Parameters can be pushed in the stack and are numbered as local variables but with a negative number starting at -3. -3 is for first parameter, -4 for second, etc.

To maintain classic stack work, a sub-program call not only pop PC, FP and local data but also the following parameters. This is why the `return` instructions takes an additional parameter _k_ to pop the subprogram argument.


System call are implemented as function that takes as parameter the VM state, the command (parameter of **INVOKE**) and its own generic state. This function is allowed to change VM state and has to return its own generic state, possibly updated. The meaning of the commands and the stacked required arguments depends on the system call itself that is in charge of popping the argument and pushing the results in the stack.


## Assembly

The following syntax is supported in the assembler:
* @ for comment until end of line,
* **PUSH** #k for constant
* **GET** and **SET** supports `Ri` for their argument (_i_ positive or negative).
* `ID:` to define a label then used in **GOTO**, **GOTO_cnd** instruction as _ID_.
* Basically, the program starts with label `_start`. If not provided, starts at address 0.


## Meta-information

Basically, the instructions can only handle integer values. To support special values (boolean float, string, etc but also labels) that are usually passed to system, there is a special table, _meta-table_, containing these values. Content of this table is declared with special `.meta` directives:

```
.meta	0	true
.meta	1	666
.meta	2	111.666
.meta	3	"Hello, World!"
.meta	4	_end
```

The items associates an index with a _meta-value_. For the system to access a _meta-value_, the index in the table has to be passed in the system call arguments.

