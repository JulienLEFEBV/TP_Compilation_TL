# Syntax of Logo Language

## Lexical

Delimiters encompasses spaces, tab and new line.

Infix operators: +, -, *, /, < >, <=, >=, =.

Other delimiters:
* " - string delimired,
* : name introduction,
* [] for lists
* () for computatutaion grouping,

* `;` ... EOL -- comment


## Graphics Instructions

* `FORWARD`/`FD` _length_
* `BACKWARD`/`BK` _length_
* `RIGHT`/`RT` _angle_
* `LEFT`/`LT` _angle_
* `PENUP`/`PU`
* `PENDOWN`/`PD`
* `HOME`
* `CLEARSCREEN`/`CS`
* `SETPENCOLOR`/`SETPC` _color_
* `SETPENSIZE` _size_
* `FILL`
* `SHOWTURTLE`/`ST`
* `HIDRTUTRLE`/`HT`
* `PRINT` _expression_


## Control Instructions

* `REPEAT` _count_ `[` _instructions_ `]`
* `IF`_condition_ `[`_instructions_ `]`
* `IFELSE` _condition_ `[` _instructions_ `]` `[` _instructions_ `]`
* `MAKE` `"`_identifier_ _expression_


## Expressions

* `:`_identifier_
* `THING` `"`_identifier_
* `READWORD` -- read a word from user interface


## Procedures

* `TO` _identifier_ (`:`_parameter)*
	_instructions_
	`END`
* _identfier_ _expression_*


## References

* [Berkeley Logo 6.2 User Manual](https://people.eecs.berkeley.edu/~bh/docs/html/usermanual.html?utm_source=chatgpt.com)
* [Logo Language Syntax](https://pclogo.fandom.com/wiki/Logo_Language_Syntax?utm_source=chatgpt.com)
* https://cheatsheetshero.com/user/all/369-logo-programming-language-cheatsheet
* https://programming.muthu.co/posts/beginners-guide-to-logo/#basic-commands-and-concepts
