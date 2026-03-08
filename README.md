*At the moment, aether does not have full documentation and this file may contain inconsistencies/errors/lies
std in alpha-version, i will upload when it be beta*


# Aether-Language
  Aether is a stack-based, interpreted programming language implemented in Haskell. It features dynamic typing, first-class code blocks, file I/O, list manipulation, and a simple but expressive syntax.
## Features
  1. Stack-oriented execution – all operations work on a value stack.
  2. Rich data types: integers, strings, booleans, lists, and executable code blocks.
  3. Code as values – create code blocks with [[ ... ]], store them with weave, and evaluate dynamically with eval.
  4. Control structures: if/else and while loops.
  5. Built-in list operations: append, at, len, remove, set.
  6. String utilities: atStr, lenStr, slice.
  7. File I/O: readfile, writefile.
  8. Command-line arguments via the args instruction.

## Start
  1. ```ghc aether.hs -o aether``` or dowland aether (ELF)
  
  2. ```Bash \n./aether path/to/program.ath [arguments...]```

# Language Basics
## Stack and Memory
  Values are pushed onto a stack.
  -> name pops the top value and stores it in a variable.
  A variable name used as a token pushes its value onto the stack (or executes it if it holds a code block).

## Literals
  Integers: 42, -7
  Strings: "hello"
  Booleans: result of comparisons or logical ops (represented internally, no literal syntax)
  Empty list: []
  Code block: [[ instructions ]]

## Arithmetic and Comparison
Token |	Operation	| Stack effect

1. "+"   | Add or concatenate      | a b +
2. "-"   | Subtract (integers)     | a b -
3. "*"   | Multiply (integers)     | a b *
4. "/"	  | Integer division	      | a b /
5. ">"   | Greater than (integers) | a b >
6. "<"	  | Less than               | a b <
7. "=="  | Equal	                  | a b ==
8. "!="  | Not equal	              | a b !=
9. "&&"  | Logical AND (booleans)  | a b &&
10. "||"  | Logical OR (booleans)	  | a b ||
11. "not" |	Logical NOT	            | a not
12. "!!"  | Comment                 | ignore symbols in line after "!!"
13. "?"   | None                    | none

## Variables and Code Storage

Token |	Effect

-> name |	Pop top value and store in name
weave	| Pop name (string) and code (block), store code under that name
eval	| Pop a string, parse and execute it as Aether code

## Lists

  Token	| Stack effect | Description
  append | value list append → newlist  | Append value to list
  at	   | index list at → element      | Get element at index
  len    | list len → length	          | Length of list
  remove | index list remove → newlist  | Remove element at index
  set    | new index list set → newlist | Replace element at index

## Strings

Token | Stack effect | Description

atStr	 | index string atStr → char	        | Get character as one‑char string
lenStr | string lenStr → length	            | Length of string
slice	 | end start string slice → substring | Substring from start to end (exclusive)

## File I/O

Token |	Stack effect | Description

readfile  | path readfile → content  | Read file contents
writefile |	content path writefile → | Write content to file

## Command‑line Arguments

args pushes a list of strings containing the program arguments (excluding the program name).

## Documentation

For a detailed language reference, see aether_documentation.docx (in Russian).
