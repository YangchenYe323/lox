# crafting-interpreter ![test](https://github.com/YangchenYe323/lox/actions/workflows/test.yml/badge.svg)
My implementation of the lox programming language in rust following [Crafting Intepretors](https://craftinginterpreters.com/)

- [x] Scanning
- [x] Parsing ASTs
The grammar of my lox implementation has several differences with the book, some of which are extensions:
+ Automatic semicolon insertion: semicolons are automatically inserted at the end of statements if (i) the next token is on another line or (ii) the next token is EOF
+ If and loop conditions don't need to be in parenthesis and braces are not omittable (similar to rust's if/loop syntax rather than C's)
+ Break statement is suported.
+ C-like ternary expression is implemented.
+ Function formal parameters support default initialization: `fun foo(a = 1, b = "A") {}`, if no explicit default initialization is set, parameter is initialized to `nil` in cases no actual arguments is provided. 
+ Function can handle variable numbers of parameters, arity is not checked by the interpretor itself.
+ Print statement is replaced with a builtin print function which handles arbitrary number of arguments.
+ Shadow declaration is not an error:
```
var a = 1;
var a = 2; // OK
```
```
var a = 1;
var a = a + 1; // OK: a = 2
```
+ "this" is a valid identifier rather than a reserved keyword, code like `var this = 1` will compile. This identifier is only treated specially in that it will get bind to the object where a method is called. The binding rule is the same with the book, but users can overwrite the variable in the inner scope.

#### Tree Walk Interpretor:
- [x] Evaluating expressions
- [x] Evaluating statements and program state
- [x] Control flow and loops
- [x] Functions
- [x] Closures
- [x] Block level scoping rules
+ Block scope is implementd at runtime rather than compile time, using the aproach of persistent runtime scope chain (See [scope.rs](/rlox/src/interpreter/scope.rs))
- [x] Class
+ static member funtion is supported with the syntax:
```
class Foo {
  class staticMethod() {
  }
}
```
- [ ] inheritance and dynamic dispatch
- [x] An REPL terminal client with history support: `cargo run -p rlox`

#### Bytecode Virtual Machine
WIP
