# Emil

A pascal compiler, named after the legend Niklaus Emil Wirth.

# Plans/Ideas

* [ ] Implement parser and semantic analysis based on the ISO 7185:1990 standard and Doug Cooper\'s 'Standard Pascal -- User Reference Manual'
* [ ] Use LLVM through inkwell for the backend, maybe consider Cranelift
* [ ] Work on a backend written from scratch, with the help of Cooper & Torczon's 'Engineering a Compiler'
* [ ] Focus on making compiler a "complying processor", enforcing the full range of errors correctly
  * [ ] Enhance diagnostics, using standard terminology where it isn't needlessly confusing/complex
* [ ] Begin extending the implementation with \[gated\] usability features, choosing from from the following:
  * Extended Pascal per ISO 10206:1990:
    * [ ] Modules/interfaces and separate compilation
    * [ ] Schemata - seem a little complex, may be out-of-scope for this project
    * [ ] String enhancements, including variable length strings
    * [ ] Constant expressions
    * [ ] Better file I/O
    * [ ] Structured value constructors - Array/record/set literals
    * [ ] Initial variable state
    * [ ] Case statements/variant records can have ranges in the case patterns
    * [ ] snake_case identifiers
    * [ ] More numeric literal styles
    * [ ] Ability to supply expressions in subrange bounds
  * UCSD Pascal:
    * More research required
  * Modula-2:
    * Modules obviously, but further research required
  * Oberon:
    * Don't even know where to look 😅
