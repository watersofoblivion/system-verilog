System Verilog
===

A [IEEE 1800-2012](http://www.ece.uah.edu/~gaede/cpe526/2012%20System%20Verilog%20Language%20Reference%20Manual.pdf)-compliant System Verilog compiler.

Goals:

* Parsing
* Type Checking
* Elaboration
* (Basic) Simulation
* Formal Verification (with [Alt-Ergo](https://alt-ergo.ocamlpro.com/))

Completeness
---

This documents the completeness of the implementation in terms of the specification.

### Part 1: Design and Verification Constructs

* Clause 1: Overview :white_check_mark:
* Clause 2: Normative References :white_check_mark:
* Clause 3: Design and Verification Building Blocks (In-Progress)
  * Supports all files and individual files as compilation unit modes (3.12.1)
  * (TODO) `$unit` scope (3.12.1)
  * (TODO) Namespaces (3.13): Will implement as needed when implementing passes
  * (TODO) `\`timescale` compiler directive (3.14.2.1)
* Clause 4: Scheduling Semantics (TODO)
* Clause 5: Lexical Conventions (TODO)
* Clause 6: Data Types (TODO)
* Clause 7: Aggregate Data Types (TODO)
* Clause 8: Classes (TODO)
* Clause 9: Processes (TODO)
* Clause 10: Assignment Statements (TODO)
* Clause 11: Operators and Expressions (TODO)
* Clause 12: Procedural Programming Statements (TODO)
* Clause 13: Tasks and Functions (Subroutines) (TODO)
* Clause 14: Clocking Blocks (TODO)
* Clause 15: Interprocess Synchronization and Communication (TODO)
* Clause 16: Assertions (TODO)
* Clause 17: Checkers (TODO)
* Clause 18: Constrained Random Value Generation (TODO)
* Clause 19: Functional Coverage (TODO)
* Clause 20: Utility System Tasks and System Functions (TODO)
* Clause 21: Input/Output System Tasks and System Functions (TODO)
* Clause 22: Compiler Directives (TODO)

### Part 2: Hierarchy Constructs

* Clause 23: Modules and Hierarchy (TODO)
* Clause 24: Programs (TODO)
* Clause 25: Interfaces (TODO)
* Clause 26: Packages (TODO)
* Clause 27: Generate Constructs (TODO)
* Clause 28: Gate-Level and Switch-Level Modeling (TODO)
* Clause 29: User-Defined Primitives (TODO)
* Clause 30: Specify Blocks (TODO)
* Clause 31: Timing Checks (TODO)
* Clause 32: Backannotation Using the Standard Delay Format (TODO)
* Clause 33: Configuring the Contents of a Design (TODO)
* Clause 34: Protected Envelopes (TODO)

### Part 3: Application Programming Interfaces

* Clause 35: Direct Programming Interface (TODO)
* Clause 36: Programming Language Interface (PLI/VPI) Overview (TODO)
* Clause 37: VPI Object Model Diagrams (TODO)
* Clause 38: VPI Routine Definitions (TODO)
* Clause 39: Assertion API (TODO)
* Clause 40: Code Coverage Control and API (TODO)
* Clause 41: Data Read API (TODO)

### Part 4: Annexes

* Annex A: Formal Syntax (TODO)
* Annex B: Keywords (TODO)
* Annex C: Deprecation (TODO)
* Annex D: Optional System Tasks and System Functions (TODO)
* Annex E: Optional Compiler Directives (TODO)
* Annex F: Formal Semantics of Concurrent Assertions (TODO)
* Annex G: Std Package (TODO)
* Annex H: DPI C Layer (TODO)
* Annex I: `svdpi.h` (TODO)
* Annex J: Inclusion of Foreign Language Code (TODO)
* Annex K: `vpi_user.h` (TODO)
* Annex L: `vpi_compatibility.h` (TODO)
* Annex M: `sv_vpi_user.h` (TODO)
* Annex N: Algorithm for Probabilistic Distribution Functions (TODO)
* Annex O: Encryption/Decryption Flow (TODO)
