# Slang compiler tests

## File naming convention

* A tests: `a.chapter.subchapter.slang`
* B tests: `b.chapter.subchapter.slang`

## Questions

* Interface to compiler?
  * At least with a month
* What is specification?
  * There is only single document yet
* How to write tests according to specification?
  * Custom numeration or textual
* `raise` operator works with any type of operands?(string, exception, whatever) Or it passes string as message to Exception constructor?
  * no information about it
* What happens on `check` violation? Which exception is thrown?
  * Throws some standard library exception 
* Tuples named tuples as resulting type
  * (yes, it's not forbidden)
* passing tuples as parameters
  * Not now (maybe in future versions of language)
* Application.Exit vs return ?
  * Don't care
* what is `super` operator?

## Structure

* Declaration
  * variables
    * constructors (different variations) +
    * constructing with operator `as` ?
  * containers
    * as modules -
    * as types -
    * concurent -
    * abstract -
    * contracts -
    * as enums ?
    * generics -
  * functions +/-
    * pure/safe +
  * operator-functions
* Operators
  * ?, (math), in, ..., use - 
* Contracts -
* Generics -

## Don't forget to cover:

* Concurrency
* Inheritance | Multiinheritance
* alias
* Generics
* using `units` as 
* `mod` and `rem` ?
* `external` 
* external functions declaration
* testing standard library methods(math, Integer, etc)
* passing named tuples to functions
