![Nix CI](https://github.com/pnotequalnp/ordinis/actions/workflows/nix.yml/badge.svg)

# ordinis
Ordinis, Latin for row. The idea is a simple ML-inspired pure FP language with heavy inspiration
from Haskell and OCaml. It doesn't work at all yet and it's just a fun project, not intended for
actual usage.

## Features
* Lexer
* Parser (somewhat)

## Planned Features (subject to drastic change)
* Extensive type inference
* Impredicative polymorphism with quick look
* Row polymorphism supporting extensible records and polymorphic variants
* No I will not provide non-Unicode equivalents
* I/O primitives similar to Haskell
* Equirecursive types
* Existential quantification
* Pseudo-typeclasses

## Example (doesn't work yet and will probably change over time)
```
id : ∀a. a -> a
id x = x

(* This is just a type alias, the angle brackets are sugar
 * for `Variant ( nothing, just : a )` where `Variant : Row -> Type`.
 * The lack of type on `Nothing` is implicitly the unit type.
 *)
type Maybe a = 〈 nothing, just : a 〉

maybe : ∀a b. b -> (a -> b) -> Maybe a -> b
maybe z _ 〈 nothing 〉 = z
maybe _ f 〈 just = x 〉 = f x

foo : Maybe String
foo = 〈 just = "Hello" 〉

(* `(.length)` is a projection like the new Haskell record syntax *)
bar : UInt64
bar = maybe 42 (.length) foo

(* The braces are likewise sugar for `Record : Row -> Type`. The `r`
 * is a row variable allowing for extension.
 *)
type User r = { username : String, accountId : UInt64 | r }

jimmy : User ( password : String )
jimmy = { username = "Jimmy", accountId = 42, password = "hunter2" }

(* `( )` is the empty `Row` *)
stripPassword : ∀a r. { password : a | r } -> { ( ) | r }
stripPassword x = { x \\ password }

jimmyNoPassword : User ( )
jimmyNoPassword = stripPassword jimmy

(* The empty record is the unit type *)
type Unit = { }

unit : Unit
unit = { }

(* The empty variant is the empty type *)
type Void = 〈 〉

(*
 * bottom : Void
 * bottom = (* impossible *)
 *)
 
(* Arbitrary nesting of types *)
type List a = 〈 nil, cons : { head : a, tail : List a }

(* Syntax sugar for list-like types *)
evens : List Nat
evens = [ 0, 2, 4, 6, 8 ]

(* Record access with dot syntax, custom operators *)
sum : List Nat -> Nat
sum 〈 nil 〉 = 0
sum 〈 cons = xs 〉 = xs.head + sum xs.tail

(* Existential quantification *)
things : [ ∃a. { show : a -> String, val : a } ] (* Brackets are sugar for the array type *)
things = [ { show = printInt64, val = 42 }, { show = id, val = "Hello there!" } ]

(* Do notation *)
printThings : [ ∃a. { show : a -> String, val : a } ] -> IO { }
printThings 〈 nil 〉 = pure { }
printThings 〈 cons = xs 〉 = do
  printLn (xs.head.show xs.head.val)
  printThings xs.tail
```
