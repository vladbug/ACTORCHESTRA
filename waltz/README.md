# WALTZ Specification Language
WALTZ is a specification logic tailored for the actor-based model that allows the specification of properties that span across multiple actor interactions.
## Overview

WALTZ allows you to specify and verify properties about message exchanges between actors. You can express patterns like "actor A sends message X to actor B, then B responds with message Y", along with constraints on the message contents.

## Quick Examples

### Simple message constraint
```
send {client -> server} WITH {request} : TOP
```
Specifies that client sends a message with pattern `request` to server.

### Message with data constraints
```
send {sensor -> controller} WITH {temp, T} : T > 25 AND T < 100
```
Sensor sends temperature reading where T must be between 25 and 100.

### Sequence of messages (protocol)
```
send {client -> server} WITH {login, User} : TOP ;
send {server -> client} WITH {ack} : TOP
```
Client logs in, then server acknowledges.

### Modal Operators
All WALTZ properties must be encapsulated within one of the modal operators defined for verification purposes.
```
OMEGA(
  send {client -> server} WITH {login, User} : TOP ;
  send {server -> client} WITH {ack} : TOP
)
```
All the contexts in the system, after a sucesefull login must be acknowledged by the server.

### Existential monitoring
```
THETA(send {user -> server} WITH {send_number, N} : N == 10)
```
Monitors for the existence of a message from a user to the server (one context) with a number equal to 10.

---

## Language Reference

### Message Signatures

The core construct is a message signature with a constraint:

```
send {source -> destination} WITH {pattern} : constraint
```

**Components:**
- `source` and `destination`: Actor names (lowercase identifiers)
- `pattern`: Describes the expected message structure
- `constraint`: Boolean expression that must hold

### Patterns

Patterns match against message contents:

| Pattern | Description | Example |
|---------|-------------|---------|
| `atom` | Exact match (lowercase) | `login`, `request`, `ack` |
| `Variable` | Binds value (uppercase) | `X`, `Value`, `UserId` |
| `_` | Wildcard, matches anything | `_` |
| `{p1, p2, ...}` | Tuple/list pattern | `{request, User, _}` |

**Examples:**
```
{login}              # Matches message containing just "login"
{request, X}         # Binds X to second element
{msg, _, Y}          # Ignores second element, binds third to Y
{cmd, {X, Y}}        # Nested pattern
```

### Constraints

Constraints are boolean expressions over variables bound in patterns:

#### Literals
- `42`, `100` — Integer numbers
- `TOP` — Always true (⊤)
- `BOTTOM` — Always false (⊥)

#### Arithmetic Operators
```
+  -  *  /
```

#### Comparison Operators
```
==   =/=   >   <   >=   =<
```

#### Logical Operators
```
AND        # Disjunction
OR         # Conjunction
not        # Negation (also: ¬)
```

**Examples:**
```
X > 10
X + Y == 100
(X > 0 AND X < 100) OR (Status == active)
not (Value =/= 42)
```

### Formula Composition

Combine specifications using temporal and modal operators:

#### Sequence (`;`)
```
formula ; formula
```
First formula must occur before the second.

```
send {a -> b} WITH {req} : TOP ;
send {b -> a} WITH {resp} : TOP
```

#### Recursive Context (`OMEGA`)
```
OMEGA(formula)
```
Check whether the property holds in all the contexts of the system.

```
OMEGA(send {client -> server} WITH {number, N} : N > 10)
```

#### Existential Monitoring (`THETA`)
```
THETA(formula)
```
Property must hold in some context.

```
THETA(send {client -> server} WITH {number, N} : N > 10)
```

Compared to the `OMEGA` property, in this one it is sufficient to observe one message from
`client` to `server` with signature `{number, N}` where `N` is greater than `10`. While in the 
`OMEGA` all such messages must adhere to the boolean constraint.

---

## Complete Grammar

For parser implementation reference:

```ebnf
(* Entry point *)
program ::= formula

(* ATL Formulas *)
formula ::=
          | signature : constraint_expr
          | formula ; formula
          | OMEGA ( formula )
          | THETA ( formula )
          | ( formula )

(* Message signature *)
signature ::= send { LIDENT -> LIDENT } WITH { pattern }

(* Patterns *)
pattern ::= basic_pattern
          | pattern_list

basic_pattern ::= _                    (* wildcard *)
                | LIDENT               (* atom *)
                | UIDENT               (* variable *)
                | { pattern }

pattern_list ::= basic_pattern , basic_pattern
               | basic_pattern , pattern_list

(* Constraint expressions *)
constraint_expr ::= TOP | BOTTOM | BOOL | LIDENT | UIDENT | NUM
                  | constraint_expr (+ | - | * | /) constraint_expr
                  | constraint_expr (== | =/= | > | < | >= | =<) constraint_expr
                  | constraint_expr (andalso | orelse) constraint_expr
                  | not constraint_expr
                  | ( constraint_expr )

(* Lexical *)
LIDENT ::= [a-z][a-zA-Z0-9_]*          (* lowercase identifier *)
UIDENT ::= [A-Z][a-zA-Z0-9_]*          (* uppercase identifier *)
NUM    ::= [0-9]+
BOOL   ::= true | false
```

## Common Patterns

### Request-Response
```
send {client -> server} WITH {request, ReqId} : TOP ;
send {server -> client} WITH {response, ReqId} : TOP
```

### Three-way Handshake
```
send {a -> b} WITH {syn} : TOP ;
send {b -> a} WITH {synack} : TOP ;
send {a -> b} WITH {ack} : TOP
```

### Calculator interaction
```
send {client -> server} WITH {A, B, add} : A > 0 AND B > 0;
send {server -> calc_add} WITH {A, B} : TOP
```

### Monitoring over all contexts
```
OMEGA(
  send {a -> b} WITH {c, d, W} : W >= 0 ;
  send {b -> c} WITH {e, K} : K == 2*W;
  send {c -> d} WITH {f, A} : A == K;
)
```

### Monitoring for one context 
```
THETA(
  send {a -> b} WITH {c, d, W} : W == 0
)
```

---

## Syntax Highlighting

For syntax highlighting in editors, treat:
- **Keywords:** `send`, `WITH`, `OMEGA`, `THETA`, `TOP`, `BOTTOM`, `true`, `false`, `not`, `AND`, `OR`
- **Operators:** `->`, `:`, `;`, `==`, `=/=`, `>`, `<`, `>=`, `=<`, `+`, `-`, `*`, `/`
- **Delimiters:** `{`, `}`, `(`, `)`, `,`
- **Variables:** Uppercase identifiers (e.g., `X`, `Value`)
- **Atoms:** Lowercase identifiers (e.g., `request`, `login`)
- **Literals:** Numbers and booleans

---

## Implementation Notes

- Built using OCaml with ocamllex (lexer) and menhir (parser)
- Source files: `lexer.mll`, `parser.mly`, `expressions.ml`
- Pattern matching is structural — variables bind to values, atoms must match exactly
- All patterns in `WITH {}` are treated as tuples

---
