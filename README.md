# Why Greenspun?

I want the simplicity and low-level control of C, Lisp syntax and macros, and the Haskell-inspired typing of Swift in one language.

# FAQs

Rather than enumerate the design decisions behind the language, I'll present them through a fictitious FAQ.

### Why doesn't this "Lisp" have feature X?

I'll counter with this: why don't more languages just use Lisp syntax and reap the low-hanging benefits of easy parsing and macros without feeling the obligation to adopt and implement the entire paradigm? I've been asking myself this question since becoming an obsessive reader of Paul Graham essays. This is what I'm exploring with Greenspun and I see it less as a Lisp dialect and more as C descendant with Lisp syntax. So in regards to feature X, I might implement it in the future, but the main things I want from Lisp are compile-time macros and runtime evals.

# Reference

## Forms

### Var
Creates mutable variable(s).

```(var I32 x 0)```

Multiple variables can be defined at once, all sharing the same type and initial value.

```(var I32 x y z 0)```

Either the type or the initial value (but not both) can be omitted. If the initial value is omitted, the variables get set to the default value for that type.

```(var I32 x y z)```

```(var x y z 0)```

Greenspun improves upon C's multiple variable syntax as all variables below are pointers.

```(var I32* x y z)```
