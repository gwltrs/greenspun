# Why Greenspun?

I want the simplicity and low-level control of C, Lisp syntax and macros, and the Haskell-inspired typing of Swift in one language.

# Reference

## Forms

### Fun

Defines a function. 

```
(fun I32 main (I32 count Str* args)
    (return 0)
)
```
Functions with a return type must ```return``` (or ```abort```) in all branches. However, the final ```return``` can be implicit if the function ends with an expression that resolves to a value.
```
(fun F32 clamp (F32 value F32 min F32 max)
    (if (< value min) (return min))
    (if (> value max) (return max))
    value
)
```
Functions that don't return a value can still use ```(return)``` to exit the function early.
```
(fun Void push_to_main_on_april_1st ()
    (if (> (rand) 0.001) (return))
    (abort)
)
```
Functions that don't return a value can omit both the return type and the return statement.
```
(fun do_nothing ())
```

Binary functions that have arguments of different types can opt in to being flippable with ```:flip```, allowing them to be called with the arguments reversed.

```
(fun :flip I32 scale_vec2 (Vec2 v F32 f)
    (make_vec2 (* v.x f) (* v.y f))
)
(= secret_base_location (scale_vec2 4.0 secret_base_location))
```

Binary functions with a return type that matches the type of the first parameter can opt in to being a folding function with ```:fold```, allowing them to be called in a variadic manner. Under the hood, the      Other folding overrides of that function will be searched formay be use

```
(fun :fold I32 mult (I32 a I32 b) (* a b))
(var I32 factorial_of_7 (mult 1 2 3 4 5 6 7))
```

### Let

Sequentially binds the given name-value pairs and evaluates to the last expression given.

```(let s (lambda x (* x x)) as (s a) bs (s a) (sqrt (+ as bs)))```

The variables are mutable.

```(let x 0 _ (++ x) _ (++ x) x)```

Let forms with no bindings are permitted.

```(sqrt (let 3.14))```

### Var
Creates mutable variable(s).

```(var I32 x 0)```

Multiple variables can be defined at once, all sharing the same type and initial value.

```(var I32 x y z 0)```

Either the type or the initial value (but not both) can be omitted. If the initial value is omitted, the variables get set to the default value for built-in types and ```(default)``` for custom types.

```(var I32 x y z)```

```(var x y z 0)```

Initialization can be opted out of.

```(var I32 x :no_init)```

Greenspun improves upon C's multiple variable syntax as all variables below (not just the first) are pointers.

```(var I32* x y z)```



























