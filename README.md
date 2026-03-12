# Why Greenspun?

I want the simplicity and low-level control of C, Lisp syntax and macros, and the Haskell-inspired typing of Swift in one language.

# FAQs

Rather than enumerate the design decisions behind the language, I'll present them through a fictitious FAQ.

### Why doesn't this "Lisp" have feature X?

I'll counter with this: why don't more languages just use Lisp syntax and reap the low-hanging benefits of easy parsing and macros without feeling the obligation to adopt and implement the entire paradigm? I've been asking myself this question since becoming an obsessive reader of Paul Graham essays. This is what I'm exploring with Greenspun and I see it less as a Lisp dialect and more as C with Lisp syntax. This is especially exciting because I feel like low-level, performance-cognizant languages have the most to gain from Lisp macros. So in regards to feature X, I might implement it in the future, but the main things I want from Lisp are compile-time macros and runtime evals.

### Operators?! In a Lisp?!

In general, the reason why I have an unusual feature is also [why I'm missing standard Lisp features](#why-doesnt-this-lisp-have-feature-x). In regards to operators, when I first started learning about Lisp, I fell in love with the naming flexibility and started writing a bunch of little functions like ```0?```. In hindsight, ```(is_zero x)``` instead of ```(0? x)``` isn't the devastating loss of brevity I might have once thought. So if we give up the symbol-number-letter combinations in our variable and function names, which isn't really pushing the expressiveness needle forward that much, what can we get in return? We get operators. With strict operator rules, we get to do C-style things like ```-box.pos.x``` while not interfering with macros.

### Why is everything named in a verbose manner?

Let's take ```function``` for example. In a previous Lisp I implemented, I leaned heavily into concise variable names. ```function``` was ```fn```, ```double``` was ```*2```, ```last``` was ```@-1```. I felt like I had crammed all my functions into the combinatorial space of 2-3 character names. Heaven on earth achieved. And then the unthinkable happened. I kept having to look up names. Was it ```fn``` (**Rust**), or ```fun``` (**Kotlin**), or ```func``` (**Swift**), or ```defn``` (**Clojure**), or ```defun``` (**Common Lisp**), or ```function``` (**JavaScript**), or even ```->```? (a symbol I'd considered) I discovered that once abbreviations are fair game, the increased difficulty of remembering what compression I'd settled on erased any time-saving gains I got from the short identifiers when typing them out. Another benefit we get from full-word global identifiers is that it keeps the namespace uncluttered for local variables, which I think should be only one or a few characters long. Lastly, wordy form identifiers make the code nicer to read; I find it easier to ascertain the shape of the file with ```function``` and ```operator``` popping out at me rather than having to squint my eyes at ```fn``` and ```op```. However, I won't be un-abbreviating standard C functions and operators; I'll be keeping ```+``` instead of ```add```, ```<``` instead of ```less_than```, and ```ceil``` instead of ```ceiling```. 

### Why is the type inference so bad?

For similar reasons to [why everything is named in a verbose manner](#why-is-everything-named-in-a-verbose-manner). Omitting types doesn't personally save me time in the long run. I lean on type-driven development so much that when types are inferred, I find that much of my headspace is consumed by my anxiety- and curiosity-driven desire to know the concrete types of the data I'm working with. In addition, when I have a confusing compile-time type error that I'm not able to easily resolve because I'm in a language that champions type inference (like Haskell), I find that adding optional type annotations to the problem code either clarifies my confusion quickly or at least ends up being a time-saving precursor to deeper debugging. This raises the question: why not just add the types in the first place?

# Reference

## Forms

### Function

Defines a function. 

```
(function I32 main (I32 count Str* args)
    (return 0)
)
```
Functions with a return type must ```return``` (or ```abort```) in all branches. However, the final ```return``` can be omitted the function ends with an expression that evaluates to a value.
```
(function F32 clamp (F32 value F32 min F32 max)
    (if (< value min) (return min))
    (if (> value max) (return max))
    value
)
```
Functions that don't return a value can still use return.
```
(function Void push_to_main_on_april_1st ()
    (if (> (rand) 0.001) (return))
    (abort)
)
```
Functions that don't return a value can omit both the return type and return statement.
```
(function do_nothing ())
```
```abort``` can be helpful when prototyping function signatures.
```
(function [T] flat ([[T]] arrays) (abort))
(function [T] flat4 ([[[[T]]]] arrays)
    (flat (flat arrays))
)
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




















