# Why Greenspun?

I want the simplicity and low-level control of C, Lisp syntax and macros, and the Haskell-inspired typing of Swift in one language.

# FAQs

Rather than enumerate the design decisions behind the language, I'll present them through a fictitious FAQ.

### Why doesn't this "Lisp" have feature X?

I'll counter with this: why don't more languages just use Lisp syntax and reap the low-hanging benefits of easy parsing and macros without feeling the obligation to adopt and implement the entire paradigm? I've been asking myself this question since becoming an obsessive reader of Paul Graham essays. This is what I'm exploring with Greenspun and I see it less as a Lisp dialect and more as C descendant with Lisp syntax. So in regards to feature X, I might implement it in the future, but the main things I want from Lisp are compile-time macros and runtime evals.

### Operators?! In a Lisp?!

Yup. When I first started learning about Lisp, I fell in love with the naming flexibility and started writing a bunch of little functions like ```0?```. In hindsight, ```(is_zero x)``` instead of ```(0? x)``` isn't the devastating loss of brevity I might have once thought. So if we give up the symbol-number-letter combinations in our variable and function names, which isn't really pushing the expressiveness needle forward that much, what can we get in return? I think the answer is operators.

### Why is everything named in a verbose manner?

Let's take ```function``` for example. In a previous Lisp I implemented, I leaned heavily into concise variable names. ```function``` was ```fn```, ```double``` was ```*2```, ```last``` was ```@-1```. I felt like I had crammed all my functions into the combinatorial space of 2-3 character names. Heaven on earth achieved. And then the unthinkable happened. I kept having to look up names. Was it ```fn``` (**Rust**), or ```fun``` (**Kotlin**), or ```func``` (**Swift**), or ```defn``` (**Clojure**), or ```defun``` (**Common Lisp**), or ```function``` (**JavaScript**), or even ```->```? (a symbol I'd considered) I discovered that once abbreviations are fair game, the increased difficulty of remembering what compression I settled on erased any time-saving gains I got from the shorter identifiers when typing them out. However, I think that 

Another positive property we get from full-name global identifiers is that it cleares out the namespace for local variables, which I think often should be only one or a few characters long as 

I also find that verbose top-level form identifiers are nice as they easier to recognize helps be I also enjoy longer top-level form identifiers as 

That being said, (sticking to C names)

and 

I property that I recently articualted is the ability the quickly ascertain the shape of a file

# Reference



## Forms

### Function

```
(function map ()
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

Either the type or the initial value (but not both) can be omitted. If the initial value is omitted, the variables get set to the default value for that type.

```(var I32 x y z)```

```(var x y z 0)```

Greenspun improves upon C's multiple variable syntax as all variables below (not just the first) are pointers.

```(var I32* x y z)```






