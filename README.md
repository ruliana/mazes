# mazes
Mazes for Programmes in Guile Scheme

I have 3 goals doing this:
- Learn Guile Scheme. I'm particularly interested in FFI and multithreading. I think the C integration + Guix can bring the same reasoning to Guile as Clojure + Java. If we have an easy way to incorporate C code to Guile (and install C libs), that gives me the same "batteries included" trait of Clojure.
- Improve my macro-fu. That's really a very hot topic for me.
- Create a "Guile flavor" that makes me happy.

That means the code, in the long run, will not be Guile Scheme anymore, but a bunch of macros and definitions that make me more enjoyment while programing. What means I'm following the Ruby principle of [make programmers happy](https://www.artima.com/intv/rubyP.html).

# The principles

1. One verb, multiple objects. "Append" is "append" no matter what kind of structure we're using.
2. Code for the protocol, not the implementation. A collection is a collection, not a vector, list, hashtable, stream, io...
3. Parenthesis are gold, useless parenthesis are not. So remove "structural" code.
4. Prefix notation is gold, but don't be afraid of infix. Use it to remove structural parenthesis if we can't rely just in forms order.
5. Frequent used forms are short. The lesser the frequency, the longer the word.
6. Blantantly steal from other languages. If we are going to reinvent the wheel, at least let's use the good designs out there!

# Example "let"

Let's illustrate some of the principles above.

What's the problem?

`let` in Scheme is very flexible, but it costs a bunch of extra structural parenthesis to do so.

```Scheme
(let ([x (something 1 2 3)])
  (+ x 1))
```

Not bad, but that extra parenthesis are just adding noise in this case. So why not:

```Scheme
(let [x (something 1 2 3)]
  (+ x 1))
```
