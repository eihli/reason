# Neural Guided Constraint Logic Programming

An implementation of a system similar to the one described in: [Neural Guided Constraint Logic Programming](https://arxiv.org/abs/1809.02840).

**What does it do?**

Imagine you have some input/output examples:

``` racket
;; Note: not the real syntax.
(solve
  ((input "Mini Kanren") (output "M. K."))
  ((input "Pro Log")     (output "P. L.")))
```

This code will find functions that produces those outputs when given those inputs. For example:

``` racket
;; Note: not the real output.
(lambda (input)
  "Split the input on the space character, 
   take first character of each word, 
   follow that by a period, 
   then join with a space."
  (string-join
    (map 
      (lambda (word)
        (string-append (substring word 0 1) "."))
      (string-split input " "))))
```

**How does it do it?**

The input/output examples are "constraints" that get run through a relational/logic programming language called miniKanren (it's similar to Prolog). The logic interpreter tries every possible combination of programs until it finds one that satisfies all of the constraints.

The search space is huge (infinite). What kind of search algorithm will find solutions in a reasonable amount of time? 

Breadth first? Depth first? Biased interleaving search? No!

*Neural Guided Search!*

We train a neural network to pick which paths to explore.

## Training and Architecture

TODO: ...

# miniKanren Demo

The best way to describe miniKanren (or more generally, a relational programming language) is by example.

Glance over the following code and the output on the last line.

What does it look like this code is doing?

``` scheme
(define-relation (bit-xoro x y result)
  (conde
   ((== 0 x) (== 0 y) (== 0 result))
   ((== 0 x) (== 1 y) (== 1 result))
   ((== 1 x) (== 0 y) (== 1 result))
   ((== 1 x) (== 1 y) (== 0 result))))

(run* (x y) 
  (bit-xoro x y 0))
;; => ((0 0) (1 1))
```

A typical program would run `bit-xor(0, 0)` and output `0`.

But with relational programming, we can ask the question "What _inputs_ would result in an output of `0`?" and the language will return a stream of _all_ possible inputs that result in an output of `0`. (We can also ask "What does an input of `0, 0` output?", but that's less interesting since it's what every programming language does.)

In the code above, we're asking what inputs would result in an output of `0` and we get back the list of `(0 0)` and `(1 1)`, both inputs `0` or both inputs `1` (you can ignore the other text, that's just noise for now).

## Links

- [A Framework for Extending µKanren with Constraints](https://arxiv.org/pdf/1701.00633)
- [A Tutorial Reconstruction of miniKanren with Constraints](https://drive.google.com/file/d/1svTPpIowx4InQIaUU3DyVEMpcoSZP6Bm/view)
- [An Annotated Implementation of miniKanren with Constraints](https://github.com/cicada-lang/chimera/blob/master/docs/papers/an-annotated-implementation-of-minikanren-with-constraints.pdf)
- [µKanren: A Minimal Functional Core for Relational Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)
- [A Unified Approach to Solving Seven Programming Problems (Functional Pearl)](https://dl.acm.org/doi/pdf/10.1145/3110252)
- [miniKanren, Live and Untagged: Quine Generation via Relational Interpreters (Programming Pearl)](http://webyrd.net/quines/quines.pdf)
- [Faster miniKanren](https://github.com/miniKanren/faster-miniKanren)
- [Efficient Variational Inference in miniKanren with Weighted Model Counting](https://www.evandonahue.com/pdf/donahue_emkanren2022.pdf)
  - [aiKanren](https://github.com/emdonahue/aiKanren)
- [Goals as Constraints: Writing miniKanren Constraints in miniKanren](http://www.evandonahue.com/pdf/donahue_goalsasconstraints2023.pdf)
- [Proof Search with an LLM Kanren](https://github.com/philzook58/philzook58.github.io/blob/master/_drafts/llm_kanren.ipynb)
- [Neural Architecture Search: Insights from 1000 Papers](https://arxiv.org/pdf/2301.08727)

# Related work

- FlashFill https://github.com/Qi-Zhan/FlashFill
- LambdaBeam https://github.com/ellisk42/LambdaBeam
- RobustFill https://github.com/yeoedward/Robust-Fill
- DeepCoder https://games-automata-play.github.io/blog/deepcoder/
