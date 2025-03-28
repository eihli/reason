# Neural Guided Constraint Logic Programming for Neural Architecture Search

Applying [Neural Guided Constraint Logic Programming](https://arxiv.org/abs/1809.02840) to [Neural Architecture Search](https://arxiv.org/abs/1809.02840).

## miniKanren Demo

The best way to describe miniKanren (or more generally, a relational programming language) is by example.

The following code implements `xor` in miniKanren. `xor` is a simple function that takes two inputs and returns `1` if exactly one input is `1` (if both inputs are `1`, then `xor` returns `0`). `xor` comes from the name "exclusive or". You can compare it to the regular `or` which would return `1` if _at least_ one input is `1`.

First, a Python example (`xor` is the `^` operator in Python):

``` python
def bit_xor(x, y):
    return x ^ y
print(bit_xor(0, 0), bit_xor(0, 1), bit_xor(1, 0), bit_xor(1, 1))
# => 0 1 1 0
```

Now, glance over the following code and the output on the last line.

What does it look like this code is doing?

``` scheme
(defrel (bit-xoro x y result)
  (conde
   ((== 0 x) (== 0 y) (== 0 result))
   ((== 0 x) (== 1 y) (== 1 result))
   ((== 1 x) (== 0 y) (== 1 result))
   ((== 1 x) (== 1 y) (== 0 result))))

(run* (x y result) 
  (bit-xoro x y 0))
;; => ((0 0 _.0) (1 1 _.0))
```

A typical program would run `bit-xor(0, 0)` and output `0`.

But with relational programming, we can ask the question "What _inputs_ would result in an output of `0`?" and the language will return a stream of _all_ possible inputs that result in an output of `0`. (We can also ask "What does an input of `0, 0` output?", but that's less interesting since it's what every programming language does.)

In the code above, we're asking what inputs would result in an output of `0` and we get back the list of `(0 0 _)` and `(1 1 _)`, both inputs `0` or both inputs `1` (you can ignore the other text, that's just noise for now).

## What does this have to do with neural networks?

How would you find the optimal hyperparameters of a model?

You could simply use [Grid Search](https://scikit-learn.org/stable/modules/grid_search.html) from scikit-learn. Relational programming doesn't provide much benefit there (well... maybe it can... we'll get back to that later).

So, where does relational programming come into play?

How would you find the optimal _architecture_? How would you, for example, find whether you should use a single [Conv2d](https://pytorch.org/docs/stable/generated/torch.nn.Conv2d.html) layer, or 2? (Or _n_?)

Perhaps you could create a schema, in JSON for example, where you could list out all of the architectures that you want to explore, and then you could have a harness that tries all of the architectures in your configuration and logs the results for you to inspect when it finishes.

That's a great first step. But it still requires _you_ to _manually_ define and explicitly configure each architecture in the search. Just like grid search requires you to manually choose the set of hyperparameters that you want to search. 

How do we get rid of you? How do we take the human effort out of the search?

It might be challenging. Searching over architectures is not like searching over hyperparameters. With hyperparameters, there's no "invalid" combinations. A learning rate of 0.01 won't fail with any particular value of batch size or loss function. No hyperparameter particularly depends on any other hyperparameter.

But architecture search, unlike hyperparameter search, has a lot of interdependencies. If you have a working architecture that includes a linear layer, a convolutional layer, and another linear layer, you can't change the kernel size of the convolutional layer without _also_ changing the input dimensions of the final linear layer.

Those dependencies make it hard to efficiently automate the search. There are far more _invalid_ combinations of architectures than there are _valid_ combinations. We need some way to intelligently iterate over _valid_ architectures _without human input_.

And _that's_ where relational programming comes in.

Instead of `bit-xoro` above, imagine `linear-layero`

``` scheme
(defrel (linear-layero in-dims out-dims)
  (numbero in-dims)
  (numbero out-dims))

(defrel (fully-connecto layer-1-in layer-1-out layer-2-in layer-2-out)
  (linear-layero layer-1-in layer-1-out)
  (linear-layero layer-2-in layer-2-out)
  (== layer-1-out layer-2-in))

(run 10 (layer-1-in layer-1-out layer-2-in layer-2-out)
  (== layer-1-out 784)                                            ;; Assume we know we are working with MNIST, so this is our starting point.
  (== layer-2-out 10)
  (fully-connecto layer-1-in layer-1-out layer-2-in layer-2-out))
;; => ((784 2 2 10) (784 4 4 10) (784 8 8 10) (784 16 16 10) (784 32 32 10) ...)
```

You can imagine – like with `bit-xor` where we asked "Given an output of 0, what could our inputs have been?" – asking "Given input/output dimensions of 784 and 10, what could our inner layers have been?"

The only thing left to do is make sure that our relational programming language can intelligently search the possible middle layers. I hard-coded an example search algorithm in the block above – I imagined just starting at 2 and doubling. That's not really an example of the kind of search that a relational language would use. So, what _would_ a relational language use?

That's where [Neural Guided Constraint Logic Programming for Program Synthesis](https://arxiv.org/abs/1809.02840) comes into play.

In that paper, they describe how they trained a neural network to guide the search of their relational language.

That's what I want to do for searching over neural network architectures.

I expect we can code our relations/constraints to ensure our engine never produces an invalid architecture.

Our model will train on the metrics produced by its architectures; metrics like accuracy, loss, rate of change of loss, memory usage, etc...

Over time, it will learn to favor search paths that are likely to produce good architectures.

Well... that's the idea. I'm in over my head. Grateful for any and all comments, input, guidance, warnings, etc...

## Links

- [Neural Architecture Search: Insights from 1000 Papers](https://arxiv.org/pdf/2301.08727)
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

# Deep Dive

Let's start from where [first-order-miniKanren](https://github.com/gregr/first-order-miniKanren) ends. Let's assume we have a first-order miniKanren written in Racket.

Our next step is to communicate its list of choices, i.e. the list of possible branches/paths its search can take, to a model.

The model will communicate its choice to the miniKanren interpreter, which will expand the chosen path, and communicate the new list of choices back to the model.

This process will proceed until either an architecture is found or the search fails to find an architecture.

If an architecture is found, that architecture is communicated to a training process. The training process trains the model and reports to the model a metric that the model can use to update its weights.

The iteration I just described is basically what's known as Deep Q-Learning. You can see a simple example of Deep Q-Learning in this [Colab notebook](https://colab.research.google.com/drive/1GUxN8PZm4a0Zm3gZOYWVeUHbKRDE6ymo?usp=sharing).

## Loss

What is our loss calculation?

We want it to describe how well the model trains. If we track the loss of each batch, a good model will have a graph of losses that decreases sharply until it reaches some minimum and levels off. What formula will give us a single number representing how well a series of loss data points follows that pattern?

One idea: 

- fit a curve to the losses
- calculate the gradient of the curve at every point

A good model will have a low number, because either:

- it will have a few sharper decreases followed by a leveling-off (negative gradients of high magnitude followed by near-zero gradients)
- it will have many shallower decreases (negative gradients of medium magnitude relatively consistent throughout the training run)

A bad model will have a high number because its gradients will be in the opposite direction – positive.

The reward will simply be the negative of this value, so that our model can make its decision based on the maximum of its reward.

## Model Architecture

### Input/Output

Input: a utf-8 description of choices in a format that looks something like an s-expression.

Output: ...

Oh. Interesting.

Let's go with our first thought.

Output: an integer, the index of our chosen choice in the list

What's a problem with that?

Our output needs to be constrained to the number of choices in the list. But the number of choices in the list varies.

Output: a one-hot encoded vector, the index of the one-hot represents the index of our chosen choice in the list

That suffers from the same problem.

Do we just give negative rewards when our model selects a number outside the list? Do we pick an arbitrarily high maximum length for our one-hot encoded vector and hope that we're never presented with more choices than the length of our output vector?

Ah. Of course. This is better.

Input: a utf-8 description of a single choice, in a format that looks something like an s-expression

Output: the score of that choice

It's not the model's responsibility to output a particular choice. The model just scores choices. Of course.

### Embedding

Given our input is a utf-8 description of a single choice, in a format that looks something like an s-expression, I kind of want to just go with a typical LLM tokenization and embedding scheme.

One added benefit if we go the route of transformers is that we can visualize the query/key values and see which tokens are most important when making a decision. This might provide some valuable interpretability to our model. We might gain insights like "it's always valuable to follow large increases in linear dimensions with large dropouts".

This scheme will serve double duty because we can prefix the choice with high-level description of what we're trying to do, like "categorize 28x28x3 images of handwritten digits into 10 categories | (bind (state (((var #f 0) (var a 11) (var b 12))) () () ()) (conj (conj (== (var a 11) ((var a1 13) . (var a2 14))) (relate (appendo #s(var a2 14) #s(var b 12) #s(var res 15)))) (== (1 2 3 4) ((var a1 13) . (var res 15)))))"

# Related work

- DeepCoder https://games-automata-play.github.io/blog/deepcoder/
- LambdaBeam https://github.com/ellisk42/LambdaBeam
- RobustFill https://github.com/yeoedward/Robust-Fill
- FlashFill https://github.com/Qi-Zhan/FlashFill
