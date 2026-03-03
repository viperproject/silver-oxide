<style>
code, s {
    padding: .2em 0;
    margin: 0;
    font-size: 85%;
    border-radius: 3px;
}
:not(pre) > code::before, :not(pre) > code::after, s::before, s::after {
  letter-spacing: -.2em;
  content: "\a0";
}
s, div.viper > pre, div.viper > div > pre, div.parallel > div:nth-child(odd) > pre, div.parallel > div:nth-child(odd) > div > pre {
    font-family: monospace;
    background-color:#fff2e6 !important;
    text-decoration: none;
}
div.parallel {
    display: grid;
    grid-template: auto / 49% 49%;
    grid-gap: 5px 10px;
    margin-bottom: 16px;
}
div.parallel pre {
    margin-bottom: 0;
}

span.cm-strikethrough {
    text-decoration: none;
    font-style: italic;
}
div#doc {
    max-width: 1280px !important;
}
</style>

# Viper MIR
An elaborated Viper representation where a lot of the things that happen implicitly behind the scenes are made explicit. I believe that the fundamental reason for poor performance is the hidden complexity which prevents optimisations. There is no fundamental reason why Viper should not be able to check Prusti's core proof as quickly as the Rust compiler type/borrowchecks a program. Additionally, this representation should make it easier to map concepts back to Viper (for all projects where someone says: it's notoriously difficult to map silicon state/expressions back to Viper).

I will use VMIR (pronounced "vee-meer") as shorthand for Viper MIR to differentiate from Viper (vanilla). VMIR pseudocode is listed as `code` and Viper code as ~~code~~. The proposal is still a work in progress, so I will rate each part out of 🔟 on how final it is (i.e. how confident I am that it cannot change). Things that seem nice but are not critical (i.e. they can be dropped easily) are marked *️⃣.

## Translation prerequisites 🔟

**Typecheck.** All (sub)expressions in VMIR have an explicit type. Therefore, only type-checked Viper programs can be translated to VMIR.

**Desugar.** Syntactic sugar is eliminated in VMIR.

- ~~acc~~ related implicits: desugar predicate ~~p(...)~~ to ~~acc(p(...))~~ (if it wasn't already), desugar ~~acc(e)~~ to ~~acc(e, write)~~ (for presentational purposes I will omit the ~~write~~ in this document)
- short-circuiting boolean ops: desugar ~~l || r~~ to ~~l ? true : r~~, and ~~l ==> r~~ to ~~l ? r : true~~. We cover ~~&&~~ later as it requires special handling for impure ~~l~~.
- ~~acc~~ in ~~assert/assume~~: desugar ~~acc(e, p)~~ to ~~perm(e) >= p~~ *️⃣
- redundant binary ops: rewrite ~~l != r~~ as ~~!(l == r)~~; ~~l <==> r~~ as ~~l == r~~; ~~l > r~~ as ~~r < l~~; ~~l >= r~~ as ~~r <= l~~ *️⃣

## Resources 8️⃣

> Stems from the question: what is the type in ~~acc(loc: ?, p: Perm)~~? Clearly the ~~x.f~~ in ~~x.f + 5~~ is different to that of ~~acc(x.f)~~.

Ignoring ~~unfolding~~ for now, pure Viper expressions (those without ~~acc~~) can interact with the heap in only two ways: dereference (a field) and query permission (of a field/predicate). Though the latter is already explicit (`perm`), VMIR also makes dereference explicit as `*` (think ~~*x.f + 5~~). Both operations take the same type of argument: a *resource identifier* (think address). This type is explicit in VMIR and is written as `&T`. Though both are builtin primitives, they can be thought of as `function *[T](loc: &T): T` and `function perm[T](loc: &T): Perm` (both with an implicit heap "argument").

**Viper fields and opaque predicates** are translated to (opaque) VMIR functions. For example:
<div class=parallel><div>

```typescript
field f_: Int
inhale acc(x.f_) && perm(x.f_) < x.f_/1
```
</div><div>

```javascript
function f_(Ref): &Int
inhale acc(f_(x)) && perm(f_(x)) < *f_(x)/1
```
</div><div>

```typescript
predicate pr_(x1: Ref, x2: Ref, i: Int)
```
</div><div>

```javascript
function pr_(x1: Ref, x2: Ref, i: Int): &...
```
</div></div>

> I'm undecided on the resource type to use for opaque predicates and have left it blank (`...`) for now.

However, non-opaque Viper predicates (~~pr_~~ with body ~~E~~) are richer than simply an identifier. They additionally represent: a self-framing assertion ~~E~~, along with the ability to fold/unfold between ~~E <-> acc(pr_(...))~~. In VMIR, these various aspects are split up. To represent the heap-permission aspect, VMIR uses *resource members*. These are much like Viper predicates, but cannot be recursive, only talk about resources, and cannot be folded/unfolded. Instead, once a `resource` is verified, all uses are "unfolded" automatically. An example translation:

<div class=parallel><div>

```typescript
predicate pr_(x1: Ref, x2: Ref, i: Int) {
     acc(x1.f_, 1/2)
  && (x1 != x2 ==> acc(x2.f_) && x1.f_ == i)
  && x2.f_ != i
}
```
</div><div>

```javascript
function pr_(x1: Ref, x2: Ref, i: Int): &pr_heap

resource pr_heap(x1: Ref, x2: Ref, i: Int) {
  acc(f_(x1), 1/2) ** (x1 != x2 ? acc(f_(x2)))
} with {
  (x1 != x2 ? *f_(x1) == i : true) && *f_(x2) != i
}
```
</div></div>

> I think of `resource pr_heap(...)` as `function pr_heap(...): Heap`, where `Heap` is a tuple of (permission mask, snapshot).

As mentioned above, a `resource` contains only resource assertions, which are potentially under path conditions (I use `e ? r` as shorthand for `e ? r : emp`). Additionally, each `resource` has an associated adt type (think silicon snapshot) with the same name (this is the `pr_heap` that the return of `pr_` uses) as well as a snapshot function which constructs the snapshot from the `resource`.

An accompanying boolean expression carries the pure facts, I use the `with` as shorthand for a separate boolean `function`. Breaking down impure expressions into resources + pure like so is always possible in Viper! Note that in VMIR the `*` in `requires*` means that the assertion is impure. In VMIR everything that must be verified lives in a `{ ... }` and each such block represents a single (generally) self-contained verification obligation. The result of verifying each `{ ... }` block can be cached and reused in dependencies (like silicon does I think).

> The implicit `resource` adt type could also be made explicit, but I have not done so for simplicity. Rate implicit/explicit decision as 2️⃣. The explicit form would be something like (`Option` defined as expected):
> ```javascript
> adt pr_heap {
>   Snap(f_0: Int, f_1: Option[Int])
> }
> function pr_heap_snap(x1: Ref, x2: Ref, i: Int): pr_heap
>   requires* pr_heap(x1, x2, i)
> {
>   Snap(*f_(x1), x1 != x2 ? Some(*f_(x2)) : None)
> }
> ```

**VMIR types.** To recap, VMIR extends the Viper types (`Bool`, `Int`, `Ref`, `Perm`, and user-defined domains and adts) with a new type: a *resource identifier* (written `&T`). For `resource` snapshots it uses the existing `adt` type.

## Ghost methods 7️⃣

> Stems from the questions: what do ~~fold~~, ~~unfold~~ "mean" and why are they dual? Why can I use them in expressions as ~~unfolding~~ (and ~~apply~~ with ~~applying~~ for that matter)?

Now to the good stuff. VMIR has a concept of *ghost methods*, which are reversible methods that can be both "called" and "uncalled". The body of a ghost method may only contain reversible statements, such that it can be "executed" both forwards and backwards, and it provides a proof that we can arbitrarily go between the precondition and the postcondition (it proves a bi-implication between pre and post/it proves an equisem equality that can be used in isosem). In addition to the `function` and `resource` above, each Viper predicate generates one such ghost method. The example above would be translated as:

<div class=parallel><div>

```typescript
predicate pr_(x1: Ref, x2: Ref, i: Int) {
  ...
}
```
</div><div>

```javascript
ghost method pr_fold(x1: Ref, x2: Ref, i: Int)
  requires* pr_heap(x1, x2, i)
  ensures*  acc(pr_(x1, x2, i))
  preserves old(*pr_heap(x1, x2, i)) == *pr_(x1, x2, i)
```
</div></div>

> In VMIR "impure" expressions (anything with `acc`) are **only** allowed inside `resource` and pure expressions only in `function` bodies! However, for presentational purposes, I omit these in simple cases (such as the `ensures*` above) and directly write the body of the `resource` or `function`. I also use `*pr_heap(...)` as pseudocode to represent calling the conceptual `pr_heap_snap` function.

Since it has no body, this ghost method is an axiom and it states that we can go from the unfolded state to the folded state and back while preserving the snapshot. The `preserves` is a kind of postcondition in both directions (with the `old` flipped when unapplying).

In Viper, a method that unfolds a predicate twice (e.g. a linked list) is a simple example of what could be a ghost method with a body in VMIR. A naïve way to verify it in current Viper would be to verify two methods: one identical and one with the statement order reversed, the folds replaced with unfolds, and the pre/post swapped. However, I conjecture that a VMIR verifier can be more efficient and simply prove equivalence between the heap at the end of the method and the postcondition heap, along with a side condition that the end heap implies the `preserves` assertion. Unlike the naïve one, this proof holds only if all statements in the body are reversible (i.e. no assignment).

> Note that a method that swaps the values in `x.f` and `y.f` is potentially also reversible. I'm not sure if it would be sound (a user could only write it as an axiom), but if yes then maybe `ghost` isn't a good name.

## Recursive functions 3️⃣

> Why should the backends have to implement all this complex logic for limited functions?

The following is a linked list predicate translated as above (the pure part is `true` and is omitted):

<div class=parallel><div>

```typescript
field n_: Ref

predicate ll_(x: Ref) {
  x != null ==> acc(x.f_) &&
    acc(x.n_) && acc(ll_(x.n_))
}
```
</div><div>

```javascript
function n_(x: Ref): &Ref

function ll_(x: Ref): &ll_body
resource ll_body(x: Ref) {
     x != null ? acc(f_(x))
  ** x != null ? acc(n_(x))
  ** x != null ? acc(ll_(*n_(x)))
}
ghost method ll_fold(x: Ref)
  requires* ll_body(x)
  ensures*  acc(ll_(x))
  preserves old(*ll_body(x)) == *ll_(x)
```
</div></div>

Then a recursive ~~length~~ function is translated as so:

<div class=parallel><div>

```typescript
function length(x: Ref): Int
  requires ll_(x)
  ensures  0 <= result
{
  x == null ? 0 : 1 + unfolding ll_(x) in length(x.n_)
}
```
</div><div>

```javascript
function length(x: Ref): Int
  requires* acc(ll_(x))
  ensures   0 <= result
  guard     { ll_(x) }
{
  x == null ? 0 : 1 +
    uncalling acc(ll_fold(x), wildcard) in length(*n_(x))
}
```
</div></div>

> Note how ghost methods can be used in expressions with `calling/uncalling` and can be used with an arbitrary positive permission multiplier.

For `function`s the job of verification is to purify the body. For `length` this would mean (where `ll_body` is the `resource`-associated `adt` snapshot type with a three-value constructor):

```javascript
function length(x: Ref, heap: ll_body): Int
  ensures 0 <= result
  guard   { ll_(x) }
{
  x == null ? 0 : 1 + length(heap.n_, heap.ll_)
}
```
<!---->

As we saw above, functions can specify an additional *definitional guard*. This guard is used to prevent definitional matching loops: any VMIR recursive function without such a guard will likely result in a matching loop. The guard is added to the axiom defining the function, in our example as so:

```
∀ x: Ref, heap: ll_body. { length(x, heap), ll_(x) } length(x, heap) == ...
```

**Sidetrack: automatic fold/unfold.** Such a mechanism could maybe be used to eliminate the need for fold/unfold in many cases. Non-recursive Viper predicates could be translated to VMIR `resource`s which are always fully unfolded and a recursive predicate could use the above "trigger" mechanism. For example, the `ll_` from above could be:

```javascript
resource ll_body(x: Ref)
    guard { f_(x) } { n_(x) }
{
  x != null ? acc(f_(x)) ** ...
}
```

## Loops 6️⃣

> Complex back-edge elimination, don't make the verifiers implement this!

Viper loops are translated to an equivalent recursive method. To support this (along with Viper ~~inhale/exhale~~) VMIR `resource`s can be evaluated *contextually*. That is, they are verified/purified just like all the self-framing resources above, but if a permission is missing then the outer context can be used to find the value. The following is a translation of an iterative `mlen` method:

<div class=parallel><div>

```typescript
method mlen(x: Ref, m: Int) returns (i: Int)
  requires acc(x.n_) && ll_(x.n_)
  ensures  i == old(length(x.n_)) * m
{
  i := 0
  while (x.n_ != null)
    invariant acc(x.n_) && ll_(x.n_)
    invariant i == m * (old(length(x.n_)) - length(x.n_))
  {
    i := i + m
    unfold ll_(x.n_)
    x.n_ := x.n_.n_
  }
  exhale x.n_ == null && acc(x.n_) && x.n_ == null
}
```
</div><div>

```javascript
resource mlen_pre(x: Ref, m: Int) {
  acc(n_(x)) ** acc(ll_(*n_(x)))
}
method mlen(x: Ref, m: Int) returns (i: Int)
  requires* mlen_pre(x, m)
  ensures  i == old(length(x)) * m
{
  i := 0
  resource mlen_inv(i: Int) {
    acc(n_(x)) ** acc(ll_(*n_(x)))
  } with {
    i == m * (old(length(*n_(x))) - length(*n_(x)))
  }
  method mlen_rec(i_in: Int) returns (i_out: Int)
    requires* mlen_inv(i_in)
    ensures*  mlen_inv(i_out)
  {
    assume *n_(x) != null
    i_out := i_in + m
    uncall acc(ll_fold(*n_(x)))
    *n_(x) := *n_(*n_(x))
    i_out := mlen_rec(i_out)
  }
  i := mlen_rec(i)
  assume *n_(x) == null

  resource mlen_exhale(x: Ref) {
    *n_(x) == null ? acc(n_(x))
  } with {
    *n_(x) == null && *n_(x) == null
  }
  uncall acc(inhale(mlen_exhale(x)))
}
```
</div></div>

In this example, the invariant is self-framing save for the `old(length(*n_(x)))`. During verification, this subexpression would be evaluated in the outer context and the resulting value substituted in. The `resource` at the end (for the exhale) will be handled likewise: its body will be verified just like a Viper predicate, except the `*n_(x)` will be substituted with the `null` value from the outher context. Then in the pure `with` both `*n_(x)` will instead reference the `n_(x)` permission from the resource itself and thus that null assertion is checked later when the resource is used.

In the current proposal the result of `resource` applications (something roughly of type Heap) is not a first-class citizen. However, as a curiosity, if it was then `inhale` and `exhale` could be seen as simply a `call` and `uncall` of an unsound ghost method that requires true and ensures whatever `resource` is passed in.

## Magic wands 4️⃣

The classical representation of wands as a separating implication is a huge headache as they can be constructed by disproving the lhs. This feature is never used in practice, but makes wands extremely unwieldy due to all the restrictions Viper must enforce to make them sound. VMIR has reversible wands with a semantically different meaning. A reversible wand `x --** y` is defined as `∃ f. f && (x ** f <==> y)`, which represents the permission to the footprint `f` along with a pure "proof" of bi-implication. Now the bi-implication might sound familiar, it is defined as `∃ ghost method. requires* x ** f ensures* y`. The following example translates Viper wands to VMIR `--**`:

<div class=parallel><div>

```typescript
method tail(x: Ref) returns (t: Ref)
  requires ll_(x) && x != null
  ensures  acc(t.n_) && ll_(t.n_)
  ensures  acc(t.n_) && ll_(t.n_) --* ll_(x)
{
  unfold ll_(x)
  if (x.n_ == null) {
    t := x
    package acc(t.n_) && ll_(t.n_) --* ll_(x) {
      fold ll_(x)
    }
  } else {
    var x_n: Ref := x.n_
    t := tail(x_n)
    package acc(t.n_) && ll_(t.n_) --* ll_(x) {
      apply acc(t.n_) && ll_(t.n_) --* ll_(x_n)
      fold ll_(x)
    }
  }
}
```
</div><div>

```javascript
resource tail_post(x: Ref, t: Ref)
  requires* acc(ll_(x)) && x != null
{
     acc(n_(t))
  ** acc(ll_(*n_(t)))
  ** acc(
       acc(n_(t)) ** acc(ll_(*n_(t))) --** acc(ll_(x))
     )
}
method tail(x: Ref) returns (t: Ref)
  requires* acc(ll_(x)) && x != null
  ensures*  tail_post(x, t)
{
  uncall acc(ll_fold(x))
  if (*n_(x) == null) {
    t := x
    package ghost method tail_wand_1()
      requires* acc(ll_(x))
      ensures*  acc(n_(t)) && acc(ll_(*n_(t))) && ?
    {
      uncall acc(ll_fold(x))
      // ? <- `acc(f_(x))`
    }
  } else {
    var x_n: Ref := *n_(x)
    t := tail(x_n)
    package ghost method tail_wand_2()
      requires* acc(ll_(x))
      ensures*  acc(n_(t)) && acc(ll_(*n_(t))) && ?
    {
      uncall acc(ll_fold(x))
      // state `acc(f_(x)) ** acc(n_(x)) ** acc(ll_(*n_(x)))`
      // infer `acc(n_(x)) && *n_(x) == x_n` in footprint
      uncall acc(
        acc(n_(t)) && acc(ll_(*n_(t))) --** acc(ll_(x_n))
      )
      // ? <- `acc(f_(x)) && acc(n_(x)) && *n_(x) == x_n &&
      //          acc(acc(n_(t)) && acc(ll_(*n_(t)))
      //                                 --** acc(ll_(x_n)))
    }
  }
}
```
</div></div>

Viper ~~package~~ blocks are encoded as ghost methods with the proof block reversed. The basic premise is that one starts with the wand's rhs and, using a ghost method body, repacks it to find the lhs. Whatever remains is the footprint. A VMIR `package` takes such a ghost method and, after verifying the body and inferring the footprint, finds and packages up the footprint in the current heap into the wand form (as definition with `∃` above). To apply a wand, one simply `calls` the `∃ ghost method. requires* x ** f ensures* y` that is carried around with the `x --** y` wand. Note that above we proved the ghost methods with `requires` and `ensures` the other way around, but this proof implies the required `∃`.

Note how the existence of this ghost method is a (duplicable) pure fact which in the example above is pulled into the second `package`. The `call acc(... --** ...)` line essentially unapplies a wand by going from rhs to lhs + wand. Also note that this call requires the verifier to find `acc(ll_(x_n))` in the state:
```javascript
acc(f_(x)) && acc(n_(x)) && acc(ll_(*n_(x)))
```
The verifier achieves this by removing `acc(n_(x))` from the current state and adding `acc(n_(x)) && *n_(x) == x_n` to the footprint.

**Classical wands.** Though cool, these `--**` wands do run into an issue: they can struggle to express pure lhs restrictions. Consider the ~~42 <= x.f_~~ in the following example:

<div class=parallel><div>

```typescript
predicate nat_(x: Ref) {
  acc(x.f_) && acc(x.n_) && 0 <= x.f_
}
method mk_wand(x: Ref)
  requires acc(x.n_)
  ensures acc(x.f_) && 42 <= x.f_ --* nat_(x)
{
  package acc(x.f_) && 42 <= x.f_ --* nat_(x) {
    fold nat_(x)
  }
}
```
</div><div>

```javascript
// ...
{
  package method mk_wand_1()
    requires* acc(f_(x)) && 42 <= *f_(x) && ?
    ensures*  acc(nat_(x))
  {
    // infers that `acc(n_(x))` is required
    call acc(nat_fold(x))
  }
}
```
</div></div>

Such a wand is not reversible: there is no footprint `?` such that we could go from `acc(nat(x))` to `acc(f_(x)) && 42 <= *f_(x) && ?`<!--<>-->. Therefore in VMIR one can also use classical wands `x --* y` which package a non-ghost method (we'd probably translate some Viper wands like this).

Notice how with `--**` we put the `?` in the `ensures*`, while with `--*` we had to put it in the `requires*`. I think that the footprint inference will be easier (faster) with the former. However, since `--**` uses ghost methods we can choose to swap the `requires` and `ensures` and have the `?` in the precondition. The footprint inference with `?` in the pre can be implemented pretty much identically to how it currently works in silicon.

<!--## Quantified permissions ?

#️⃣*️⃣0️⃣1️⃣2️⃣3️⃣4️⃣5️⃣6️⃣7️⃣8️⃣9️⃣🔟-->
