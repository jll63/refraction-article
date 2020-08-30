# Function Generation in D: the Good, the Bad, the Ugly, and the Bolt

## Introduction

A while ago Andrei started a thread ((Perfect
Forwarding)[https://forum.dlang.org/post/rfim9h$a45$1@digitalmars.com]) in the
D forum about a challenge, which came up during the July 2020 beerconf:

> Write an idiomatic template `forward` that takes an alias `fun` and defines
> (generates) one overload for each overload of `fun`.

During the discussion, several people came up with solutions. It also appeared
that sometimes there is a need to alter the function's properties - like add,
remove or change user-defined attributes or parameter storage classes.

It tuns out that this is exactly the problem I struggled with while trying to
support all the bells and whistles of D functions in my `openmethods`
library. Eventually I created a module that helps with this problem, and
contributed it to (the bolts meta-programming
library)[https://aliak00.github.io/bolts/bolts.html].

But first, let's take a closer look at function generation in D.

## The Good

I speculate that many a programmer who is a moderately advanced beginner in D
would quickly come up with a mostly correct solution to the "Perfect
Forwarding" challenge - especially those with a C++ background and with an
interest in performing all sorts of magic tricks by means of template
meta-programming. The solution will probably look like this:

```d
template forward(alias fun)
{
  import std.traits: Parameters;
  static foreach (
    ovl; __traits(getOverloads, __traits(parent, fun), __traits(identifier, fun))) {
    auto forward(Parameters!ovl args)
    {
      return ovl(args);
    }
  }
}

...

int plus(int a, int b) { return a + b; }
string plus(string a, string b) { return a ~ b; }

assert(forward!plus(1, 2) == 3);        // pass
assert(forward!plus("a", "b") == "ab"); // pass
```

This solution is not perfect, as we shall see; it is not far off either. It
covers many situations, including some that a beginner may not even be aware
of. For example, `forward` handles the following function without dropping
function attributes or parameter storage classes:

```d
class Matrix { ... }

Matrix times(scope const Matrix a, scope const Matrix b) pure @safe
{
  return ...;
}

pragma(msg, typeof(times));
// pure @safe Matrix(scope const(Matrix) a, scope const(Matrix) b)

pragma(msg, typeof(forward!times));
// pure @safe Matrix(scope const(Matrix) _param_0, scope const(Matrix) _param_1)

```

It even handles user-defined attributes (UDAs) on parameters:

```d
struct testParameter;

void testPlus(@testParameter int a, @testParameter int b);

pragma(msg, typeof(testPlus));
// void(@(testParameter) int a, @(testParameter) int b)

pragma(msg, typeof(forward!testPlus));
// void(@(testParameter) int a, @(testParameter) int b)
```

Talking about UDAs, that's one of the issues with the solution above: it
doesn't carry *function* UDAs. Also, it doesn't work with `ref` returning
functions. Both issues are easily fixed:

```d
template forward(alias fun)
{
  import std.traits: Parameters;
  static foreach (ovl; __traits(getOverloads, __traits(parent, fun), __traits(identifier, fun)))
  {
    @(__traits(getAttributes, fun)) // also copy UDAs
    auto ref forward(Parameters!ovl args)
    {
      return ovl(args);
    }
  }
}
```

This solution is not 100% correct though. If the forwardee is `@trusted`, the forwarder will be `@safe`:

```d
@trusted void useSysCall() { ... }

pragma(msg, typeof(&useSysCall));         // void function() @trusted
pragma(msg, typeof(&forward!useSysCall)); // void function() @safe
```

This happens because the body of the forwarder consists of a single statement:
call `useSysCall`. Since calling a trusted function is safe, the forwarder is
automatically deemed safe by the compiler.

## The Bad

Andrei's challenge was not exactly what we discussed in the previous
section. It came with a bit of pseudocode that suggested that the template
should not be eponymous. in other word, I believe that the exact task was to
write a template that would be used like this: `forward!fun.fun(...)`. Here is
the pseudocode:

```d
// the instantiation of forward!myfun would be (stylized):

template forward!myfun
{
    void myfun(int a, ref double b, out string c)
    {
        return myfun(a, b, c);
    }
    int myfun(in string a, inout double b)
    {
        return myfun(a, b);
    }
}
```

This looks like a small difference, but, if we want to implement exactly this,
a complication arises. In the eponymous `forward`, we did not need to create a
new identifier: we simply used the template name as the function name. Thus the
function name was fixed. Now we need to create a function with a name that
depends on the forwardee's name. And the only way to do this is with a string
mixin.

The first time I had to do this, I tried the following:

```d
template forward(alias fun)
{
  import std.format : format;
  import std.traits: Parameters;
  enum name = __traits(identifier, fun);
  static foreach (ovl; __traits(getOverloads, __traits(parent, fun), name)) {
    @(__traits(getAttributes, fun))
    auto ref mixin(name)(Parameters!ovl args)
    {
      return ovl(args);
    }
  }
}
```

This doesn't work because a string mixin can only be used to create expressions
or statement lists.

The solution is thus to simply extend the mixin untill it encompasses the
entire function definition. The token-quote operator `q{}` is very handy for
this:

```d
template forward(alias fun)
{
  import std.format : format;
  import std.traits: Parameters;
  enum name = __traits(identifier, fun);
  static foreach (ovl; __traits(getOverloads, __traits(parent, fun), name)) {
    mixin(q{
        @(__traits(getAttributes, fun))
          auto ref %s(Parameters!ovl args)
        {
          return ovl(args);
        }
      }.format(name));
  }
}
```

String mixins are powerful, but they are essentially C macros. For many D
programmers, resorting on a string mixin always feels like a defeat.

Let us now move to a similar, yet quite more difficult challenge:

> Write a class template that mocks an interface.

For example:

```d
interface JsonSerializable
{
  string asJson() const;
}

void main()
{
  auto mock = new Mock!JsonSerializable();
}
```

Extrapolating the techniques acquired during the previous challenge, a beginner
would probably try this first:

```d
class Mock(alias Interface) : Interface
{
  import std.format : format;
  import std.traits: Parameters;
  static foreach (member; __traits(allMembers, Interface)) {
    static foreach (fun; __traits(getOverloads, Interface, member)) {
      mixin(q{
          @(__traits(getAttributes, fun))
          auto ref %s(Parameters!fun args)
          {
            // record call
            static if (!is(ReturnType!fun == void)) {
              return ReturnType!fun.init;
            }
          }
        }.format(member));
    }
  }
}
```

Note that this time we are forced to generate functions with different names,
hence resort on string mixins.

Alas this fails to compile with errors like:

```
Error: function `challenge.Mock!(JsonSerializable).Mock.asJson` return type
inference is not supported if may override base class function
```

In other words, `auto` cannot be used here. We have to fall back on specifying
the return type explicitly:

```d
class Mock(alias Interface) : Interface
{
  import std.format : format;
  import std.traits: Parameters, ReturnType;
  static foreach (member; __traits(allMembers, Interface)) {
    static foreach (fun; __traits(getOverloads, Interface, member)) {
      mixin(q{
          @(__traits(getAttributes, fun))
          ReturnType!fun %s(Parameters!fun args)
          {
            // record call
            static if (!is(ReturnType!fun == void)) {
              return ReturnType!fun.init;
            }
          }
        }.format(member));
    }
  }
}
```

This will not handle `ref` functions though. What about throwing in a `ref` in
front of the return type, like we did in the first challenge?

```d
// as before
          ref ReturnType!fun %s(Parameters!fun args) ...
```

But this will fail with all the functions in the interface that do *not* return
a reference.

See, the reason why everything worked almost by magic in the first challenge is
that we called the wrapped function. It enabled the compiler to deduce almost
all the characteristics of the original function, and copy them to the
forwarder function. Here we have no such model. The compiler will copy *some*
of the aspects of the function (`pure`, `@safe`, etc) to match those of the
overriden function, but not some others (`ref`, `const` and the other
modifiers).

Then there is the issue of the function modifiers: `const`, `immutable`,
`shared`, and `static`. These are yet another category of function "aspects".

At this point, there is no other option than to painstakingly analyze a subset
of the function attributes by means of traits, convert them to a string, to be
injected in the string mixin:


```d
      mixin(q{
          @(__traits(getAttributes, fun))
          %sReturnType!fun %s(Parameters!fun args)
          {
            // record call
            static if (!is(ReturnType!fun == void)) {
              return ReturnType!fun.init;
            }
          }
        }.format(
            (functionAttributes!fun & FunctionAttribute.const_ ? "const " : "")
          ~ (functionAttributes!fun & FunctionAttribute.ref_ ? "ref " : "")
          ~ ...,
          member));
    }
```

If you look at the implementation of `std.typecons.wrap`, you will see that
part of the code deals with synthesizing bits of string mixin for the storage
classes and modifiers.

## The Ugly

So far we have looked at the function storage classes, modifiers, and UDAs, but
we have merely passed the parameter list as a single, monolithic
block. However, it is sometimes needed to perform adjustments to the parameter
list of the generated function. This may seem far-fetched, but it does happen.
I encountered this problem in my `openmethods` library. During the "Perfect
Forwaring" discussion, it appeared that I was not the only one who wanted to do
this.

I won't dwelve into the details of `openmethods` here(see this blog post for
that); for the purpose of this article, it suffices to say that, given a
function declaration like this one:


```d
Matrix plus(virtual!Matrix a, double b);
```

`openmethods` generates this function:

```d
Matrix dispatcher(Matrix a, double b)
{
  return resolve(a)(a, b);
}
```

The `virtual` template acts simply as a marker: it indicates which parameters
should be taken into account (i.e. passed to `resolve`) when picking the
appropriate specialization of `times`. Note that only `a` is passed to the
`resolve` function - that is because the first parameter uses the `virtual!`
marker and the second does not.

Bear in mind, though, that `dispatcher` is not allowed to use the type of the
parameters directly. Inside the `openmethods` module, there is no `Matrix`
type. Thus, when `openmethods` is handed a function declaration, it needs to
synthesize a `dispatcher` function that refers to the declaration's parameter
types exclusively *via* the declaration. In other words, it needs to use
the `ReturnType` and `Parameters` templates from `std.traits` to extract the
types involved i the declaration - just like we did in the examples above.

Let's put aside function attributes and UDAs - we already discussed those in
the previous section. Then the obvious solution seems to be:

```d
ReturnType!times dispatcher(
  RemoveVirtual!(Parameters!times[0]) a, Parameters!times[1] b)
{
  return resolve(a)(a, b);
}

pragma(msg, typeof(&dispatcher)); // Matrix function(Matrix, double)
```

...where for `RemoveVirtual` is a simple template that peels off the
`virtual!` marker from the type.

Does this preserve *parameter* storage classes and UDAs? Unfortunately, it
doesn't:

```d
@nogc void scale(ref virtual!Matrix m, lazy double by);

@nogc ReturnType!scale dispatcher(
  RemoveVirtual!(Parameters!scale[0]) a, Parameters!scale[1] b)
{
  return resolve(a)(a, b);
}

pragma(msg, typeof(&dispatcher)); // void function(Matrix a, double b)
```

We lost the `ref` on the first parameter, and the `lazy` on the second. What
happened to them?

The culprit is `Parameters`. The template is a wrapper around an obscure
feature of the [`is`](https://dlang.org/spec/expression.html#is_expression)
operator used in conjunction with the `__parameters` type specialization. And
it is quite a strange beast. Above we used it to copy the parameter list of a
function, as a whole, to another function and it worked perfectly. The problem
is what happens when you try to process the parameters one by one. Let's look
at a few examples:


```d
pragma(msg, Parameters!scale.stringof); // (ref virtual!(Matrix), lazy double)
pragma(msg, Parameters!scale[0].stringof); // virtual!(Matrix)
pragma(msg, Parameters!scale[1].stringof); // double
```

We see that accessing a parameter individually loses everything about it bar
the type.

There is actually a way of extracting everything about a single parameter: use
a *slice* instead of an element (yes this is getting strange):

```d
pragma(msg, Parameters!scale[0..1].stringof); // (ref virtual!(Matrix))
pragma(msg, Parameters!scale[1..2].stringof); // (lazy double)
```

So this gives us a solution for handling the second parameter of `scale`:

```d
ReturnType!scale dispatcher(???, Parameters!scale[1..2]) { ... }
```

But what can we put in place of `???`. `RemoveVirtual!(Parameters!scale[0..1])`
would not work. `RemoveVirtual` expects a type, and `Parameters!scale[1..2]` is
not a type - it is a strange conglomerate that contains a type, and perhaps
storage classes, type constructors and UDAs.

At this point we have no other choice but, once again, to construct a string
mixin. Something like this:

```d
mixin(q{
    %s ReturnType!(scale) dispatcher(
      %s RemoveVirtual!(Parameters!(scale)[1]) a,
      Parameters!(scale)[1..2] b)
    {
        resolve(a)(a, b);
    }
  }.format(
    functionAttributes!scale & FunctionAttribute.nogc ? "@nogc " : ""
    /* also handle other function attributes */,
    __traits(getParameterStorageClasses, scale, 0)));

pragma(msg, typeof(dispatcher)); // @nogc void(ref double a, lazy double)
```

This is not quite sufficient though, because it doesn't take care of parameter
UDAs.

### To Boltly Refract...

`openmethods` once contained kilometers of mixin code like the above. It came
to the point where I had no joy working on it anymore - it was too ugly, too
messy. So I decided to sweep all the ugliness under a neat interface, once and
for all. The result was a "refraction" module, which I later carved out of
`openmethods` and donated to Ali Akhtarzada's excellent `bolts`
library. `bolts` attempts to fill the gaps in, and bring some regularity to
D's motley set of features related to meta-programming.

`refraction`'s entry point is the `refract` function template. It takes a
function and an "anchor" string, and returns an immutable `Function` object
that captures all the aspects of a function. `Function` objects can be used at
compile-time. It is, actually, their raison d'etre.

`Function` has a `mixture` property that returns a declaration for the original
function. For example:

```d
enum original = refract!(scale, "scale");
pragma(msg, original.mixture);
// @nogc @system ReturnType!(scale) scale(Parameters!(scale) _0);
```

Why does `refract` need the anchor string? Can't the string `"scale"` be
inferred from the function, by means of `__traits(identifier...)`?  Yes it can,
but we don't want to use it. The whole point of the library is to be used in
templates, where, typically, the function is passed to `refract` via an
alias. In general, the function's has no meaning in the template's scope - or
if, by chance, the name exists in the template's scope, it does not represent
the function. All the meta-expressions used to dissect the function must work
in terms of the alias, i.e. the *local* symbol `fun`.

Consider:

```d
module matrix;

Matrix plus(virtual!Matrix a, double b);

Method!plus plusMethod; // openmethods creates a `Method` object for each
                        // declared method

module openmethods;

struct Method(alias fun)
{
    enum returnTypeMixture = refract!(fun, "fun").returnType;
    pragma(msg, returnTypeMixture);              // ReturnType!(fun)
    mixin("alias R = ", returnTypeMixture, ";"); // ok
    pragma(msg, R.stringof);                     // Matrix
}
```

There is no `scale`, and no `Matrix`, in `module openmethods`; and even if they
existed, they could *not* be the `scale` function and the `Matrix` class from
`module matrix`, as this would assume circular dependencies between the two
modules - something that D forbids by default.

The return type is accessible, though, but it has to be expressed in terms of
`fun`, thus: `ReturnType!(fun)`.

All the aspects of the function are available piecemeal; for example:

```d
pragma(msg, original.parameters[0].storageClasses); // ["ref"]
```

`Function` also has methods that return a new `Function` object, with an
alteration to one of the aspects. They can be used to create a variation of a
function. For example:

```d
pragma(msg,
  original
    .withName("dispatcher")
    .withBody(q{{ resolve(_0[0])(_0); }})
    .mixture
);
```

This is what I mean by "refraction": make the blueprint of a function, perform
some alterations, and return a string - called a mixture - that, passed to
`mixin`, will create a new function.


```d @nogc @system ReturnType!(scale)
dispatcher(Parameters!(scale) _0) { resolve(_0[0])(_0); }
```

`openmethods` needs to change the type of the first parameter - while
preserving storage classes. With `bolts.experimental.refraction` this becomes
easy:

```d
pragma(msg,
  original
    .withName("dispatcher")
  .withParameters(
    [original.parameters[0].withType(
       "RemoveVirtual!("~ original.parameters[0].type~")"),
     original.parameters[1],
    ])
    .withBody(q{{ resolve(_0)(_0, _1); }})
    .mixture
);
```

This time the generated code splits the parameter pack into individual
components:
mag

```d
@nogc @system ReturnType!(scale) dispatcher(
  ref RemoveVirtual!(Parameters!(scale)[0]) _0, Parameters!(scale)[1..2] _1)
{
  resolve(_0)(_0, _1);
}
```

Note how the first and second parameters are handled differently. The first
parameter is cracked open, because we need to replace the type. That forces us
to access the first `Parameters` value using indexation - and that loses the
storage classes, UDAs, etc. So they need to be re-applied explicitly.

The second parameter, on the other hand, does not have this problem. It is not
edited; thus the slice of ``Parameters` trick can be used. The `lazy` is indeed
there - but it is inside the parameter conglomerate.

## Conclusion

Initially, D looked almost as good as Lisp for generating functions. As we
tried to gain finer control on the generated code, our code started to look a
lot more like C macros; in fact, in some cases, it was even worse: we have to
put an entire function definition in a string mixin to generate just its
name. This is due to the fact that D is not as "regular" a language
as Lisp.

Some of the people helming the evolution of D are working on addressing this
problem, and it is my hope that a much better D will emerge.

In the meantime, the experimental refraction module from the bolts
meta-programming library offers a saner, easier way of generating functions
without compromising on the galore of idiosyncrasies that come with them. It
allows you to pretend that functions can be disassembled and reassembled at
will, while hiding the gory details of the string mixins that are necessarily
involved in that task.
