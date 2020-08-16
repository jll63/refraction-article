# Function Generation in D: the Good, the Bad, the Ugly

## Introduction

A while ago Andrei started a thread ("Perfect Forwarding") in the D forum about
a challenge, which came up during the July 2020 beerconf:

> Write an idiomatic template `forward` that takes an alias `fun` and defines
> (generates) one overload for each overload of `fun`".

During the discussion, several people came up with solutions. It also appeared
that sometimes there is a need to alter the function's properties - like add,
remove or changeuser-defined attributes or parameter storage classes.

It tuns out that this is exactly the problem I struggled with while trying to
support all the bells and whistles of function in my openmethods
library. Eventually I created a module that helps with this problem, and
contributed it to bolts.

But first, let's take a closer look at function generation in D.

## The Good

I speculate that many a programmer who is a moderately advanced beginner in D
would quickly come up with a mostly correct solution - especially those with a
C++ background and with an interest in performing all sorts of magic tricks by
means of template meta-programming. The solution will probably look like this:

```d
template forward(alias fun) {
  import std.traits: Parameters;
  static foreach (ovl; __traits(getOverloads, __traits(parent, fun),
    __traits(identifier, fun))) {
    auto forward(Parameters!ovl args) {
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

This solution is not perfect, but it is not far off either. It covers many
cases, including some that a beginner may not even be aware of. For example,
`forward` handles the following function without dropping function attributes
or parameter storage classes:

```d
class Matrix { ... }

Matrix times(scope const Matrix a, scope const Matrix b) pure @safe {
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
// int function(@("test") bool pred, lazy int a, lazy int b) @safe

pragma(msg, typeof(forward!testPlus));
// int function(@("test") bool _param_0, lazy int _param_1, lazy int _param_2) @safe
```

Talking about UDAs, that's one of the issues with the solution above: it
doesn't carry *function* UDAs. Also, it doesn't work with `ref` returning
functions. Both issues are easily fixed:

```d
template forward(alias fun) {
  import std.traits: Parameters;
  static foreach (ovl; __traits(getOverloads, __traits(parent, fun), __traits(identifier, fun))) {
    @(__traits(getAttributes, fun)) // also copy UDAs
    auto ref forward(Parameters!ovl args) {
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
template forward(alias fun) {
  import std.format : format;
  import std.traits: Parameters;
  enum name = __traits(identifier, fun);
  static foreach (ovl; __traits(getOverloads, __traits(parent, fun), name)) {
    @(__traits(getAttributes, fun))
    auto ref mixin(name)(Parameters!ovl args) {
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
template forward(alias fun) {
  import std.format : format;
  import std.traits: Parameters;
  enum name = __traits(identifier, fun);
  static foreach (ovl; __traits(getOverloads, __traits(parent, fun), name)) {
    mixin(q{
        @(__traits(getAttributes, fun))
          auto ref %s(Parameters!ovl args) {
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
interface JsonSerializable {
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
class Mock(alias Interface) : Interface {
  import std.format : format;
  import std.traits: Parameters;
  static foreach (member; __traits(allMembers, Interface)) {
    static foreach (fun; __traits(getOverloads, Interface, member)) {
      mixin(q{
          @(__traits(getAttributes, fun))
          auto ref %s(Parameters!fun args) {
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
class Mock(alias Interface) : Interface {
  import std.format : format;
  import std.traits: Parameters, ReturnType;
  static foreach (member; __traits(allMembers, Interface)) {
    static foreach (fun; __traits(getOverloads, Interface, member)) {
      mixin(q{
          @(__traits(getAttributes, fun))
          ReturnType!fun %s(Parameters!fun args) {
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
          ref ReturnType!fun %s(Parameters!fun args) {
```

But this will fail with all the functions in the interface that do *not* return
a reference.

Then there is the issue of the function modifiers: `const`


See, the reason why everything worked almost by magic in the first challenge is
that we called the wrapped function. It enabled the compiler to deduce almost
all the characteristics of the original function, and copy them to the
forwarder function. Here we have no such model. The compiler will copy *some*
of the aspects of the function (`pure`, `@safe`, etc) to match those of the
overriden function, but not some others (`ref`, `const` and the other
modifiers).

At this point, there is no other option than to painstakingly analyze a subset
of the function attributes by means of traits, convert them to a string, to be
injected in the string mixin:


```d
      mixin(q{
          @(__traits(getAttributes, fun))
          %sReturnType!fun %s(Parameters!fun args) {
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

If you think that this is not bad enough, there's still more to come.

So far we have looked at the function storage classes, modifiers, and UDAs, but
we have merely passed the parameter list as a single, monolithic
block. However, it is sometimes needed to perform adjustments to the parameter
list of the generated function. I encountered this problem in my `openmethods`
library. I won't dwelve into the details (see this blog post for that); suffice
to say that, given a function declaration like this one:


```d
Matrix times(double a, scope virtual!Matrix b);
```

```d
```

```d
```

```d
```

```d
```
