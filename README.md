# Interpolator

Runtime interpolation of environment variables in records using profunctors.

## Overview

This library handles interpolation of variables nested within types using profunctors. This is
helpful if you want to store some information as templated JSON and inject variables at runtime.

Heavily influenced by [Opaleye](http://hackage.haskell.org/package/opaleye)'s use of profunctors,
and [Yaml](http://hackage.haskell.org/package/yaml)'s use of environment variable substitution.

## Quick Start

**NOTE** [Basic](examples/Basic.hs) and [ADT](examples/ADT.hs) examples available.

Define your records like this:

```haskell
data Foo' t_a t_b t_c = Foo
  { a :: t_a
  , b :: t_b
  , c :: t_c
  } deriving (Eq, Ord, Show)
```

Add your "uninterpolated" and "interpolated" types:

```haskell
type UninterpolatedFoo = Foo' (Uninterpolated Text) (Uninterpolated Int) (Uninterpolated Bool)
type Foo = Foo' Text Int Bool
```

Derive JSON encoders:

```haskell
deriveJSON defaultOptions ''Foo'
```

Add your `Default` instance and adaptor:

```haskell
makeAdaptorAndInstance "pFoo" ''Foo'
```

Now define your `Interpolator` (this part is optional since you will have the `Default` instance):

```haskell
fooInterpolator :: Interpolator UninterpolatedFoo Foo
fooInterpolator = def
```

Define the interpolation layer:

```haskell
-- make sure you define these environment variables!
uninterpolatedStr :: ByteString
uninterpolatedStr = "{\"a\": \"_env:foo\", \"b\": \"_env:bar\", \"c\": \"_env:baz\" }"

uninterpolated :: IO UninterpolatedFoo
uninterpolated = either fail pure $ eitherDecodeStrict' uninterpolatedStr

interpolated :: IO Foo
interpolated = either (fail . show) pure =<< interpolateWithContextExplicit fooInterpolator =<< uninterpolated
```

Run it:

```bash
➜  examples git:(v1) ✗ stack ghci
...
λ → interpolated
*** Exception: user error ([Interpolation key "foo" not found,Interpolation key "bar" not found,Interpolation key "baz" not found])
λ → :q
Leaving GHCi.
...
➜  examples git:(v1) ✗ foo=foo bar=1 baz=true stack ghci
...
λ → interpolated
Foo {a = "foo", b = 1, c = True}
```

That's it!

## FAQ

* How do I use sum types?

Redefine your ADTs like this:

```haskell
data Bar' a b
  = Bar1 a
  | Bar2 b
  | Bar3
  deriving (Eq, Ord, Show)
type UninterpolatedBar = Bar' (Uninterpolated Text) (Uninterpolated Int)
type Bar = Bar' Text Int
```

Then use `makeInterpolatorSumInstance` from `Data.Interpolation.TH`:

```haskell
makeInterpolatorSumInstance ''Bar'
```

* What does it mean if I have "No instance for Default Profunctor a b"?

Check your types and type synonyms. You should always be interpolating from an uninterpolated
type to an interpolated one. Typically the compiler will be clever enough to figure out exactly
which type breaks down the instance construction. If you get stuck, try being explicit about pinning
the `Default` instance as we did above with `fooInterpolator`.

## Feedback

Feel free to contact us on fpslack (@dfithian, @mprescot, @hkailahi), create an issue. Contributions
welcome!
