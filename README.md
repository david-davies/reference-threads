# Reference Threads (`RT`)
A more fine-grained version of state threads (`ST`). Instead of carrying around a type parameter to
represent the lifetime of the overall stateful computation, `RT` instead parameterises its references
with a unique lifetime. This allows them to have more fine-grained lifetimes and avoids the need
for the overall computation to carry a type parameter: this makes them more suitable for frictionless
and safe use in other libraries without exposing anything to the user unnecessarily.

As an example, [`gigaparsec`](https://github.com/j-mie6/gigaparsec) makes use of `RT` internally to
facilitate the use of references in a parser. If the user is not using stateful references, they can
completely ignore the functionality without any additional `s` parameter on their parsers.

Compared with `ST`, the downside of using `RT`'s `Ref` is that the `newRef` operation is necessarily
written in continuation-passing style, with the created reference only live during the continuation.
As an example, consider the differences between the following operation types:

| Operation          | `ST`                      | `RT`                                       |
| :----------------- | :------------------------ | :----------------------------------------- |
| Monad running      | `(forall s. ST s a) -> a` | `RT a -> a`                                |
| Reference creation | `a -> ST s (STRef s a)`   | `a -> (forall r. Ref r a -> RT b) -> RT b` |

The introducer the rank-2 type is the key difference between these abstractions. This is a small
ergonomic price to pay for the luxury of not needing the type-parameter on the rest of the computation.

## Unsoundness
The `RT` monad is a hybrid between the safe `ST` and the more unsafe `IO` monad. While `RT` cannot
perform `IO`, it is still possible for a `Ref` to leak out from underneath its enclosing scope by
packing it into an existential.

```hs
data EscapedRef a = forall r. EscapedRef (Ref r a)
escape :: a -> RT (EscapedRef a)
escape x = newRef x (return . EscapedRef)
```

This effectively decays it into the equivalent of an `IORef` when unpacked. However, it is not
possible to use `coerce` to allow a reference to escape its scope.
