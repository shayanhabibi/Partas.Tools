# LibGit2Sharp.FSharp

Wrapper.

```ansi
dotnet add package Partas.LibGit2Sharp.FSharp
```

```fsharp
open LibGit2Sharp.FSharp
```

## Source

If you're looking at the source and wondering:

> What the unholy word is all these `get` private bindings

I would start making a binding for a class by making one of these helper functions so I could use intellisense to explore available methods: `get _.`.

While looking through the methods and their docs, I would determine whether I would wrap the method with an optional, and go from there.

Since the compiler would raise errors regarding ambiguous types, I would use the IDE to insert the type annotations *after*.
