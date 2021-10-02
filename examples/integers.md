---
title: Integers
---

This is a simple example of a grammar which accepts arbitrary decimal integers
without leading zeroes.

```nbnf
<integer> = [ "-" ] <non-zero digit> { <<digit>> }
```

In contrast to BNF, non-zero digits can be defined in NBNF by using a
_character exception_:

```nbnf
<non-zero digit> = <<digit>> \ { "0" }
```

```nbnf
<<digit>> = "0" | ... | "9"
```
