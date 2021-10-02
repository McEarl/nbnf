# Description of NBNF

In contrast to BNF, NBNF comes with two kinds of non-terminals:
_Proper non-terminals_ (in the following just called _non-terminals_) and
_alphabets_. Similar to BNF, an NBNF grammar is just a list of rules defining
non-terminals and alphabets. The following paragraphs describe the syntax and
semantics of such rules.


## Non-terminals

Non-terminal rules have the following form:

```
<IDENTIFIER> ::= EXPRESSION
```

Here (and in the following) upper-case words function as variables, whereas
all other symbols are part of the syntax of NBNF.

* `IDENTIFIER` should be a unique name of the non-terminal and can consist
  of alphanumeric characters, spaces and the symbols `_` and `-`.  

* `EXPRESSION` can be any of the following _expressions_:

  * A choice expression
  * A sequence expression
  * An option expression
  * A grouping expression
  * A repetition expression
  * A case-sensitive string expression
  * A case-insensitive string expression
  * A non-terminal expression
  * An alphabet expression
  * A character exception
  * A string exception

* Instead of the _separator_ `::=` you can also use the following ones:
  `:=`, `=`, `<-` `->`


### Choices

Choice expressions have the following form:

```
EXPRESSION | EXPRESSION | ... | EXPRESSION
```

Here the dots `...` are not part of the syntax of NBNF, but indicate an
arbitrarily large list of expressions separated by the symbol `|`. As in BNF,
choice expressions match any string which is matched by at least one of the
listed expressions.


### Sequences

Sequence expressions have the following form:

```
EXPRESSION EXPRESSION ... EXPRESSION
```

Again, the dots `...` are not part of the syntax of NBNF, but indicate an
arbitrarily large list of expressions. As in BNF,sequence expressions match any
string which is a concatenation of strings matched by the listed expressions
(respecting the order in which these expressions are listed).


### Options

Option expressions have the following form:

```
[ EXPRESSION ]
```

As in BNF they match the empty string and any string which is matched by the
given expression.


### Grouping

Grouping expressions have the following form:

```
( EXPRESSION )
```

They are only used to structure an NBNF rule and match exactly the same strings
as the given expression does.


### Repetigion

Repetition expressions have the following form:

```
{ EXPRESSION }
```

As in BNF they match zero or more repetitions of strings which are matched by
the given expression.


### Case-sensitive strings

Case-sensitive string expressions have the following form:

```
"STRING"
```

As in BNF they match exactly the given string. Within the string `STRING` the
characters `'`, `"` and `\` must be escaped by a prepending backslash. Moreover,
any Unicode character with hexadecimal code point `XXXX` can be represented by
the sequence `\U+XXXX;` within `STRING`. In the following these two rules for
escaping characters and representing Unicode characters are commonly refered to
as _escaping rules_.


### Case-insensitive strings

Case-insensitive string expressions have the following form:

```
'STRING'
```

This expression matches variants of `STRING` which we get by replacing arbitrary
characters within it by its lower-case or upper-case pendants. For instance
`'ab1'` matches the strings `ab1`, `aB1`, `Ab1` and `AB1`. Again, the escaping
hold for `STRING`.


### Non-terminals

Non-terminal expressions have the following form:

```
<IDENTIFIER>
```

As in BNF they match every string which is matched the non-terminal with
identifier `IDENTIFIER`.


### Alphabets

Alphabet expressions have the following form:

```
<<IDENTIFIER>>
```

They behave just like non-terminal expressions, but match strings which match
a certain alphabet.


### Character exceptions

Character exceptions have the following form:

```
<<IDENTIFIER>> \ {CHARACTER-LIST}
```

They match every character which is matched by the alphabet with identifier
`IDENTIFIER`, except the characters listed in the _character list_
`CHARACTER-LIST`.

A character list is just a comma-separated list of characters, each enclosed
within double or single quotes, where double quotes represents the literal
character and single quotes represent all of the character's case-insensitive
variants. For each of these characters the escaping rules hold.


### String exceptions

String exceptions have the following form:

```
<<IDENTIFIER>>OPERATOR \ {STRING-LIST}
```

Depending on whether `OPERATOR` is the symbol `*` or `+`, they match every
string which is a possibly empty or non-empty word, respectively, over the
alphabet which consists of all characters that are matched by the alphabet with
identifier `IDENTIFIER`, except those strings listed in the _string list_
`STRING-LIST`.

A string list is just a comma-separated list of strings, each enclosed within
double or single quotes, where double quotes represents the literal string and
single quotes represent all of the string's case-insensitive variants. For each
of these strings the escaping rules hold.


## Alphabets

Alphabet rules have the following form:

```
<<IDENTIFIER>> ::= CHARACTER-SELECTION
```

* Again, `IDENTIFIER` should be a unique name of the non-terminal and can
  consist of alphanumeric characters, spaces and the symbols `_` and `-`.

* `CHARACTER-SELECTION` is a list of _single characters_ or _character lists_,
  separated by the symbol `|`. It matches all strings which are matched by at
  least one of the single character or character list expressions.

* Again, the symbol `::=` can also be replaced by `:=`, `=`, `<-` or `->`.


### Single characters

A single character is a literal character enclosed within double quotes and
matches every string which consists of just this single character. Note that
the escaping rules apply to it.


### Character list

A character list has the following form:

```
CHARACTER-1 | ... | CHARACTER-1
```

Here, `CHARACTER-1` and `CHARACTER-2` are arbitrary single characters (cf. the
last rule). This time, the dots `...` are part of the syntax of a character list
and do _not_ indicate an arbitrary large repetition of something. Such a
character list match every string which consists of a single character whose
Unicode code point is greater or equal than the code point of `CHARACTER-1` and
less or equal than the code point of `CHARACTER-2`.
