# NBNF Tools

Converting NBNF grammars embedded in Markdown documents to other formats


## What are NBNF grammars?

NBNF (e**N**riched **BNF**) is an extension of [BNF][1] which adds a bit
syntactic sugar to the latter. In particular this means that every grammar which
can be expressed in NBNF can also be expressed in BNF. Hence NBNF does not leave
the scope of [context-free grammars][2].

For a description of the syntax and semantics of NBNF, have a look at the file
[_docs/nbnf-description.md_](./docs/nbnf-desctiption.md). A fully formal
specification of the syntax of NBNF as it is accepted by this package's parser
can be found in the file [_docs/nbnf-syntax.md_](./docs/nbnf-syntax.md).


## Features

This package provides a tool called `nbnf-exe` which can be used to convert
[Pandoc Markdown][3] (in the following just called _Markdown_) files containing
NBNF rules to the following file formats:

  * Plain text
  * XML
  * HTML

When converted to plain text or XML the resulting file will only contain the
NBNF rules of the Markdown file, whereas when converting to HTML it will also
contains all additional content of the Markdown file like headings, tables,
sourrounding text and so on.

Thus for `nbnf-exe` to be able to distinguish between NBNF rules and other
content of the Markdown file, it is necessary to enclose every single rule
within ` ```nbnf` and ` ``` `, e.g.

```nbnf
<foo> = <bar>
      | "baz"
```


## Setup

To generate an executable from this package, first make sure that the tools
[git][4] and [stack][5] are installed on your system. Then execute the following
commands in any directory of your choice:

```sh
git clone https://github.com/McEarl/nbnf-tools.git
cd nbnf
stack build
```


## Usage

To convert a Markdown document which contains NBNF rules to plain text, XML or
HTML run the following command:

```sh
stack exec nbnf-exe -- -i INFILE -o OUTFILE [-h (standalone|jekyll)]
```

The `-i` option specifies an input file. If several instances of it occur, the
specified files will be concatenated in the order they appear and treated as one
big file.

The option `-o` specifies the output file and its file format which is derived
from its file name extension. Supported are `.txt` (plain text), `.html`
(HTML, either a stand-alone document or one adapted to Jekyll), `.xml` (XML) and
`.log` (for debugging only).  
**Warning:** If a file with the same name as the output file already exists it
will be overridden without further inquiry!

If the output format is HTML, you must specify whether the output file should be
a stand-alone document (via `-h standalone`) or a HTML file which is adapted to
be used with Jekyll (via `-h jekyll`).

Moreover, the option `--help` prints a little help message.


[1]: <https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form>
[2]: <https://en.wikipedia.org/wiki/Context-free_grammar>
[3]: <https://github.github.com/gfm/>
[4]: <https://git-scm.com/download/linux>
[5]: <https://docs.haskellstack.org/en/stable/install_and_upgrade/>
