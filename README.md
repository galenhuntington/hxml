##  Introduction

`hxml` is a simple, convenient way to write HTML and XML.  Mainly,
it reduces the clutter and burden of closing tags.

Writing HTML directly can be unpleasant, and tools exist to generate
it at different levels of abstraction.  Ultimately, however, most
developers will need to work with HTML source directly to get what
they want done.

`hxml` source compiles to regular HTML or XML, and is compatible with
it, but with a few added features.

This syntax was developed as part of a larger ecosystem written in
Haskell (called `gxml`).  However, this part was useful on its own,
and so I extracted it into one standalone file of Haskell source.

You do not need to know any Haskell to use it, and I have re-written
it to avoid all dependencies but a Haskell Platform install.
See __Installation and use__ below.


##  Features

###  Error checking

`hxml` checks your HTML for unclosed or wrongly-closed tags.  All tags
must be closed, or self-closed such as `<img src="picture.png"/>`.

###  Generalized tag closing

The design goals for XML regarded verbosity as a non-issue.
One example is that tags must be closed with the full name of the
initial tag.  This can sometimes be an aid in reading source, but
often it is not helpful at all.  For example, in HTML documents with
numerous levels of `<div>`s, seeing `</div>` tells you very little.
And for short snippets of enclosed text, it just adds work and noise.

`hxml` allows the name of the closing tag to be omitted, so that

```html
This is a <span class="purple">purple chunk</> of text.
```

compiles to

```html
This is a <span class="purple">purple chunk</span> of text.
```

Going the other way, you can supply extra information in
the closing tag:

```html
<div id="menubar" class="offset">
   <div>Home</><div>Dashboard</>
</div id="menubar">
```

Attributes on the closing tag will be checked by `hxml` to see if they
match the opening tag, and then removed.  This can catch mis-matches
that an HTML checker would miss.

###  Block tag scope

The syntax `<tag:>` with a colon will scope the tag over an indented
block.  For example,

```html
<div:>
   <h1:>This is a header
   <p class="big xyz":>This is a test paragaraph with
      multiple lines of
      content.
```

will become

```html
<div>
<h1>This is a header.</h1>
<p class="big xyz">This is a test paragaraph with
multiple lines of
content.</p></div>
```

You can include blank lines within the indented block.

###  Attributes

XML requires that all attributes be quoted.  This can be inconvenient
and pointless for simple values, so `hxml` will quote these for you:

```html
<table cellpadding=0 cellspacing=1>
```

turns into

```html
<table cellpadding="0" cellspacing="1">
```

Of course, if there are special characters such as spaces or slashes
in the attribute value, you'll still need to quote them.

An attribute with no value is expanded to having itself as a value,
so that `<input disabled>` becomes `<input disabled="disabled">`.

Attributes starting with an underscore (`_`) are removed.  These can
be used for documentation and/or matching:

```html
<div _pricelist>
   content
</div _pricelist>
```

To my knowledge, no application uses attributes that start with
an underscore.  If one turns up, I may revisit this syntax.

###  Short-content tags

A self-closing tag such as `<hr/>` has no content.  We generalize
this by allowing short content after the closing slash.  That is,

```html
This is a <b/bold> word.
```

expands to

```html
This is a <b>bold</b> word.
```

This should only be used for short text with no potential syntactic
ambiguity.  For anything more, just use the closing tag syntax above:

```html
This is a <b>bold/strong/emphasized</> phrase.
```

###  Chomments

Content enclosed with the `<#>` tag is considered an `hxml` comment and
is removed.  I dub these _chomments_.  They pattern like any other tag:

```html
<#>This will be removed.</#>
<#>This uses the simpler closing tag.</>
<#:>
   A potentially longer block
   chomment.
<#/a short chomment>
```

These are distinct from `<!-- comments -->`.  The latter are preserved
by `hxml`, and should be used for comments intended to be in the
final HTML.

Inside a chommented block, tags are not parsed and matched since the
indentation is sufficient to deliminate the block.

##  Encoding

`hxml` is fairly encoding-agnostic.  It should work with UTF-8,
ISO-8859-1, and any other 8-bit extension of ASCII.  It will not work
with UTF-16 and the like.

##  Fine points

I have tried to address various side and corner cases reasonably,
although I may try different approaches in the future.

###  Empty content

In XML a self-closed tag is equivalent to a tag with no content, but
not all browsers treat them the same, so users may want control over
which is rendered.  `hxml`'s approach is simply to preserve the input.
Thus, `<br/>` remains `<br/>`, while `<textarea></>` expands to
`<textarea></textarea>`.  `<textarea/Text>` will still expand to
`<textarea>Text</textarea>`.

###  Whitespace

HTML has an odd relationship to whitespace.  Sometimes it matters,
sometimes it's ignored.  `hxml` has careful rules for dispatching it.

As illustrated, blocked tags attach the closing tag to the end of the
last line of the block.  If space is desired before the closing tag,
you'll have to fit it in somehow, say with a space at the end of the
last line.  Chomments can help visually here, such as the empty `<#/>`.
If you require a newline, a line with only the indent can be added.

It might be simpler to use `</>` in some cases.

Controlling space after the opening tag is easier.  `hxml` also
provides a mechanism for suppressing a newline immediately after tag,
in case it is more sightly to have the whole block indented the same:
simply put a space before the colon.  Thus,

```html
<span :>
   One line
```

becomes `<span>One line</span>`.  This is perhaps a strange choice of
syntax, but it has served well enough.

Chomments pattern just like block tags in that a chommented block
will turn into a newline.  However, there is one exception: a one-line
chommented block disappears completely, so that

```html
Foo<#:>chomment
bar.
```

becomes `Foobar`.

This is halfway between a bug and a feature.  I set out to fix this
defect in the parser, but decided this exception may be useful in
practice, and so for now I am leaving it.

###  Error reporting

Error reporting should be pretty good.  I initially wrote `hxml` using
[Megaparsec](https://hackage.haskell.org/package/megaparsec), but
rewrote it to use [Parsec](https://hackage.haskell.org/package/parsec),
since it is more standard and ships with the Haskell Platform.
However, Parsec's error mechanisms are different and not everything
translated elegantly.

###  Other

Attributes on a close tag must be quoted (or not) the same as on
the opening tag.  E.g., `<i class="foo">text</i class=foo>` yields
an error.

Tags are compacted to minimal spacing.  For instance, a tag split
across several lines will be compressed to one line, so

```html
<div
   _updated=2017-07-19
   id=main_menu
   class=myclass
   ng-if=shown
   _todo=replace
   >
```

becomes `<div id="main_menu" class="myclass" ng-if="shown">`.

Tabs in the source are preserved and are treated as 4 spaces wide when
comparing indentation.  This value can be set in code as `tabWidth`.
As usual, beware when mixing tabs and spaces.


##  Installation and use

You will need to install GHC, and normally you'll want the whole
Haskell Platform.

`hxml.hs` is a standalone file that can be run with `runghc` (or its
alias `runhaskell`) and takes `stdin` to `stdout`, e.g.:

```bash
runghc hxml.hs < main.hxml > main.html
```

This compiles the code each time without optimization, so for long-term
use it's better to compile it:

```bash
ghc --make -O hxml.hs
```

Then you can run the executable `hxml`, which is suitable for batch
use, say as part of a build system:

```bash
hxml < main.hxml > main.html
```

Instead of the Haskell Platform you can install dependencies either
through your package manager, or through Cabal, e.g.,

```bash
cabal update
cabal install parsec
```

If your application is written in Haskell, you can also use
the `compileHxml` function directly.  As an example, I write
[Heist](https://hackage.haskell.org/package/heist) templates in `hxml`.

Feedback is welcome!
