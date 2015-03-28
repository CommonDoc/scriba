# scriba

A markup format similar to [Scribe](http://en.wikipedia.org/wiki/Scribe_%28markup_language%29).

# Overview

## Syntax

This is a tag: `@name(content)`.

This is a begin/end block:

```
@begin(name)
content
@end(name)
```

This is a tag with attributes: `@name[attr=a prop="Hi!"](content)`.

In a begin-end block, attributes go in the `begin` part.

```
@begin[attr=something](node)
...
@end(node)
```

This is an empty tag: `@node()`. The parentheses are required.

You can put at signs and parentheses inside nodes: `@b(Email be at me (@)
domain.com)`.

## Example

```
@title(My Document)

@begin(section)
@title(Part the First)

This is a paragraph.

This is another paragraph, with @b(bold text) and @i(@u(italicized, underlined
text)).

@end(section)

@begin(section)
@title(Markup languages)

@begin(deflist)

@term(VerTeX)
@def(A markup language with text syntax.)

@term(Scriba)
@begin(def)
A markup language with Scribe syntax.

As you can see, you can use regular tags and begin/end ones interchangeably.
@end(def)

@end(deflist)

@end(section)
```

# Usage

# Similar Projects

* [Skribilo](http://www.nongnu.org/skribilo/)
* [Scribble](http://quickdocs.org/scribble/)

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.
