# Mixture, a mixins library for OCaml

The **Mixture** project provides a comprehensive mixins library for
the OCaml module system.

[![Build Status](https://travis-ci.org/michipili/mixture.svg?branch=master)](https://travis-ci.org/michipili/mixture?branch=master)


## Example usage of mixins

The *Mixture_Compare* module defines a mixin which derives all
classical comparison operators (in prefix and infox form) from a
*compare* function.  For instance

```ocaml
module PlayingCard =
struct
  type card =
  | Card of regular
  | Joker
  and regular = { suit : card_suit; name : card_name; }
  and card_suit = Heart | Club | Spade | Diamond
  and card_name =  Ace | King | Queen | Jack | Simple of int

  module Basis =
  struct
    type t = card
    let compare = Pervasives.compare
  end

  module CompareMethods =
    Mixture_Compare.Make(Basis)

  include Basis
  include CompareMethods
end
```

The module *PlayingCard* has prefix and infix comparison operators
derived from *compare* so, if *a* and *b* are cards, one can write
`PlayingCard.lt a b` or equivalently `PlayingCard.Infix.(a < b)`.


## Provided mixins

The growing list of mixins defined in the library is:

- **Mixture_Applicative**
- **Mixture_Compare**
- **Mixture_Format**
- **Mixture_Parse**
- **Mixture_Monad**
- **Mixture_FoldRight**
- **Mixture_FoldAssoc**


## Free software

It is written by Michael Grünewald and is distributed as a free
software: copying it  and redistributing it is
very much welcome under conditions of the [CeCILL-B][licence-url]
licence agreement, found in the [COPYING][licence-en] and
[COPYING-FR][licence-fr] files of the distribution.


## Setup guide

It is easy to install **Mixture** using **opam** and its *pinning*
feature.  In a shell visiting the repository, say

```console
% autoconf
% opam pin add mixture .
```

It is also possible to install **Mixture** manually.
The installation procedure is based on the portable build system
[BSD Owl Scripts][bsdowl-home] written for BSD Make.

1. Verify that prerequisites are installed:
   - BSD Make
   - [BSD OWl][bsdowl-install]
   - OCaml
   - [Broken][broken-home]
   - GNU Autoconf

2. Get the source, either by cloning the repository or by exploding a
   [distribution tarball](releases).

3. Optionally run `autoconf` to produce a configuration script. This
   is only required if the script is not already present.

4. Run `./configure`, you can choose the installation prefix with
   `--prefix`.

5. Run `make build`.

6. Optionally run `make test` to test your build.

7. Finally run `make install`.

Depending on how **BSD Make** is called on your system, you may need to
replace `make` by `bsdmake` or `bmake` in steps 5, 6, and 7.
The **GNU Make** program usually give up the ghost, croaking
`*** missing separator. Stop.` when you mistakingly use it instead of
**BSD Make**.

Step 7 requires that you can `su -` if you are not already `root`.


Michael Grünewald in Bonn, on August 11, 2015

  [licence-url]:        http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html
  [licence-en]:         COPYING
  [licence-fr]:         COPYING-FR
  [bsdowl-home]:        https://github.com/michipili/bsdowl
  [bsdowl-install]:     https://github.com/michipili/bsdowl/wiki/Install
  [broken-home]:        https://github.com/michipili/broken
