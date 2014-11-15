[![Build Status](https://api.travis-ci.org/robx/puzzle-draw.png)][travis]

puzzle-draw
===========

puzzle-draw is a library and command-line tool for drawing pencil
puzzles using [Diagrams][diagrams]. It aims to provide a utility layer
on top of Diagrams to help with drawing arbitrary puzzles, as well as
supporting several specific puzzle types directly. In addition, it
includes functionality for parsing puzzle data from a YAML file format.

Examples
--------

A [liar slitherlink][liarslither] with solution:

![Liar Slitherlink](doc/slitherlink-liar-example.png)

This was rendered from the following YAML document:

```
type: slitherlinkliar
puzzle: |
  1..0.3
  .03222
  0....1
  3....3
  32202.
  3.3..3
solution:
  loop: |
    .┌──┐┌┐
    .│┌─┘││
    .│└──┘│
    ┌┘.┌─┐│
    └┐┌┘.││
    ┌┘│..││
    └─┘..└┘
  liars: |
    ...X..
    .X....
    X.....
    .....X
    ....X.
    ..X...
```

There is a demo web application at
[puzzle-draw-demo.herokuapp.com][demo] that provides some more
examples and that can be used to generate SVG images from such
puzzle descriptions.

Or see a [puzzle set][twentyfour] that covers the puzzle types
that are supported as of 2014-05.

Installing
----------

This is a brief overview of how to install the command line tool.
You'll need to get a Haskell environment, by installing the
[Haskell Platform][platform]. Then install `puzzle-draw` with cabal:

```
$ cabal install puzzle-draw
```

This will compile puzzle-draw against the SVG backend, and install
a binary `drawpuzzle` that formats puzzles to SVG. Alternatively,
you can compile against the Cairo backend by passing `-fcairo`:

```
$ cabal install -fcairo puzzle-draw
```

This will enable PDF and PNG output. Be warned that getting Cairo
installed can be tricky on non-Linux platforms.

Running
-------

As an example, suppose the Liar Slitherlink puzzle description above
is copied into a file `slitherliar.pzl`.

```
$ drawpuzzle slitherliar.pzl
```

This will generate two files `slitherliar.svg` and `slitherliar-sol.svg`,
containing the puzzle and solved puzzle, respectively. Run

```
$ drawpuzzle -h
```

to see some command line options that allow modifying the program's
behaviour, e.g., choosing the output format.

[travis]: https://travis-ci.org/robx/puzzle-draw
[cmdline]: https://github.com/robx/puzzle-draw-cmdline
[liarslither]: https://maybepuzzles.wordpress.com/types/liar-slither-link/
[twentyfour]: https://maybepuzzles.wordpress.com/2014/03/29/puzzle-set-24-hour-marathon/
[diagrams]: http://projects.haskell.org/diagrams/
[demo]: https://puzzle-draw-demo.herokuapp.com
[platform]: https://www.haskell.org/platform/
