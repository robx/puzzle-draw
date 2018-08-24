[![Build Status](https://api.travis-ci.org/robx/puzzle-draw.png?branch=master)][travis]

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

![Liar Slitherlink](https://raw.githubusercontent.com/robx/puzzle-draw/master/tests/examples/slitherlink-liar-example.png)

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
    . .-.-.-. .-.
      |     | | |
    . . .-.-. . .
      | |     | |
    . . .-.-.-. .
      |         |
    .-. . .-.-. .
    |     |   | |
    .-. .-. . . .
      | |     | |
    .-. . . . . .
    |   |     | |
    .-.-.     .-.
  liars: |
    ...X..
    .X....
    X.....
    .....X
    ....X.
    ..X...
```

For more examples, see:
* A [demo web application][demo] that provides some more
  examples and that can be used to generate SVG images from such
  puzzle descriptions.
* The [gallery of examples][examples] used to test the program.
* A [puzzle set][twentyfour] that covers the puzzle types
  that were supported as of 2014-05.
* Most of the puzzle graphics on the puzzle blog [maybepuzzles][maybepuzzles]
  were generated with this tool.

Installing
----------

This is a brief overview of how to install the command line tool.
The easiest way right now is to get [stack](https://www.stackage.org/),
clone this repository, and install using

```
$ stack install
```

Alternatively, get a Haskell environment by installing the
[Haskell Platform](https://www.haskell.org/platform/).
Then install `puzzle-draw` with cabal:

```
$ cabal install puzzle-draw
```

Running
-------

As an example, suppose the Liar Slitherlink puzzle description above
is copied into a file `slitherliar.pzl`.

```
$ drawpuzzle slitherliar.pzl
```

This will generate two files `slitherliar.png` and `slitherliar-sol.png`,
containing the puzzle and solved puzzle, respectively. Run

```
$ drawpuzzle -h
```

to see some command line options that allow modifying the program's
behaviour, e.g., choosing the output format.

[travis]: https://travis-ci.org/robx/puzzle-draw
[liarslither]: https://maybepuzzles.wordpress.com/types/liar-slither-link/
[twentyfour]: https://maybepuzzles.wordpress.com/2014/03/29/puzzle-set-24-hour-marathon/
[diagrams]: http://projects.haskell.org/diagrams/
[demo]: https://arp.vllmrt.net/puzzles/draw/static/puzzle.html
[maybepuzzles]: https://maybepuzzles.wordpress.com/
[examples]: https://rawgit.com/robx/puzzle-draw/master/tests/examples/examples.html
