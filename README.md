[![Build Status](https://api.travis-ci.org/robx/puzzle-draw.png)][travis]

puzzle-draw
===========

puzzle-draw is a library for drawing pencil puzzles using 
[Diagrams][diagrams]. It aims to provide a utility layer on top of 
Diagrams to help with drawing arbitrary puzzles, as well as supporting 
several specific puzzle types directly. In addition, it includes 
functionality for parsing puzzle data from a YAML file format.

See [puzzle-draw-cmdline][cmdline] for a command line tool that
uses this.

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

Or see a [puzzle set][twentyfour] that covers the puzzle types
that are supported currently.

[travis]: http://travis-ci.org/robx/puzzle-draw
[cmdline]: http://github.com/robx/puzzle-draw-cmdline
[liarslither]: http://maybepuzzles.wordpress.com/types/liar-slither-link/
[twentyfour]: http://maybepuzzles.wordpress.com/2014/03/29/puzzle-set-24-hour-marathon/
[diagrams]: http://projects.haskell.org/diagrams/
