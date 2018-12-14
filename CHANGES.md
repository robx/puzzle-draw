* Convert puzzle types to PZG:
  - boxof2or3
  - horsesnake
  - starwars
  - pentopipes
* PZG changes:
  - Add `fullgrid` grid type.
  - Add edge decorations

0.3.4.0: 20181212
-----------------

* drawpuzzle now never writes more than one output file per
  input file. Solution rendering has to be explicitly requested
  by passing `-s`.
* Introduced a new experimental lower level graphic format PZG.
  A few of the one-off types have been removed in favour of this
  format: afternoonskyscrapers, wormhole, lits-symmetry, primeplace
* Web interface changes:
  - Allow choosing device in the web interface.
  - Fix link back to github.

0.3.3.0: 20181204
-----------------

* Fix corrupt PDFs by Rasterific update.
* Fix colorakari crash.

0.3.2.3: 20181127
-----------------

* Support for scaling in web frontend.

0.3.2.2: 20181126
-----------------

* Solution for Color Akari.
* Don't crash for puzzle types that are missing
  solution parsing or rendering.

0.3.2.1: 20181126
-----------------

* Add new web frontend.

0.3.2.0: 20181126
-----------------

* Remove non-existent PostScript support.
* Embed fonts in binary.

0.3.1.0: 20181119
-----------------

* Add SVG and JPG output.
* Support differences between print and screen output. Thinner lines
  for PDF output.
* Layout improvements for puzzles with outside clues.
* Switch font.
* Support `render-as:` key in `.pzl` format.
* Remove support for drawing curves with "box drawing characters".

0.3.0.0: 20180718
-----------------

* Switch backend to diagrams-rasterific, losing the dependency on Cairo.
* Upgrade to diagrams 1.4.
* Various new puzzle types, including Minesweeper, Tents, Snake,
  Country Road, Killer Sudoku.

0.2.0.0: 20161211
-----------------

* Upgrade diagrams version.
* Add various new puzzle types.
* Add solution code markers.

0.1.0.4: 20141115
-----------------

* add installation and usage instructions to README
* merge puzzle-draw-cmdline
* update for diagrams 1.2 and ghc 7.8
* add new puzzle types: Bahnhof, Cave
* rename `countnumbers` to `meanderingnumbers`
* include and use a nicer font
* command-line option to list supported puzzle types
* various tweaks and fixes

0.1.0.3: 20140519
-----------------

* add new puzzle types: Maximal Lengths, Prime Place, Magic Labyrinth


0.1.0.2: 20140419
-----------------

* add new puzzle types: Japanese Sums, Coral

0.1.0.1: 20140409
-----------------

* add new puzzle type: Tapa

0.1.0.0
-------

* first release
