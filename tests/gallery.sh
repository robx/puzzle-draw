#!/bin/sh

set -eu

echo "<html>"
echo "<body>"
for file in "$@"; do
    file=`basename $file`
    typ=`basename $file .png`

    cat <<HERE
<a href="tests/examples/$typ.pzl">$typ</a><br>
<img src="$file" title="$file new"><br>
<img src="../tests/examples/$file" title="$file golden"><br>
<img src="diff/$file" title="$file diff"><br>
<hr>
HERE

done
