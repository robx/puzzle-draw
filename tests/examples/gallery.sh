#!/bin/bash

set -eu

echo "<html>"
echo "<body>"
for file in "$@"; do
    file=`basename "$file"`
    typ=`basename "$file" .png`

    cat <<HERE
<a href="$typ.pzl">$typ</a><br>
<img src="$file" title="$file golden"><br>
<img src="cur/$file" title="$file new"><br>
<img src="diff/$file" title="$file diff"><br>
<hr>
HERE

done
