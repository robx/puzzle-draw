#!/bin/bash

set -eu

echo "<html>"
cat <<HERE
<style>
body {
        display: grid;
        grid-template-columns: 1fr 2fr;
        grid-gap: 10px;
}
img {
    display: block;
    margin: 0 auto;
    max-width: 100%;
}
</style>
HERE

echo "<body>"
for file in "$@"; do
    base=`basename "$file" .pzl`

    echo "<pre>"
    cat "$file"
    echo "</pre>"
    echo '<img src="'$base.png'" title="'$base'">'
done