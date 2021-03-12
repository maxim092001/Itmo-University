#!/bin/sh

CMD=$1
shift
for arg do
    $CMD -n $arg | diff -u --from-file ${arg}.eta.n - || exit 1
done
