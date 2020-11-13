#!/bin/bash

set -ex

rm -f test.dat test.echo test.out
../app/miniFAVOR << EOF
test.in
EOF
diff -b test.echo.orig test.echo
numdiff -r 0.2 test.out.orig test.out
numdiff -r 1.0 -a 1.0 test.dat.orig test.dat
