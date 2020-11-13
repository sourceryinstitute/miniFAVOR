#!/bin/bash

set -ex

rm -f test.dat test.echo test.out
../app/miniFAVOR << EOF
test.in
EOF
numdiff -r 1.0e-6 test.echo.orig test.echo
numdiff -r 1.0e-6 test.out.orig test.out
numdiff -r 1.0e-6 test.dat.orig test.dat
