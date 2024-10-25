#!/bin/bash

# should use --top-dir if it is not started in the current folder
# and needs access to the store. Here all file are small enough to
# be kept in memory

if [ ! -d log ]; then
   mkdir log
fi

dune exec -- ./server.exe \
                --log-folder ./log --restart-file session.save \
                --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key
