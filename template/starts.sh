#!/bin/bash

# should use --top-dir if it is not started in the current folder
# and needs access to the store. Here all file are small enough to
# be kept in memory
dune exec -- ./server.exe --log-folder ./log
