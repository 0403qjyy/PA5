#!/bin/bash
./lexer "$@" 2>&1 | ./parser "$@" 2>&1 | ./semant "$@" 2>&1 | ./cgen "$@"
