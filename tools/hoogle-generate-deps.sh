#!/bin/sh

hoogle generate $(cabal-plan | awk 'deps==1 && /^ / {print $1} /CompNameExe "arch-rebuild"/ {deps=1}' | sed 's/\(.*\)-[[:digit:].]\+/\1/')
