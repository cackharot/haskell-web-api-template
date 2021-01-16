#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXX)
trap 'rm -r "$dir"' EXIT

stack exec cabal -- v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc

stack exec cabal -- upload -d --publish "$dir/*-docs.tar.gz"
