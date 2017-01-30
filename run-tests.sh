#!/bin/bash
set -ex

# Runs tests. Should be called from a CI system (e.g. .travis.yml);
# alternatively Chez Scheme can be used and $SCHEME can be set to
# ChezScheme: SCHEME=ChezScheme ./run-tests.sh

RUNSCHEME="${RUNSCHEME-}"

function cleanup {
  rm -f generate.out
}
trap cleanup EXIT

# Check that the programs can be started at all.
$RUNSCHEME programs/zabavno --help
$RUNSCHEME programs/zabavno-dos

# The instruction tester.
case $(uname -m) in
    x86_64|i386)
        case "$SCHEME" in
            ChezScheme)
                $RUNSCHEME tests/x86/generate.sps && chmod +x generate.out
                ./generate.out
        esac
esac
