#!/bin/bash
set -ex

function cleanup {
  rm -f generate.out hello.com
}
trap cleanup EXIT

# Check that the programs can be started at all.
programs/zabavno --help
programs/zabavno-dos

# Run a simple DOS program.
tests/x86/hello-dos.sps
programs/zabavno-dos hello.com

# The instruction tester.
case $(uname -m) in
    x86_64|i386)
        case "$SCHEME" in
            ChezScheme|Larceny)
                tests/x86/generate.sps && chmod +x generate.out
                ./generate.out
                ;;
        esac
esac
