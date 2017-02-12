# Zabavno x86 emulator

[![Build Status](https://travis-ci.org/weinholt/zabavno.svg?branch=master)](https://travis-ci.org/weinholt/zabavno)

Zabavno is an emulator for the x86 instruction set architecture. It
emulates an Intel 80386 in real-address mode by translating machine
code into Scheme and eval'ing it. The translations are cached, which
makes it reasonably snippy.

Zabavno is licensed under the MIT license. See LICENSE.TXT for
defails.

## Why?

Originally the author needed an 80386 real-mode emulator in another
project and decided to make a separate project for it. The name means
funny or interesting.

## Current status

The emulator can load simple DOS programs and start FreeDOS and MS-DOS
from boot floppies. This is done by emulating just the CPU in
combination with a pseudo BIOS written in Scheme. Programs run
responsively but not blazingly fast, it's more like on a genuine
80386.

The major missing features are protected mode, 80387 emulation, and
any sort of hardware interfaces at all (existing features are
supported by BIOS calls).

## Usage

First you need a Scheme. The recommended Scheme for running Zabavno
is [Chez Scheme](https://github.com/cisco/chezscheme/), although any
conforming [R6RS Scheme](http://www.r6rs.org/) implementation should
work. See the build status badge above to find out which Schemes have
been verified to work.

After having installed a Scheme compiler, clone the repository:
```bash
git clone https://github.com/weinholt/zabavno
```

Add the `zabavno` directory to your Scheme library path, i.e.:

```bash
export CHEZSCHEMELIBDIRS=/path/to/zabavno/..:$CHEZSCHEMELIBDIRS
```

### Booting disk images

The program for machine emulation is `programs/zabavno`.

Boot floppies can be run like this:
```bash
programs/zabavno -fda fdboot.img 2>/dev/null
```

If you don't already have something to test with, you might like to
try the [FreeDOS 1.0 floppy](http://www.freedos.org/download/).

There is support for 32MB hard drives. They can be given on the
command line using the `-hda` flag. The last disk specified becomes
the boot disk.

(There will be some printouts on stderr about missing BIOS calls and so
on, so you might want to redirect them to `/dev/null`).

### Running DOS programs

Some DOS programs can be run directly using the DOS emulation library.

Arguments to DOS programs are passed on the command line:
```bash
programs/zabavno-dos PKUNZJR.COM -o test.zip
```

The DOS emulation is quite incomplete. If it doesn't work then
consider starting the program under FreeDOS, or implement the missing
DOS calls.

## Screenshots

![FreeDOS menu](https://github.com/weinholt/zabavno/raw/master/docs/freedos.png "FreeDOS installation, with a slight glitch")

![MS-DOS installation](https://github.com/weinholt/zabavno/raw/master/docs/msdos.png "MS-DOS complains about a lack of harddrives")

![PKUNZJR example](https://github.com/weinholt/zabavno/raw/master/docs/pkunzjr.jpg "PKUNZJR.COM is working")

## Contact

The author can be contacted through the issue tracker on Github:
https://github.com/weinholt/zabavno/issues

You can also email the author at goran@weinholt.se.
