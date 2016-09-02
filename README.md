# Zabavno x86 emulator

Zabavno is an emulator for the x86 instruction set architecture. The
idea is to emulate an Intel 80386 in real-address mode, by translating
machine code to Scheme code and eval'ing it.

Zabavno is licensed under the MIT license. See LICENSE.TXT for
defails.

## Why?

The author needed an 80386 real-mode emulator in another project and
decided to make a separate project for it. The name means funny or
interesting.

## Current status

The emulator can load simple DOS programs and start FreeDOS and MS-DOS
from boot floppies. This is done by emulating just the CPU and with a
pseudo BIOS written in Scheme. Programs run responsively but not
blazingly fast, it's more like on a genuine 80386.

Protected mode is not a priority right now but should eventually be
added. Full PC or DOS emulation is also not really a priority, except
as a way to get more software running for testing. However, patches
that improve PC and DOS emulation are very welcome. The (zabavno x86)
library should be kept purely to emulating the CPU, with hooks for
everything else.

## Usage

First you need a Scheme. The recommended Scheme for running Zabavno is
[Chez Scheme](https://github.com/cisco/chezscheme/), although. any
[R6RS Scheme](http://www.r6rs.org/) implementation should work.

After having installed a Scheme compiler, clone the repository:
```bash
git clone https://github.com/weinholt/zabavno
```

The main program is `programs/zabavno`, which can run DOS .com files,
DOS .exe files and bootable PC floppy images. These are not provided
in the repository and will need to be found elsewhere. You might like
to try the [FreeDOS 1.0 floppy](http://www.freedos.org/download/). The
keyboard is not working reliably right now so it will be tricky to use
interactive programs.

Arguments to DOS programs are passed using the `--args` command line
flag, like so:
```bash
zabavno --args " -o test.zip" PKUNZJR.COM
```

Boot floppies are run like this:
```bash
zabavno fdboot.img 2>/dev/null
```

(There will be some printouts on stderr about missing BIOS calls and so
on, so you might want to redirect them to `/dev/null`).

## Future plans

One possibility is to add emulated hardware and use the Bochs BIOS.
Right now all BIOS calls are handled in Scheme code by (zabavno
firmware bios). If hardware is emulated instead then an existing BIOS
can be used instead, and it will be possible to run programs that
interact directly with the hardware. However it should still be
possible to run with the Zabavno BIOS, since for some applications it
will not be reasonable to keep around hardware emulation and the Bochs
BIOS.

The Zabavno BIOS can be improved with support for additional calls,
including setting video modes and improved keyboard support.

Modularity is a big concern when developing this library. Hard
dependencies on anything outside of the core R6RS libraries should be
kept as modular as possible, to make it easier to embed the emulator
in an existing application.

## Screenshots

![FreeDOS menu](https://github.com/weinholt/zabavno/raw/master/docs/freedos.png "FreeDOS installation, with a slight glitch")

![MS-DOS installation](https://github.com/weinholt/zabavno/raw/master/docs/msdos.png "MS-DOS complains about a lack of harddrives")

![PKUNZJR example](https://github.com/weinholt/zabavno/raw/master/docs/pkunzjr.jpg "PKUNZJR.COM is working")

## Contact

The author can be contacted through the issue tracker on Github:
https://github.com/weinholt/zabavno/issues

You can also email the author at goran@weinholt.se.
