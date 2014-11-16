Currently the emulator is very incomplete.

The idea is to emulate an Intel 80386 in real-address mode, by
translating machine code to Scheme code and eval'ing it (also known as
JIT). The (zabavno x86) library should be kept purely to emulating the
CPU. Protected mode is not a priority right now, and neither is PC
emulation (but the emulator should have hooks that make it possible to
build a PC emulator with it).

Regards,
Göran Weinholt
