# hu.dwim.genassem

## What

A suite to parse the json output of LLVM's TableGen, and use it to
generate assemblers.

## Status

**It's work in progress.**

It can parse the json version of X86.tb as a stream, and walk/filter
the instructions. Some x86_64 instruction classes are already
generated. The API of the assembler is a functional one (as opposed to
macros, i.e. it's more suitable for compilers, and less so for writing
asm by hand).

There are some [unit tests](test/).

## Why

My ultimate goal is an x86_64 assembler for
[Maru](https://github.com/attila-lendvai/maru).

But it's easier to work in a mature language were
e.g. [Slime](https://github.com/slime/slime) is available, so I'm
hoping to finish this CL lib and then either:

  - port this entire thing to Maru (ideal outcome),

  - or probably just generate the assembler for Maru from CL and then
    check it in into Maru's repo (a less pleasing but pragmatic
    shortcut).

I'm planning to work my way through the parts of the x86 and ARM
instruction sets that are still relevant today (2025).

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.genassem).
