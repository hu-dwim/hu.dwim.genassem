#$/usr/bin/env sh

# https://llvm.org/docs/TableGen/
#
# This script emits a json output from the X86 tblgen files of LLVM.
#
# x86.json: 147M
# x86-pretty.json: 222M
#
# To run it you need to get this dir from the LLVM source:
# https://github.com/llvm/llvm-project/tree/main/llvm/lib/Target/X86
# hint: may use https://downgit.github.io to download only this dir,
# or just use guix as we are doing below.
#
# WARNING: the versions of the tablegen exe and the .tb files must match!

LLVM=$(guix build llvm | sed -n '2p')
LLVM_SRC=$(guix build --source llvm)

${LLVM}/bin/llvm-tblgen -I=${LLVM_SRC}/llvm/include/ -I${LLVM_SRC}/llvm/lib/Target/X86/ ${LLVM_SRC}/llvm/lib/Target/X86/X86.td --dump-json >x86.json

# pretty print it
# guix shell jq -- jq --tab <x86.json >x86-pretty.json

# list of instruction names
# jq '.["!instanceof"]["X86Inst"]' x86.json

# full instruction entries (it's 90+% of the space)
# jq '[ .["!instanceof"]["X86Inst"][] as $name | {($name): .[$name]} ]' x86.json
