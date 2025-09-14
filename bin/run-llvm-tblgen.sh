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

SCRIPT_DIR=$(dirname "$0")
OUTPUT_DIR=$(readlink -f ${SCRIPT_DIR}/../tablegen/)

echo "Saving TableGen json output to ${OUTPUT_DIR}"

mkdir --parents ${OUTPUT_DIR}

LLVM=$(guix build llvm | sed -n '2p')
LLVM_SRC=$(guix build --source llvm)
LLVM_VERSION=$(guix package --show=llvm | recsel --print-values=version | head -1)

echo "LLVM version is ${LLVM_VERSION}"

SUFFIX=-${LLVM_VERSION}

echo "x86..."
${LLVM}/bin/llvm-tblgen -I=${LLVM_SRC}/llvm/include/ -I${LLVM_SRC}/llvm/lib/Target/X86/ ${LLVM_SRC}/llvm/lib/Target/X86/X86.td --dump-json >${OUTPUT_DIR}/x86${SUFFIX}.json
echo "arm..."
${LLVM}/bin/llvm-tblgen -I=${LLVM_SRC}/llvm/include/ -I${LLVM_SRC}/llvm/lib/Target/ARM/ ${LLVM_SRC}/llvm/lib/Target/ARM/ARM.td --dump-json >${OUTPUT_DIR}/arm${SUFFIX}.json

# pretty print it
echo "Pretty printing them..."
guix shell jq -- jq --tab <${OUTPUT_DIR}/x86${SUFFIX}.json >${OUTPUT_DIR}/x86${SUFFIX}-pretty.json
guix shell jq -- jq --tab <${OUTPUT_DIR}/arm${SUFFIX}.json >${OUTPUT_DIR}/arm${SUFFIX}-pretty.json

# list of instruction names
# jq '.["!instanceof"]["X86Inst"]' x86.json

# full instruction entries (it's 90+% of the space)
# jq '[ .["!instanceof"]["X86Inst"][] as $name | {($name): .[$name]} ]' x86.json

echo "Done."
