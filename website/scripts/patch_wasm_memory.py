#!/usr/bin/env python3
"""Patch the maximum memory pages in a WASM binary's memory import.

Usage: patch_wasm_memory.py <wasm_file> <max_pages>

Parses the WASM import section to find the memory import, then overwrites
its declared maximum with the given value. Fails if the new LEB128 encoding
is a different length (would shift all subsequent offsets).
"""

import sys


def read_leb128(data, pos):
    value = 0
    shift = 0
    start = pos
    while True:
        b = data[pos]
        pos += 1
        value |= (b & 0x7F) << shift
        shift += 7
        if not (b & 0x80):
            break
    return value, start, pos


def encode_leb128(value):
    out = bytearray()
    while True:
        b = value & 0x7F
        value >>= 7
        if value:
            b |= 0x80
        out.append(b)
        if not value:
            break
    return out


def main():
    if len(sys.argv) != 3:
        print(f"Usage: {sys.argv[0]} <wasm_file> <max_pages>", file=sys.stderr)
        sys.exit(1)

    wasm_path = sys.argv[1]
    new_max = int(sys.argv[2])

    with open(wasm_path, "rb") as f:
        data = bytearray(f.read())

    assert data[:4] == b"\x00asm", "Not a WASM file"

    pos = 8  # skip header
    while pos < len(data):
        section_id = data[pos]
        pos += 1
        section_size, _, pos = read_leb128(data, pos)
        section_end = pos + section_size

        if section_id != 2:  # not import section
            pos = section_end
            continue

        num_imports, _, pos = read_leb128(data, pos)

        for i in range(num_imports):
            # module name
            mod_len, _, pos = read_leb128(data, pos)
            pos += mod_len
            # field name
            field_len, _, pos = read_leb128(data, pos)
            pos += field_len
            # import kind
            kind = data[pos]
            pos += 1

            if kind == 0:  # function
                _, _, pos = read_leb128(data, pos)
            elif kind == 1:  # table
                pos += 1  # elemtype
                flags = data[pos]
                pos += 1
                _, _, pos = read_leb128(data, pos)
                if flags & 1:
                    _, _, pos = read_leb128(data, pos)
            elif kind == 3:  # global
                pos += 2  # valtype + mutability
            elif kind == 2:  # memory
                flags = data[pos]
                pos += 1
                initial, _, pos = read_leb128(data, pos)
                has_max = flags & 1

                if not has_max:
                    print(f"Memory import #{i} has no maximum declared", file=sys.stderr)
                    sys.exit(1)

                old_max, max_start, max_end = read_leb128(data, pos)
                pos = max_end

                new_bytes = encode_leb128(new_max)
                old_bytes = data[max_start:max_end]

                if len(new_bytes) != len(old_bytes):
                    print(
                        f"LEB128 length changed ({len(old_bytes)} -> {len(new_bytes)} bytes), "
                        "this would corrupt the binary",
                        file=sys.stderr,
                    )
                    sys.exit(1)

                data[max_start:max_end] = new_bytes

                with open(wasm_path, "wb") as f:
                    f.write(data)

                print(f"  patched memory import #{i}: max {old_max} -> {new_max} pages ({new_max * 64 // 1024}MB)")
                return

        print("No memory import found in WASM binary", file=sys.stderr)
        sys.exit(1)

    print("No import section found in WASM binary", file=sys.stderr)
    sys.exit(1)


if __name__ == "__main__":
    main()
