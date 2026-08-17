#!/usr/bin/env python3
"""Copy OTP's io_lib_pretty.erl for the AtomVM bundle, minus is_record/1.

OTP 29's io_lib_pretty pretty-prints native records and guards on
is_record/1, which compiles to opcode 186 (is_any_native_record) — unknown to
AtomVM 0.7, which aborts at module load ("Undecoded opcode: 186"). No native
record can exist in this bundle, so dropping the guard changes nothing.

usage: patch_io_lib_pretty.py <otp io_lib_pretty.erl> <output .erl>
"""
import re
import sys

src_path, out_path = sys.argv[1], sys.argv[2]
src = open(src_path).read()
before = src.count("is_record(")
src = src.replace("; is_record(Term) ->", " ->")
src = re.sub(
    r"print_length\(Record, D, T, RF, Enc, Str, Ord\) when is_record\(Record\) ->\n"
    r"\s*print_length_native_record\(Record, D, T, RF, Enc, Str, Ord\);\n",
    "",
    src,
)
after = src.count("is_record(")
if after:
    sys.exit(f"patch_io_lib_pretty: {after} is_record/1 use(s) left (was {before}); "
             "OTP's io_lib_pretty.erl changed shape — update this script")
open(out_path, "w").write(src)
print(f"io_lib_pretty: dropped {before} is_record/1 guard(s)")
