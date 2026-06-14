import arc/sourcemap/position_index.{Position}

pub fn ascii_lookup_test() {
  // "abc\ndef\nghi" — three lines, all ASCII so byte == column.
  let idx = position_index.new("abc\ndef\nghi")

  assert position_index.line_count(idx) == 3

  // Start of file.
  assert position_index.lookup(idx, 0) == Position(line: 0, column: 0)
  // 'c' on line 0.
  assert position_index.lookup(idx, 2) == Position(line: 0, column: 2)
  // The '\n' byte still belongs to line 0 (column 3).
  assert position_index.lookup(idx, 3) == Position(line: 0, column: 3)
  // First byte of line 1 ('d').
  assert position_index.lookup(idx, 4) == Position(line: 1, column: 0)
  // 'g' at start of line 2.
  assert position_index.lookup(idx, 8) == Position(line: 2, column: 0)
  // 'i' — last byte.
  assert position_index.lookup(idx, 10) == Position(line: 2, column: 2)
}

pub fn clamp_lookup_test() {
  let idx = position_index.new("ab")
  // Past end clamps to end-of-file (column 2 on line 0).
  assert position_index.lookup(idx, 99) == Position(line: 0, column: 2)
  // Negative clamps to start.
  assert position_index.lookup(idx, -5) == Position(line: 0, column: 0)
}

pub fn utf16_column_test() {
  // "é" is U+00E9: 2 UTF-8 bytes, 1 UTF-16 unit.
  // "𐐷" is U+10437: 4 UTF-8 bytes, 2 UTF-16 units (surrogate pair).
  // Source: "é𐐷x" — bytes 0..1 = é, 2..5 = 𐐷, 6 = x.
  let idx = position_index.new("é𐐷x")

  // Start.
  assert position_index.lookup(idx, 0) == Position(line: 0, column: 0)
  // After "é" — byte offset 2, but column 1 (one UTF-16 unit).
  assert position_index.lookup(idx, 2) == Position(line: 0, column: 1)
  // After "é𐐷" — byte offset 6, column 3 (1 + 2 surrogate units).
  assert position_index.lookup(idx, 6) == Position(line: 0, column: 3)
  // After "x" — byte offset 7, column 4.
  assert position_index.lookup(idx, 7) == Position(line: 0, column: 4)
}

pub fn empty_source_test() {
  let idx = position_index.new("")
  assert position_index.line_count(idx) == 1
  assert position_index.lookup(idx, 0) == Position(line: 0, column: 0)
}

pub fn crlf_test() {
  // "a\r\nb" — \r is last byte of line 0, \n splits the line.
  let idx = position_index.new("a\r\nb")
  assert position_index.line_count(idx) == 2
  // 'a'
  assert position_index.lookup(idx, 0) == Position(line: 0, column: 0)
  // '\r' still line 0.
  assert position_index.lookup(idx, 1) == Position(line: 0, column: 1)
  // 'b' starts line 1 (after \r\n, bytes 0,1,2 -> b at byte 3).
  assert position_index.lookup(idx, 3) == Position(line: 1, column: 0)
}
