//// Property names the engine itself uses, as ready-made keys, so that reading
//// `length` does not work its number out again every time. The numbers are
//// what `key.named` gives; names_test checks every one.

import arc/bytecode/key.{type PropertyKey, Named}

pub const add = Named(92_639_634, "add")

pub const calendar = Named(416_148_655_292_561_992, "calendar")

pub const callee = Named(1_562_980_234_925_462, "callee")

pub const cause = Named(6_105_393_909_142, "cause")

pub const constructor = Named(194_569_011_983_933_316, "constructor")

pub const done = Named(23_919_966_614, "done")

pub const empty = Named(6, "")

pub const era = Named(93_702_534, "era")

pub const exec = Named(23_989_425_550, "exec")

pub const flags = Named(6_157_666_459_086, "flags")

pub const groups = Named(1_580_864_682_443_214, "groups")

pub const has = Named(94_471_630, "has")

pub const index = Named(6_209_341_068_770, "index")

pub const infinity = Named(203_493_090_797_992_356, "Infinity")

pub const iterator = Named(425_897_015_423_828_148, "iterator")

pub const join = Named(24_322_614_714, "join")

pub const keys = Named(24_387_118_542, "keys")

pub const last_index = Named(95_666_154_611_911_752, "lastIndex")

pub const length = Named(1_602_631_505_924_514, "length")

pub const max_byte_length = Named(149_900_561_086_551_804, "maxByteLength")

pub const message = Named(411_399_652_123_188_630, "message")

pub const mode = Named(24_523_936_150, "mode")

pub const month_code = Named(6_081_454_441_373_912, "monthCode")

pub const name = Named(24_587_384_214, "name")

pub const nan = Named(87_655_738, "NaN")

pub const neg_infinity = Named(556_265_332_329_168_100, "-Infinity")

pub const next = Named(24_588_444_114, "next")

pub const offset = Named(1_615_842_291_586_514, "offset")

pub const padding = Named(414_759_500_955_105_694, "padding")

pub const plain_time = Named(65_047_404_117_846_008, "plainTime")

pub const proto = Named(285_587_656_632_664_716, "__proto__")

pub const prototype = Named(351_306_839_192_840_528, "prototype")

pub const raw = Named(97_093_086, "raw")

pub const raw_json = Named(417_011_625_435_675_962, "rawJSON")

pub const relative_to = Named(232_496_172_428_696_940, "relativeTo")

pub const resolve = Named(417_029_151_391_865_238, "resolve")

pub const return = Named(1_629_020_191_312_314, "return")

pub const set = Named(97_359_314, "set")

pub const size = Named(24_925_038_998, "size")

pub const source = Named(1_633_590_102_822_294, "source")

pub const stack = Named(6_381_541_625_262, "stack")

pub const stack_trace_limit = Named(221_300_613_036_969_160, "stackTraceLimit")

pub const then = Named(24_991_864_250, "then")

pub const throw = Named(6_397_920_656_862, "throw")

pub const time_zone = Named(240_941_977_526_539_172, "timeZone")

pub const to_iso_string = Named(224_288_002_643_020_276, "toISOString")

pub const to_json = Named(1_637_985_255_505_210, "toJSON")

pub const to_locale_string = Named(493_285_335_970_630_320, "toLocaleString")

pub const to_string = Named(567_970_273_839_592_496, "toString")

pub const value = Named(6_431_809_066_390, "value")

pub const with = Named(25_193_468_322, "with")

pub const all: List(PropertyKey) = [
  add,
  calendar,
  callee,
  cause,
  constructor,
  done,
  empty,
  era,
  exec,
  flags,
  groups,
  has,
  index,
  infinity,
  iterator,
  join,
  keys,
  last_index,
  length,
  max_byte_length,
  message,
  mode,
  month_code,
  name,
  nan,
  neg_infinity,
  next,
  offset,
  padding,
  plain_time,
  proto,
  prototype,
  raw,
  raw_json,
  relative_to,
  resolve,
  return,
  set,
  size,
  source,
  stack,
  stack_trace_limit,
  then,
  throw,
  time_zone,
  to_iso_string,
  to_json,
  to_locale_string,
  to_string,
  value,
  with,
]
