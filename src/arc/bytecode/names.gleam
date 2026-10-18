//// Property names the engine itself uses, as ready-made keys, so that reading
//// `length` does not work its number out again every time. The numbers are
//// what `key.named` gives; names_test checks every one.

import arc/bytecode/key.{type PropertyKey, Named}

pub const add = Named(340_256_351_672_946_728, "add")

pub const calendar = Named(416_148_655_292_561_992, "calendar")

pub const callee = Named(258_180_192_190_089_946, "callee")

pub const cause = Named(225_021_358_506_741_126, "cause")

pub const constructor = Named(194_569_011_983_933_316, "constructor")

pub const done = Named(101_593_477_352_523_290, "done")

pub const empty = Named(179_823_727_769_510_242, "")

pub const era = Named(303_728_439_841_897_712, "era")

pub const exec = Named(209_850_278_316_681_412, "exec")

pub const flags = Named(241_044_183_256_492_102, "flags")

pub const groups = Named(218_435_572_264_061_864, "groups")

pub const has = Named(231_122_051_873_551_506, "has")

pub const index = Named(450_863_100_588_335_748, "index")

pub const infinity = Named(203_493_090_797_992_358, "Infinity")

pub const iterator = Named(425_897_015_423_828_150, "iterator")

pub const join = Named(148_497_810_374_974_670, "join")

pub const keys = Named(292_975_467_517_734_398, "keys")

pub const last_index = Named(95_666_154_611_911_752, "lastIndex")

pub const length = Named(450_907_079_707_063_178, "length")

pub const max_byte_length = Named(149_900_561_086_551_804, "maxByteLength")

pub const message = Named(45_663_578_353_935_146, "message")

pub const mode = Named(285_421_896_536_660_558, "mode")

pub const month_code = Named(6_081_454_441_373_912, "monthCode")

pub const name = Named(174_053_263_642_217_938, "name")

pub const nan = Named(236_102_477_348_565_290, "NaN")

pub const neg_infinity = Named(556_265_332_329_168_100, "-Infinity")

pub const next = Named(112_767_222_323_064_210, "next")

pub const offset = Named(469_465_389_877_532_134, "offset")

pub const padding = Named(501_623_252_479_216_800, "padding")

pub const plain_time = Named(65_047_404_117_846_010, "plainTime")

pub const proto = Named(285_587_656_632_664_716, "__proto__")

pub const prototype = Named(351_306_839_192_840_530, "prototype")

pub const raw = Named(184_571_988_517_758_230, "raw")

pub const raw_json = Named(137_797_358_432_190_296, "rawJSON")

pub const relative_to = Named(232_496_172_428_696_940, "relativeTo")

pub const resolve = Named(240_043_959_395_400_684, "resolve")

pub const return = Named(464_278_738_730_347_786, "return")

pub const set = Named(307_690_807_258_379_972, "set")

pub const size = Named(197_184_125_449_205_086, "size")

pub const source = Named(447_263_542_775_864_460, "source")

pub const stack = Named(286_142_622_834_796_850, "stack")

pub const stack_trace_limit = Named(221_300_613_036_969_162, "stackTraceLimit")

pub const then = Named(519_217_037_152_013_778, "then")

pub const throw = Named(499_760_146_359_736_768, "throw")

pub const time_zone = Named(240_941_977_526_539_172, "timeZone")

pub const to_iso_string = Named(224_288_002_643_020_278, "toISOString")

pub const to_json = Named(559_356_507_754_130_940, "toJSON")

pub const to_locale_string = Named(493_285_335_970_630_320, "toLocaleString")

pub const to_string = Named(567_970_273_839_592_498, "toString")

pub const value = Named(525_302_465_986_530_988, "value")

pub const with = Named(375_035_195_507_622_202, "with")

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
