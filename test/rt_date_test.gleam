//// Date on the arc/rt runtime under an injected time zone: local getters and
//// setters, the DST boundary rule, Date.parse forms and the toLocale*
//// fallbacks.

import arc/host_hooks.{HostHooks}
import arc/internal/host_time
import arc/rt/builtins as rt_builtins
import arc/rt/call as rt_call
import arc/rt/types.{
  type Agent, type JsVal, JInt, JNan, KNum, KStr, classify, mk_number, mk_object,
  mk_string,
}
import gleam/list
import rt_helpers

fn agent_in(zone: host_time.TimeZone) -> Agent {
  rt_builtins.new_agent(HostHooks(..rt_helpers.quiet_hooks(), time_zone: zone))
}

fn new_york() -> Agent {
  let assert Ok(zone) = host_time.time_zone_named("America/New_York")
  agent_in(zone)
}

fn utc() -> Agent {
  agent_in(host_time.utc_time_zone())
}

fn ints(ns: List(Int)) -> List(JsVal) {
  list.map(ns, fn(n) { mk_number(JInt(n)) })
}

/// `Date.UTC(...fields)` as an Int.
fn date_utc(st: Agent, fields: List(Int)) -> Int {
  let #(date, st) = rt_helpers.global(st, "Date")
  let #(v, _) = rt_helpers.call_method(st, date, "UTC", ints(fields))
  let assert KNum(JInt(ms)) = classify(v)
  ms
}

/// `new Date(...args)`.
fn new_date(st: Agent, args: List(JsVal)) -> #(JsVal, Agent) {
  let #(date, st) = rt_helpers.global(st, "Date")
  let #(h, st) = rt_call.t_construct(st, date, args, date)
  #(mk_object(h), st)
}

fn num(st: Agent, d: JsVal, method: String, args: List(JsVal)) -> Int {
  let #(v, _) = rt_helpers.call_method(st, d, method, args)
  let assert KNum(JInt(n)) = classify(v)
  n
}

fn str(st: Agent, d: JsVal, method: String) -> String {
  let #(v, _) = rt_helpers.call_method(st, d, method, [])
  let assert KStr(s) = classify(v)
  s
}

fn parse(st: Agent, s: String) -> types.JsNum {
  let #(date, st) = rt_helpers.global(st, "Date")
  let #(v, _) = rt_helpers.call_method(st, date, "parse", [mk_string(s)])
  let assert KNum(n) = classify(v)
  n
}

pub fn local_getters_follow_zone_test() {
  let st = new_york()
  // 2021-01-15T17:00:00Z is 12:00 EST (UTC-5).
  let winter = date_utc(st, [2021, 0, 15, 17, 0, 0])
  let #(d, st) = new_date(st, ints([winter]))
  assert num(st, d, "getHours", []) == 12
  assert num(st, d, "getUTCHours", []) == 17
  assert num(st, d, "getTimezoneOffset", []) == 300
  assert str(st, d, "toString") == "Fri Jan 15 2021 12:00:00 GMT-0500"
  assert str(st, d, "toISOString") == "2021-01-15T17:00:00.000Z"
  // 2021-07-15T16:00:00Z is 12:00 EDT (UTC-4).
  let summer = date_utc(st, [2021, 6, 15, 16, 0, 0])
  let #(d, st) = new_date(st, ints([summer]))
  assert num(st, d, "getHours", []) == 12
  assert num(st, d, "getTimezoneOffset", []) == 240
  assert str(st, d, "toTimeString") == "12:00:00 GMT-0400"
}

pub fn local_setters_follow_zone_test() {
  let st = new_york()
  let winter = date_utc(st, [2021, 0, 15, 17, 0, 0])
  let #(d, st) = new_date(st, ints([winter]))
  // setHours(0): local midnight Jan 15 EST is 05:00Z.
  assert num(st, d, "setHours", ints([0]))
    == date_utc(st, [2021, 0, 15, 5, 0, 0])
  assert num(st, d, "getDate", []) == 15
  // Component constructor is local time too.
  let #(d, st) = new_date(st, ints([2021, 0, 15, 12]))
  assert num(st, d, "getTime", []) == winter
}

pub fn dst_boundary_uses_offset_before_transition_test() {
  let st = new_york()
  // Spring forward 2021-03-14: 02:30 local does not exist; it is read with
  // the pre-transition offset (EST), landing on 03:30 EDT.
  let #(d, st) = new_date(st, ints([2021, 2, 14, 2, 30]))
  assert num(st, d, "getHours", []) == 3
  assert num(st, d, "getTime", []) == date_utc(st, [2021, 2, 14, 7, 30])
  // Fall back 2021-11-07: 01:30 local happens twice; the first (EDT) wins.
  let #(d, st) = new_date(st, ints([2021, 10, 7, 1, 30]))
  assert num(st, d, "getTimezoneOffset", []) == 240
  // One hour later by the clock on the wall it is EST.
  let #(d, st) = new_date(st, ints([2021, 10, 7, 2, 30]))
  assert num(st, d, "getTimezoneOffset", []) == 300
}

pub fn utc_zone_test() {
  let st = utc()
  let t = date_utc(st, [2021, 0, 15, 17, 0, 0])
  let #(d, st) = new_date(st, ints([t]))
  assert num(st, d, "getHours", []) == 17
  assert num(st, d, "getTimezoneOffset", []) == 0
  assert str(st, d, "toString") == "Fri Jan 15 2021 17:00:00 GMT+0000"
  assert str(st, d, "toUTCString") == "Fri, 15 Jan 2021 17:00:00 GMT"
}

pub fn parse_formats_test() {
  let st = new_york()
  // Date-only forms are UTC.
  assert parse(st, "2021-01-15") == JInt(date_utc(st, [2021, 0, 15]))
  assert parse(st, "2021-01") == JInt(date_utc(st, [2021, 0, 1]))
  // Date-time with no designator is local.
  assert parse(st, "2021-01-15T12:00")
    == JInt(date_utc(st, [2021, 0, 15, 17, 0]))
  assert parse(st, "2021-01-15T12:00:00.500Z")
    == JInt(date_utc(st, [2021, 0, 15, 12, 0, 0, 500]))
  assert parse(st, "2021-01-15T12:00+02:00")
    == JInt(date_utc(st, [2021, 0, 15, 10, 0]))
  assert parse(st, "+002021-01-15T00:00:00.000Z")
    == JInt(date_utc(st, [2021, 0, 15]))
  assert parse(st, "-000001-01-01T00:00:00Z") == JInt(-62_198_755_200_000)
  // 24:00 is the end-of-day designator only.
  assert parse(st, "2021-01-15T24:00:00Z") == JInt(date_utc(st, [2021, 0, 16]))
  assert parse(st, "2021-13-01") == JNan
  assert parse(st, "2021-01-15T25:00Z") == JNan
  assert parse(st, "-000000-01-01T00:00:00Z") == JNan
  assert parse(st, "nonsense") == JNan
}

pub fn to_locale_fallbacks_test() {
  let st = new_york()
  let t = date_utc(st, [2021, 0, 15, 17, 5, 9])
  let #(d, st) = new_date(st, ints([t]))
  assert str(st, d, "toLocaleString") == "1/15/2021, 12:05:09 PM"
  assert str(st, d, "toLocaleDateString") == "1/15/2021"
  assert str(st, d, "toLocaleTimeString") == "12:05:09 PM"
}

pub fn invalid_date_test() {
  let st = utc()
  let #(d, st) = new_date(st, [mk_number(JNan)])
  assert str(st, d, "toString") == "Invalid Date"
  let #(v, _) = rt_helpers.call_method(st, d, "getHours", [])
  assert classify(v) == KNum(JNan)
}
