import arc/engine.{JsBool, JsString, Returned}
import arc/rt/builtins/temporal_tz
import gleam/dict.{type Dict}
import gleam/list
import gleam/string

@external(erlang, "arc_tz_links_ffi", "links")
fn bundled_links() -> Dict(String, String)

@external(erlang, "arc_tz_links_ffi", "version")
fn bundled_version() -> String

fn canonical(id: String) -> String {
  let assert Ok(zone) = temporal_tz.lookup(id)
  temporal_tz.canonical(zone)
}

fn proper(id: String) -> String {
  let assert Ok(zone) = temporal_tz.lookup(id)
  temporal_tz.zone_id(zone)
}

fn js(source: String) -> engine.JsValueKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  engine.classify(value)
}

pub fn link_resolves_to_canonical_test() {
  assert canonical("Asia/Calcutta") == "Asia/Kolkata"
  assert canonical("US/Eastern") == "America/New_York"
  assert canonical("Europe/Kiev") == "Europe/Kyiv"
  assert canonical("Asia/Kolkata") == "Asia/Kolkata"
  assert canonical("UTC") == "UTC"
  assert canonical("Etc/UTC") == "UTC"
  assert canonical("Etc/UCT") == "UTC"
  assert canonical("Zulu") == "UTC"
  assert canonical("GMT") == "UTC"
  assert canonical("Etc/GMT0") == "UTC"
  assert canonical("Etc/GMT+1") == "Etc/GMT+1"
}

pub fn lookup_keeps_link_name_test() {
  assert proper("Asia/Calcutta") == "Asia/Calcutta"
  assert proper("US/Eastern") == "US/Eastern"
}

pub fn lookup_is_ascii_case_insensitive_test() {
  assert proper("asia/calcutta") == "Asia/Calcutta"
  assert proper("ASIA/CALCUTTA") == "Asia/Calcutta"
  assert proper("eTc/gMt+1") == "Etc/GMT+1"
  assert proper("utc") == "UTC"
  assert proper("america/argentina/buenos_aires")
    == "America/Argentina/Buenos_Aires"
  // U+212A kelvin sign folds to k under unicode rules but not ascii
  assert temporal_tz.lookup("Asia/Kol\u{212A}ata") == Error(Nil)
}

pub fn unknown_id_rejected_test() {
  assert temporal_tz.lookup("Asia/Nowhere") == Error(Nil)
  assert temporal_tz.lookup("IST") == Error(Nil)
  assert temporal_tz.lookup("Factory") == Error(Nil)
  assert temporal_tz.lookup("posixrules") == Error(Nil)
  assert temporal_tz.lookup("") == Error(Nil)
}

pub fn bundled_table_drives_resolution_test() {
  assert bundled_version() != ""
  let links = dict.to_list(bundled_links())
  assert list.length(links) > 200
  list.each(links, fn(pair) {
    let #(link, target) = pair
    assert proper(link) == link
    assert proper(string.uppercase(link)) == link
    let expected = case target {
      "Etc/UTC" | "Etc/GMT" -> "UTC"
      t -> t
    }
    assert canonical(link) == expected
  })
}

pub fn available_ids_are_canonical_and_sorted_test() {
  let ids = temporal_tz.available_ids()
  assert ids == list.sort(ids, string.compare)
  assert list.contains(ids, "UTC")
  assert list.contains(ids, "Asia/Kolkata")
  assert list.contains(ids, "Etc/GMT+5")
  assert !list.contains(ids, "Etc/UTC")
  assert !list.contains(ids, "Etc/GMT")
  assert !list.contains(ids, "Asia/Calcutta")
  assert !list.contains(ids, "US/Eastern")
  assert !list.contains(ids, "Factory")
  list.each(ids, fn(id) {
    assert canonical(id) == id
  })
}

pub fn supported_values_of_excludes_links_test() {
  assert js(
      "const ids = Intl.supportedValuesOf('timeZone');"
      <> "ids.includes('Asia/Kolkata') && ids.includes('UTC')"
      <> " && !ids.includes('Asia/Calcutta') && !ids.includes('Etc/UTC')"
      <> " && ids.join() === [...ids].sort().join()"
      <> " && new Set(ids).size === ids.length",
    )
    == JsBool(True)
}

pub fn zoned_date_time_accepts_links_test() {
  assert js("new Temporal.ZonedDateTime(0n, 'asia/calcutta').timeZoneId")
    == JsString("Asia/Calcutta")
  assert js(
      "const a = new Temporal.ZonedDateTime(0n, 'US/Eastern');"
      <> "const b = new Temporal.ZonedDateTime(0n, 'America/New_York');"
      <> "a.equals(b) && a.offsetNanoseconds === b.offsetNanoseconds",
    )
    == JsBool(True)
  assert js(
      "new Intl.DateTimeFormat('en', { timeZone: 'Europe/Kiev' })"
      <> ".resolvedOptions().timeZone",
    )
    == JsString("Europe/Kiev")
}
