import arc/engine.{JsBool, JsString, Returned}
import arc/rt/builtins/temporal_tz
import arc/zoneinfo
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/string

@external(erlang, "arc_tz_links_ffi", "links")
fn bundled_links() -> Dict(String, String)

@external(erlang, "arc_tz_links_ffi", "version")
fn bundled_version() -> String

fn primary(id: String) -> String {
  let assert Some(identifier) = temporal_tz.known_identifier(id)
  temporal_tz.primary_identifier(identifier)
}

fn identifier(id: String) -> String {
  let assert Some(identifier) = temporal_tz.known_identifier(id)
  identifier
}

fn js(source: String) -> engine.JsValueKind {
  let assert Ok(#(Returned(value:), _)) = engine.eval(engine.new(), source)
  engine.classify(value)
}

pub fn link_resolves_to_primary_test() {
  assert primary("Asia/Calcutta") == "Asia/Kolkata"
  assert primary("US/Eastern") == "America/New_York"
  assert primary("Europe/Kiev") == "Europe/Kyiv"
  assert primary("Asia/Kolkata") == "Asia/Kolkata"
  assert primary("UTC") == "UTC"
  assert primary("Etc/UTC") == "UTC"
  assert primary("Etc/UCT") == "UTC"
  assert primary("Zulu") == "UTC"
  assert primary("GMT") == "UTC"
  assert primary("Etc/GMT0") == "UTC"
  assert primary("Etc/GMT+1") == "Etc/GMT+1"
}

pub fn lookup_keeps_link_name_test() {
  assert identifier("Asia/Calcutta") == "Asia/Calcutta"
  assert identifier("US/Eastern") == "US/Eastern"
}

pub fn lookup_is_ascii_case_insensitive_test() {
  assert identifier("asia/calcutta") == "Asia/Calcutta"
  assert identifier("ASIA/CALCUTTA") == "Asia/Calcutta"
  assert identifier("eTc/gMt+1") == "Etc/GMT+1"
  assert identifier("utc") == "UTC"
  assert identifier("america/argentina/buenos_aires")
    == "America/Argentina/Buenos_Aires"
  // U+212A kelvin sign folds to k under unicode rules but not ascii
  assert temporal_tz.known_identifier("Asia/Kol\u{212A}ata") == None
}

pub fn unknown_id_rejected_test() {
  assert temporal_tz.known_identifier("Asia/Nowhere") == None
  assert temporal_tz.known_identifier("IST") == None
  assert temporal_tz.known_identifier("Factory") == None
  assert temporal_tz.known_identifier("posixrules") == None
  assert temporal_tz.known_identifier("") == None
}

pub fn bundled_table_drives_resolution_test() {
  assert bundled_version() != ""
  let links = dict.to_list(bundled_links())
  assert list.length(links) > 200
  list.each(links, fn(pair) {
    let #(link, target) = pair
    assert identifier(link) == link
    assert identifier(string.uppercase(link)) == link
    let expected = case target {
      "Etc/UTC" | "Etc/GMT" -> "UTC"
      t -> t
    }
    assert primary(link) == expected
  })
}

pub fn available_ids_are_primary_and_sorted_test() {
  let ids = temporal_tz.available_ids(zoneinfo.available_ids())
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
    assert primary(id) == id
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
