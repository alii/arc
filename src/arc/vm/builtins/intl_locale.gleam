//// Unicode BCP 47 locale identifier parsing and canonicalization.
////
//// Implements the UTS 35 `unicode_locale_id` grammar used by ECMA-402:
////   IsStructurallyValidLanguageTag  (ES Intl §6.2.2)
////   CanonicalizeUnicodeLocaleId     (ES Intl §6.2.3, UTS 35 §3.2.1)
////
//// Pure module — no heap/state dependencies. Alias data is the subset of
//// CLDR supplementalMetadata that ECMA-402 implementations commonly ship
//// (language/region/variant aliases, u-extension value aliases).

import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// A parsed unicode_locale_id.
pub type LocaleId {
  LocaleId(
    language: String,
    script: Option(String),
    region: Option(String),
    variants: List(String),
    extensions: List(Extension),
    private_use: List(String),
  )
}

pub type Extension {
  /// -u- extension: attributes then key/value keywords. Empty type = "".
  UExt(attributes: List(String), keywords: List(#(String, String)))
  /// -t- extension: optional tlang plus (tkey, tvalue) fields.
  TExt(tlang: Option(LocaleId), fields: List(#(String, String)))
  /// Any other singleton (a-z except t/u/x).
  OtherExt(singleton: String, subtags: List(String))
}

// ============================================================================
// Character class helpers (ASCII only — non-ASCII rejected up front)
// ============================================================================

fn all_codepoints(s: String, pred: fn(Int) -> Bool) -> Bool {
  string.to_utf_codepoints(s)
  |> list.all(fn(cp) { pred(string.utf_codepoint_to_int(cp)) })
}

fn is_ascii_alpha_cp(c: Int) -> Bool {
  { c >= 0x41 && c <= 0x5a } || { c >= 0x61 && c <= 0x7a }
}

fn is_ascii_digit_cp(c: Int) -> Bool {
  c >= 0x30 && c <= 0x39
}

fn is_ascii_alnum_cp(c: Int) -> Bool {
  is_ascii_alpha_cp(c) || is_ascii_digit_cp(c)
}

fn is_alpha(s: String) -> Bool {
  s != "" && all_codepoints(s, is_ascii_alpha_cp)
}

fn is_digits(s: String) -> Bool {
  s != "" && all_codepoints(s, is_ascii_digit_cp)
}

fn is_alnum(s: String) -> Bool {
  s != "" && all_codepoints(s, is_ascii_alnum_cp)
}

fn len(s: String) -> Int {
  string.length(s)
}

/// unicode_language_subtag = alpha{2,3} | alpha{5,8}
fn is_language(s: String) -> Bool {
  let n = len(s)
  is_alpha(s) && { n == 2 || n == 3 || { n >= 5 && n <= 8 } }
}

/// unicode_script_subtag = alpha{4}
fn is_script(s: String) -> Bool {
  is_alpha(s) && len(s) == 4
}

/// unicode_region_subtag = alpha{2} | digit{3}
fn is_region(s: String) -> Bool {
  { is_alpha(s) && len(s) == 2 } || { is_digits(s) && len(s) == 3 }
}

/// unicode_variant_subtag = alphanum{5,8} | digit alphanum{3}
fn is_variant(s: String) -> Bool {
  let n = len(s)
  case is_alnum(s) {
    False -> False
    True ->
      { n >= 5 && n <= 8 }
      || {
        n == 4
        && case string.first(s) {
          Ok(c) -> is_digits(c)
          Error(Nil) -> False
        }
      }
  }
}

/// key = alphanum alpha
fn is_ukey(s: String) -> Bool {
  len(s) == 2
  && case string.to_utf_codepoints(s) {
    [a, b] ->
      is_ascii_alnum_cp(string.utf_codepoint_to_int(a))
      && is_ascii_alpha_cp(string.utf_codepoint_to_int(b))
    _ -> False
  }
}

/// tkey = alpha digit
fn is_tkey(s: String) -> Bool {
  len(s) == 2
  && case string.to_utf_codepoints(s) {
    [a, b] ->
      is_ascii_alpha_cp(string.utf_codepoint_to_int(a))
      && is_ascii_digit_cp(string.utf_codepoint_to_int(b))
    _ -> False
  }
}

/// type/attribute/tvalue piece = alphanum{3,8}
fn is_type_subtag(s: String) -> Bool {
  let n = len(s)
  is_alnum(s) && n >= 3 && n <= 8
}

fn is_singleton(s: String) -> Bool {
  len(s) == 1 && is_alnum(s)
}

// ============================================================================
// Parsing
// ============================================================================

/// Parse + structural validation of a language tag (any case).
/// Error(Nil) means structurally invalid → callers throw RangeError.
pub fn parse(tag: String) -> Result(LocaleId, Nil) {
  let lowered = string.lowercase(tag)
  // Reject non-ASCII / control chars up front; split("-") handles structure.
  case is_tag_charset(tag) {
    False -> Error(Nil)
    True -> {
      let parts = string.split(lowered, "-")
      // Empty parts mean leading/trailing/double "-".
      case list.any(parts, fn(p) { p == "" }) {
        True -> Error(Nil)
        False -> parse_language(parts)
      }
    }
  }
}

/// Only a-z A-Z 0-9 and "-" may appear in a tag.
fn is_tag_charset(s: String) -> Bool {
  all_codepoints(s, fn(c) { is_ascii_alnum_cp(c) || c == 0x2d })
}

fn parse_language(parts: List(String)) -> Result(LocaleId, Nil) {
  case parts {
    [lang, ..rest] ->
      case is_language(lang) {
        False -> Error(Nil)
        True -> {
          use #(script, rest) <- result.try(parse_script(rest))
          use #(region, rest) <- result.try(parse_region(rest))
          use #(variants, rest) <- result.try(parse_variants(rest, []))
          use #(extensions, private_use) <- result.try(
            parse_extensions(rest, [], []),
          )
          Ok(LocaleId(
            language: lang,
            script:,
            region:,
            variants:,
            extensions:,
            private_use:,
          ))
        }
      }
    [] -> Error(Nil)
  }
}

fn parse_script(
  parts: List(String),
) -> Result(#(Option(String), List(String)), Nil) {
  case parts {
    [p, ..rest] ->
      case is_script(p) {
        True -> Ok(#(Some(p), rest))
        False -> Ok(#(None, parts))
      }
    [] -> Ok(#(None, []))
  }
}

fn parse_region(
  parts: List(String),
) -> Result(#(Option(String), List(String)), Nil) {
  case parts {
    [p, ..rest] ->
      case is_region(p) {
        True -> Ok(#(Some(p), rest))
        False -> Ok(#(None, parts))
      }
    [] -> Ok(#(None, []))
  }
}

fn parse_variants(
  parts: List(String),
  acc: List(String),
) -> Result(#(List(String), List(String)), Nil) {
  case parts {
    [p, ..rest] ->
      case is_variant(p) {
        True ->
          case list.contains(acc, p) {
            // Duplicate variant → structurally invalid.
            True -> Error(Nil)
            False -> parse_variants(rest, [p, ..acc])
          }
        False -> Ok(#(list.reverse(acc), parts))
      }
    [] -> Ok(#(list.reverse(acc), []))
  }
}

fn parse_extensions(
  parts: List(String),
  exts: List(Extension),
  seen: List(String),
) -> Result(#(List(Extension), List(String)), Nil) {
  case parts {
    [] -> Ok(#(list.reverse(exts), []))
    ["x", ..rest] ->
      // pu_extensions = "x" (sep alphanum{1,8})+ — must be last, non-empty.
      case rest != [] && list.all(rest, fn(s) { is_alnum(s) && len(s) <= 8 }) {
        True -> Ok(#(list.reverse(exts), rest))
        False -> Error(Nil)
      }
    [s, ..rest] ->
      case is_singleton(s) {
        False -> Error(Nil)
        True ->
          case list.contains(seen, s) {
            // Duplicate singleton → structurally invalid.
            True -> Error(Nil)
            False -> {
              let #(body, after) = take_until_singleton(rest, [])
              use ext <- result.try(case s {
                "u" -> parse_u_ext(body)
                "t" -> parse_t_ext(body)
                _ ->
                  // other_extensions = singleton (sep alphanum{2,8})+
                  case
                    body != []
                    && list.all(body, fn(p) {
                      is_alnum(p) && len(p) >= 2 && len(p) <= 8
                    })
                  {
                    True -> Ok(OtherExt(singleton: s, subtags: body))
                    False -> Error(Nil)
                  }
              })
              parse_extensions(after, [ext, ..exts], [s, ..seen])
            }
          }
      }
  }
}

/// Collect subtags until the next singleton (1-char subtag) or end.
fn take_until_singleton(
  parts: List(String),
  acc: List(String),
) -> #(List(String), List(String)) {
  case parts {
    [] -> #(list.reverse(acc), [])
    [p, ..rest] ->
      case len(p) == 1 {
        True -> #(list.reverse(acc), parts)
        False -> take_until_singleton(rest, [p, ..acc])
      }
  }
}

/// unicode_locale_extensions = "u" ((sep keyword)+ | (sep attribute)+ (sep keyword)*)
fn parse_u_ext(body: List(String)) -> Result(Extension, Nil) {
  case body {
    [] -> Error(Nil)
    _ -> {
      // Leading attributes: alphanum{3,8} before the first key.
      let #(attributes, rest) =
        list.split_while(body, fn(p) { is_type_subtag(p) })
      use keywords <- result.try(parse_u_keywords(rest, []))
      Ok(UExt(attributes:, keywords:))
    }
  }
}

fn parse_u_keywords(
  parts: List(String),
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), Nil) {
  case parts {
    [] -> Ok(list.reverse(acc))
    [key, ..rest] ->
      case is_ukey(key) {
        False -> Error(Nil)
        True -> {
          let #(type_parts, after) =
            list.split_while(rest, fn(p) { is_type_subtag(p) })
          // Duplicate ukeys are valid; the first occurrence wins (UTS 35).
          case list.any(acc, fn(kv) { kv.0 == key }) {
            True -> parse_u_keywords(after, acc)
            False ->
              parse_u_keywords(after, [
                #(key, string.join(type_parts, "-")),
                ..acc
              ])
          }
        }
      }
  }
}

/// transformed_extensions = "t" ((sep tlang (sep tfield)*) | (sep tfield)+)
fn parse_t_ext(body: List(String)) -> Result(Extension, Nil) {
  case body {
    [] -> Error(Nil)
    [first, ..] ->
      case is_tkey(first) {
        True -> {
          use fields <- result.try(parse_t_fields(body, []))
          Ok(TExt(tlang: None, fields:))
        }
        False -> {
          // tlang = language (script)? (region)? (variant)* — no extensions.
          let #(lang_parts, field_parts) =
            list.split_while(body, fn(p) { !is_tkey(p) })
          use tlang <- result.try(parse_tlang(lang_parts))
          use fields <- result.try(parse_t_fields(field_parts, []))
          Ok(TExt(tlang: Some(tlang), fields:))
        }
      }
  }
}

fn parse_tlang(parts: List(String)) -> Result(LocaleId, Nil) {
  use lid <- result.try(parse_language(parts))
  // tlang cannot itself contain extensions or private use.
  case lid.extensions, lid.private_use {
    [], [] -> Ok(lid)
    _, _ -> Error(Nil)
  }
}

fn parse_t_fields(
  parts: List(String),
  acc: List(#(String, String)),
) -> Result(List(#(String, String)), Nil) {
  case parts {
    [] -> Ok(list.reverse(acc))
    [key, ..rest] ->
      case is_tkey(key) {
        False -> Error(Nil)
        True ->
          case list.any(acc, fn(kv) { kv.0 == key }) {
            // Duplicate tkey → structurally invalid.
            True -> Error(Nil)
            False -> {
              let #(value_parts, after) =
                list.split_while(rest, fn(p) { is_type_subtag(p) })
              // tfield requires at least one tvalue.
              case value_parts {
                [] -> Error(Nil)
                _ ->
                  parse_t_fields(after, [
                    #(key, string.join(value_parts, "-")),
                    ..acc
                  ])
              }
            }
          }
      }
  }
}

// ============================================================================
// Canonicalization — UTS 35 §3.2.1
// ============================================================================

/// CLDR languageAlias subset, pre-parsed. Both sides of every row are parsed
/// exactly once here: the key is re-rendered in the same canonical lookup form
/// `apply_language_alias` builds its candidates in (variants sorted), and the
/// replacement is stored as a `LocaleId` so a lookup never re-parses. A row
/// that does not parse is a data bug, and panics loudly at construction rather
/// than degrading into "alias silently never applied".
///
/// Rows whose key cannot be produced by the lookup at all — legacy tags whose
/// subtags are not valid script/region/variant subtags, e.g. `zh-min-nan`,
/// `zh-gan`, `no-bok` — are therefore not listed: `parse` rejects those tags
/// outright, so no locale could ever match them.
fn language_aliases() -> dict.Dict(String, LocaleId) {
  language_alias_rows()
  |> list.map(fn(row) {
    let #(key, replacement) = row
    #(alias_lookup_key(key), parse_alias(replacement))
  })
  |> dict.from_list
}

/// Parse an alias-table tag; a malformed row is a bug in the table above.
fn parse_alias(tag: String) -> LocaleId {
  case parse(tag) {
    Ok(lid) -> lid
    Error(Nil) -> panic as { "malformed languageAlias row: " <> tag }
  }
}

/// The exact key form `apply_language_alias` looks up with.
fn alias_lookup_key(tag: String) -> String {
  let lid = parse_alias(tag)
  key_join(
    [lid.language],
    lid.script,
    lid.region,
    list.sort(lid.variants, string.compare),
  )
}

fn language_alias_rows() -> List(#(String, String)) {
  [
    // Full-tag / variant-inclusive aliases (legacy tags)
    #("art-lojban", "jbo"),
    #("cel-gaulish", "xtg"),
    #("zh-guoyu", "zh"),
    #("zh-hakka", "hak"),
    #("zh-xiang", "hsn"),
    #("ja-latn-hepburn-heploc", "ja-latn-alalc97"),
    #("hy-arevela", "hy"),
    #("hy-arevmda", "hyw"),
    #("sgn-gr", "gss"),
    #("sgn-de", "gsg"),
    #("sgn-nl", "dse"),
    // Deprecated ISO 639-1 codes
    #("in", "id"),
    #("iw", "he"),
    #("ji", "yi"),
    #("jw", "jv"),
    #("mo", "ro"),
    #("tl", "fil"),
    #("twi", "ak"),
    #("tw", "ak"),
    // ISO 639-2/3 → 639-1 canonical replacements
    #("aar", "aa"),
    #("abk", "ab"),
    #("afr", "af"),
    #("amh", "am"),
    #("ara", "ar"),
    #("asm", "as"),
    #("aze", "az"),
    #("bel", "be"),
    #("ben", "bn"),
    #("bod", "bo"),
    #("bos", "bs"),
    #("bul", "bg"),
    #("cat", "ca"),
    #("ces", "cs"),
    #("cmn", "zh"),
    #("cym", "cy"),
    #("dan", "da"),
    #("deu", "de"),
    #("ell", "el"),
    #("eng", "en"),
    #("est", "et"),
    #("eus", "eu"),
    #("fas", "fa"),
    #("fin", "fi"),
    #("fra", "fr"),
    #("gle", "ga"),
    #("glg", "gl"),
    #("guj", "gu"),
    #("heb", "he"),
    #("hin", "hi"),
    #("hrv", "hr"),
    #("hun", "hu"),
    #("hye", "hy"),
    #("ind", "id"),
    #("isl", "is"),
    #("ita", "it"),
    #("jpn", "ja"),
    #("kat", "ka"),
    #("kaz", "kk"),
    #("khm", "km"),
    #("kan", "kn"),
    #("kor", "ko"),
    #("lao", "lo"),
    #("lit", "lt"),
    #("lav", "lv"),
    #("mkd", "mk"),
    #("mal", "ml"),
    #("mon", "mn"),
    #("mar", "mr"),
    #("msa", "ms"),
    #("mya", "my"),
    #("nld", "nl"),
    #("nor", "no"),
    #("pan", "pa"),
    #("pol", "pl"),
    #("por", "pt"),
    #("ron", "ro"),
    #("rus", "ru"),
    #("slk", "sk"),
    #("slv", "sl"),
    #("spa", "es"),
    #("sqi", "sq"),
    #("srp", "sr"),
    #("swe", "sv"),
    #("tam", "ta"),
    #("tel", "te"),
    #("tha", "th"),
    #("tur", "tr"),
    #("ukr", "uk"),
    #("urd", "ur"),
    #("uzb", "uz"),
    #("vie", "vi"),
    #("zho", "zh"),
    // Legacy macro-language replacements with extra subtags
    #("sh", "sr-latn"),
    #("cnr", "sr-me"),
  ]
}

/// CLDR territoryAlias subset — single-replacement entries.
fn region_aliases() -> dict.Dict(String, String) {
  dict.from_list([
    #("dd", "DE"),
    #("bu", "MM"),
    #("yd", "YE"),
    #("zr", "CD"),
    #("fx", "FR"),
    #("tp", "TL"),
    #("uk", "GB"),
    #("an", "CW"),
    // Overlong numeric codes for current territories
    #("020", "AD"),
    #("024", "AO"),
    #("028", "AG"),
    #("031", "AZ"),
    #("032", "AR"),
    #("044", "BS"),
    #("048", "BH"),
    #("050", "BD"),
    #("051", "AM"),
    #("052", "BB"),
    #("060", "BM"),
    #("064", "BT"),
    #("068", "BO"),
    #("070", "BA"),
    #("072", "BW"),
    #("084", "BZ"),
    #("090", "SB"),
    #("092", "VG"),
    #("096", "BN"),
    #("100", "BG"),
    #("104", "MM"),
    #("108", "BI"),
    #("112", "BY"),
    #("116", "KH"),
    #("120", "CM"),
    #("132", "CV"),
    #("136", "KY"),
    #("140", "CF"),
    #("144", "LK"),
    #("148", "TD"),
    #("152", "CL"),
    #("158", "TW"),
    #("170", "CO"),
    #("174", "KM"),
    #("178", "CG"),
    #("180", "CD"),
    #("184", "CK"),
    #("188", "CR"),
    #("191", "HR"),
    #("192", "CU"),
    #("196", "CY"),
    #("203", "CZ"),
    #("204", "BJ"),
    #("208", "DK"),
    #("212", "DM"),
    #("214", "DO"),
    #("218", "EC"),
    #("222", "SV"),
    #("226", "GQ"),
    #("231", "ET"),
    #("232", "ER"),
    #("233", "EE"),
    #("242", "FJ"),
    #("246", "FI"),
    #("262", "DJ"),
    #("266", "GA"),
    #("268", "GE"),
    #("270", "GM"),
    #("288", "GH"),
    #("300", "GR"),
    #("304", "GL"),
    #("308", "GD"),
    #("320", "GT"),
    #("324", "GN"),
    #("328", "GY"),
    #("332", "HT"),
    #("340", "HN"),
    #("344", "HK"),
    #("348", "HU"),
    #("352", "IS"),
    #("360", "ID"),
    #("364", "IR"),
    #("368", "IQ"),
    #("372", "IE"),
    #("376", "IL"),
    #("384", "CI"),
    #("388", "JM"),
    #("398", "KZ"),
    #("400", "JO"),
    #("404", "KE"),
    #("408", "KP"),
    #("410", "KR"),
    #("414", "KW"),
    #("417", "KG"),
    #("418", "LA"),
    #("422", "LB"),
    #("426", "LS"),
    #("428", "LV"),
    #("430", "LR"),
    #("434", "LY"),
    #("438", "LI"),
    #("440", "LT"),
    #("442", "LU"),
    #("446", "MO"),
    #("450", "MG"),
    #("454", "MW"),
    #("458", "MY"),
    #("462", "MV"),
    #("466", "ML"),
    #("470", "MT"),
    #("478", "MR"),
    #("480", "MU"),
    #("492", "MC"),
    #("496", "MN"),
    #("498", "MD"),
    #("499", "ME"),
    #("504", "MA"),
    #("508", "MZ"),
    #("512", "OM"),
    #("516", "NA"),
    #("520", "NR"),
    #("524", "NP"),
    #("540", "NC"),
    #("548", "VU"),
    #("554", "NZ"),
    #("558", "NI"),
    #("562", "NE"),
    #("566", "NG"),
    #("583", "FM"),
    #("584", "MH"),
    #("585", "PW"),
    #("586", "PK"),
    #("591", "PA"),
    #("598", "PG"),
    #("600", "PY"),
    #("604", "PE"),
    #("608", "PH"),
    #("620", "PT"),
    #("624", "GW"),
    #("626", "TL"),
    #("634", "QA"),
    #("642", "RO"),
    #("646", "RW"),
    #("682", "SA"),
    #("686", "SN"),
    #("688", "RS"),
    #("690", "SC"),
    #("694", "SL"),
    #("702", "SG"),
    #("703", "SK"),
    #("704", "VN"),
    #("705", "SI"),
    #("706", "SO"),
    #("710", "ZA"),
    #("716", "ZW"),
    #("728", "SS"),
    #("729", "SD"),
    #("740", "SR"),
    #("748", "SZ"),
    #("760", "SY"),
    #("762", "TJ"),
    #("764", "TH"),
    #("768", "TG"),
    #("776", "TO"),
    #("780", "TT"),
    #("784", "AE"),
    #("788", "TN"),
    #("792", "TR"),
    #("795", "TM"),
    #("798", "TV"),
    #("800", "UG"),
    #("804", "UA"),
    #("807", "MK"),
    #("818", "EG"),
    #("834", "TZ"),
    #("858", "UY"),
    #("860", "UZ"),
    #("862", "VE"),
    #("882", "WS"),
    #("887", "YE"),
    #("894", "ZM"),
    #("004", "AF"),
    #("008", "AL"),
    #("012", "DZ"),
    #("036", "AU"),
    #("040", "AT"),
    #("056", "BE"),
    #("076", "BR"),
    #("124", "CA"),
    #("156", "CN"),
    #("250", "FR"),
    #("276", "DE"),
    #("356", "IN"),
    #("380", "IT"),
    #("392", "JP"),
    #("484", "MX"),
    #("528", "NL"),
    #("578", "NO"),
    #("616", "PL"),
    #("643", "RU"),
    #("724", "ES"),
    #("752", "SE"),
    #("756", "CH"),
    #("826", "GB"),
    #("840", "US"),
  ])
}

/// CLDR territoryAlias subset — multi-replacement entries. The right
/// replacement is picked via likely subtags (UTS 35 §3.2.1).
fn region_multi_aliases() -> dict.Dict(String, List(String)) {
  let su = [
    "RU", "AM", "AZ", "BY", "EE", "GE", "KZ", "KG", "LV", "LT", "MD", "TJ", "TM",
    "UA", "UZ",
  ]
  dict.from_list([
    #("su", su),
    #("810", su),
    #("cs", ["RS", "ME"]),
    #("200", ["CZ", "SK"]),
    #("nt", ["SA", "IQ"]),
    #("yu", ["RS", "ME"]),
    #("890", ["RS", "ME", "SI", "HR", "MK", "BA"]),
  ])
}

/// CLDR variantAlias subset.
fn variant_aliases() -> dict.Dict(String, String) {
  dict.from_list([#("heploc", "alalc97"), #("polytoni", "polyton")])
}

/// Likely-subtags subset: language or und-script → likely region.
/// Used only to disambiguate multi-replacement territory aliases.
fn likely_region(language: String, script: Option(String)) -> Option(String) {
  let by_lang =
    dict.from_list([
      #("ru", "RU"),
      #("en", "US"),
      #("hy", "AM"),
      #("az", "AZ"),
      #("sr", "RS"),
      #("be", "BY"),
      #("et", "EE"),
      #("ka", "GE"),
      #("kk", "KZ"),
      #("ky", "KG"),
      #("lv", "LV"),
      #("lt", "LT"),
      #("ro", "RO"),
      #("tg", "TJ"),
      #("tk", "TM"),
      #("uk", "UA"),
      #("uz", "UZ"),
      #("cs", "CZ"),
      #("sk", "SK"),
      #("sl", "SI"),
      #("hr", "HR"),
      #("mk", "MK"),
      #("bs", "BA"),
      #("und", "US"),
    ])
  let by_script =
    dict.from_list([
      #("armn", "AM"),
      #("cyrl", "RU"),
      #("geor", "GE"),
      #("glag", "BG"),
    ])
  case script {
    Some(sc) ->
      case dict.get(by_script, string.lowercase(sc)) {
        Ok(r) -> Some(r)
        Error(Nil) -> dict.get(by_lang, language) |> option.from_result
      }
    None -> dict.get(by_lang, language) |> option.from_result
  }
}

/// Canonicalize a u-extension value supplied as a constructor option.
pub fn canonical_u_value(key: String, v: String) -> String {
  u_type_alias(key, v)
}

/// u-extension type-value aliases per key (CLDR bcp47 data subset).
/// Applied after lowercasing; "yes" → "true" only for boolean-ish col keys.
fn u_type_alias(key: String, value: String) -> String {
  case key, value {
    "ca", "islamicc" -> "islamic-civil"
    "ca", "ethiopic-amete-alem" -> "ethioaa"
    "ks", "primary" -> "level1"
    "ks", "secondary" -> "level2"
    "ks", "tertiary" -> "level3"
    "ks", "quarternary" | "ks", "quaternary" -> "level4"
    "ks", "identical" -> "identic"
    "ms", "imperial" -> "uksystem"
    "kb", "yes" | "kc", "yes" | "kh", "yes" | "kk", "yes" | "kn", "yes" ->
      "true"
    "rg", v | "sd", v -> subdivision_alias(v)
    "tz", "cnckg" -> "cnsha"
    "tz", "eire" -> "iedub"
    "tz", "est" -> "papty"
    "tz", "gmt0" -> "gmt"
    "tz", "uct" -> "utc"
    "tz", "zulu" -> "utc"
    _, _ -> value
  }
}

/// CLDR bcp47 transform field value aliases (subset).
fn t_value_alias(key: String, v: String) -> String {
  case key, v {
    "m0", "names" -> "prprname"
    _, _ -> v
  }
}

fn subdivision_alias(v: String) -> String {
  case v {
    "no23" -> "no50"
    "cn11" -> "cnbj"
    "cz10a" -> "cz110"
    "fra" -> "frges"
    "frg" -> "frges"
    "lud" -> "lucl"
    _ -> v
  }
}

/// CanonicalizeUnicodeLocaleId — assumes `lid` came from `parse`.
pub fn canonicalize(lid: LocaleId) -> LocaleId {
  // 1. Alias replacement on language/script/region/variants.
  let lid = apply_aliases(lid)
  // 2. Variants sorted.
  let lid = LocaleId(..lid, variants: list.sort(lid.variants, string.compare))
  // 3. Canonicalize extensions: sort by singleton; canonicalize contents.
  let exts =
    lid.extensions
    |> list.map(canonicalize_extension)
    |> list.sort(fn(a, b) {
      string.compare(extension_singleton(a), extension_singleton(b))
    })
  LocaleId(..lid, extensions: exts)
}

fn extension_singleton(ext: Extension) -> String {
  case ext {
    UExt(..) -> "u"
    TExt(..) -> "t"
    OtherExt(singleton:, ..) -> singleton
  }
}

fn canonicalize_extension(ext: Extension) -> Extension {
  case ext {
    UExt(attributes:, keywords:) -> {
      let attributes = list.sort(list.unique(attributes), string.compare)
      let keywords =
        keywords
        |> list.map(fn(kv) {
          let #(k, v) = kv
          let v = u_type_alias(k, v)
          // Any type value "true" is removed (UTS 35 §3.2.1).
          let v = case v {
            "true" -> ""
            _ -> v
          }
          #(k, v)
        })
        |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
      UExt(attributes:, keywords:)
    }
    TExt(tlang:, fields:) -> {
      let tlang = option.map(tlang, fn(tl) { canonicalize(apply_aliases(tl)) })
      let fields =
        fields
        |> list.map(fn(kv) {
          let #(k, v) = kv
          #(k, t_value_alias(k, v))
        })
        |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
      TExt(tlang:, fields:)
    }
    OtherExt(..) -> ext
  }
}

/// Replace language/region/variant aliases (UTS 35 §3.2.1 step "Replace
/// aliases in the unicode_language_id and tlang").
fn apply_aliases(lid: LocaleId) -> LocaleId {
  let lid = apply_language_alias(lid)
  let lid = apply_region_alias(lid)
  apply_variant_alias(lid)
}

/// Try language-alias keys from most to least specific. Components included
/// in a matched key are consumed (replaced by the replacement's components).
/// One alias-lookup key plus which of the original locale's fields the key
/// spelled out (and which the replacement therefore consumes). Named fields,
/// not a tuple of three same-typed booleans: transposing "consumes region" and
/// "consumes variants" would otherwise compile cleanly and silently produce a
/// wrong canonical tag.
type AliasCandidate {
  AliasCandidate(
    key: String,
    consumes_script: Bool,
    consumes_region: Bool,
    consumes_variants: Bool,
  )
}

fn apply_language_alias(lid: LocaleId) -> LocaleId {
  let aliases = language_aliases()
  let sorted_variants = list.sort(lid.variants, string.compare)
  let candidates = [
    AliasCandidate(
      key: key_join([lid.language], lid.script, lid.region, sorted_variants),
      consumes_script: True,
      consumes_region: True,
      consumes_variants: True,
    ),
    AliasCandidate(
      key: key_join([lid.language], lid.script, None, sorted_variants),
      consumes_script: True,
      consumes_region: False,
      consumes_variants: True,
    ),
    AliasCandidate(
      key: key_join([lid.language], None, lid.region, sorted_variants),
      consumes_script: False,
      consumes_region: True,
      consumes_variants: True,
    ),
    AliasCandidate(
      key: key_join([lid.language], None, None, sorted_variants),
      consumes_script: False,
      consumes_region: False,
      consumes_variants: True,
    ),
    AliasCandidate(
      key: key_join([lid.language], lid.script, lid.region, []),
      consumes_script: True,
      consumes_region: True,
      consumes_variants: False,
    ),
    AliasCandidate(
      key: key_join([lid.language], None, lid.region, []),
      consumes_script: False,
      consumes_region: True,
      consumes_variants: False,
    ),
    AliasCandidate(
      key: key_join([lid.language], lid.script, None, []),
      consumes_script: True,
      consumes_region: False,
      consumes_variants: False,
    ),
    AliasCandidate(
      key: key_join([lid.language], None, None, []),
      consumes_script: False,
      consumes_region: False,
      consumes_variants: False,
    ),
  ]
  find_language_alias(lid, aliases, candidates)
}

fn key_join(
  lang: List(String),
  script: Option(String),
  region: Option(String),
  variants: List(String),
) -> String {
  let parts =
    list.flatten([
      lang,
      option.map(script, fn(s) { [s] }) |> option.unwrap([]),
      option.map(region, fn(r) { [r] }) |> option.unwrap([]),
      variants,
    ])
  string.join(parts, "-")
}

fn find_language_alias(
  lid: LocaleId,
  aliases: dict.Dict(String, LocaleId),
  candidates: List(AliasCandidate),
) -> LocaleId {
  case candidates {
    [] -> lid
    [candidate, ..rest] ->
      case dict.get(aliases, candidate.key) {
        Error(Nil) -> find_language_alias(lid, aliases, rest)
        Ok(rep) -> {
          // Replacement components fill in; original components are kept only
          // when the key did not consume them and the replacement is silent.
          let script = case candidate.consumes_script {
            True -> rep.script
            False -> option.or(lid.script, rep.script)
          }
          let region = case candidate.consumes_region {
            True -> rep.region
            False -> option.or(lid.region, rep.region)
          }
          let variants = case candidate.consumes_variants {
            True -> rep.variants
            False -> list.append(lid.variants, rep.variants)
          }
          LocaleId(..lid, language: rep.language, script:, region:, variants:)
        }
      }
  }
}

fn apply_region_alias(lid: LocaleId) -> LocaleId {
  case lid.region {
    None -> lid
    Some(region) -> {
      let region = string.lowercase(region)
      case dict.get(region_aliases(), region) {
        Ok(rep) -> LocaleId(..lid, region: Some(string.lowercase(rep)))
        Error(Nil) ->
          case dict.get(region_multi_aliases(), region) {
            Error(Nil) -> lid
            Ok([first, ..] as reps) -> {
              // Pick the likely territory for language(+script) if it is in
              // the replacement list; otherwise the first entry.
              let likely = likely_region(lid.language, lid.script)
              let chosen = case likely {
                Some(r) ->
                  case list.contains(reps, r) {
                    True -> r
                    False -> first
                  }
                None -> first
              }
              LocaleId(..lid, region: Some(string.lowercase(chosen)))
            }
            Ok([]) -> lid
          }
      }
    }
  }
}

fn apply_variant_alias(lid: LocaleId) -> LocaleId {
  let aliases = variant_aliases()
  let variants =
    list.map(lid.variants, fn(v) { dict.get(aliases, v) |> result.unwrap(v) })
  LocaleId(..lid, variants:)
}

// ============================================================================
// Serialization
// ============================================================================

/// Render in canonical case: language lowercase, script Titlecase,
/// region UPPERCASE, everything else lowercase.
pub fn to_string(lid: LocaleId) -> String {
  base_name(lid) <> extensions_suffix(lid)
}

/// unicode_language_id part only (language-script-region-variants).
pub fn base_name(lid: LocaleId) -> String {
  let parts =
    list.flatten([
      [string.lowercase(lid.language)],
      case lid.script {
        Some(s) -> [titlecase(s)]
        None -> []
      },
      case lid.region {
        Some(r) -> [string.uppercase(r)]
        None -> []
      },
      list.map(lid.variants, string.lowercase),
    ])
  string.join(parts, "-")
}

fn extensions_suffix(lid: LocaleId) -> String {
  let ext_parts = list.map(lid.extensions, extension_to_string)
  let px = case lid.private_use {
    [] -> []
    subtags -> ["x-" <> string.join(list.map(subtags, string.lowercase), "-")]
  }
  case list.append(ext_parts, px) {
    [] -> ""
    parts -> "-" <> string.join(parts, "-")
  }
}

fn extension_to_string(ext: Extension) -> String {
  case ext {
    UExt(attributes:, keywords:) -> {
      let kw_parts =
        list.map(keywords, fn(kv) {
          case kv {
            #(k, "") -> k
            #(k, v) -> k <> "-" <> v
          }
        })
      string.join(["u", ..list.append(attributes, kw_parts)], "-")
    }
    TExt(tlang:, fields:) -> {
      let lang_part = case tlang {
        Some(tl) -> [string.lowercase(to_string(tl))]
        None -> []
      }
      let field_parts = list.map(fields, fn(kv) { kv.0 <> "-" <> kv.1 })
      string.join(["t", ..list.append(lang_part, field_parts)], "-")
    }
    OtherExt(singleton:, subtags:) -> string.join([singleton, ..subtags], "-")
  }
}

fn titlecase(s: String) -> String {
  case string.pop_grapheme(s) {
    Ok(#(first, rest)) -> string.uppercase(first) <> string.lowercase(rest)
    Error(Nil) -> s
  }
}

/// Full pipeline: parse → canonicalize → serialize.
pub fn canonicalize_tag(tag: String) -> Result(String, Nil) {
  use lid <- result.map(parse(tag))
  to_string(canonicalize(lid))
}

// ============================================================================
// Lookup matching (ES Intl §9.2.2 BestAvailableLocale / LookupMatcher)
// ============================================================================

/// Locales this engine ships formatting behavior for. Kept deliberately
/// small: the formatters implement root/English ("en"-family) patterns.
pub fn available_locales() -> List(String) {
  [
    "ar", "bg", "cs", "da", "de", "de-AT", "de-CH", "el", "en", "en-AU", "en-CA",
    "en-GB", "en-IN", "en-NZ", "en-US", "es", "es-419", "es-ES", "es-MX", "fa",
    "fi", "fil", "fr", "fr-CA", "he", "hi", "hr", "hu", "id", "it", "ja", "ko",
    "lt", "lv", "ms", "nb", "nl", "pl", "pt", "pt-BR", "pt-PT", "ro", "ru", "sk",
    "sl", "sr", "sv", "th", "tr", "uk", "vi", "zh", "zh-CN", "zh-Hans",
    "zh-Hant", "zh-TW",
  ]
}

pub fn default_locale() -> String {
  "en-US"
}

/// §9.2.2 BestAvailableLocale — truncate by removing trailing subtags.
pub fn best_available_locale(candidate: String) -> Option(String) {
  case list.contains(available_locales(), candidate) {
    True -> Some(candidate)
    False -> {
      let parts = string.split(candidate, "-")
      case list.length(parts) <= 1 {
        True -> None
        False -> {
          let shorter =
            parts |> list.take(list.length(parts) - 1) |> string.join("-")
          best_available_locale(shorter)
        }
      }
    }
  }
}

/// Strip all extensions/private use, keeping only the unicode_language_id.
pub fn strip_extensions(tag: String) -> String {
  case parse(tag) {
    Ok(lid) -> base_name(lid)
    Error(Nil) -> tag
  }
}

// ============================================================================
// Likely subtags (UTS 35 §4.3, CLDR subset) — maximize / minimize
// ============================================================================

/// language → #(script, region) likely subtags.
fn likely_subtags(language: String) -> Option(#(String, String)) {
  case language {
    "en" -> Some(#("latn", "us"))
    "de" -> Some(#("latn", "de"))
    "fr" -> Some(#("latn", "fr"))
    "es" -> Some(#("latn", "es"))
    "it" -> Some(#("latn", "it"))
    "pt" -> Some(#("latn", "br"))
    "nl" -> Some(#("latn", "nl"))
    "sv" -> Some(#("latn", "se"))
    "da" -> Some(#("latn", "dk"))
    "nb" | "no" -> Some(#("latn", "no"))
    "nn" -> Some(#("latn", "no"))
    "fi" -> Some(#("latn", "fi"))
    "is" -> Some(#("latn", "is"))
    "pl" -> Some(#("latn", "pl"))
    "cs" -> Some(#("latn", "cz"))
    "sk" -> Some(#("latn", "sk"))
    "hu" -> Some(#("latn", "hu"))
    "ro" -> Some(#("latn", "ro"))
    "bg" -> Some(#("cyrl", "bg"))
    "ru" -> Some(#("cyrl", "ru"))
    "uk" -> Some(#("cyrl", "ua"))
    "sr" -> Some(#("cyrl", "rs"))
    "hr" -> Some(#("latn", "hr"))
    "sl" -> Some(#("latn", "si"))
    "el" -> Some(#("grek", "gr"))
    "tr" -> Some(#("latn", "tr"))
    "ar" -> Some(#("arab", "eg"))
    "he" -> Some(#("hebr", "il"))
    "fa" -> Some(#("arab", "ir"))
    "ur" -> Some(#("arab", "pk"))
    "hi" -> Some(#("deva", "in"))
    "bn" -> Some(#("beng", "bd"))
    "ta" -> Some(#("taml", "in"))
    "te" -> Some(#("telu", "in"))
    "th" -> Some(#("thai", "th"))
    "lo" -> Some(#("laoo", "la"))
    "km" -> Some(#("khmr", "kh"))
    "my" -> Some(#("mymr", "mm"))
    "ka" -> Some(#("geor", "ge"))
    "hy" -> Some(#("armn", "am"))
    "az" -> Some(#("latn", "az"))
    "kk" -> Some(#("cyrl", "kz"))
    "uz" -> Some(#("latn", "uz"))
    "vi" -> Some(#("latn", "vn"))
    "id" -> Some(#("latn", "id"))
    "ms" -> Some(#("latn", "my"))
    "ja" -> Some(#("jpan", "jp"))
    "ko" -> Some(#("kore", "kr"))
    "zh" -> Some(#("hans", "cn"))
    "und" -> Some(#("latn", "us"))
    "jbo" -> Some(#("latn", "001"))
    "aae" -> Some(#("latn", "it"))
    "eo" -> Some(#("latn", "001"))
    "ia" -> Some(#("latn", "001"))
    _ -> None
  }
}

/// AddLikelySubtags. Extensions/private use are preserved.
pub fn maximize(lid: LocaleId) -> LocaleId {
  let lid = canonicalize(lid)
  // Script/region interplay special cases (CLDR likelySubtags).
  let special = case lid.language, lid.script, lid.region {
    "zh", None, Some("tw") -> Some(#("hant", "tw"))
    "zh", None, Some("hk") -> Some(#("hant", "hk"))
    "zh", None, Some("mo") -> Some(#("hant", "mo"))
    "zh", Some("hant"), None -> Some(#("hant", "tw"))
    "en", Some("shaw"), None -> Some(#("shaw", "gb"))
    "en", Some("dsrt"), None -> Some(#("dsrt", "us"))
    "und", Some(s), _ -> und_script_likely(s, lid.region)
    _, _, _ -> None
  }
  case special {
    Some(#(script, region)) ->
      LocaleId(
        ..lid,
        language: case lid.language {
          "und" -> und_script_language(script)
          l -> l
        },
        script: Some(script),
        region: Some(region),
      )
    None ->
      case likely_subtags(lid.language) {
        Some(#(script, region)) ->
          LocaleId(
            ..lid,
            language: case lid.language {
              "und" -> "en"
              l -> l
            },
            script: option.or(lid.script, Some(script)),
            region: option.or(lid.region, Some(region)),
          )
        None ->
          LocaleId(
            ..lid,
            script: option.or(lid.script, Some("latn")),
            region: option.or(lid.region, Some("us")),
          )
      }
  }
}

fn und_script_likely(
  script: String,
  region: Option(String),
) -> Option(#(String, String)) {
  case script {
    "latn" -> Some(#("latn", option.unwrap(region, "us")))
    "cyrl" -> Some(#("cyrl", option.unwrap(region, "ru")))
    "arab" -> Some(#("arab", option.unwrap(region, "eg")))
    "armn" -> Some(#("armn", option.unwrap(region, "am")))
    "hans" -> Some(#("hans", option.unwrap(region, "cn")))
    "hant" -> Some(#("hant", option.unwrap(region, "tw")))
    "jpan" -> Some(#("jpan", option.unwrap(region, "jp")))
    "kore" -> Some(#("kore", option.unwrap(region, "kr")))
    _ -> None
  }
}

fn und_script_language(script: String) -> String {
  case script {
    "latn" -> "en"
    "cyrl" -> "ru"
    "arab" -> "ar"
    "armn" -> "hy"
    "hans" | "hant" -> "zh"
    "jpan" -> "ja"
    "kore" -> "ko"
    _ -> "en"
  }
}

/// RemoveLikelySubtags.
pub fn minimize(lid: LocaleId) -> LocaleId {
  let max = maximize(lid)
  let base = LocaleId(..max, script: None, region: None)
  let try_lang = maximize(base)
  case same_base(try_lang, max) {
    True -> strip_to(max, None, None)
    False -> {
      let try_region = maximize(LocaleId(..max, script: None))
      case same_base(try_region, max) {
        True -> strip_to(max, None, max.region)
        False -> {
          let try_script = maximize(LocaleId(..max, region: None))
          case same_base(try_script, max) {
            True -> strip_to(max, max.script, None)
            False -> max
          }
        }
      }
    }
  }
}

fn strip_to(
  lid: LocaleId,
  script: Option(String),
  region: Option(String),
) -> LocaleId {
  LocaleId(..lid, script:, region:)
}

fn same_base(a: LocaleId, b: LocaleId) -> Bool {
  a.language == b.language && a.script == b.script && a.region == b.region
}
