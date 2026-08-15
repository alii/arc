//// ECMA-402 internal-slot data shared by the Intl builtins and the pure
//// formatting engine (`rt/builtins/intl_format`): the closed option enums,
//// and the per-service resolved state an `IntlObj` carries.
////
//// A leaf module: its only runtime imports are the two time-zone leaves
//// (`host_time`, `temporal_tz`), so both `rt/types` (for the `IntlObj`
//// object kind) and the formatting engine can depend on it.

import arc/internal/host_time
import arc/rt/builtins/temporal_tz
import gleam/option.{type Option, None}

// --- Intl.NumberFormat closed option sets (§15.1) -------------------------
//
// Each of these options admits a fixed set of spellings. They are parsed
// (and validated) exactly once, in the constructors in `builtins/intl`; the
// formatting engine then dispatches on the variants exhaustively, so an
// out-of-set value cannot reach — or be silently defaulted by — a formatter.
// The spec spellings for resolvedOptions live alongside their consumer in
// `builtins/intl.gleam`.

/// `[[Style]]` (§15.1.3 SetNumberFormatUnitOptions). The style-conditional
/// slots live *inside* the variant that selects them: `[[Currency]]` /
/// `[[CurrencyDisplay]]` / `[[CurrencySign]]` exist exactly when the style is
/// currency, `[[Unit]]` / `[[UnitDisplay]]` exactly when it is unit. A
/// currency style without a currency code is therefore not representable, and
/// no formatter has to invent a default for a slot the style did not select.
pub type NumStyle {
  StyleDecimal
  StylePercent
  StyleCurrency(currency: String, display: CurrencyDisplay, sign: CurrencySign)
  StyleUnit(unit: String, display: UnitDisplay)
}

/// `[[Notation]]` — shared by NumberFormat and PluralRules. `[[CompactDisplay]]`
/// only exists under compact notation, so it lives in that variant.
pub type Notation {
  NotationStandard
  NotationScientific
  NotationEngineering
  NotationCompact(display: CompactDisplay)
}

/// `[[CompactDisplay]]` — only meaningful under compact notation.
pub type CompactDisplay {
  CompactShort
  CompactLong
}

/// `[[SignDisplay]]`.
pub type SignDisplay {
  SignAuto
  SignNever
  SignAlways
  SignExceptZero
  SignNegative
}

/// `[[CurrencyDisplay]]` — only meaningful for the currency style.
pub type CurrencyDisplay {
  CurCode
  CurSymbol
  CurNarrowSymbol
  CurName
}

/// `[[CurrencySign]]` — only meaningful for the currency style.
pub type CurrencySign {
  CurStandard
  CurAccounting
}

/// `[[UnitDisplay]]` — only meaningful for the unit style.
pub type UnitDisplay {
  UnitShort
  UnitNarrow
  UnitLong
}

/// `[[RoundingMode]]` (§15.5.2).
pub type RoundingMode {
  RoundCeil
  RoundFloor
  RoundExpand
  RoundTrunc
  RoundHalfCeil
  RoundHalfFloor
  RoundHalfExpand
  RoundHalfTrunc
  RoundHalfEven
}

/// `[[RoundingType]]` selection priority (§15.1.6).
pub type RoundingPriority {
  PriorityAuto
  PriorityMorePrecision
  PriorityLessPrecision
}

/// `[[TrailingZeroDisplay]]`.
pub type TrailingZeroDisplay {
  TzdAuto
  TzdStripIfInteger
}

/// SetNumberFormatDigitOptions result (§15.1.6) — shared by NumberFormat and
/// PluralRules. The fraction/significant `(min, max)` pairs are absent when
/// that rounding kind was not requested, and resolvedOptions omits absent
/// pairs.
pub type IntlDigitOptions {
  IntlDigitOptions(
    minimum_integer_digits: Int,
    fraction_digits: Option(#(Int, Int)),
    significant_digits: Option(#(Int, Int)),
    rounding_increment: Int,
    rounding_mode: RoundingMode,
    rounding_priority: RoundingPriority,
    trailing_zero_display: TrailingZeroDisplay,
  )
}

/// `[[UseGrouping]]` — spec-wise either the boolean `false` or one of the
/// strings "min2" / "auto" / "always" (a `true` option normalizes to
/// "always", `false` to never). resolvedOptions must surface never as the
/// boolean `false`, the rest as their string spelling.
pub type IntlUseGrouping {
  GroupingAuto
  GroupingAlways
  GroupingMin2
  GroupingNever
}

/// Widths of the name components (weekday, era, dayPeriod).
pub type NameWidth {
  WLong
  WShort
  WNarrow
}

/// `[[Type]]` (§16.1.2 InitializePluralRules).
pub type PluralType {
  Cardinal
  Ordinal
}

/// `[[Type]]` (§13.1.2). `UnitList` is the "unit" spelling — the list-format
/// type, not to be confused with `NumStyle`'s `StyleUnit`.
pub type ListFormatType {
  Conjunction
  Disjunction
  UnitList
}

/// `[[Style]]` (§13.1.2). The engine picks its separator patterns by matching
/// `#(ListFormatType, ListFormatStyle)` exhaustively, so no combination can
/// silently fall through to the conjunction/long pattern.
pub type ListFormatStyle {
  LLong
  LShort
  LNarrow
}

/// `[[Style]]` (§17.1.2) — selects the unit spellings ("3 hr. ago").
pub type RtfStyle {
  RtfLong
  RtfShort
  RtfNarrow
}

/// `[[Numeric]]` (§17.1.2) — "auto" allows the special names ("yesterday").
pub type RtfNumeric {
  RtfAlways
  RtfAuto
}

/// A DurationFormat unit's resolved `[[<Unit>Style]]`. `DurFractional` is the
/// internal-only style a sub-second unit folds into when it rides on the
/// preceding numeric unit's fraction; resolvedOptions spells it "numeric".
pub type DurationUnitStyle {
  DurLong
  DurShort
  DurNarrow
  DurNumeric
  DurTwoDigit
  DurFractional
}

/// A DurationFormat unit's resolved `[[<Unit>Display]]`.
pub type DurationDisplay {
  DisplayAuto
  DisplayAlways
}

/// The DurationFormat `style` option (`[[Style]]`).
pub type DurationBaseStyle {
  BsLong
  BsShort
  BsNarrow
  BsDigital
}

/// One DurationFormat unit's resolved `[[<Unit>Style]]` / `[[<Unit>Display]]`
/// pair (GetDurationUnitOptions). `style` is the INTERNAL style.
pub type DurationUnitOptions {
  DurationUnitOptions(style: DurationUnitStyle, display: DurationDisplay)
}

// ---------------------------------------------------------------------------
// Intl per-service resolved state (ECMA-402 internal slots)
// ---------------------------------------------------------------------------
//
// Each Intl service stores its resolved constructor options in a dedicated
// record — one field per internal slot, with its real type. The bound
// `format` / `compare` function cache lives on the `IntlObj` kind, not here,
// so this data stays free of heap handles.

/// Which Intl service an Intl instance object (or native fn) belongs to.
/// Used both as the brand for [[InitializedX]] internal-slot checks and to
/// route shared method implementations (resolvedOptions, supportedLocalesOf).
pub type IntlService {
  IntlLocale
  IntlCollator
  IntlNumberFormat
  IntlDateTimeFormat
  IntlPluralRules
  IntlListFormat
  IntlRelativeTimeFormat
  IntlSegmenter
  IntlDisplayNames
  IntlDurationFormat
  /// %SegmentsPrototype% instances returned by Segmenter.prototype.segment.
  IntlSegments
  /// %SegmentIteratorPrototype% instances.
  IntlSegmentIterator
}

/// The spec name of a service ("NumberFormat"), as used in `Intl.<name>` and
/// in error messages.
pub fn service_name(service: IntlService) -> String {
  case service {
    IntlLocale -> "Locale"
    IntlCollator -> "Collator"
    IntlNumberFormat -> "NumberFormat"
    IntlDateTimeFormat -> "DateTimeFormat"
    IntlPluralRules -> "PluralRules"
    IntlListFormat -> "ListFormat"
    IntlRelativeTimeFormat -> "RelativeTimeFormat"
    IntlSegmenter -> "Segmenter"
    IntlDisplayNames -> "DisplayNames"
    IntlDurationFormat -> "DurationFormat"
    IntlSegments -> "Segments"
    IntlSegmentIterator -> "Segment Iterator"
  }
}

/// The three services whose prototype exposes a lazily-created bound function
/// (`Collator.prototype.compare`, `NumberFormat.prototype.format`,
/// `DateTimeFormat.prototype.format`). Narrower than `IntlService` on purpose:
/// a bound getter for a service without such an accessor cannot be
/// registered.
pub type BoundGetterService {
  BgCollator
  BgNumberFormat
  BgDateTimeFormat
}

/// The brand a `BoundGetterService`'s receiver must carry.
pub fn bound_getter_service(service: BoundGetterService) -> IntlService {
  case service {
    BgCollator -> IntlCollator
    BgNumberFormat -> IntlNumberFormat
    BgDateTimeFormat -> IntlDateTimeFormat
  }
}

/// The Intl services with a public constructor. `IntlSegments` /
/// `IntlSegmentIterator` are ordinary objects handed out by
/// `Segmenter.prototype.segment` and its iterator — they have no constructor,
/// so they are absent here rather than an "illegal constructor" arm.
pub type ConstructibleService {
  CsLocale
  CsCollator
  CsNumberFormat
  CsDateTimeFormat
  CsPluralRules
  CsListFormat
  CsRelativeTimeFormat
  CsSegmenter
  CsDisplayNames
  CsDurationFormat
}

/// The brand instances of a `ConstructibleService` carry.
pub fn constructible_service(service: ConstructibleService) -> IntlService {
  case service {
    CsLocale -> IntlLocale
    CsCollator -> IntlCollator
    CsNumberFormat -> IntlNumberFormat
    CsDateTimeFormat -> IntlDateTimeFormat
    CsPluralRules -> IntlPluralRules
    CsListFormat -> IntlListFormat
    CsRelativeTimeFormat -> IntlRelativeTimeFormat
    CsSegmenter -> IntlSegmenter
    CsDisplayNames -> IntlDisplayNames
    CsDurationFormat -> IntlDurationFormat
  }
}

/// The resolved state carried by an `IntlObj`. The variant IS the brand:
/// `Intl.NumberFormat.prototype.format` cannot be handed Collator state.
pub type IntlData {
  LocaleData(LocaleState)
  CollatorData(CollatorState)
  NumberFormatData(NumberFormatState)
  DateTimeFormatData(DateTimeFormatState)
  PluralRulesData(PluralRulesState)
  ListFormatData(ListFormatState)
  RelativeTimeFormatData(RelativeTimeFormatState)
  SegmenterData(SegmenterState)
  DisplayNamesData(DisplayNamesState)
  DurationFormatData(DurationFormatState)
  SegmentsData(SegmentsState)
  SegmentIteratorData(SegmentIteratorState)
}

/// The service tag for a given Intl state — the two are 1:1.
pub fn intl_service(data: IntlData) -> IntlService {
  case data {
    LocaleData(_) -> IntlLocale
    CollatorData(_) -> IntlCollator
    NumberFormatData(_) -> IntlNumberFormat
    DateTimeFormatData(_) -> IntlDateTimeFormat
    PluralRulesData(_) -> IntlPluralRules
    ListFormatData(_) -> IntlListFormat
    RelativeTimeFormatData(_) -> IntlRelativeTimeFormat
    SegmenterData(_) -> IntlSegmenter
    DisplayNamesData(_) -> IntlDisplayNames
    DurationFormatData(_) -> IntlDurationFormat
    SegmentsData(_) -> IntlSegments
    SegmentIteratorData(_) -> IntlSegmentIterator
  }
}

/// Intl.Locale — the canonicalized `[[Locale]]` tag. Every getter/method is
/// derived by re-parsing it.
pub type LocaleState {
  LocaleState(locale: String)
}

/// Intl.Collator resolved options (§10.1.2 InitializeCollator).
pub type CollatorState {
  CollatorState(
    locale: String,
    usage: CollatorUsage,
    sensitivity: CollatorSensitivity,
    ignore_punctuation: Bool,
    collation: String,
    numeric: Bool,
    case_first: CaseFirst,
  )
}

/// `[[Usage]]` (§10.1.2 InitializeCollator).
pub type CollatorUsage {
  UsageSort
  UsageSearch
}

/// `[[Sensitivity]]` (§10.1.2 InitializeCollator). Parsed once at construction
/// so the comparator dispatches on the variants exhaustively.
pub type CollatorSensitivity {
  SensBase
  SensAccent
  SensCase
  SensVariant
}

/// `[[CaseFirst]]` (§10.1.2 InitializeCollator, UTS 35 `kf`).
pub type CaseFirst {
  CaseFirstUpper
  CaseFirstLower
  CaseFirstFalse
}

/// A DateTimeFormat formatting component: one option name of the §11.1.2
/// component table. A closed enum so the fixed component tables in
/// `intl.gleam` cannot name a component that does not exist.
pub type DtfComponent {
  DtfWeekday
  DtfEra
  DtfYear
  DtfMonth
  DtfDay
  DtfDayPeriod
  DtfHour
  DtfMinute
  DtfSecond
  DtfFractionalSecondDigits
  DtfTimeZoneName
}

// --- DateTimeFormat closed option sets (§11.1.2 component table) -----------
//
// Each component admits its own fixed set of widths — the §11.1.2 table gives
// weekday/era/dayPeriod « narrow, short, long », year/day/hour/minute/second
// « 2-digit, numeric », month all five, timeZoneName its own six.

/// Widths of the numeric-only components (year, day, hour, minute, second).
pub type NumericWidth {
  WNumeric
  WTwoDigit
}

/// `month` is the one component that admits both a numeric and a name width.
pub type MonthWidth {
  MonthNum(NumericWidth)
  MonthName(NameWidth)
}

/// `timeZoneName` widths (§11.1.2).
pub type TimeZoneNameWidth {
  TzShort
  TzLong
  TzShortOffset
  TzLongOffset
  TzShortGeneric
  TzLongGeneric
}

/// `[[HourCycle]]` (§11.1.2).
pub type HourCycle {
  H11
  H12
  H23
  H24
}

/// `[[DateStyle]]`.
pub type DateStyle {
  DsFull
  DsLong
  DsMedium
  DsShort
}

/// `[[TimeStyle]]`.
pub type TimeStyle {
  TsFull
  TsLong
  TsMedium
  TsShort
}

/// The active DateTimeFormat formatting components — which date/time fields
/// the output contains and in which width (§11.5 DateTimeFormat records).
/// `None` means the component is not part of the format.
pub type DtfComponents {
  DtfComponents(
    weekday: Option(NameWidth),
    era: Option(NameWidth),
    year: Option(NumericWidth),
    month: Option(MonthWidth),
    day: Option(NumericWidth),
    day_period: Option(NameWidth),
    hour: Option(NumericWidth),
    minute: Option(NumericWidth),
    second: Option(NumericWidth),
    /// `fractionalSecondDigits` is a digit count in 1..3, not a width.
    fractional_second_digits: Option(Int),
    time_zone_name: Option(TimeZoneNameWidth),
  )
}

/// A `DtfComponents` with every component absent.
pub const empty_dtf_components = DtfComponents(
  weekday: None,
  era: None,
  year: None,
  month: None,
  day: None,
  day_period: None,
  hour: None,
  minute: None,
  second: None,
  fractional_second_digits: None,
  time_zone_name: None,
)

/// A DateTimeFormat's [[TimeZone]].
///
/// A UTC offset is never stored on the formatter: it is a function of the
/// zone *and the instant being formatted*, so it is resolved per format call
/// (see `intl_timezone.offset_at`).
pub type DtfTimeZone {
  /// The host environment's default zone (`HostHooks.time_zone`). Its
  /// identifier is not observable, so resolvedOptions reports "UTC"; the
  /// offset is that zone's offset at the formatted instant.
  HostZone(zone: host_time.TimeZone)
  /// A named IANA zone, validated against the system tzdata. Its offset
  /// varies with the instant (DST). The zone handle *is* the identifier.
  NamedZone(zone: temporal_tz.Zone)
  /// A zone whose offset never varies: "UTC", "GMT", "Etc/GMT+3", "+05:30".
  FixedZone(id: String, offset_minutes: Int)
}

/// The identifier resolvedOptions() reports for a formatter's time zone.
pub fn dtf_time_zone_id(tz: DtfTimeZone) -> String {
  case tz {
    HostZone(_) -> "UTC"
    NamedZone(zone:) -> temporal_tz.zone_id(zone)
    FixedZone(id:, ..) -> id
  }
}

/// Intl.DateTimeFormat resolved options (§11.1.2 CreateDateTimeFormat).
///
/// The `weekday` … `time_style` fields are the resolvedOptions view (the
/// component options as the user requested them, plus locale defaults);
/// `components` is the effective formatting table (style expansion applied,
/// and re-derived per Temporal type at format time). The two intentionally
/// differ once dateStyle/timeStyle is involved.
///
/// `explicit` lists the component options the user provided explicitly —
/// GetDateTimeFormat needs it for Temporal ~relevant~ inheritance.
pub type DateTimeFormatState {
  DateTimeFormatState(
    locale: String,
    calendar: String,
    numbering_system: String,
    time_zone: DtfTimeZone,
    hour_cycle: Option(HourCycle),
    weekday: Option(NameWidth),
    era: Option(NameWidth),
    year: Option(NumericWidth),
    month: Option(MonthWidth),
    day: Option(NumericWidth),
    day_period: Option(NameWidth),
    hour: Option(NumericWidth),
    minute: Option(NumericWidth),
    second: Option(NumericWidth),
    fractional_second_digits: Option(Int),
    time_zone_name: Option(TimeZoneNameWidth),
    date_style: Option(DateStyle),
    time_style: Option(TimeStyle),
    explicit: List(DtfComponent),
    components: DtfComponents,
  )
}

/// Intl.PluralRules resolved options (§16.1.2 InitializePluralRules).
/// `[[CompactDisplay]]` rides on the compact `Notation` variant.
pub type PluralRulesState {
  PluralRulesState(
    locale: String,
    plural_type: PluralType,
    notation: Notation,
    digits: IntlDigitOptions,
  )
}

/// Intl.ListFormat resolved options (§13.1.2).
pub type ListFormatState {
  ListFormatState(
    locale: String,
    list_type: ListFormatType,
    style: ListFormatStyle,
  )
}

/// Intl.RelativeTimeFormat resolved options (§17.1.2). `numeric` here is the
/// "always"/"auto" enum option, not a number.
pub type RelativeTimeFormatState {
  RelativeTimeFormatState(
    locale: String,
    style: RtfStyle,
    numeric: RtfNumeric,
    numbering_system: String,
  )
}

/// Intl.Segmenter resolved options (§18.1.2).
pub type SegmenterState {
  SegmenterState(locale: String, granularity: Granularity)
}

/// `[[SegmenterGranularity]]` (§18.1.2).
pub type Granularity {
  GGrapheme
  GWord
  GSentence
}

/// One segment of a segmented string: its text, its UTF-16 start index in the
/// input, and whether it is word-like (only meaningful for word granularity).
pub type Segment {
  Segment(text: String, index: Int, word_like: Bool)
}

/// Intl.DisplayNames resolved options (§12.1.2). `language_display` is only
/// present for type "language".
pub type DisplayNamesState {
  DisplayNamesState(
    locale: String,
    style: NameWidth,
    display_type: DisplayNamesType,
    fallback: DisplayNamesFallback,
    language_display: Option(LanguageDisplay),
  )
}

/// `[[Type]]` (§12.1.2) — the code space `Intl.DisplayNames.prototype.of`
/// interprets its argument in.
pub type DisplayNamesType {
  DnLanguage
  DnRegion
  DnScript
  DnCurrency
  DnCalendar
  DnDateTimeField
}

/// `[[Fallback]]` (§12.1.2) — what `.of()` returns when there is no name.
pub type DisplayNamesFallback {
  FbCode
  FbNone
}

/// `[[LanguageDisplay]]` (§12.1.2), only meaningful for type "language".
pub type LanguageDisplay {
  LdDialect
  LdStandard
}

/// %SegmentsPrototype% instance state: the segmenter's granularity plus the
/// string being segmented.
pub type SegmentsState {
  SegmentsState(string: String, granularity: Granularity)
}

/// %SegmentIteratorPrototype% instance state: a `SegmentsState` plus the
/// segments not yet yielded. The full segmentation is computed once at
/// iterator creation so `next()` is O(1) per step, not O(n).
pub type SegmentIteratorState {
  SegmentIteratorState(
    string: String,
    granularity: Granularity,
    remaining: List(Segment),
  )
}

/// Intl.NumberFormat resolved options (§15.1.2 InitializeNumberFormat).
/// The style-/notation-conditional slots (currency*, unit*, compactDisplay)
/// live inside the `NumStyle` / `Notation` variant that selects them.
pub type NumberFormatState {
  NumberFormatState(
    locale: String,
    numbering_system: String,
    style: NumStyle,
    digits: IntlDigitOptions,
    use_grouping: IntlUseGrouping,
    notation: Notation,
    sign_display: SignDisplay,
  )
}

/// Intl.DurationFormat resolved options (Intl.DurationFormat §1.1.3), one
/// `DurationUnitOptions` field per duration unit.
pub type DurationFormatState {
  DurationFormatState(
    locale: String,
    numbering_system: String,
    style: DurationBaseStyle,
    years: DurationUnitOptions,
    months: DurationUnitOptions,
    weeks: DurationUnitOptions,
    days: DurationUnitOptions,
    hours: DurationUnitOptions,
    minutes: DurationUnitOptions,
    seconds: DurationUnitOptions,
    milliseconds: DurationUnitOptions,
    microseconds: DurationUnitOptions,
    nanoseconds: DurationUnitOptions,
    fractional_digits: Option(Int),
  )
}
