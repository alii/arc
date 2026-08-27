import arc/internal/host_time
import arc/rt/builtins/temporal_tz
import gleam/option.{type Option, None}

pub type NumStyle {
  StyleDecimal
  StylePercent
  StyleCurrency(currency: String, display: CurrencyDisplay, sign: CurrencySign)
  StyleUnit(unit: String, display: UnitDisplay)
}

pub type Notation {
  NotationStandard
  NotationScientific
  NotationEngineering
  NotationCompact(display: CompactDisplay)
}

pub type CompactDisplay {
  CompactShort
  CompactLong
}

pub type SignDisplay {
  SignAuto
  SignNever
  SignAlways
  SignExceptZero
  SignNegative
}

pub type CurrencyDisplay {
  CurCode
  CurSymbol
  CurNarrowSymbol
  CurName
}

pub type CurrencySign {
  CurStandard
  CurAccounting
}

pub type UnitDisplay {
  UnitShort
  UnitNarrow
  UnitLong
}

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

pub type RoundingPriority {
  PriorityAuto
  PriorityMorePrecision
  PriorityLessPrecision
}

pub type TrailingZeroDisplay {
  TzdAuto
  TzdStripIfInteger
}

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

pub type IntlUseGrouping {
  GroupingAuto
  GroupingAlways
  GroupingMin2
  GroupingNever
}

pub type NameWidth {
  WLong
  WShort
  WNarrow
}

pub type PluralType {
  Cardinal
  Ordinal
}

pub type ListFormatType {
  Conjunction
  Disjunction
  UnitList
}

pub type ListFormatStyle {
  LLong
  LShort
  LNarrow
}

pub type RtfStyle {
  RtfLong
  RtfShort
  RtfNarrow
}

pub type RtfNumeric {
  RtfAlways
  RtfAuto
}

pub type DurationUnitStyle {
  DurLong
  DurShort
  DurNarrow
  DurNumeric
  DurTwoDigit
  DurFractional
}

pub type DurationDisplay {
  DisplayAuto
  DisplayAlways
}

pub type DurationBaseStyle {
  BsLong
  BsShort
  BsNarrow
  BsDigital
}

pub type DurationUnitOptions {
  DurationUnitOptions(style: DurationUnitStyle, display: DurationDisplay)
}

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
  IntlSegments
  IntlSegmentIterator
}

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

pub type BoundGetterService {
  BgCollator
  BgNumberFormat
  BgDateTimeFormat
}

pub fn bound_getter_service(service: BoundGetterService) -> IntlService {
  case service {
    BgCollator -> IntlCollator
    BgNumberFormat -> IntlNumberFormat
    BgDateTimeFormat -> IntlDateTimeFormat
  }
}

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

pub type LocaleState {
  LocaleState(locale: String)
}

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

pub type CollatorUsage {
  UsageSort
  UsageSearch
}

pub type CollatorSensitivity {
  SensBase
  SensAccent
  SensCase
  SensVariant
}

pub type CaseFirst {
  CaseFirstUpper
  CaseFirstLower
  CaseFirstFalse
}

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

pub type NumericWidth {
  WNumeric
  WTwoDigit
}

pub type MonthWidth {
  MonthNum(NumericWidth)
  MonthName(NameWidth)
}

pub type TimeZoneNameWidth {
  TzShort
  TzLong
  TzShortOffset
  TzLongOffset
  TzShortGeneric
  TzLongGeneric
}

pub type HourCycle {
  H11
  H12
  H23
  H24
}

pub type DateStyle {
  DsFull
  DsLong
  DsMedium
  DsShort
}

pub type TimeStyle {
  TsFull
  TsLong
  TsMedium
  TsShort
}

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
    // digit count 1..3
    fractional_second_digits: Option(Int),
    time_zone_name: Option(TimeZoneNameWidth),
  )
}

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

pub type DtfTimeZone {
  // host zone, reported as "UTC"
  HostZone(zone: host_time.TimeZone)
  NamedZone(zone: temporal_tz.Zone)
  FixedZone(id: String, offset_minutes: Int)
}

pub fn dtf_time_zone_id(tz: DtfTimeZone) -> String {
  case tz {
    HostZone(_) -> "UTC"
    NamedZone(zone:) -> temporal_tz.zone_id(zone)
    FixedZone(id:, ..) -> id
  }
}

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

pub type PluralRulesState {
  PluralRulesState(
    locale: String,
    plural_type: PluralType,
    notation: Notation,
    digits: IntlDigitOptions,
  )
}

pub type ListFormatState {
  ListFormatState(
    locale: String,
    list_type: ListFormatType,
    style: ListFormatStyle,
  )
}

pub type RelativeTimeFormatState {
  RelativeTimeFormatState(
    locale: String,
    style: RtfStyle,
    numeric: RtfNumeric,
    numbering_system: String,
  )
}

pub type SegmenterState {
  SegmenterState(locale: String, granularity: Granularity)
}

pub type Granularity {
  GGrapheme
  GWord
  GSentence
}

pub type Segment {
  Segment(text: String, index: Int, word_like: Bool)
}

pub type DisplayNamesState {
  DisplayNamesState(
    locale: String,
    style: NameWidth,
    display_type: DisplayNamesType,
    fallback: DisplayNamesFallback,
    language_display: Option(LanguageDisplay),
  )
}

pub type DisplayNamesType {
  DnLanguage
  DnRegion
  DnScript
  DnCurrency
  DnCalendar
  DnDateTimeField
}

pub type DisplayNamesFallback {
  FbCode
  FbNone
}

pub type LanguageDisplay {
  LdDialect
  LdStandard
}

pub type SegmentsState {
  SegmentsState(string: String, granularity: Granularity)
}

pub type SegmentIteratorState {
  SegmentIteratorState(
    string: String,
    granularity: Granularity,
    remaining: List(Segment),
  )
}

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
