import arc/time_zone
import gleam/option.{type Option, None, Some}

pub type NumberStyle {
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
  CurrencyCode
  CurrencySymbol
  CurrencyNarrowSymbol
  CurrencyName
}

pub type CurrencySign {
  StandardSign
  AccountingSign
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
  TrailingZeroAuto
  TrailingZeroStripIfInteger
}

pub type DigitRange {
  DigitRange(min: Int, max: Int)
}

pub type IntlDigitOptions {
  IntlDigitOptions(
    minimum_integer_digits: Int,
    fraction_digits: Option(DigitRange),
    significant_digits: Option(DigitRange),
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
  WidthLong
  WidthShort
  WidthNarrow
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
  ListLong
  ListShort
  ListNarrow
}

pub type RelativeTimeStyle {
  RelativeLong
  RelativeShort
  RelativeNarrow
}

pub type RelativeTimeNumeric {
  NumericAlways
  NumericAuto
}

pub type DurationUnitStyle {
  UnitStyleLong
  UnitStyleShort
  UnitStyleNarrow
  UnitStyleNumeric
  UnitStyleTwoDigit
  UnitStyleFractional
}

pub type DurationDisplay {
  DisplayAuto
  DisplayAlways
}

pub type DurationBaseStyle {
  BaseLong
  BaseShort
  BaseNarrow
  BaseDigital
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
  BoundCollator
  BoundNumberFormat
  BoundDateTimeFormat
}

pub fn bound_getter_service(service: BoundGetterService) -> IntlService {
  case service {
    BoundCollator -> IntlCollator
    BoundNumberFormat -> IntlNumberFormat
    BoundDateTimeFormat -> IntlDateTimeFormat
  }
}

pub type ConstructibleService {
  LocaleService
  CollatorService
  NumberFormatService
  DateTimeFormatService
  PluralRulesService
  ListFormatService
  RelativeTimeFormatService
  SegmenterService
  DisplayNamesService
  DurationFormatService
}

pub fn constructible_service(service: ConstructibleService) -> IntlService {
  case service {
    LocaleService -> IntlLocale
    CollatorService -> IntlCollator
    NumberFormatService -> IntlNumberFormat
    DateTimeFormatService -> IntlDateTimeFormat
    PluralRulesService -> IntlPluralRules
    ListFormatService -> IntlListFormat
    RelativeTimeFormatService -> IntlRelativeTimeFormat
    SegmenterService -> IntlSegmenter
    DisplayNamesService -> IntlDisplayNames
    DurationFormatService -> IntlDurationFormat
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

pub type DateTimeComponent {
  WeekdayComponent
  EraComponent
  YearComponent
  MonthComponent
  DayComponent
  DayPeriodComponent
  HourComponent
  MinuteComponent
  SecondComponent
  FractionalSecondDigitsComponent
  TimeZoneNameComponent
}

pub type NumericWidth {
  Numeric
  TwoDigit
}

pub type MonthWidth {
  MonthNum(NumericWidth)
  MonthName(NameWidth)
}

pub type TimeZoneNameWidth {
  ZoneShort
  ZoneLong
  ZoneShortOffset
  ZoneLongOffset
  ZoneShortGeneric
  ZoneLongGeneric
}

pub type HourCycle {
  H11
  H12
  H23
  H24
}

pub type DateStyle {
  DateFull
  DateLong
  DateMedium
  DateShort
}

pub type TimeStyle {
  TimeFull
  TimeLong
  TimeMedium
  TimeShort
}

pub type DateTimeComponents {
  DateTimeComponents(
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

pub const empty_date_time_components = DateTimeComponents(
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

pub type FormatTimeZone {
  // host zone, reported as "UTC"
  HostZone(zone: time_zone.TimeZone)
  NamedZone(zone: time_zone.Zone)
  FixedZone(id: String, offset_minutes: Int)
}

pub fn format_time_zone_id(tz: FormatTimeZone) -> String {
  case tz {
    HostZone(_) -> "UTC"
    NamedZone(zone:) -> time_zone.zone_id(zone)
    FixedZone(id:, ..) -> id
  }
}

pub type DateTimeFormatState {
  DateTimeFormatState(
    locale: String,
    calendar: String,
    numbering_system: String,
    time_zone: FormatTimeZone,
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
    explicit: List(DateTimeComponent),
    components: DateTimeComponents,
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
    style: RelativeTimeStyle,
    numeric: RelativeTimeNumeric,
    numbering_system: String,
  )
}

pub type SegmenterState {
  SegmenterState(locale: String, granularity: Granularity)
}

pub type Granularity {
  GraphemeGranularity
  WordGranularity
  SentenceGranularity
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
  LanguageNames
  RegionNames
  ScriptNames
  CurrencyNames
  CalendarNames
  DateTimeFieldNames
}

pub type DisplayNamesFallback {
  CodeFallback
  NoFallback
}

pub type LanguageDisplay {
  DialectNames
  StandardNames
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
    style: NumberStyle,
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

pub fn collator_usage_text(v: CollatorUsage) -> String {
  case v {
    UsageSort -> "sort"
    UsageSearch -> "search"
  }
}

pub fn collator_sensitivity_text(v: CollatorSensitivity) -> String {
  case v {
    SensBase -> "base"
    SensAccent -> "accent"
    SensCase -> "case"
    SensVariant -> "variant"
  }
}

pub fn case_first_text(v: CaseFirst) -> String {
  case v {
    CaseFirstUpper -> "upper"
    CaseFirstLower -> "lower"
    CaseFirstFalse -> "false"
  }
}

pub fn case_first_from_text(s: String) -> Option(CaseFirst) {
  case s {
    "upper" -> Some(CaseFirstUpper)
    "lower" -> Some(CaseFirstLower)
    "false" -> Some(CaseFirstFalse)
    _ -> None
  }
}

pub fn num_style_text(v: NumberStyle) -> String {
  case v {
    StyleDecimal -> "decimal"
    StylePercent -> "percent"
    StyleCurrency(..) -> "currency"
    StyleUnit(..) -> "unit"
  }
}

pub fn notation_text(v: Notation) -> String {
  case v {
    NotationStandard -> "standard"
    NotationScientific -> "scientific"
    NotationEngineering -> "engineering"
    NotationCompact(..) -> "compact"
  }
}

pub fn compact_display_text(v: CompactDisplay) -> String {
  case v {
    CompactShort -> "short"
    CompactLong -> "long"
  }
}

pub fn sign_display_text(v: SignDisplay) -> String {
  case v {
    SignAuto -> "auto"
    SignNever -> "never"
    SignAlways -> "always"
    SignExceptZero -> "exceptZero"
    SignNegative -> "negative"
  }
}

pub fn currency_display_text(v: CurrencyDisplay) -> String {
  case v {
    CurrencyCode -> "code"
    CurrencySymbol -> "symbol"
    CurrencyNarrowSymbol -> "narrowSymbol"
    CurrencyName -> "name"
  }
}

pub fn currency_sign_text(v: CurrencySign) -> String {
  case v {
    StandardSign -> "standard"
    AccountingSign -> "accounting"
  }
}

pub fn unit_display_text(v: UnitDisplay) -> String {
  case v {
    UnitShort -> "short"
    UnitNarrow -> "narrow"
    UnitLong -> "long"
  }
}

pub fn rounding_mode_text(v: RoundingMode) -> String {
  case v {
    RoundCeil -> "ceil"
    RoundFloor -> "floor"
    RoundExpand -> "expand"
    RoundTrunc -> "trunc"
    RoundHalfCeil -> "halfCeil"
    RoundHalfFloor -> "halfFloor"
    RoundHalfExpand -> "halfExpand"
    RoundHalfTrunc -> "halfTrunc"
    RoundHalfEven -> "halfEven"
  }
}

pub fn rounding_priority_text(v: RoundingPriority) -> String {
  case v {
    PriorityAuto -> "auto"
    PriorityMorePrecision -> "morePrecision"
    PriorityLessPrecision -> "lessPrecision"
  }
}

pub fn trailing_zero_display_text(v: TrailingZeroDisplay) -> String {
  case v {
    TrailingZeroAuto -> "auto"
    TrailingZeroStripIfInteger -> "stripIfInteger"
  }
}

pub fn numeric_width_text(v: NumericWidth) -> String {
  case v {
    Numeric -> "numeric"
    TwoDigit -> "2-digit"
  }
}

pub fn name_width_text(v: NameWidth) -> String {
  case v {
    WidthLong -> "long"
    WidthShort -> "short"
    WidthNarrow -> "narrow"
  }
}

pub fn month_width_text(v: MonthWidth) -> String {
  case v {
    MonthNum(w) -> numeric_width_text(w)
    MonthName(w) -> name_width_text(w)
  }
}

pub fn time_zone_name_width_text(v: TimeZoneNameWidth) -> String {
  case v {
    ZoneShort -> "short"
    ZoneLong -> "long"
    ZoneShortOffset -> "shortOffset"
    ZoneLongOffset -> "longOffset"
    ZoneShortGeneric -> "shortGeneric"
    ZoneLongGeneric -> "longGeneric"
  }
}

pub fn hour_cycle_text(v: HourCycle) -> String {
  case v {
    H11 -> "h11"
    H12 -> "h12"
    H23 -> "h23"
    H24 -> "h24"
  }
}

pub fn date_style_text(v: DateStyle) -> String {
  case v {
    DateFull -> "full"
    DateLong -> "long"
    DateMedium -> "medium"
    DateShort -> "short"
  }
}

pub fn time_style_text(v: TimeStyle) -> String {
  case v {
    TimeFull -> "full"
    TimeLong -> "long"
    TimeMedium -> "medium"
    TimeShort -> "short"
  }
}

pub fn plural_type_text(v: PluralType) -> String {
  case v {
    Cardinal -> "cardinal"
    Ordinal -> "ordinal"
  }
}

pub fn list_format_type_text(v: ListFormatType) -> String {
  case v {
    Conjunction -> "conjunction"
    Disjunction -> "disjunction"
    UnitList -> "unit"
  }
}

pub fn list_format_style_text(v: ListFormatStyle) -> String {
  case v {
    ListLong -> "long"
    ListShort -> "short"
    ListNarrow -> "narrow"
  }
}

pub fn relative_time_style_text(v: RelativeTimeStyle) -> String {
  case v {
    RelativeLong -> "long"
    RelativeShort -> "short"
    RelativeNarrow -> "narrow"
  }
}

pub fn relative_time_numeric_text(v: RelativeTimeNumeric) -> String {
  case v {
    NumericAlways -> "always"
    NumericAuto -> "auto"
  }
}

pub fn granularity_text(v: Granularity) -> String {
  case v {
    GraphemeGranularity -> "grapheme"
    WordGranularity -> "word"
    SentenceGranularity -> "sentence"
  }
}

pub fn display_names_type_text(v: DisplayNamesType) -> String {
  case v {
    LanguageNames -> "language"
    RegionNames -> "region"
    ScriptNames -> "script"
    CurrencyNames -> "currency"
    CalendarNames -> "calendar"
    DateTimeFieldNames -> "dateTimeField"
  }
}

pub fn display_names_fallback_text(v: DisplayNamesFallback) -> String {
  case v {
    CodeFallback -> "code"
    NoFallback -> "none"
  }
}

pub fn language_display_text(v: LanguageDisplay) -> String {
  case v {
    DialectNames -> "dialect"
    StandardNames -> "standard"
  }
}

pub fn duration_unit_style_text(v: DurationUnitStyle) -> String {
  case v {
    UnitStyleLong -> "long"
    UnitStyleShort -> "short"
    UnitStyleNarrow -> "narrow"
    UnitStyleNumeric -> "numeric"
    UnitStyleTwoDigit -> "2-digit"
    UnitStyleFractional -> "numeric"
  }
}

pub fn duration_display_text(v: DurationDisplay) -> String {
  case v {
    DisplayAuto -> "auto"
    DisplayAlways -> "always"
  }
}

pub fn duration_base_style_text(v: DurationBaseStyle) -> String {
  case v {
    BaseLong -> "long"
    BaseShort -> "short"
    BaseNarrow -> "narrow"
    BaseDigital -> "digital"
  }
}
