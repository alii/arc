//// ECMA-402 internal-slot data shared by the Intl builtins and the pure
//// formatting engine (`rt/builtins/intl_format`): the closed option enums,
//// and the per-service resolved state an `IntlObj` carries.
////
//// A leaf module: no runtime imports, so both `rt/types` (for the `IntlObj`
//// object kind) and the formatting engine can depend on it.

import gleam/option.{type Option}

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

/// Which Intl service an Intl instance object belongs to.
pub type IntlService {
  IntlNumberFormat
  IntlDurationFormat
}

/// The resolved state carried by an `IntlObj`. The variant IS the brand:
/// `Intl.NumberFormat.prototype.format` cannot be handed DurationFormat state.
pub type IntlData {
  NumberFormatData(NumberFormatState)
  DurationFormatData(DurationFormatState)
}

/// The service tag for a given Intl state — the two are 1:1.
pub fn intl_service(data: IntlData) -> IntlService {
  case data {
    NumberFormatData(_) -> IntlNumberFormat
    DurationFormatData(_) -> IntlDurationFormat
  }
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
