//// public time zone types the host hands the engine

// opaque host zone for Date
pub type TimeZone

// parsed transition data for one zone, host supplied
pub type Rules

pub type TzError {
  NoZoneinfo
  Unreadable(detail: String)
  Unparseable(detail: String)
}

pub fn describe(error: TzError) -> String {
  case error {
    NoZoneinfo -> "no time zone database on this host"
    Unreadable(detail:) -> "unreadable time zone data (" <> detail <> ")"
    Unparseable(detail:) -> "corrupt time zone data (" <> detail <> ")"
  }
}
