use serde::Serialize;
use serde_json::value::RawValue as RawJsonValue;

pub mod direct;

/// The base trait that all event content types implement.
///
/// Implementing this trait allows content types to be serialized as well as deserialized.
pub trait AccountDataContent: Sized + Serialize {
    /// A matrix event identifier, like `m.room.message`.
    fn data_type(&self) -> &str;

    /// Constructs the given event content.
    fn from_parts(event_type: &str, content: &RawJsonValue) -> serde_json::Result<Self>;
}

/// Marker trait for the content of a global account data event.
pub trait GlobalAccountDataContent: AccountDataContent {}

/// Marker trait for the content of a room account data event.
pub trait RoomAccountDataContent: AccountDataContent {}

/// Global account data.
#[derive(Clone, Debug, AccountData)]
pub struct GlobalAccountData<C: GlobalAccountDataContent> {
    /// Data specific to the event type.
    pub content: C,
}

/// Room-bound account data.
#[derive(Clone, Debug, AccountData)]
pub struct RoomAccountData<C: RoomAccountDataContent> {
    /// Data specific to the event type.
    pub content: C,
}

/* account_data_enum! {
    /// Any global account data event.
    enum GlobalAccountData {
        "m.direct",
        "m.ignored_user_list",
        "m.push_rules",
    }

    /// Any room account data event.
    enum RoomAccountData {
        "m.fully_read",
        "m.tag",
    }
} */
