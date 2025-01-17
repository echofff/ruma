//! Endpoints for server-side key backups.

pub mod add_backup_keys;
pub mod add_backup_keys_for_room;
pub mod add_backup_keys_for_session;
pub mod create_backup_version;
pub mod delete_backup_keys;
pub mod delete_backup_keys_for_room;
pub mod delete_backup_keys_for_session;
pub mod delete_backup_version;
pub mod get_backup_info;
pub mod get_backup_keys;
pub mod get_backup_keys_for_room;
pub mod get_backup_keys_for_session;
pub mod get_latest_backup_info;
pub mod update_backup_version;

use std::collections::BTreeMap;

use js_int::UInt;
use ruma_identifiers::{DeviceKeyId, UserId};
use ruma_serde::{Base64, Raw};
use serde::{Deserialize, Serialize};

/// A wrapper around a mapping of session IDs to key data.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[cfg_attr(not(feature = "unstable-exhaustive-types"), non_exhaustive)]
pub struct RoomKeyBackup {
    /// A map of session IDs to key data.
    pub sessions: BTreeMap<String, Raw<KeyBackupData>>,
}

impl RoomKeyBackup {
    /// Creates a new `RoomKeyBackup` with the given sessions.
    pub fn new(sessions: BTreeMap<String, Raw<KeyBackupData>>) -> Self {
        Self { sessions }
    }
}

/// The algorithm used for storing backups.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "algorithm", content = "auth_data")]
#[cfg_attr(not(feature = "unstable-exhaustive-types"), non_exhaustive)]
pub enum BackupAlgorithm {
    /// `m.megolm_backup.v1.curve25519-aes-sha2` backup algorithm.
    #[serde(rename = "m.megolm_backup.v1.curve25519-aes-sha2")]
    MegolmBackupV1Curve25519AesSha2 {
        /// The curve25519 public key used to encrypt the backups, encoded in unpadded base64.
        public_key: Base64,

        /// Signatures of the auth_data as Signed JSON.
        signatures: BTreeMap<Box<UserId>, BTreeMap<Box<DeviceKeyId>, String>>,
    },
}

/// Information about the backup key.
///
/// To create an instance of this type, first create a `KeyBackupDataInit` and convert it via
/// `KeyBackupData::from` / `.into()`.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[cfg_attr(not(feature = "unstable-exhaustive-types"), non_exhaustive)]
pub struct KeyBackupData {
    /// The index of the first message in the session that the key can decrypt.
    pub first_message_index: UInt,

    /// The number of times this key has been forwarded via key-sharing between devices.
    pub forwarded_count: UInt,

    /// Whether the device backing up the key verified the device that the key is from.
    pub is_verified: bool,

    /// Data about the session.
    pub session_data: SessionData,
}

/// Information about the backup key.
///
/// This struct will not be updated even if additional fields are added to `SessionData` in a
/// new (non-breaking) release of the Matrix specification.
#[derive(Debug)]
#[allow(clippy::exhaustive_structs)]
pub struct KeyBackupDataInit {
    /// The index of the first message in the session that the key can decrypt.
    pub first_message_index: UInt,

    /// The number of times this key has been forwarded via key-sharing between devices.
    pub forwarded_count: UInt,

    /// Whether the device backing up the key verified the device that the key is from.
    pub is_verified: bool,

    /// Data about the session.
    pub session_data: SessionData,
}

impl From<KeyBackupDataInit> for KeyBackupData {
    fn from(init: KeyBackupDataInit) -> Self {
        let KeyBackupDataInit { first_message_index, forwarded_count, is_verified, session_data } =
            init;
        Self { first_message_index, forwarded_count, is_verified, session_data }
    }
}

/// The algorithm used for storing backups.
///
/// To create an instance of this type, first create a `SessionDataInit` and convert it via
/// `SessionData::from` / `.into()`.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[cfg_attr(not(feature = "unstable-exhaustive-types"), non_exhaustive)]
pub struct SessionData {
    /// Unpadded base64-encoded public half of the ephemeral key.
    pub ephemeral: Base64,

    /// Ciphertext, encrypted using AES-CBC-256 with PKCS#7 padding, encoded in base64.
    pub ciphertext: Base64,

    /// First 8 bytes of MAC key, encoded in base64.
    pub mac: Base64,
}

/// The algorithm used for storing backups.
///
/// This struct will not be updated even if additional fields are added to `SessionData` in a
/// new (non-breaking) release of the Matrix specification.
#[derive(Debug)]
#[allow(clippy::exhaustive_structs)]
pub struct SessionDataInit {
    /// Unpadded base64-encoded public half of the ephemeral key.
    pub ephemeral: Base64,

    /// Ciphertext, encrypted using AES-CBC-256 with PKCS#7 padding, encoded in base64.
    pub ciphertext: Base64,

    /// First 8 bytes of MAC key, encoded in base64.
    pub mac: Base64,
}

impl From<SessionDataInit> for SessionData {
    fn from(init: SessionDataInit) -> Self {
        let SessionDataInit { ephemeral, ciphertext, mac } = init;
        Self { ephemeral, ciphertext, mac }
    }
}
