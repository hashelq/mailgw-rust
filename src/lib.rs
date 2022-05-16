//!
//! A simple async module for registering a mail.gw account and getting messages from it.
//! 
//! Example of a simple loop:
//! ```rust
//! use mailgw::*;
//! use std::{error::Error, time::Duration};
//! 
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn Error>> {
//!     let mail = MailGwBuilder::new().acquire().await?;
//! 
//!     println!("{}", mail.address());
//! 
//!     loop {
//!         std::thread::sleep(Duration::from_millis(5000));
//!         println!("Messages: {:#?}", mail.messages().await?);
//!     }
//! }
//! ```
//!

use lazy_static::lazy_static;
use rand::{seq::SliceRandom, Rng};
use reqwest::{header::HeaderMap, Client, Error as RError};
use serde_derive::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::{Arc, Mutex};
use thiserror::Error as TError;

lazy_static! {
  /// Global static list of available domains.
  pub static ref DOMAINS: Mutex<Domains> = Mutex::new(Domains::new_raw());
}

/// Error that is thrown when we cannot obtain domains list
#[derive(TError, Debug)]
pub enum DomainsObtainingError {
    /// ::reqwest error
    #[error("reqwest lib error")]
    Reqwest(RError),

    /// There are no available domains
    #[error("no domains")]
    NoDomains,
}

impl From<RError> for DomainsObtainingError {
    fn from(s: RError) -> Self {
        Self::Reqwest(s)
    }
}

/// Smartcontroller of domains list.
/// You don't need to create nor use this struct directly.
/// Instead use `mailgw::DOMAINS.lock()?.list()`
pub struct Domains {
    list: Option<Arc<Vec<String>>>,
}

impl Domains {
    pub(crate) fn new_raw() -> Self {
        Self { list: None }
    }

    /// Reload list of avaiable domains
    pub async fn reload(
        &mut self,
        client: Option<Client>,
    ) -> Result<Arc<Vec<String>>, DomainsObtainingError> {
        let client = client.unwrap_or(Client::new());
        let mut response: Value = client
            .get("https://api.mail.gw/domains")
            .send()
            .await?
            .json()
            .await?;

        if let Some(Value::Array(array)) = response.get_mut("hydra:member") {
            if array.is_empty() {
                return Err(DomainsObtainingError::NoDomains);
            }
            self.list = Some(Arc::new(
                array
                    .iter_mut()
                    .map(|x| {
                        if let Value::String(v) = x["domain"].take() {
                            v
                        } else {
                            panic!("Domain is not a string")
                        }
                    })
                    .collect(),
            ));
            Ok(self.list.as_ref().unwrap().clone())
        } else {
            panic!("Undefined error while getting domains: {:?}", response)
        }
    }

    /// Get ***OR*** obtain list of available domains.
    pub async fn list(
        &mut self,
        client: Option<Client>,
    ) -> Result<Arc<Vec<String>>, DomainsObtainingError> {
        match self.list {
            Some(ref v) => Ok(v.clone()),
            None => self.reload(client).await,
        }
    }
}

/// Error that is thrown when we cannot acquire email address.
#[derive(TError, Debug)]
pub enum EmailAcquiringError {
    /// ::reqwest error
    #[error("reqwest lib error")]
    Reqwest(RError),

    /// Service violation. Usually unsupported domain, invalid email, etc.
    #[error("service violation error")]
    Violation(String),

    /// There are no avaiable domains
    #[error("no domains")]
    NoDomains,
}

impl From<RError> for EmailAcquiringError {
    fn from(s: RError) -> Self {
        Self::Reqwest(s)
    }
}

impl From<DomainsObtainingError> for EmailAcquiringError {
    fn from(s: DomainsObtainingError) -> Self {
        match s {
            DomainsObtainingError::Reqwest(r) => Self::Reqwest(r),
            DomainsObtainingError::NoDomains => Self::NoDomains,
        }
    }
}

#[derive(Serialize)]
struct EmailAcquiringRequestBody<'a> {
    address: &'a str,
    password: &'a str,
}

type GetTokenRequestBody<'a> = EmailAcquiringRequestBody<'a>;

#[derive(Deserialize)]
struct EmailAcquiringResponse {}

/// The sender of the message
#[derive(Deserialize, Debug)]
pub struct Sender {
    /// Email address
    pub address: String,

    /// Name
    pub name: String,
}

/// Email message type as is.
#[derive(Deserialize, Debug)]
#[allow(non_snake_case)]
pub struct Message {
    pub id: String,
    pub accountId: String,
    pub msgid: String,

    /// Sender
    pub from: Sender,
    
    /// Subject of the email
    pub subject: String,

    /// The content itself
    pub intro: String,
    
    pub seen: bool,
    
    /// Whether this message has attachments
    pub hasAttachments: bool,

    /// Size of the messsage
    pub size: u32,
}

/// Error that is thrown when we cannot execute `&self` method.
#[derive(TError, Debug)]
pub enum MethodError {
    /// ::reqwest error
    #[error("reqwest lib error")]
    Reqwest(RError),

    /// The authorization data is invalid.
    #[error("unauthorized")]
    Unauthorized,

    /// Other fail code
    #[error("failcode")]
    FailCode(i64),

    /// Unknown error
    #[error("other")]
    Other(Value),
}

impl From<RError> for MethodError {
    fn from(s: RError) -> Self {
        Self::Reqwest(s)
    }
}

/// Instance of MailGW service. Stores authorization data that is used to recieve emails.
pub struct MailGw {
    client: Client,
    token: String,
    address: String,
    password: String,
}

impl MailGw {
    /// Acquire by directly passing options. Use `MailGwBuilder` to make it easier.
    /// NOTE: address must only contain a-z0-9 characters! No uppercase.
    async fn acquire(
        client: Client,
        address: String,
        password: String,
    ) -> Result<Self, EmailAcquiringError> {
        let mut s = Self {
            client,
            token: String::new(),
            address,
            password,
        };

        let mut response: Value = s
            .client
            .post("https://api.mail.gw/accounts")
            .json(&EmailAcquiringRequestBody {
                address: &s.address,
                password: &s.password,
            })
            .send()
            .await?
            .json()
            .await?;

        if let Some(ref mut violation) = response.get_mut("violations") {
            Err(EmailAcquiringError::Violation({
                if let Value::String(w) = violation[0]["message"].take() {
                    w
                } else {
                    panic!("Acquire error violation message is not a string")
                }
            }))
        } else {
            s.reload_token()
                .await
                .expect("Cannot obtain token first time. Probably code bug");
            Ok(s)
        }
    }

    /// Reload token
    pub async fn reload_token(&mut self) -> Result<&str, MethodError> {
        let data: Value = self
            .client
            .post("https://api.mail.gw/token")
            .json(&GetTokenRequestBody {
                address: &self.address,
                password: &self.password,
            })
            .send()
            .await?
            .json()
            .await?;

        let mut data = parse_method_response(data)?;

        // FIXME: data.get().take() returns &Value... ????
        if let Some(token) = data.get_mut("token").take() {
            if let Value::String(s) = token.take() {
                self.token = s;
                Ok(&self.token)
            } else {
                Err(MethodError::Other(data))
            }
        } else {
            Err(MethodError::Other(data))
        }
    }

    /// Gether incoming messages.
    pub async fn messages(&self) -> Result<Vec<Message>, MethodError> {
        use serde::Deserialize;

        let data = self
            .client
            .get("https://api.mail.gw/messages")
            .headers({
                let mut hm = HeaderMap::new();
                hm.insert(
                    "Authorization",
                    ["Bearer ", &self.token].concat().parse().unwrap(),
                );
                hm
            })
            .send()
            .await?
            .json::<Value>()
            .await?;

        let data = parse_method_response(data)?;
        if let Some(mdata) = data.get("hydra:member").take() {
            if let Ok(messages) = Vec::<Message>::deserialize(mdata) {
                Ok(messages)
            } else {
                Err(MethodError::Other(data))
            }
        } else {
            Err(MethodError::Other(data))
        }
    }

    pub fn address(&self) -> &str {
        &self.address
    }
}

/// Builder for MailGw, may be used to create a MailGw instance without passing anything, like
/// `MailGwBuilder::new().acquire().await?`
pub struct MailGwBuilder {
    pub client: Option<Client>,
    pub address: Option<String>,
    pub password: Option<String>,
}

impl MailGwBuilder {
    /// New instance
    pub fn new() -> Self {
        Self {
            client: None,
            address: None,
            password: None,
        }
    }

    /// Set reqwest client
    pub fn with_client(mut self, client: impl Into<Client>) -> Self {
        self.client = Some(client.into());
        self
    }

    /// Set address
    pub fn with_address(mut self, address: impl Into<String>) -> Self {
        self.address = Some(address.into());
        self
    }

    /// Set password
    pub fn with_password(mut self, password: impl Into<String>) -> Self {
        self.password = Some(password.into());
        self
    }

    /// Create `MailGw` by passing known values or generating them.
    /// ***Default size*** of address and password is 32 characters.
    pub async fn acquire(self) -> Result<MailGw, EmailAcquiringError> {
        MailGw::acquire(
            self.client.unwrap_or(Client::new()),
            match self.address {
                Some(v) => v,
                None => construct_address(8).await?,
            },
            self.password.unwrap_or_else(|| construct_password(8)),
        )
        .await
    }
}

async fn construct_address(len: usize) -> Result<String, DomainsObtainingError> {
    Ok([
        construct_password(len).as_str(),
        "@",
        DOMAINS
            .lock()
            .unwrap()
            .list(None)
            .await?
            .choose(&mut rand::thread_rng())
            .unwrap()
            .as_str(),
    ]
    .concat())
}

fn construct_password(len: usize) -> String {
    const CHARSET: &[u8] = b"abcdefghijklmnopqrstuvwxyz0123456789";
    let mut rng = rand::thread_rng();

    (0..len)
        .map(|_| {
            let idx = rng.gen_range(0..CHARSET.len());
            CHARSET[idx] as char
        })
        .collect()
}

fn parse_method_response(data: Value) -> Result<Value, MethodError> {
    if let Some(Value::Number(code)) = data.get("code") {
        let code = code.as_i64().expect("Bad code");
        Err(match code {
            401 => MethodError::Unauthorized,
            _ => MethodError::FailCode(code),
        })
    } else {
        Ok(data)
    }
}
