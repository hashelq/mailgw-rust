use mailgw::*;
use std::{error::Error, time::Duration};

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let mail = MailGwBuilder::new().acquire().await?;

    println!("{}", mail.address());

    loop {
        std::thread::sleep(Duration::from_millis(5000));
        println!("Messages: {:#?}", mail.messages().await?);
    }
}
