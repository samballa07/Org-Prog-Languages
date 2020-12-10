#[derive(Debug, PartialEq)]

pub enum Command {
    Power(bool, i32),    // [Increase/Decrease] power by [number].
    Missiles(bool, i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),        // Turn [On/Off] the shield.
    Try,                 // Try calling pepper.
    Invalid,             // [anything else]
}

/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str(&self) -> String {
        let string: String;
        match &self {
            Command::Power(b, x) => {
                if b == &true {
                    string = format!("Power increased by {}%", x);
                } else {
                    string = format!("Power decreased by {}%", x);
                }
            }
            Command::Missiles(b, x) => {
                if b == &true {
                    string = format!("Missiles increased by {}", x);
                } else {
                    string = format!("Missiles decreased by {}", x);
                }
            }
            Command::Shield(b) => {
                if b == &true {
                    string = String::from("Shield turned on");
                } else {
                    string = String::from("Shield turned off");
                }
            }
            Command::Try => {
                string = String::from("Call attempt failed");
            }
            Command::Invalid => {
                string = String::from("Not a command");
            }
        }
        return string;
    }
}
/**
    Complete this method that converts a string to a command
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
    let mut cmd = Command::Invalid;
    let mut first_word: &str = "";
    let mut iter = s.split_ascii_whitespace();
    match iter.next() {
        Some(x) => first_word = x,
        None => cmd = Command::Invalid,
    }
    if first_word == "power" {
        match iter.next() {
            Some(slice) => match iter.next() {
                Some(num) => match num.parse::<i32>() {
                    Ok(n) => {
                        if slice == "inc" {
                            cmd = Command::Power(true, n);
                        } else if slice == "dec" {
                            cmd = Command::Power(false, n);
                        }
                    }
                    Err(_) => cmd = Command::Invalid,
                },
                None => cmd = Command::Invalid,
            },
            None => cmd = Command::Invalid,
        }
    } else if first_word == "fire" {
        match iter.next() {
            Some(num) => match iter.next() {
                Some(word) => match num.parse::<i32>() {
                    Ok(n) => {
                        if word == "missiles" {
                            cmd = Command::Missiles(false, n);
                        }
                    }
                    Err(_) => cmd = Command::Invalid,
                },
                None => cmd = Command::Invalid,
            },
            None => cmd = Command::Invalid,
        }
    } else if first_word == "add" {
        match iter.next() {
            Some(num) => match iter.next() {
                Some(word) => match num.parse::<i32>() {
                    Ok(n) => {
                        if word == "missiles" {
                            cmd = Command::Missiles(true, n);
                        }
                    }
                    Err(_) => cmd = Command::Invalid,
                },
                None => cmd = Command::Invalid,
            },
            None => cmd = Command::Invalid,
        }
    } else if first_word == "shield" {
        match iter.next() {
            Some(slice) => {
                if slice == "on" {
                    cmd = Command::Shield(true);
                } else if slice == "off" {
                    cmd = Command::Shield(false);
                }
            }
            None => cmd = Command::Invalid,
        }
    } else if s == "try calling Miss Potts" {
        cmd = Command::Try;
    } else {
        cmd = Command::Invalid;
    }
    return cmd;
}
