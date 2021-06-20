use log::{Level, LevelFilter, Metadata, Record, SetLoggerError};
use std::{env, fs};

struct CompilerLooger;

impl log::Log for CompilerLooger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("{} - {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}

static LOGGER: CompilerLooger = CompilerLooger;

fn init() -> Result<(), SetLoggerError> {
    log::set_logger(&LOGGER).map(|()| log::set_max_level(LevelFilter::Info))
}

fn main() {
    match init() {
        Ok(_) => (),
        Err(e) => {
            println!("Error on log initialization: {}", e);
            return;
        }
    }

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Missing the filename");
        std::process::exit(1);
    }

    let file_name = &args[1];

    let file_contents =
        fs::read_to_string(file_name).expect(&format!("Could not read file {}", file_name));

    println!("{}", file_contents)
}
