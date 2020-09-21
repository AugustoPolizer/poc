use std::{env, fs};

fn main() {
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
