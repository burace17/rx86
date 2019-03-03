mod cpu;
use cpu::CPU;

use std::env;
use std::io;
use std::io::prelude::*;
use std::fs::File;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("usage: {} <input_program>", args[0]);
        return Ok(());
    } 

    let mut f = File::open(&args[1])?;
    let mut buffer = [0; 65536];
    f.read(&mut buffer)?;

    let mut cpu = CPU::new(true);
    cpu.load_into_memory(&buffer);
    cpu.emulate();
    Ok(())
}
