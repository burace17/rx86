mod cpu;
use cpu::CPU;

use std::env;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("usage: {} <input_program>", args[0]);
        return Ok(());
    }

    let mut buffer = std::fs::read(&args[1])?;
    // accept mem size as argument and verify specified memory is larger than input program size
    buffer.resize(65535, 0);

    let mut cpu = CPU::new(true, buffer.into_boxed_slice());
    cpu.emulate();
    Ok(())
}
