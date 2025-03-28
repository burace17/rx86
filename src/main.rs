mod bits;
mod cpu;
mod instructions;
mod memory;
mod operations;
mod traits;

use cpu::Cpu;

use std::env;
use tracing_subscriber::FmtSubscriber;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("usage: {} <input_program>", args[0]);
        return Ok(());
    }

    let subscriber = FmtSubscriber::builder()
        // all spans/events with a level higher than TRACE (e.g, debug, info, warn, etc.)
        // will be written to stdout.
        .with_max_level(tracing::Level::TRACE)
        // completes the builder.
        .finish();

    tracing::subscriber::set_global_default(subscriber).expect("setting default subscriber failed");

    let mut buffer = std::fs::read(&args[1])?;
    // accept mem size as argument and verify specified memory is larger than input program size
    buffer.resize(65535, 0);

    let mut cpu = Cpu::new(buffer.into_boxed_slice());
    cpu.emulate();
    cpu.dump_video_ram();
    Ok(())
}
