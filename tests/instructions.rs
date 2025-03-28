use rx86::cpu::Cpu;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use test_log::test;
use tracing::debug_span;

#[derive(Serialize, Deserialize, Debug)]
struct CpuRegisters {
    ax: Option<u16>,
    bx: Option<u16>,
    cx: Option<u16>,
    dx: Option<u16>,
    si: Option<u16>,
    di: Option<u16>,
    bp: Option<u16>,
    sp: Option<u16>,
    ip: Option<u16>,
    cs: Option<u16>,
    ds: Option<u16>,
    ss: Option<u16>,
    es: Option<u16>,
    flags: Option<u16>,
}

#[derive(Serialize, Deserialize, Debug)]
struct CpuState {
    regs: CpuRegisters,
    ram: Vec<Vec<usize>>,
    queue: Vec<u8>,
}

#[derive(Serialize, Deserialize, Debug)]
struct InstructionTestCase {
    name: String,
    bytes: Vec<u8>,
    initial: CpuState,
    #[serde(alias = "final")]
    final_state: CpuState,
    #[serde(skip)]
    #[allow(unused)]
    cycles: Vec<Value>,
    hash: String,
    idx: u64,
}

fn set_cpu_state_from_test_case(cpu: &mut Cpu, test_case: &InstructionTestCase) {
    // test cases always define every register for the initial state
    cpu.ax = test_case.initial.regs.ax.unwrap();
    cpu.bx = test_case.initial.regs.bx.unwrap();
    cpu.cx = test_case.initial.regs.cx.unwrap();
    cpu.dx = test_case.initial.regs.dx.unwrap();
    cpu.si = test_case.initial.regs.si.unwrap();
    cpu.di = test_case.initial.regs.di.unwrap();
    cpu.bp = test_case.initial.regs.bp.unwrap();
    cpu.sp = test_case.initial.regs.sp.unwrap();
    cpu.ip = test_case.initial.regs.ip.unwrap();
    cpu.cs = test_case.initial.regs.cs.unwrap();
    cpu.ds = test_case.initial.regs.ds.unwrap();
    cpu.ss = test_case.initial.regs.ss.unwrap();
    cpu.es = test_case.initial.regs.es.unwrap();
    cpu.flag = rx86::cpu::CpuFlags::from_bits_retain(test_case.initial.regs.flags.unwrap());

    for pair in test_case.initial.ram.iter() {
        cpu.mem[pair[0]] = pair[1] as u8;
    }
}

fn check_cpu_state_after_test(cpu: &Cpu, test_case: &InstructionTestCase) {
    if let Some(ax) = test_case.final_state.regs.ax {
        assert_eq!(cpu.ax, ax, "unexpected ax value after: {}", test_case.name);
    }
    if let Some(bx) = test_case.final_state.regs.bx {
        assert_eq!(cpu.bx, bx, "unexpected bx value after: {}", test_case.name);
    }
    if let Some(cx) = test_case.final_state.regs.cx {
        assert_eq!(cpu.cx, cx, "unexpected cx value after: {}", test_case.name);
    }
    if let Some(dx) = test_case.final_state.regs.dx {
        assert_eq!(cpu.dx, dx, "unexpected dx value after: {}", test_case.name);
    }
    if let Some(si) = test_case.final_state.regs.si {
        assert_eq!(cpu.si, si, "unexpected si value after: {}", test_case.name);
    }
    if let Some(di) = test_case.final_state.regs.di {
        assert_eq!(cpu.di, di, "unexpected di value after: {}", test_case.name);
    }
    if let Some(bp) = test_case.final_state.regs.bp {
        assert_eq!(cpu.bp, bp, "unexpected bp value after: {}", test_case.name);
    }
    if let Some(sp) = test_case.final_state.regs.sp {
        assert_eq!(cpu.sp, sp, "unexpected sp value after: {}", test_case.name);
    }
    if let Some(ip) = test_case.final_state.regs.ip {
        assert_eq!(cpu.ip, ip, "unexpected ip value after: {}", test_case.name);
    }
    if let Some(cs) = test_case.final_state.regs.cs {
        assert_eq!(cpu.cs, cs, "unexpected cs value after: {}", test_case.name);
    }
    if let Some(ds) = test_case.final_state.regs.ds {
        assert_eq!(cpu.ds, ds, "unexpected ds value after: {}", test_case.name);
    }
    if let Some(ss) = test_case.final_state.regs.ss {
        assert_eq!(cpu.ss, ss, "unexpected ss value after: {}", test_case.name);
    }
    if let Some(es) = test_case.final_state.regs.es {
        assert_eq!(cpu.es, es, "unexpected es value after: {}", test_case.name);
    }
    for pair in test_case.final_state.ram.iter() {
        assert_eq!(
            cpu.mem[pair[0]], pair[1] as u8,
            "unexpected ram value at {} after: {}",
            pair[0], test_case.name
        );
    }
    if let Some(flags) = test_case.final_state.regs.flags {
        let test_case_flags = rx86::cpu::CpuFlags::from_bits_retain(flags);
        assert_eq!(
            cpu.flag, test_case_flags,
            "unexpected flags value after: {}",
            test_case.name
        );
    }
}

const TEST_MEM_SIZE: usize = 11000000;

fn run_instruction_test_case(path: &str) {
    let test_json = std::fs::read_to_string(path).unwrap();
    let test_cases: Vec<InstructionTestCase> = serde_json::from_str(&test_json).unwrap();
    for test_case in test_cases.iter() {
        let _span_ = debug_span!("", test_index = test_case.idx).entered();
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        set_cpu_state_from_test_case(&mut cpu, &test_case);
        cpu.do_cycle();
        check_cpu_state_after_test(&cpu, &test_case);
    }
}

#[test]
fn test_00() {
    run_instruction_test_case("tests/instructions_cases/00.json");
}

#[test]
fn test_01() {
    run_instruction_test_case("tests/instructions_cases/01.json");
}

#[test]
fn test_02() {
    run_instruction_test_case("tests/instructions_cases/02.json");
}

#[test]
fn test_03() {
    run_instruction_test_case("tests/instructions_cases/03.json");
}

#[test]
fn test_05() {
    run_instruction_test_case("tests/instructions_cases/05.json");
}

#[test]
fn test_06() {
    run_instruction_test_case("tests/instructions_cases/06.json");
}

#[test]
fn test_08() {
    run_instruction_test_case("tests/instructions_cases/08.json");
}