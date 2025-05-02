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

fn cpu_assert_matches_expected<T>(
    test: &InstructionTestCase,
    value_name: &str,
    current: T,
    expected: Option<T>,
) where
    T: std::fmt::Debug + PartialEq,
{
    if let Some(expected) = expected {
        assert_eq!(
            current, expected,
            "{} ({}): unexpected {} value",
            test.name, test.idx, value_name
        );
    }
}

fn check_cpu_state_after_test(cpu: &Cpu, test_case: &InstructionTestCase) {
    cpu_assert_matches_expected(&test_case, "ax", cpu.ax, test_case.final_state.regs.ax);
    cpu_assert_matches_expected(&test_case, "bx", cpu.bx, test_case.final_state.regs.bx);
    cpu_assert_matches_expected(&test_case, "cx", cpu.cx, test_case.final_state.regs.cx);
    cpu_assert_matches_expected(&test_case, "dx", cpu.dx, test_case.final_state.regs.dx);
    cpu_assert_matches_expected(&test_case, "si", cpu.si, test_case.final_state.regs.si);
    cpu_assert_matches_expected(&test_case, "di", cpu.di, test_case.final_state.regs.di);
    cpu_assert_matches_expected(&test_case, "bp", cpu.bp, test_case.final_state.regs.bp);
    cpu_assert_matches_expected(&test_case, "sp", cpu.sp, test_case.final_state.regs.sp);
    cpu_assert_matches_expected(&test_case, "ip", cpu.ip, test_case.final_state.regs.ip);
    cpu_assert_matches_expected(&test_case, "cs", cpu.cs, test_case.final_state.regs.cs);
    cpu_assert_matches_expected(&test_case, "ds", cpu.ds, test_case.final_state.regs.ds);
    cpu_assert_matches_expected(&test_case, "ss", cpu.ss, test_case.final_state.regs.ss);
    cpu_assert_matches_expected(&test_case, "es", cpu.es, test_case.final_state.regs.es);
    for pair in test_case.final_state.ram.iter() {
        assert_eq!(
            cpu.mem[pair[0]], pair[1] as u8,
            "{} ({}): unexpected ram value at {}",
            test_case.name, test_case.idx, pair[0]
        );
        cpu_assert_matches_expected(&test_case, "ram", cpu.mem[pair[0]], Some(pair[1] as u8));
    }
    if let Some(flags) = test_case.final_state.regs.flags {
        let test_case_flags = rx86::cpu::CpuFlags::from_bits_retain(flags);
        assert_eq!(
            cpu.flag, test_case_flags,
            "{} ({}): unexpected flags value",
            test_case.name, test_case.idx
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

macro_rules! instruction_test_case {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() {
            run_instruction_test_case($path);
        }
    };
}

instruction_test_case!(test_00, "tests/instructions_cases/00.json");
instruction_test_case!(test_01, "tests/instructions_cases/01.json");
instruction_test_case!(test_02, "tests/instructions_cases/02.json");
instruction_test_case!(test_03, "tests/instructions_cases/03.json");
instruction_test_case!(test_04, "tests/instructions_cases/04.json");
instruction_test_case!(test_05, "tests/instructions_cases/05.json");
instruction_test_case!(test_06, "tests/instructions_cases/06.json");
instruction_test_case!(test_07, "tests/instructions_cases/07.json");
instruction_test_case!(test_08, "tests/instructions_cases/08.json");
instruction_test_case!(test_09, "tests/instructions_cases/09.json");
instruction_test_case!(test_0a, "tests/instructions_cases/0A.json");
instruction_test_case!(test_0b, "tests/instructions_cases/0B.json");
instruction_test_case!(test_0c, "tests/instructions_cases/0C.json");
instruction_test_case!(test_0d, "tests/instructions_cases/0D.json");
instruction_test_case!(test_0e, "tests/instructions_cases/0E.json");
instruction_test_case!(test_10, "tests/instructions_cases/10.json");
instruction_test_case!(test_11, "tests/instructions_cases/11.json");
instruction_test_case!(test_12, "tests/instructions_cases/12.json");
instruction_test_case!(test_13, "tests/instructions_cases/13.json");
instruction_test_case!(test_14, "tests/instructions_cases/14.json");
instruction_test_case!(test_15, "tests/instructions_cases/15.json");
instruction_test_case!(test_16, "tests/instructions_cases/16.json");
instruction_test_case!(test_17, "tests/instructions_cases/17.json");
instruction_test_case!(test_18, "tests/instructions_cases/18.json");
instruction_test_case!(test_19, "tests/instructions_cases/19.json");
instruction_test_case!(test_1a, "tests/instructions_cases/1A.json");
instruction_test_case!(test_1b, "tests/instructions_cases/1B.json");
instruction_test_case!(test_1c, "tests/instructions_cases/1C.json");
instruction_test_case!(test_1d, "tests/instructions_cases/1D.json");
instruction_test_case!(test_1e, "tests/instructions_cases/1E.json");
instruction_test_case!(test_1f, "tests/instructions_cases/1F.json");
instruction_test_case!(test_20, "tests/instructions_cases/20.json");
instruction_test_case!(test_21, "tests/instructions_cases/21.json");
instruction_test_case!(test_22, "tests/instructions_cases/22.json");
instruction_test_case!(test_23, "tests/instructions_cases/23.json");
instruction_test_case!(test_24, "tests/instructions_cases/24.json");
instruction_test_case!(test_25, "tests/instructions_cases/25.json");
instruction_test_case!(test_28, "tests/instructions_cases/28.json");
instruction_test_case!(test_29, "tests/instructions_cases/29.json");
instruction_test_case!(test_2a, "tests/instructions_cases/2A.json");
instruction_test_case!(test_2b, "tests/instructions_cases/2B.json");
instruction_test_case!(test_2c, "tests/instructions_cases/2C.json");
instruction_test_case!(test_2d, "tests/instructions_cases/2D.json");
instruction_test_case!(test_30, "tests/instructions_cases/30.json");
instruction_test_case!(test_31, "tests/instructions_cases/31.json");
instruction_test_case!(test_32, "tests/instructions_cases/32.json");
instruction_test_case!(test_33, "tests/instructions_cases/33.json");
instruction_test_case!(test_34, "tests/instructions_cases/34.json");
instruction_test_case!(test_35, "tests/instructions_cases/35.json");