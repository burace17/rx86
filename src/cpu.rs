use crate::bits::Bits;
use crate::instructions::{
    ModRmByte, RegisterOrMemory, dec_byte, dec_reg, inc_byte, inc_reg, is_addressing_mode, jmp_if,
    jmp_if_any_set, jmp_if_none_set, parse_mod_rm_byte, swap_reg,
};
use crate::memory::{read_word, write_word};
use crate::operations;
use crate::operations::swap_args;
use crate::traits::NumericOps;
use bitflags::bitflags;
use std::fmt::UpperHex;
use std::io;
use tracing::{debug, debug_span};

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Default)]
    pub struct CpuFlags: u16 {
        const CARRY = 1 << 0;
        const PARITY = 1 << 2;
        const AUX_CARRY = 1 << 4;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
        const TRAP = 1 << 8;
        const INTERRUPT = 1 << 9;
        const DIRECTION = 1 << 10;
        const OVERFLOW = 1 << 11;
    }
}

#[derive(Default, Debug)]
pub struct Cpu {
    // Registers
    pub ax: u16,
    pub bx: u16,
    pub cx: u16,
    pub dx: u16,
    pub si: u16,
    pub di: u16,
    pub bp: u16,
    pub sp: u16,
    pub ip: u16,

    seg_override: Option<SegmentOverride>,
    pub cs: u16,
    pub ds: u16,
    pub ss: u16,
    pub es: u16,

    pub flag: CpuFlags,
    pub mem: Box<[u8]>,

    pub breakpoints: Vec<u16>,
}

#[derive(Copy, Clone, Debug)]
enum SegmentOverride {
    Code,
    Data,
    Stack,
    Extra,
}

#[derive(Copy, Clone)]
#[allow(unused)]
enum CpuMemoryAccessType {
    InstructionFetch,
    StackOperation,
    Variable,
    StringSource,
    StringDestination,
    BpBaseRegister,
}

#[derive(Copy, Clone)]
enum CpuRegister {
    Ax,
    Bx,
    Cx,
    Dx,
    Si,
    Di,
    Bp,
    Sp,
    Ip,

    Cs,
    Ds,
    Ss,
    Es,
}

enum CpuBreakReason {
    Breakpoint,
    #[allow(unused)]
    Panic(String),
}

enum CpuBreakResult {
    Continue,
    StepOver,
    Abort,
}

impl Cpu {
    pub fn new(mem: Box<[u8]>) -> Cpu {
        Cpu {
            mem,
            ..Default::default()
        }
    }

    #[allow(unused)]
    pub fn new_with_mem_size(mem_size: usize) -> Cpu {
        Cpu::new(vec![0; mem_size].into_boxed_slice())
    }

    pub fn emulate(&mut self) {
        let mut break_on_next = false;
        loop {
            if break_on_next || self.breakpoints.contains(&self.ip) {
                match self.debug_break(CpuBreakReason::Breakpoint) {
                    CpuBreakResult::Continue => break_on_next = false,
                    CpuBreakResult::StepOver => break_on_next = true,
                    CpuBreakResult::Abort => break,
                }
            }
            let should_continue_emulation = self.do_cycle();
            if !should_continue_emulation {
                break;
            }
        }
    }

    pub fn dump_registers(&self) {
        println!(
            "ax: 0x{:X}    bx: 0x{:X}    cx: 0x{:X}    dx: 0x{:X}
si: 0x{:X}    di: 0x{:X}    bp: 0x{:X}    sp: 0x{:X}
ip: 0x{:X}

cs: 0x{:X}    ds: 0x{:X}    ss: 0x{:X}    es: 0x{:X}

cf: {:?}     zf: {:?}
sf: {:?}",
            self.ax,
            self.bx,
            self.cx,
            self.dx,
            self.si,
            self.di,
            self.bp,
            self.sp,
            self.ip,
            self.cs,
            self.ds,
            self.ss,
            self.es,
            self.flag.contains(CpuFlags::CARRY),
            self.flag.contains(CpuFlags::ZERO),
            self.flag.contains(CpuFlags::SIGN),
        );
    }

    pub fn dump_video_ram(&self) {
        let mut j = 0;
        for i in 0x8000..0x87D0 {
            let val = self.mem[i as usize];
            if val == 0 {
                print!(" ");
            } else {
                let c = char::from(val);
                print!("{}", c);
            }

            j += 1;
            if j == 80 {
                println!();
                j = 0;
            }
        }
    }

    fn debug_extract_segment_and_offset(input: &str) -> Option<(usize, usize)> {
        // FIXME: can handle errors better here
        let parts: Vec<&str> = input.split(":").collect();
        if parts.len() == 2 {
            // FIXME: should also support just specifying the name of a segment register rather than only an offset
            let segment = usize::from_str_radix(parts[0], 16).unwrap();
            let offset = usize::from_str_radix(parts[1], 16).unwrap();
            Some((segment, offset))
        } else {
            None
        }
    }

    fn debug_read_memory_command(&self, input: &str, read_word: bool) {
        // FIXME: can handle errors better here
        let parts: Vec<&str> = input.split_whitespace().collect();
        if parts.len() == 2 {
            if let Some((segment, offset)) = Self::debug_extract_segment_and_offset(parts[1]) {
                let ea = (segment + offset) & 0xFFFFF; //self.compute_effective_address(segment, offset);
                if read_word {
                    println!("0x{:X}", crate::cpu::read_word(&self.mem, ea));
                } else {
                    println!("0x{:X}", self.mem[ea]);
                }
            }
        } else {
            println!("Invalid read command. Usage: read_<byte/word> <address>");
        }
    }

    fn debug_break(&self, reason: CpuBreakReason) -> CpuBreakResult {
        let opcode = self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip);
        match reason {
            CpuBreakReason::Breakpoint => {
                println!(
                    "Breakpoint hit at 0x{:X} while processing opcode 0x{:X}",
                    self.ip, opcode
                );
            }
            CpuBreakReason::Panic(msg) => {
                println!(
                    "Cpu panicked at 0x{:X} while processing opcode 0x{:X}: {}",
                    self.ip, opcode, msg
                );
            }
        }
        self.dump_registers();
        self.dump_video_ram();
        println!();
        let result;
        loop {
            let mut input = String::new();
            if io::stdin().read_line(&mut input).is_ok() {
                match input.trim() {
                    "g" => {
                        result = CpuBreakResult::Continue;
                        break;
                    }
                    "n" => {
                        result = CpuBreakResult::StepOver;
                        break;
                    }
                    "q" => {
                        result = CpuBreakResult::Abort;
                        break;
                    }
                    _ if input.contains("read_word") => {
                        self.debug_read_memory_command(&input, true)
                    }
                    _ if input.contains("read_byte") => {
                        self.debug_read_memory_command(&input, false)
                    }
                    _ => println!("Unknown debug command. Type 'g' to continue"),
                }
            } else {
                println!("Couldn't read from stdin");
                result = CpuBreakResult::Abort;
                break;
            }
        }
        result
    }

    fn cpu_panic(&self, msg: &str) -> ! {
        #[cfg(not(test))]
        self.debug_break(CpuBreakReason::Panic(msg.to_owned()));
        panic!("{}", msg);
    }

    fn uses_current_segment_override(&self, access_type: CpuMemoryAccessType) -> bool {
        match self.seg_override {
            Some(seg_override) => match seg_override {
                SegmentOverride::Code => {
                    matches!(
                        access_type,
                        CpuMemoryAccessType::InstructionFetch
                            | CpuMemoryAccessType::Variable
                            | CpuMemoryAccessType::BpBaseRegister
                    )
                }
                SegmentOverride::Data => {
                    matches!(
                        access_type,
                        CpuMemoryAccessType::Variable
                            | CpuMemoryAccessType::StringSource
                            | CpuMemoryAccessType::BpBaseRegister
                    )
                }
                SegmentOverride::Extra => matches!(
                    access_type,
                    CpuMemoryAccessType::Variable
                        | CpuMemoryAccessType::StringSource
                        | CpuMemoryAccessType::BpBaseRegister
                ),
                SegmentOverride::Stack => {
                    matches!(
                        access_type,
                        CpuMemoryAccessType::StackOperation
                            | CpuMemoryAccessType::BpBaseRegister
                            | CpuMemoryAccessType::Variable
                    )
                }
            },
            None => false,
        }
    }

    fn get_active_segment_base(&self, access_type: CpuMemoryAccessType) -> usize {
        (match self.seg_override {
            Some(seg_override) if self.uses_current_segment_override(access_type) => {
                match seg_override {
                    SegmentOverride::Code => self.cs,
                    SegmentOverride::Data => self.ds,
                    SegmentOverride::Extra => self.es,
                    SegmentOverride::Stack => self.ss,
                }
            }
            Some(_) | None => match access_type {
                CpuMemoryAccessType::InstructionFetch => self.cs,
                CpuMemoryAccessType::StackOperation => self.ss,
                CpuMemoryAccessType::Variable => self.ds,
                CpuMemoryAccessType::StringSource => self.ds,
                CpuMemoryAccessType::StringDestination => self.es,
                CpuMemoryAccessType::BpBaseRegister => self.ss,
            },
        }) as usize
            * 0x10
    }

    fn compute_effective_address(&self, access_type: CpuMemoryAccessType, offset: u16) -> usize {
        let offset = offset as usize;
        let target_segment = self.get_active_segment_base(access_type);
        (target_segment + offset) & 0xFFFFF // limited to 20 bits on 8086 only?
    }

    fn read_mem_byte(&self, access_type: CpuMemoryAccessType, segment_offset: u16) -> u8 {
        let effective_address = self.compute_effective_address(access_type, segment_offset);
        self.mem[effective_address]
    }

    fn read_mem_word(&self, access_type: CpuMemoryAccessType, segment_offset: u16) -> u16 {
        let mut bytes: [u8; 2] = [0; 2];
        let effective_address = self.compute_effective_address(access_type, segment_offset);
        bytes[0] = self.mem[effective_address];

        let effective_address = self.compute_effective_address(access_type, segment_offset.wrapping_add(1));
        bytes[1] = self.mem[effective_address];

        u16::from_le_bytes(bytes)
    }

    /*     fn write_mem_byte(&mut self, access_type: CpuMemoryAccessType, segment_offset: u16, value: u8) {
        let effective_address = self.compute_effective_address(access_type, segment_offset);
        self.mem[effective_address] = value;
    } */

    fn write_mem_word(
        &mut self,
        access_type: CpuMemoryAccessType,
        segment_offset: u16,
        value: u16,
    ) {
        let bytes = value.to_le_bytes();

        let effective_address = self.compute_effective_address(access_type, segment_offset);
        self.mem[effective_address] = bytes[0];

        let effective_address =
            self.compute_effective_address(access_type, segment_offset.wrapping_add(1));
        self.mem[effective_address] = bytes[1];
    }

    fn get_register_value(&self, target_register: CpuRegister) -> u16 {
        match target_register {
            CpuRegister::Ax => self.ax,
            CpuRegister::Bx => self.bx,
            CpuRegister::Cx => self.cx,
            CpuRegister::Dx => self.dx,
            CpuRegister::Si => self.si,
            CpuRegister::Di => self.di,
            CpuRegister::Bp => self.bp,
            CpuRegister::Sp => self.sp,
            CpuRegister::Ip => self.ip,

            CpuRegister::Cs => self.cs,
            CpuRegister::Ds => self.ds,
            CpuRegister::Ss => self.ss,
            CpuRegister::Es => self.es,
        }
    }

    fn get_register_byte_value(&self, target_register: CpuRegister, high: bool) -> u8 {
        match target_register {
            CpuRegister::Ax if high => self.ax.get_high(),
            CpuRegister::Ax => self.ax.get_low(),
            CpuRegister::Bx if high => self.bx.get_high(),
            CpuRegister::Bx => self.bx.get_low(),
            CpuRegister::Cx if high => self.cx.get_high(),
            CpuRegister::Cx => self.cx.get_low(),
            CpuRegister::Dx if high => self.dx.get_high(),
            CpuRegister::Dx => self.dx.get_low(),
            _ => unreachable!(),
        }
    }

    fn set_register_value(&mut self, target_register: CpuRegister, value: u16) {
        match target_register {
            CpuRegister::Ax => self.ax = value,
            CpuRegister::Bx => self.bx = value,
            CpuRegister::Cx => self.cx = value,
            CpuRegister::Dx => self.dx = value,
            CpuRegister::Si => self.si = value,
            CpuRegister::Di => self.di = value,
            CpuRegister::Bp => self.bp = value,
            CpuRegister::Sp => self.sp = value,
            CpuRegister::Ip => self.ip = value,

            CpuRegister::Cs => self.cs = value,
            CpuRegister::Ds => self.ds = value,
            CpuRegister::Ss => self.ss = value,
            CpuRegister::Es => self.es = value,
        }
    }

    fn set_register_byte_value(&mut self, target_register: CpuRegister, high: bool, value: u8) {
        match target_register {
            CpuRegister::Ax if high => self.ax.set_high(value),
            CpuRegister::Ax => self.ax.set_low(value),
            CpuRegister::Bx if high => self.bx.set_high(value),
            CpuRegister::Bx => self.bx.set_low(value),
            CpuRegister::Cx if high => self.cx.set_high(value),
            CpuRegister::Cx => self.cx.set_low(value),
            CpuRegister::Dx if high => self.dx.set_high(value),
            CpuRegister::Dx => self.dx.set_low(value),
            _ => unreachable!(),
        }
    }

    fn get_register_value_by_modrm_reg_code(&self, reg_code: u8) -> u16 {
        match reg_code {
            0b000 => self.ax,
            0b001 => self.cx,
            0b010 => self.dx,
            0b011 => self.bx,
            0b100 => self.sp,
            0b101 => self.bp,
            0b110 => self.si,
            0b111 => self.di,
            _ => self.cpu_panic("get_reg_word_code: failed to parse REG bits of mod R/M"),
        }
    }

    fn set_register_value_by_modrm_reg_code(&mut self, reg_code: u8, value: u16) {
        match reg_code {
            0b000 => self.ax = value,
            0b001 => self.cx = value,
            0b010 => self.dx = value,
            0b011 => self.bx = value,
            0b100 => self.sp = value,
            0b101 => self.bp = value,
            0b110 => self.si = value,
            0b111 => self.di = value,
            _ => self.cpu_panic("set_reg_word_code: failed to parse REG bits of mod R/M"),
        }
    }

    fn get_register_byte_value_by_modrm_reg_code(&self, reg_code: u8) -> u8 {
        match reg_code {
            0b000 => self.ax.get_low(),
            0b001 => self.cx.get_low(),
            0b010 => self.dx.get_low(),
            0b011 => self.bx.get_low(),
            0b100 => self.ax.get_high(),
            0b101 => self.cx.get_high(),
            0b110 => self.dx.get_high(),
            0b111 => self.bx.get_high(),
            _ => self.cpu_panic("set_reg_byte_code: failed to parse REG bits of mod R/M"),
        }
    }

    fn set_register_byte_value_by_modrm_reg_code(&mut self, reg_code: u8, value: u8) {
        match reg_code {
            0b000 => self.ax.set_low(value),
            0b001 => self.cx.set_low(value),
            0b010 => self.dx.set_low(value),
            0b011 => self.bx.set_low(value),
            0b100 => self.ax.set_high(value),
            0b101 => self.cx.set_high(value),
            0b110 => self.dx.set_high(value),
            0b111 => self.bx.set_high(value),
            _ => self.cpu_panic("set_reg_byte_code: failed to parse REG bits of mod R/M"),
        }
    }

    fn get_segment_register_value_by_modrm_reg_code(&self, reg_code: u8) -> u16 {
        match reg_code {
            0b000 => self.es,
            0b001 => self.cs,
            0b010 => self.ss,
            0b011 => self.ds,
            _ => self.cpu_panic("set_reg_byte_code: failed to parse REG bits of mod R/M"),
        }
    }

    fn set_segment_register_value_by_modrm_reg_code(&mut self, reg_code: u8, value: u16) {
        match reg_code {
            0b000 => self.es = value,
            0b001 => println!("warning: ignoring write to CS"),
            0b010 => self.ss = value,
            0b011 => self.ds = value,
            _ => self.cpu_panic("set_reg_byte_code: failed to parse REG bits of mod R/M"),
        }
    }

    // From a the mod and rm parts of the modrm byte, return the memory address and instruction length so far
    fn read_address_from_modrm(&self, id_mod: u8, id_rm: u8) -> (usize, u16) {
        // Operand is a memory address.
        let instruction_length = match id_mod {
            0x00 if id_rm == 0x06 => 4, // bp uses 16 bit displacement
            0x01 => 3,
            0x02 => 4,
            _ => 2,
        };

        let displacement = match id_mod {
            0x01 => {
                (self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 2) as i8)
                    as i16
            }
            0x02 => self.read_mem_word(CpuMemoryAccessType::InstructionFetch, self.ip + 2) as i16,
            _ => 0,
        };

        let address: usize = match id_rm {
            0x00 => self.compute_effective_address(
                CpuMemoryAccessType::Variable,
                self.bx
                    .wrapping_add(self.si)
                    .wrapping_add_signed(displacement),
            ),
            0x01 => self.compute_effective_address(
                CpuMemoryAccessType::Variable,
                self.bx
                    .wrapping_add(self.di)
                    .wrapping_add_signed(displacement),
            ),
            0x02 => self.compute_effective_address(
                CpuMemoryAccessType::BpBaseRegister,
                self.bp
                    .wrapping_add(self.si)
                    .wrapping_add_signed(displacement),
            ),
            0x03 => self.compute_effective_address(
                CpuMemoryAccessType::BpBaseRegister,
                self.bp
                    .wrapping_add(self.di)
                    .wrapping_add_signed(displacement),
            ),
            0x04 => self.compute_effective_address(
                CpuMemoryAccessType::Variable,
                self.si.wrapping_add_signed(displacement),
            ),
            0x05 => self.compute_effective_address(
                CpuMemoryAccessType::Variable,
                self.di.wrapping_add_signed(displacement),
            ),
            0x06 if id_mod == 0 => self.compute_effective_address(
                CpuMemoryAccessType::Variable,
                self.read_mem_word(CpuMemoryAccessType::InstructionFetch, self.ip + 2),
            ),
            0x06 => self.compute_effective_address(
                CpuMemoryAccessType::BpBaseRegister,
                self.bp.wrapping_add_signed(displacement),
            ),
            0x07 => self.compute_effective_address(
                CpuMemoryAccessType::Variable,
                self.bx.wrapping_add_signed(displacement),
            ),
            _ => self.cpu_panic("read_address_from_modrm: failed to parse R/M bits of mod R/M"),
        };
        debug!(address, instruction_length, displacement);

        (address, instruction_length)
    }

    fn do_grp2_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u8, &mut u8, &mut CpuFlags),
    {
        self.do_modrm_byte_inst(op, |_, modrm_byte, _| modrm_byte.id_reg, |_, _, _| {}, 0, 2)
    }

    fn do_byte_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u8, &mut u8, &mut CpuFlags),
    {
        self.do_modrm_byte_inst(
            op,
            |cpu, modrm_byte, _| cpu.get_register_byte_value_by_modrm_reg_code(modrm_byte.id_reg),
            Self::set_register_byte_value_by_modrm_reg_code,
            0,
            2,
        )
    }

    fn do_modrm_byte_inst<F, RegGetter, RegSetter>(
        &mut self,
        op: F,
        reg_getter: RegGetter,
        reg_setter: RegSetter,
        ip_increment_if_address: u16,
        ip_increment_if_register: u16,
    ) -> u16
    where
        F: Fn(&mut u8, &mut u8, &mut CpuFlags),
        RegGetter: Fn(&Self, ModRmByte, u16) -> u8,
        RegSetter: Fn(&mut Self, u8, u8),
    {
        self.do_modrm_inst(
            op,
            |cpu, reg_or_mem| match reg_or_mem {
                RegisterOrMemory::Memory(address) => cpu.mem[address],
                RegisterOrMemory::Register(reg_code) => {
                    cpu.get_register_byte_value_by_modrm_reg_code(reg_code)
                }
            },
            |cpu, reg_or_mem, value| match reg_or_mem {
                RegisterOrMemory::Memory(address) => cpu.mem[address] = value,
                RegisterOrMemory::Register(reg_code) => {
                    cpu.set_register_byte_value_by_modrm_reg_code(reg_code, value)
                }
            },
            reg_getter,
            reg_setter,
            ip_increment_if_address,
            ip_increment_if_register,
        )
    }

    fn get_opext_group1_inst<T>(
        &self,
        id_reg: u8,
    ) -> impl Fn(&mut T, &mut T, &mut CpuFlags) + use<T>
    where
        T: NumericOps,
    {
        match id_reg {
            0x00 => operations::add,
            0x01 => operations::bitwise_or,
            0x02 => operations::add_with_carry,
            0x03 => operations::sub_with_borrow,
            0x04 => operations::bitwise_and,
            0x05 => operations::sub,
            0x06 => operations::bitwise_xor,
            0x07 => operations::cmp,
            reg => self.cpu_panic(&format!("Opext instruction: failed to parse REG: {}", reg)),
        }
    }

    fn do_opext_inst(&mut self, word_inst: bool, imm_byte: bool) -> u16 {
        let modrm_byte = self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 1);
        let (id_mod, id_reg, id_rm) = parse_mod_rm_byte(modrm_byte).unpack();

        if is_addressing_mode(id_mod) {
            // R/M is memory.
            let (address, ip_increment) = self.read_address_from_modrm(id_mod, id_rm);
            //println!("ip increment: {:X}", ip_increment);
            if word_inst && !imm_byte {
                let mut imm =
                    self.read_mem_word(CpuMemoryAccessType::InstructionFetch, self.ip + 4);
                let mut rm = read_word(&self.mem, address);
                let word_op = self.get_opext_group1_inst(id_reg);
                word_op(&mut rm, &mut imm, &mut self.flag);
                write_word(&mut self.mem, address, rm);
                ip_increment + 2
            } else if word_inst && imm_byte {
                let imm_byte =
                    self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 4) as i8;
                let imm = imm_byte as i16;
                let mut imm = imm as u16;
                let mut rm = read_word(&self.mem, address);
                let word_op = self.get_opext_group1_inst(id_reg);
                word_op(&mut rm, &mut imm, &mut self.flag);
                write_word(&mut self.mem, address, rm);
                ip_increment + 1
                // TODO check ip increment for this case..
            } else {
                let mut imm =
                    self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 4);
                let mut rm = self.mem[address];
                let byte_op = self.get_opext_group1_inst(id_reg);
                byte_op(&mut rm, &mut imm, &mut self.flag);
                self.mem[address] = rm;
                ip_increment + 1
            }
        } else {
            // R/M is a register

            if word_inst && !imm_byte {
                let mut imm =
                    self.read_mem_word(CpuMemoryAccessType::InstructionFetch, self.ip + 2);
                let mut rm = self.get_register_value_by_modrm_reg_code(id_rm);
                let word_op = self.get_opext_group1_inst(id_reg);
                word_op(&mut rm, &mut imm, &mut self.flag);
                self.set_register_value_by_modrm_reg_code(id_rm, rm);
                4
            } else if word_inst && imm_byte {
                // NOTE byte is sign extended!
                let imm_byte =
                    self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 2) as i8;
                let imm = imm_byte as i16; // sign extend
                let mut imm = imm as u16;
                let mut rm = self.get_register_value_by_modrm_reg_code(id_rm);
                let word_op = self.get_opext_group1_inst(id_reg);
                word_op(&mut rm, &mut imm, &mut self.flag);
                self.set_register_value_by_modrm_reg_code(id_rm, rm);
                3
            } else {
                let mut imm =
                    self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 2);
                let mut rm = self.get_register_byte_value_by_modrm_reg_code(id_rm);
                let byte_op = self.get_opext_group1_inst(id_reg);
                byte_op(&mut rm, &mut imm, &mut self.flag);
                self.set_register_byte_value_by_modrm_reg_code(id_rm, rm);
                3
            }
        }
    }

    fn do_word_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u16, &mut u16, &mut CpuFlags),
    {
        self.do_modrm_word_inst(
            op,
            |cpu, modrm_byte, _| cpu.get_register_value_by_modrm_reg_code(modrm_byte.id_reg),
            Self::set_register_value_by_modrm_reg_code,
            0,
            2,
        )
    }

    fn do_segment_reg_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u16, &mut u16, &mut CpuFlags),
    {
        self.do_modrm_word_inst(
            op,
            |cpu, modrm_byte, _| {
                cpu.get_segment_register_value_by_modrm_reg_code(modrm_byte.id_reg)
            },
            Self::set_segment_register_value_by_modrm_reg_code,
            0,
            2,
        )
    }

    fn do_modrm_word_inst<F, RegGetter, RegSetter>(
        &mut self,
        op: F,
        reg_getter: RegGetter,
        reg_setter: RegSetter,
        ip_increment_if_address: u16,
        ip_increment_if_register: u16,
    ) -> u16
    where
        F: Fn(&mut u16, &mut u16, &mut CpuFlags),
        RegGetter: Fn(&Self, ModRmByte, u16) -> u16,
        RegSetter: Fn(&mut Self, u8, u16),
    {
        self.do_modrm_inst(
            op,
            |cpu, reg_or_mem| match reg_or_mem {
                RegisterOrMemory::Memory(address) => read_word(&cpu.mem, address),
                RegisterOrMemory::Register(reg_code) => {
                    cpu.get_register_value_by_modrm_reg_code(reg_code)
                }
            },
            |cpu, reg_or_mem, value| match reg_or_mem {
                RegisterOrMemory::Memory(address) => write_word(&mut cpu.mem, address, value),
                RegisterOrMemory::Register(reg_code) => {
                    cpu.set_register_value_by_modrm_reg_code(reg_code, value)
                }
            },
            reg_getter,
            reg_setter,
            ip_increment_if_address,
            ip_increment_if_register,
        )
    }

    fn do_modrm_inst<F, InstructionDataType, RmGetter, RmSetter, RegGetter, RegSetter>(
        &mut self,
        op: F,
        rm_getter: RmGetter,
        rm_setter: RmSetter,
        reg_getter: RegGetter,
        reg_setter: RegSetter,
        ip_increment_if_address: u16,
        ip_increment_if_register: u16,
    ) -> u16
    where
        F: Fn(&mut InstructionDataType, &mut InstructionDataType, &mut CpuFlags),
        RmGetter: Fn(&Self, RegisterOrMemory) -> InstructionDataType,
        RmSetter: Fn(&mut Self, RegisterOrMemory, InstructionDataType),
        RegGetter: Fn(&Self, ModRmByte, u16) -> InstructionDataType,
        RegSetter: Fn(&mut Self, u8, InstructionDataType),
        InstructionDataType: UpperHex + PartialEq + Copy,
    {
        let modrm_byte = self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 1);
        let modrm_byte = parse_mod_rm_byte(modrm_byte);
        let (id_mod, id_reg, id_rm) = modrm_byte.unpack();
        debug!(
            "mod: 0x{:X}, reg: 0x{:X}, rm: 0x{:X}",
            id_mod, id_reg, id_rm
        );

        if is_addressing_mode(id_mod) {
            let (address, ip_increment) = self.read_address_from_modrm(id_mod, id_rm);
            let mut rm_value = rm_getter(self, RegisterOrMemory::Memory(address));
            let mut reg = reg_getter(self, modrm_byte, ip_increment);

            op(&mut rm_value, &mut reg, &mut self.flag);

            rm_setter(self, RegisterOrMemory::Memory(address), rm_value);
            reg_setter(self, id_reg, reg);

            ip_increment + ip_increment_if_address
        } else {
            // R/M is a register
            let mut reg = reg_getter(self, modrm_byte, 0);
            let mut rm = rm_getter(self, RegisterOrMemory::Register(id_rm));

            let old_reg = reg;
            let old_rm = rm;

            op(&mut rm, &mut reg, &mut self.flag);

            if id_rm != id_reg || old_reg != reg {
                reg_setter(self, id_reg, reg);
            }
            if id_rm != id_reg || old_rm != rm {
                rm_setter(self, RegisterOrMemory::Register(id_rm), rm);
            }

            ip_increment_if_register
        }
    }

    fn do_rm_imm_word_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u16, &mut u16, &mut CpuFlags),
    {
        let get_reg_func = |s: &Cpu, modrm_byte: ModRmByte, ip_increment| {
            if is_addressing_mode(modrm_byte.id_mod) {
                s.read_mem_word(CpuMemoryAccessType::InstructionFetch, s.ip + ip_increment)
            } else {
                s.read_mem_word(CpuMemoryAccessType::InstructionFetch, s.ip + 2)
            }
        };
        // no setter required, only r/m is being modified
        self.do_modrm_word_inst(op, get_reg_func, |_s, _id_reg, _new_val| (), 2, 4)
    }

    fn do_ax_byte_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u8, &mut u8, &mut CpuFlags),
    {
        self.do_reg_imm_byte_inst(CpuRegister::Ax, false, op)
    }

    fn do_ax_word_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u16, &mut u16, &mut CpuFlags),
    {
        self.do_reg_imm_word_inst(CpuRegister::Ax, op)
    }

    fn do_reg_imm_byte_inst<F>(&mut self, target_register: CpuRegister, high: bool, op: F) -> u16
    where
        F: Fn(&mut u8, &mut u8, &mut CpuFlags),
    {
        let mut imm = self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 1);
        let mut val = self.get_register_byte_value(target_register, high);
        op(&mut val, &mut imm, &mut self.flag);
        self.set_register_byte_value(target_register, high, val);
        2
    }

    fn do_reg_imm_word_inst<F>(&mut self, target_register: CpuRegister, op: F) -> u16
    where
        F: Fn(&mut u16, &mut u16, &mut CpuFlags),
    {
        let mut imm = self.read_mem_word(CpuMemoryAccessType::InstructionFetch, self.ip + 1);
        let mut val = self.get_register_value(target_register);
        op(&mut val, &mut imm, &mut self.flag);
        self.set_register_value(target_register, val);
        3
    }

    fn do_cycle_with_segment_override(&mut self, seg_override: SegmentOverride) -> u16 {
        if self.seg_override.is_some() {
            self.cpu_panic("do_cycle_with_segment_override: seg_override already set!");
        }
        self.seg_override = Some(seg_override);
        self.ip += 1;
        self.do_cycle();
        self.seg_override = None;
        0
    }

    fn push(&mut self, register: CpuRegister) -> u16 {
        // TODO: 286 handles this differently..
        self.sp = self.sp.wrapping_sub(2);
        let value = self.get_register_value(register);
        self.write_mem_word(CpuMemoryAccessType::StackOperation, self.sp, value);
        1
    }

    fn push_val(&mut self, value: u16) -> u16 {
        // TODO: 286 handles this differently..
        self.sp = self.sp.wrapping_sub(2);
        self.write_mem_word(CpuMemoryAccessType::StackOperation, self.sp, value);
        1
    }

    fn pop(&mut self, register: CpuRegister) -> u16 {
        self.set_register_value(
            register,
            self.read_mem_word(CpuMemoryAccessType::StackOperation, self.sp),
        );
        self.sp = self.sp.wrapping_add(2);
        1
    }

    pub fn do_cycle(&mut self) -> bool {
        let opcode = self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip);
        let _span_ = debug_span!("do_cycle", "0x{:X}", opcode).entered();

        let mut should_continue_emulation = true;
        let ip_increment = match opcode {
            0x00 => self.do_byte_inst(operations::add),
            0x01 => self.do_word_inst(operations::add),
            0x02 => self.do_byte_inst(swap_args(operations::add)),
            0x03 => self.do_word_inst(swap_args(operations::add)),
            0x04 => self.do_ax_byte_inst(operations::add),
            0x05 => self.do_ax_word_inst(operations::add),
            0x06 => self.push(CpuRegister::Es),
            0x07 => self.pop(CpuRegister::Es),
            0x08 => self.do_byte_inst(operations::bitwise_or),
            0x09 => self.do_word_inst(operations::bitwise_or),
            0x0A => self.do_byte_inst(swap_args(operations::bitwise_or)),
            0x0B => self.do_word_inst(swap_args(operations::bitwise_or)),
            0x0C => self.do_ax_byte_inst(operations::bitwise_or),
            0x0D => self.do_ax_word_inst(operations::bitwise_or),
            0x0E => self.push(CpuRegister::Cs),
            // TODO: 0x0F is pop_reg for cs but only on 8086
            0x0F => self.pop(CpuRegister::Cs),
            0x10 => self.do_byte_inst(operations::add_with_carry),
            0x11 => self.do_word_inst(operations::add_with_carry),
            0x12 => self.do_byte_inst(swap_args(operations::add_with_carry)),
            0x13 => self.do_word_inst(swap_args(operations::add_with_carry)),
            0x14 => self.do_ax_byte_inst(operations::add_with_carry),
            0x15 => self.do_ax_word_inst(operations::add_with_carry),
            0x16 => self.push(CpuRegister::Ss),
            0x17 => self.pop(CpuRegister::Ss),
            0x18 => self.do_byte_inst(operations::sub_with_borrow),
            0x19 => self.do_word_inst(operations::sub_with_borrow),
            0x1A => self.do_byte_inst(swap_args(operations::sub_with_borrow)),
            0x1B => self.do_word_inst(swap_args(operations::sub_with_borrow)),
            0x1C => self.do_ax_byte_inst(operations::sub_with_borrow),
            0x1D => self.do_ax_word_inst(operations::sub_with_borrow),
            0x1E => self.push(CpuRegister::Ds),
            0x1F => self.pop(CpuRegister::Ds),
            0x20 => self.do_byte_inst(operations::bitwise_and),
            0x21 => self.do_word_inst(operations::bitwise_and),
            0x22 => self.do_byte_inst(swap_args(operations::bitwise_and)),
            0x23 => self.do_word_inst(swap_args(operations::bitwise_and)),
            0x24 => self.do_ax_byte_inst(operations::bitwise_and),
            0x25 => self.do_ax_word_inst(operations::bitwise_and),
            0x26 => self.do_cycle_with_segment_override(SegmentOverride::Extra),
            0x28 => self.do_byte_inst(operations::sub),
            0x29 => self.do_word_inst(operations::sub),
            0x2A => self.do_byte_inst(swap_args(operations::sub)),
            0x2B => self.do_word_inst(swap_args(operations::sub)),
            0x2C => self.do_ax_byte_inst(operations::sub),
            0x2D => self.do_ax_word_inst(operations::sub),
            0x2E => self.do_cycle_with_segment_override(SegmentOverride::Code),
            0x30 => self.do_byte_inst(operations::bitwise_xor),
            0x31 => self.do_word_inst(operations::bitwise_xor),
            0x32 => self.do_byte_inst(swap_args(operations::bitwise_xor)),
            0x33 => self.do_word_inst(swap_args(operations::bitwise_xor)),
            0x34 => self.do_ax_byte_inst(operations::bitwise_xor),
            0x35 => self.do_ax_word_inst(operations::bitwise_xor),
            0x36 => self.do_cycle_with_segment_override(SegmentOverride::Stack),
            0x38 => self.do_byte_inst(operations::cmp),
            0x39 => self.do_word_inst(operations::cmp),
            0x3A => self.do_byte_inst(swap_args(operations::cmp)),
            0x3B => self.do_word_inst(swap_args(operations::cmp)),
            0x3C => self.do_ax_byte_inst(operations::cmp),
            0x3D => self.do_ax_word_inst(operations::cmp),
            0x3E => self.do_cycle_with_segment_override(SegmentOverride::Data),
            0x40 => inc_reg(&mut self.ax, &mut self.flag),
            0x41 => inc_reg(&mut self.cx, &mut self.flag),
            0x42 => inc_reg(&mut self.dx, &mut self.flag),
            0x43 => inc_reg(&mut self.bx, &mut self.flag),
            0x44 => inc_reg(&mut self.sp, &mut self.flag),
            0x45 => inc_reg(&mut self.bp, &mut self.flag),
            0x46 => inc_reg(&mut self.si, &mut self.flag),
            0x47 => inc_reg(&mut self.di, &mut self.flag),
            0x48 => dec_reg(&mut self.ax, &mut self.flag),
            0x49 => dec_reg(&mut self.cx, &mut self.flag),
            0x4A => dec_reg(&mut self.dx, &mut self.flag),
            0x4B => dec_reg(&mut self.bx, &mut self.flag),
            0x4C => dec_reg(&mut self.sp, &mut self.flag),
            0x4D => dec_reg(&mut self.bp, &mut self.flag),
            0x4E => dec_reg(&mut self.si, &mut self.flag),
            0x4F => dec_reg(&mut self.di, &mut self.flag),
            0x50 => self.push(CpuRegister::Ax),
            0x51 => self.push(CpuRegister::Cx),
            0x52 => self.push(CpuRegister::Dx),
            0x53 => self.push(CpuRegister::Bx),
            0x54 => self.push(CpuRegister::Sp),
            0x55 => self.push(CpuRegister::Bp),
            0x56 => self.push(CpuRegister::Si),
            0x57 => self.push(CpuRegister::Di),
            0x58 => self.pop(CpuRegister::Ax),
            0x59 => self.pop(CpuRegister::Cx),
            0x5A => self.pop(CpuRegister::Dx),
            0x5B => self.pop(CpuRegister::Bx),
            0x5C => self.pop(CpuRegister::Sp),
            0x5D => self.pop(CpuRegister::Bp),
            0x5E => self.pop(CpuRegister::Si),
            0x5F => self.pop(CpuRegister::Di),
            0x70 => jmp_if_any_set(&self.mem, &mut self.ip, self.flag, CpuFlags::OVERFLOW),
            0x71 => jmp_if_none_set(&self.mem, &mut self.ip, self.flag, CpuFlags::OVERFLOW),
            0x72 => jmp_if_any_set(&self.mem, &mut self.ip, self.flag, CpuFlags::CARRY),
            0x73 => jmp_if_none_set(&self.mem, &mut self.ip, self.flag, CpuFlags::CARRY),
            0x74 => jmp_if_any_set(&self.mem, &mut self.ip, self.flag, CpuFlags::ZERO),
            0x75 => jmp_if_none_set(&self.mem, &mut self.ip, self.flag, CpuFlags::ZERO),
            0x76 => jmp_if_any_set(
                &self.mem,
                &mut self.ip,
                self.flag,
                CpuFlags::CARRY | CpuFlags::ZERO,
            ),
            0x77 => jmp_if_none_set(
                &self.mem,
                &mut self.ip,
                self.flag,
                CpuFlags::CARRY | CpuFlags::ZERO,
            ),
            0x78 => jmp_if_any_set(&self.mem, &mut self.ip, self.flag, CpuFlags::SIGN),
            0x79 => jmp_if_none_set(&self.mem, &mut self.ip, self.flag, CpuFlags::SIGN),
            0x7A => jmp_if_any_set(&self.mem, &mut self.ip, self.flag, CpuFlags::PARITY),
            0x7B => jmp_if_none_set(&self.mem, &mut self.ip, self.flag, CpuFlags::PARITY),
            0x7C => jmp_if(&self.mem, &mut self.ip, || {
                self.flag.contains(CpuFlags::SIGN) != self.flag.contains(CpuFlags::OVERFLOW)
            }),
            0x7D => jmp_if(&self.mem, &mut self.ip, || {
                self.flag.contains(CpuFlags::SIGN) == self.flag.contains(CpuFlags::OVERFLOW)
            }),
            0x7E => jmp_if(&self.mem, &mut self.ip, || {
                self.flag.contains(CpuFlags::ZERO)
                    || (self.flag.contains(CpuFlags::SIGN)
                        != self.flag.contains(CpuFlags::OVERFLOW))
            }),
            0x7F => jmp_if(&self.mem, &mut self.ip, || {
                !self.flag.contains(CpuFlags::ZERO)
                    && (self.flag.contains(CpuFlags::SIGN)
                        == self.flag.contains(CpuFlags::OVERFLOW))
            }),
            0x80 => self.do_opext_inst(false, false),
            0x81 => self.do_opext_inst(true, false),
            0x82 => self.do_opext_inst(false, false),
            0x83 => self.do_opext_inst(true, true),
            0x86 => self.do_byte_inst(operations::xchg),
            0x87 => self.do_word_inst(operations::xchg),
            0x88 => self.do_byte_inst(operations::mov),
            0x89 => self.do_word_inst(operations::mov),
            0x8A => self.do_byte_inst(swap_args(operations::mov)),
            0x8B => self.do_word_inst(swap_args(operations::mov)),
            0x8C => self.do_segment_reg_inst(operations::mov),
            0x8E => self.do_segment_reg_inst(swap_args(operations::mov)),
            0x90 => 1,
            0x91 => swap_reg(&mut self.ax, &mut self.cx),
            0x92 => swap_reg(&mut self.ax, &mut self.dx),
            0x93 => swap_reg(&mut self.ax, &mut self.bx),
            0x94 => swap_reg(&mut self.ax, &mut self.sp),
            0x95 => swap_reg(&mut self.ax, &mut self.bp),
            0x96 => swap_reg(&mut self.ax, &mut self.si),
            0x97 => swap_reg(&mut self.ax, &mut self.di),
            0xB0 => self.do_reg_imm_byte_inst(CpuRegister::Ax, false, operations::mov),
            0xB1 => self.do_reg_imm_byte_inst(CpuRegister::Cx, false, operations::mov),
            0xB2 => self.do_reg_imm_byte_inst(CpuRegister::Dx, false, operations::mov),
            0xB3 => self.do_reg_imm_byte_inst(CpuRegister::Bx, false, operations::mov),
            0xB4 => self.do_reg_imm_byte_inst(CpuRegister::Ax, true, operations::mov),
            0xB5 => self.do_reg_imm_byte_inst(CpuRegister::Cx, true, operations::mov),
            0xB6 => self.do_reg_imm_byte_inst(CpuRegister::Dx, true, operations::mov),
            0xB7 => self.do_reg_imm_byte_inst(CpuRegister::Bx, true, operations::mov),
            0xB8 => self.do_reg_imm_word_inst(CpuRegister::Ax, operations::mov),
            0xB9 => self.do_reg_imm_word_inst(CpuRegister::Cx, operations::mov),
            0xBA => self.do_reg_imm_word_inst(CpuRegister::Dx, operations::mov),
            0xBB => self.do_reg_imm_word_inst(CpuRegister::Bx, operations::mov),
            0xBC => self.do_reg_imm_word_inst(CpuRegister::Sp, operations::mov),
            0xBD => self.do_reg_imm_word_inst(CpuRegister::Bp, operations::mov),
            0xBE => self.do_reg_imm_word_inst(CpuRegister::Si, operations::mov),
            0xBF => self.do_reg_imm_word_inst(CpuRegister::Di, operations::mov),
            0xC3 => {
                // RET
                self.pop(CpuRegister::Ip);
                0
            }
            0xC7 => self.do_rm_imm_word_inst(operations::mov),
            0xE8 => {
                // CALL
                let return_address = self.ip + 3; // next instruction after this one (3 bytes after)
                self.push_val(return_address);
                // add the displacement to IP
                let disp = self.read_mem_word(CpuMemoryAccessType::InstructionFetch, self.ip + 1);
                // todo check if this is correct
                self.ip = self.ip.wrapping_add(disp);
                3
            }
            0xE9 => {
                self.ip += self.read_mem_word(CpuMemoryAccessType::InstructionFetch, self.ip + 1);
                3
            }
            0xEB => {
                let sval =
                    self.read_mem_byte(CpuMemoryAccessType::InstructionFetch, self.ip + 1) as i8;
                self.ip = self.ip.wrapping_add_signed(sval as i16);
                2
            }
            0xF4 => {
                should_continue_emulation = false;
                0
            }
            0xF8 => {
                self.flag.remove(CpuFlags::CARRY);
                1
            }
            0xF9 => {
                self.flag.set(CpuFlags::CARRY, true);
                1
            }
            0xFE => self.do_grp2_inst(|rm, reg, flags| {
                let _ = match reg {
                    0x00 => inc_byte(rm, flags),
                    0x01 => dec_byte(rm, flags),
                    _ => panic!("inc/dec: unexpected reg value"),
                };
            }),

            _ => self.cpu_panic("Unknown instruction"),
        };
        self.ip = self.ip.wrapping_add(ip_increment);
        should_continue_emulation
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::*;
    use test_log::test;

    const TEST_MEM_SIZE: usize = 11000000;

    #[test]
    fn cpu_sanity() {
        // NOP
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0] = 0x90;
        cpu.sp = 0x0100;
        cpu.do_cycle();
        assert_eq!(cpu.ip, 1);
        assert_eq!(cpu.ax, 0);
        assert_eq!(cpu.bx, 0);
        assert_eq!(cpu.cx, 0);
        assert_eq!(cpu.dx, 0);
        assert_eq!(cpu.bp, 0);
        assert_eq!(cpu.sp, 0x0100);
        assert_eq!(cpu.flag.bits(), 0);

        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.cs = 0xFFFF;
        let ea: usize = 0xFFFF * 0x10;
        cpu.ax = 69;
        cpu.cx = 42;
        cpu.dx = 1;
        cpu.ip = 0;
        cpu.mem[ea] = 0x01; // ADD
        cpu.mem[ea + 1] = 0b11000010; // ADD DX, AX
        cpu.mem[ea + 2] = 0x01; // ADD
        cpu.mem[ea + 3] = 0b00001110; // ADD [0x102], CX
        cpu.mem[ea + 4] = 102; // Address
        cpu.mem[ea + 5] = 0;

        // Flag tests
        cpu.mem[ea + 6] = 0x01;
        cpu.mem[ea + 7] = 0b11000010; // ADD DX, AX
        cpu.mem[ea + 8] = 0x01;
        cpu.mem[ea + 9] = 0b11000010; // ADD DX, AX
        cpu.mem[ea + 10] = 0x01;
        cpu.mem[ea + 11] = 0b11000010; // ADD DX, AX
        cpu.mem[102] = 42;
        cpu.do_cycle();
        assert_eq!(cpu.dx, 70);
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
        cpu.do_cycle();
        assert_eq!(cpu.mem[102], 84);
        assert_eq!(cpu.dx, 70);
        assert_eq!(cpu.ip, 6);

        // zero flag test
        cpu.dx = 0;
        cpu.ax = 0;
        cpu.do_cycle();
        assert_eq!(cpu.dx, 0);
        assert_eq!(cpu.ax, 0);
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));

        // sign flag test
        cpu.dx = 32766;
        cpu.ax = 10;
        cpu.do_cycle();
        assert_eq!(cpu.dx, 32776);
        assert_eq!(cpu.ax, 10);
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::SIGN));

        cpu.dx = 65534;
        cpu.do_cycle();
        assert_eq!(cpu.dx, 8);
        assert_eq!(cpu.ax, 10);
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));

        // ADD AX, BX
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x1234;
        cpu.bx = 0x5678;
        cpu.flag.insert(CpuFlags::CARRY);
        cpu.mem[0] = 0x03;
        cpu.mem[1] = 0xc3;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x68AC);
        assert_eq!(cpu.bx, 0x5678);
        assert_eq!(cpu.ip, 0x02);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));

        // MOV [BX+SI+0x1234], AX
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.es = 0xF000;
        cpu.ax = 0xA55A;
        cpu.bx = 0x1000;
        cpu.si = 0x2000;
        let ea = 0xF000 * 0x10 + 0x1000 + 0x2000 + 0x1234;
        cpu.mem[ea] = 0;
        cpu.mem[0] = 0x26; // ES segment override
        cpu.mem[1] = 0x89;
        cpu.mem[2] = 0x80;
        cpu.mem[3] = 0x34;
        cpu.mem[4] = 0x12;
        /*cpu.mem[0x4234] = 0;
        cpu.mem[0] = 0x89;
        cpu.mem[1] = 0x80;
        cpu.mem[2] = 0x34;
        cpu.mem[3] = 0x12;*/
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0xA55A);
        assert_eq!(cpu.bx, 0x1000);
        assert_eq!(cpu.si, 0x2000);
        assert_eq!(cpu.ip, 0x05);
        assert_eq!(read_word(&cpu.mem, ea), 0xA55A);
    }

    #[test]
    fn cpu_byte_sanity() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 27;
        cpu.mem[0] = 0x00;
        cpu.mem[1] = 0x06;
        cpu.mem[2] = 0x40;
        cpu.mem[0x40] = 10;
        cpu.do_cycle();
        assert_eq!(cpu.mem[0x40], 37);
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));

        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.cx = 245;
        cpu.mem[0] = 0x00;
        cpu.mem[1] = 0x0E;
        cpu.mem[2] = 0x40;
        cpu.mem[0x40] = 15;
        cpu.do_cycle();
        assert_eq!(cpu.mem[0x40], 4);
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));

        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.bx = 125;
        cpu.mem[0] = 0x00;
        cpu.mem[1] = 0x1E;
        cpu.mem[2] = 0x40;
        cpu.mem[0x40] = 10;
        cpu.do_cycle();
        assert_eq!(cpu.mem[0x40], 135);
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::SIGN));

        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0] = 0x04;
        cpu.mem[1] = 1;
        cpu.ax = 0x3445;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x3446);
    }

    #[test]
    fn cpu_mov_sanity() {
        // MOV AX, 0x1234
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.cs = 0xF000;
        let ea: usize = 0xF000 * 0x10;
        cpu.mem[ea] = 0xB8;
        cpu.mem[ea + 1] = 0x34;
        cpu.mem[ea + 2] = 0x12;
        let old_flags = cpu.flag;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x1234);
        assert_eq!(cpu.ip, 0x03);
        assert_eq!(cpu.flag, old_flags);

        // MOV [0x1234], 0x5678
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ds = 0xFFF0;
        let ea = 0x1134;
        cpu.mem[0] = 0xC7;
        cpu.mem[1] = 0x06;
        cpu.mem[2] = 0x34;
        cpu.mem[3] = 0x12;
        cpu.mem[4] = 0x78;
        cpu.mem[5] = 0x56;
        let old_flags = cpu.flag;
        cpu.do_cycle();
        assert_eq!(read_word(&cpu.mem, ea), 0x5678);
        assert_eq!(cpu.flag, old_flags);

        // MOV BX, 0x1234
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.bx = 0;
        cpu.mem[0] = 0xC7;
        cpu.mem[1] = 0xC3;
        cpu.mem[2] = 0x34;
        cpu.mem[3] = 0x12;
        let old_flags = cpu.flag;
        cpu.do_cycle();
        assert_eq!(cpu.bx, 0x1234);
        assert_eq!(cpu.ip, 0x04);
        assert_eq!(cpu.flag, old_flags);

        // saw this cause an overflow
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x123;
        cpu.bx = 0x8000;
        cpu.cx = 0x7;
        cpu.di = 0xFFF2;
        cpu.sp = 0xFFA;
        cpu.mem[0] = 0x88;
        cpu.mem[1] = 0x01;
        cpu.do_cycle();

        // MOV [0x1234], AX
        // A3 not implemented yet
        /*
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x5678;
        cpu.mem[0] = 0xA3;
        cpu.mem[1] = 0x34;
        cpu.mem[2] = 0x12;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x5678);
        assert_eq!(read_word(&cpu.mem, 0x1234), 0x5678);
        assert_eq!(cpu.ip, 0x03);
        */
    }

    #[test]
    fn cpu_stack_sanity() {
        // PUSH AX
        // POP BX
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x1234;
        cpu.sp = 0x0100;
        cpu.mem[0] = 0x50;
        cpu.mem[1] = 0x5B;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x1234);
        assert_eq!(cpu.bx, 0);
        assert_eq!(cpu.sp, 0x0100 - 2);
        assert_eq!(cpu.ip, 0x01);
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x1234);
        assert_eq!(cpu.bx, 0x1234);
        assert_eq!(cpu.sp, 0x0100);
        assert_eq!(cpu.ip, 0x02);
    }

    #[test]
    fn cpu_xchg_sanity() {
        // XCHG AX, BX
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x1234;
        cpu.bx = 0x5678;
        cpu.mem[0] = 0x87;
        cpu.mem[1] = 0xC3;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x5678);
        assert_eq!(cpu.bx, 0x1234);
        assert_eq!(cpu.ip, 0x02);
    }

    #[test]
    fn cpu_arithmetic_sanity() {
        // ADD AX, BX
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0xFFFF;
        cpu.bx = 0x0001;
        cpu.mem[0] = 0x01;
        cpu.mem[1] = 0xD8;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));

        // STC
        // ADC AX, BX
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x0001;
        cpu.bx = 0x0001;
        cpu.mem[0] = 0xF9;
        cpu.mem[1] = 0x11;
        cpu.mem[2] = 0xD8;
        cpu.do_cycle();
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0003);
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));

        // SUB AX, BX
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x0001;
        cpu.bx = 0x0002;
        cpu.mem[0] = 0x29;
        cpu.mem[1] = 0xD8;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0xFFFF);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));

        // add    WORD PTR ds:0x1d3,0x4c
        // 83 06 d3 01 4c
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ds = 0xF000;
        cpu.mem[0] = 0x83;
        cpu.mem[1] = 0x06;
        cpu.mem[2] = 0xD3;
        cpu.mem[3] = 0x01;
        cpu.mem[4] = 0x4C;
        cpu.do_cycle();
        const EA: usize = 0xF000 * 0x10 + 0x1D3;
        assert_eq!(cpu.mem[EA], 0x4C);
        assert_eq!(cpu.ip, 5);
    }

    #[test]
    fn cpu_logical_op_sanity() {
        // AND AX, BX
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0xFF00;
        cpu.bx = 0x00FF;
        cpu.mem[0] = 0x21;
        cpu.mem[1] = 0xD8;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.bx, 0x00FF);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));

        // XOR AX, BX
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x1234;
        cpu.bx = 0x1234;
        cpu.mem[0] = 0x31;
        cpu.mem[1] = 0xD8;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.bx, 0x1234);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));

        // 80 8f d5 01 00
        // or     BYTE PTR [bx+0x1d5],0x0
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x20;
        cpu.bx = 0x2;
        cpu.cx = 0x15;
        cpu.dx = 0x190;
        cpu.di = 0xFFBE;
        cpu.sp = 0x1000;
        cpu.ip = 0x11D;
        cpu.mem[0x11D] = 0x80;
        cpu.mem[0x11E] = 0x8F;
        cpu.mem[0x11F] = 0xD5;
        cpu.mem[0x120] = 0x01;
        cpu.mem[0x121] = 0x00;
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x122);

        // 81 3e d3 01 e0 01
        // cmp    WORD PTR ds:0x1d3,0x1e0
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        write_word(&mut cpu.mem, 0x01D3, 0x01E0);
        cpu.mem[0] = 0x81;
        cpu.mem[1] = 0x3E;
        cpu.mem[2] = 0xD3;
        cpu.mem[3] = 0x01;
        cpu.mem[4] = 0xE0;
        cpu.mem[5] = 0x01;
        cpu.do_cycle();
        assert_eq!(read_word(&cpu.mem, 0x01D3), 0x01E0);
        assert_eq!(cpu.ip, 0x06);
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
        cpu.ip = 0;
        write_word(&mut cpu.mem, 0x01D3, 0x00FF);
        cpu.do_cycle();
        assert_eq!(read_word(&cpu.mem, 0x01D3), 0x00FF);
        assert_eq!(cpu.ip, 0x06);
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        cpu.ip = 0;
        write_word(&mut cpu.mem, 0x01D3, 0xFFFF);
        cpu.do_cycle();
        assert_eq!(read_word(&cpu.mem, 0x01D3), 0xFFFF);
        assert_eq!(cpu.ip, 0x06);
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
    }

    #[test]
    fn cpu_inc_dec_sanity() {
        // INC AX
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0xFFFF;
        cpu.mem[0] = 0x40;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 1);
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));

        // DEC AX
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x00001;
        cpu.mem[0] = 0x48;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 1);
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));

        // INC BYTE PTR [0x1234]
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0] = 0xFE;
        cpu.mem[1] = 0x06;
        cpu.mem[2] = 0x34;
        cpu.mem[3] = 0x12;
        cpu.mem[0x1234] = 0xFF;
        cpu.do_cycle();
        assert_eq!(cpu.mem[0x1234], 0);
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
        assert!(!cpu.flag.contains(CpuFlags::CARRY));

        // DEC AL
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x6901;
        cpu.mem[0] = 0xFE;
        cpu.mem[1] = 0xC8;
        cpu.do_cycle();
        println!("{0:X}", cpu.ax);
        assert_eq!(cpu.ax, 0x6900);
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn cpu_jmp_sanity() {
        // CMP AX, BX
        // JZ +2
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ax = 0x1234;
        cpu.bx = 0x1234;
        cpu.mem[0] = 0x3B;
        cpu.mem[1] = 0xC3;
        cpu.mem[2] = 0x74;
        cpu.mem[3] = 0x02;
        cpu.do_cycle();
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x0006);
        assert!(cpu.flag.contains(CpuFlags::ZERO));

        // JMP +2
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0] = 0xEB;
        cpu.mem[1] = 0x02;
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x0004);

        // eb f5
        cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.ip = 0x1B4;
        cpu.mem[0x1B4] = 0xEB;
        cpu.mem[0x1B5] = 0xF5;
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x1AB);
    }

    #[test]
    fn cpu_subroutine_sanity() {
        // CALL 0x0005
        // RET
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.sp = 0x0100;
        cpu.mem[0] = 0xE8;
        cpu.mem[1] = 0x02;
        cpu.mem[2] = 0x00;
        cpu.mem[0x0005] = 0xC3;
        cpu.do_cycle();
        assert_eq!(cpu.sp, 0x00FE);
        assert_eq!(cpu.ip, 0x0005);
        cpu.do_cycle();
        assert_eq!(cpu.sp, 0x0100);
        assert_eq!(cpu.ip, 0x0003);
    }

    #[test]
    fn cpu_flag_sanity() {
        // STC
        // CLC
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0] = 0xF9;
        cpu.mem[1] = 0xF8;
        cpu.do_cycle();
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        cpu.do_cycle();
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
    }

    // Instruction tests
    #[test]
    fn test_80_add_mem() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // ADD BYTE [0x1234], 0x0A
        cpu.mem[0..5].copy_from_slice(&[0x80, 0x06, 0x34, 0x12, 0x0A]);
        cpu.mem[0x1234] = 0x0A;
        cpu.do_cycle();

        assert_eq!(cpu.mem[0x1234], 0x14);
        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_80_add_reg_overflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // ADD AL, 0x01
        cpu.mem[0..3].copy_from_slice(&[0x80, 0xC0, 0x01]);
        cpu.ax = 0x00FF;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_80_sub_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SUB BL, 0x01
        cpu.mem[0..3].copy_from_slice(&[0x80, 0xEB, 0x01]);
        cpu.bx = 0x0000;
        cpu.do_cycle();

        assert_eq!(cpu.bx, 0x00FF);
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_80_cmp_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // CMP BYTE [0x1234], 0x12
        cpu.ds = 0x923F;
        cpu.mem[0..5].copy_from_slice(&[0x80, 0x3E, 0x34, 0x12, 0x12]);
        const EA: usize = 0x923F * 0x10 + 0x1234;
        cpu.mem[EA] = 0x12;
        cpu.do_cycle();

        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_30_xor_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // XOR CL, CL
        cpu.mem[0..2].copy_from_slice(&[0x30, 0xC9]);
        cpu.cx = 0x00AA;
        cpu.do_cycle();

        assert_eq!(cpu.cx, 0x0000);
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_81_add_reg_overflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // ADD AX, 0x0001
        cpu.mem[0..4].copy_from_slice(&[0x81, 0xC0, 0x01, 0x00]);
        cpu.ax = 0xFFFF;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 4);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_81_cmp_reg_signed() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // CMP BX, 0x8000
        cpu.mem[0..4].copy_from_slice(&[0x81, 0xFB, 0x00, 0x80]);
        cpu.bx = 0x7FFF;
        cpu.do_cycle();

        assert_eq!(cpu.bx, 0x7FFF); // Unchanged
        assert_eq!(cpu.ip, 4);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_81_sub_mem_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SUB WORD [0x1234], 0xFF80 (equivalent to adding 0x0080)
        cpu.mem[0..6].copy_from_slice(&[0x81, 0x2E, 0x34, 0x12, 0x80, 0xFF]);
        write_word(&mut cpu.mem, 0x1234, 0x0005);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0x0085); // 5 - (-128) = 133
        assert_eq!(cpu.ip, 6);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_81_add_max_signed() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // ADD AX, 0x7FFF
        cpu.mem[0..4].copy_from_slice(&[0x81, 0xC0, 0xFF, 0x7F]);
        cpu.ax = 0x0001;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x8000);
        assert_eq!(cpu.ip, 4);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_82_add_positive() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD BYTE [0x1234], 0x01
        cpu.mem[0..5].copy_from_slice(&[0x82, 0x06, 0x34, 0x12, 0x01]);
        cpu.mem[0x1234] = 0x01;
        cpu.do_cycle();

        assert_eq!(cpu.mem[0x1234], 0x02);
        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_82_add_al_positive() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD AL, 0x01
        cpu.mem[0..3].copy_from_slice(&[0x82, 0xC0, 0x01]);
        cpu.ax = 0x0001; // AL = 0x01
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0002); // AL = 0x02
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_82_add_overflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD BYTE [0x1234], 0x01
        cpu.mem[0..5].copy_from_slice(&[0x82, 0x06, 0x34, 0x12, 0x01]);
        cpu.mem[0x1234] = 0xFF;
        cpu.do_cycle();

        assert_eq!(cpu.mem[0x1234], 0x00);
        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_82_add_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD BYTE [0x1234], 0x01
        cpu.mem[0..5].copy_from_slice(&[0x82, 0x06, 0x34, 0x12, 0x01]);
        cpu.mem[0x1234] = 0x7F; // 127 + 1 = 128 (0x80, signed -128)
        cpu.do_cycle();

        assert_eq!(cpu.mem[0x1234], 0x80);
        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_82_sub_borrow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: SUB BYTE [0x1234], 0xFF (subtract 255)
        cpu.mem[0..5].copy_from_slice(&[0x82, 0x2E, 0x34, 0x12, 0xFF]);
        cpu.mem[0x1234] = 0x05;
        cpu.do_cycle();

        assert_eq!(cpu.mem[0x1234], 0x06); // 5 - (-1) = 6 (overflow in signed)
        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_82_sub_bl_borrow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..3].copy_from_slice(&[0x82, 0xEB, 0xFF]);
        cpu.bx = 0x0005; // BL = 0x05
        cpu.do_cycle();

        assert_eq!(cpu.bx, 0x0006); // BL = 0x06 (5 - (-1))
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_82_cmp_signed() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: CMP BYTE [0x1234], 0x80 (compare 127 vs -128)
        cpu.mem[0..5].copy_from_slice(&[0x82, 0x3E, 0x34, 0x12, 0x80]);
        cpu.mem[0x1234] = 0x7F; // 127
        cpu.do_cycle();

        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_82_add_max_positive() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD BYTE [0x1234], 0x7F (127)
        cpu.mem[0..5].copy_from_slice(&[0x82, 0x06, 0x34, 0x12, 0x7F]);
        cpu.mem[0x1234] = 0x01;
        cpu.do_cycle();

        assert_eq!(cpu.mem[0x1234], 0x80); // 1 + 127 = 128 (signed -128)
        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_add_positive() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD WORD [0x1234], 0x01
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x06, 0x34, 0x12, 0x01]);
        write_word(&mut cpu.mem, 0x1234, 0x0001);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0x0002);
        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_add_ax_overflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..3].copy_from_slice(&[0x83, 0xC0, 0x01]);
        cpu.ax = 0xFFFF;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_add_carry() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD WORD [0x1234], 0x01
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x06, 0x34, 0x12, 0x01]);
        write_word(&mut cpu.mem, 0x1234, 0xFFFF);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0x0000);
        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_add_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD WORD [0x1234], 0xFF
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x06, 0x34, 0x12, 0xFF]);
        write_word(&mut cpu.mem, 0x1234, 0x0002);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0x0001);
        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_add_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADD WORD [0x1234], 0xFF
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x06, 0x34, 0x12, 0xFF]);
        write_word(&mut cpu.mem, 0x1234, 0x0001);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0x0000);
        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_sub_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: SUB WORD [0x1234], 0x01
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x2E, 0x34, 0x12, 0x01]);
        write_word(&mut cpu.mem, 0x1234, 0x0000);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0xFFFF);
        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_sub_ax_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..3].copy_from_slice(&[0x83, 0xE8, 0x80]);
        cpu.ax = 0x0000;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0080); // 0 - (-128) = 128
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_sub_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: SUB WORD [0x1234], 0xFF
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x2E, 0x34, 0x12, 0xFF]);
        write_word(&mut cpu.mem, 0x1234, 0x0005);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0x0006);
        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_sub_bx_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..3].copy_from_slice(&[0x83, 0xEB, 0xFF]);
        cpu.bx = 0x0005;
        cpu.do_cycle();

        assert_eq!(cpu.bx, 0x0006); // 5 - (-1) = 6
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_cmp_equal() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: CMP WORD [0x1234], 0x34
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x3E, 0x34, 0x12, 0x34]);
        write_word(&mut cpu.mem, 0x1234, 0x0034);
        cpu.do_cycle();

        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_cmp_unsigned_less() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: CMP WORD [0x1234], 0x80
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x3E, 0x34, 0x12, 0x80]);
        write_word(&mut cpu.mem, 0x1234, 0x007F);
        cpu.do_cycle();

        assert_eq!(cpu.ip, 5);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_adc() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: ADC WORD [0x1234], 0x01
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x16, 0x34, 0x12, 0x01]);
        write_word(&mut cpu.mem, 0x1234, 0x0001);
        cpu.flag.insert(CpuFlags::CARRY);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0x0003);
        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_83_sbb() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // Instruction: SBB WORD [0x1234], 0x01
        cpu.mem[0..5].copy_from_slice(&[0x83, 0x1E, 0x34, 0x12, 0x01]);
        write_word(&mut cpu.mem, 0x1234, 0x0005);
        cpu.flag.insert(CpuFlags::CARRY);
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x1234), 0x0003);
        assert_eq!(cpu.ip, 5);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }
    // ----------------------------
    // 0x1C: SBB AL, imm8 (Subtract with Borrow)
    // ----------------------------
    #[test]
    fn test_1c_sbb_al_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SBB AL, 0x01 (Subtract with borrow: AL = AL - 0x01 - CF)
        cpu.mem[0..2].copy_from_slice(&[0x1C, 0x01]);
        cpu.ax = 0x0000; // AL = 0x00
        cpu.flag.insert(CpuFlags::CARRY); // Previous borrow exists
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x00FE); // 0x00 - 0x01 - 1 = 0xFE (CF=1)
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_1c_sbb_al_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SBB AL, 0x01
        cpu.mem[0..2].copy_from_slice(&[0x1C, 0x01]);
        cpu.ax = 0x0001; // AL = 0x01
        cpu.flag.remove(CpuFlags::CARRY); // No previous borrow
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000); // 0x01 - 0x01 - 0 = 0x00
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    // ----------------------------
    // 0x1D: SBB AX, imm16 (Subtract with Borrow)
    // ----------------------------
    #[test]
    fn test_1d_sbb_ax_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SBB AX, 0x0001
        cpu.mem[0..3].copy_from_slice(&[0x1D, 0x01, 0x00]);
        cpu.ax = 0x0000;
        cpu.flag.insert(CpuFlags::CARRY); // Previous borrow
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0xFFFE); // 0x0000 - 0x0001 - 1 = 0xFFFE
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_1d_sbb_ax_signed_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SBB AX, 0x0001
        cpu.mem[0..3].copy_from_slice(&[0x1D, 0x01, 0x00]);
        cpu.ax = 0x8000; // -32768 (signed)
        cpu.flag.remove(CpuFlags::CARRY); // No borrow
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x7FFF); // -32768 - 1 = -32769
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_1d_sbb_ax_edge_case() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SBB AX, 0x7FFF (Subtract 32767)
        cpu.mem[0..3].copy_from_slice(&[0x1D, 0xFF, 0x7F]);
        cpu.ax = 0x8000; // -32768 (signed)
        cpu.flag.insert(CpuFlags::CARRY); // Previous borrow
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000); // -32768 - 32767 - 1 = -65536 → wraps to 0x0000
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    // ----------------------------
    // 0x24: AND AL, imm8
    // ----------------------------
    #[test]
    fn test_24_and_al_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // AND AL, 0x00
        cpu.mem[0..2].copy_from_slice(&[0x24, 0x00]);
        cpu.ax = 0x00FF; // AL = 0xFF
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_24_and_al_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // AND AL, 0x80
        cpu.mem[0..2].copy_from_slice(&[0x24, 0x80]);
        cpu.ax = 0x00FF; // AL = 0xFF
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0080);
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    // ----------------------------
    // 0x25: AND AX, imm16
    // ----------------------------
    #[test]
    fn test_25_and_ax_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // AND AX, 0x8000
        cpu.mem[0..3].copy_from_slice(&[0x25, 0x00, 0x80]);
        cpu.ax = 0xFFFF;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x8000);
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_25_and_ax_partial_mask() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // AND AX, 0x00FF
        cpu.mem[0..3].copy_from_slice(&[0x25, 0xFF, 0x00]);
        cpu.ax = 0x1234;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0034);
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(!cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    #[test]
    fn test_25_and_ax_edge_case() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // AND AX, 0x0000
        cpu.mem[0..3].copy_from_slice(&[0x25, 0x00, 0x00]);
        cpu.ax = 0xFFFF;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY));
        assert!(cpu.flag.contains(CpuFlags::ZERO));
        assert!(!cpu.flag.contains(CpuFlags::SIGN));
    }

    // ----------------------------
    // 0x2C: SUB AL, imm8
    // ----------------------------
    #[test]
    fn test_2c_sub_al_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SUB AL, 0x01
        cpu.mem[0..2].copy_from_slice(&[0x2C, 0x01]);
        cpu.ax = 0x0000; // AL = 0x00
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x00FF);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.contains(CpuFlags::CARRY), "CF should be set");
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(cpu.flag.contains(CpuFlags::SIGN), "SF should be set");
    }

    #[test]
    fn test_2c_sub_al_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SUB AL, 0x05
        cpu.mem[0..2].copy_from_slice(&[0x2C, 0x05]);
        cpu.ax = 0x0005; // AL = 0x05
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(cpu.flag.contains(CpuFlags::ZERO), "ZF should be set");
        assert!(!cpu.flag.contains(CpuFlags::SIGN), "SF should be clear");
    }

    // ----------------------------
    // 0x2D: SUB AX, imm16
    // ----------------------------
    #[test]
    fn test_2d_sub_ax_signed_boundary() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SUB AX, 0x0001
        cpu.mem[0..3].copy_from_slice(&[0x2D, 0x01, 0x00]);
        cpu.ax = 0x8000; // -32768 signed
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x7FFF);
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(!cpu.flag.contains(CpuFlags::SIGN), "SF should be clear");
    }

    #[test]
    fn test_2d_sub_ax_edge_case() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SUB AX, 0xFFFF
        cpu.mem[0..3].copy_from_slice(&[0x2D, 0xFF, 0xFF]);
        cpu.ax = 0x0001;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0002);
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY), "CF should be set");
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(!cpu.flag.contains(CpuFlags::SIGN), "SF should be clear");
    }

    #[test]
    fn test_2d_sub_ax_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // SUB AX, 0x0001
        cpu.mem[0..3].copy_from_slice(&[0x2D, 0x01, 0x00]);
        cpu.ax = 0x0000;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0xFFFF);
        assert_eq!(cpu.ip, 3);
        assert!(cpu.flag.contains(CpuFlags::CARRY), "CF should be set");
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(cpu.flag.contains(CpuFlags::SIGN), "SF should be set");
    }

    // ----------------------------
    // 0x34: XOR AL, imm8
    // ----------------------------
    #[test]
    fn test_34_xor_al_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // XOR AL, 0xFF
        cpu.mem[0..2].copy_from_slice(&[0x34, 0xFF]);
        cpu.ax = 0x00FF; // AL = 0xFF (AX = 0x00FF)
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000); // 0xFF ^ 0xFF = 0x00
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(cpu.flag.contains(CpuFlags::ZERO), "ZF should be set");
        assert!(!cpu.flag.contains(CpuFlags::SIGN), "SF should be clear");
    }

    #[test]
    fn test_34_xor_al_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // XOR AL, 0x80
        cpu.mem[0..2].copy_from_slice(&[0x34, 0x80]);
        cpu.ax = 0x0000; // AL = 0x00
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0080); // 0x00 ^ 0x80 = 0x80
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(cpu.flag.contains(CpuFlags::SIGN), "SF should be set");
    }

    // ----------------------------
    // 0x35: XOR AX, imm16
    // ----------------------------
    #[test]
    fn test_35_xor_ax_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // XOR AX, 0xFFFF
        cpu.mem[0..3].copy_from_slice(&[0x35, 0xFF, 0xFF]);
        cpu.ax = 0xFFFF;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000); // 0xFFFF ^ 0xFFFF = 0x0000
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(cpu.flag.contains(CpuFlags::ZERO), "ZF should be set");
        assert!(!cpu.flag.contains(CpuFlags::SIGN), "SF should be clear");
    }

    #[test]
    fn test_35_xor_ax_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // XOR AX, 0x8000
        cpu.mem[0..3].copy_from_slice(&[0x35, 0x00, 0x80]);
        cpu.ax = 0x0000;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x8000); // 0x0000 ^ 0x8000 = 0x8000
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(cpu.flag.contains(CpuFlags::SIGN), "SF should be set");
    }

    #[test]
    fn test_35_xor_ax_edge_case() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // XOR AX, 0x1234
        cpu.mem[0..3].copy_from_slice(&[0x35, 0x34, 0x12]);
        cpu.ax = 0x1234;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0000); // 0x1234 ^ 0x1234 = 0x0000
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(cpu.flag.contains(CpuFlags::ZERO), "ZF should be set");
        assert!(!cpu.flag.contains(CpuFlags::SIGN), "SF should be clear");
    }

    // ----------------------------
    // 0x3C: CMP AL, imm8
    // ----------------------------
    #[test]
    fn test_3c_cmp_al_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // CMP AL, 0x05
        cpu.mem[0..2].copy_from_slice(&[0x3C, 0x05]);
        cpu.ax = 0x0005; // AL = 0x05
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0005); // Register unchanged
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(cpu.flag.contains(CpuFlags::ZERO), "ZF should be set");
        assert!(!cpu.flag.contains(CpuFlags::SIGN), "SF should be clear");
    }

    #[test]
    fn test_3c_cmp_al_unsigned_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // CMP AL, 0x02
        cpu.mem[0..2].copy_from_slice(&[0x3C, 0x02]);
        cpu.ax = 0x0001; // AL = 0x01
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0001); // Register unchanged
        assert_eq!(cpu.ip, 2);
        assert!(
            cpu.flag.contains(CpuFlags::CARRY),
            "CF=1 (0x01 < 0x02 unsigned)"
        );
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(cpu.flag.contains(CpuFlags::SIGN), "SF=1 (0xFF is negative)");
    }

    #[test]
    fn test_3c_cmp_al_signed_positive() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // CMP AL, 0x01
        cpu.mem[0..2].copy_from_slice(&[0x3C, 0x01]);
        cpu.ax = 0x0080; // AL = 0x80 (signed -128)
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0080); // Register unchanged
        assert_eq!(cpu.ip, 2);
        assert!(
            !cpu.flag.contains(CpuFlags::CARRY),
            "CF=0 (0x80 > 0x01 unsigned)"
        );
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(
            !cpu.flag.contains(CpuFlags::SIGN),
            "SF=0 (0x7F is positive)"
        );
    }

    // ----------------------------
    // 0x3D: CMP AX, imm16
    // ----------------------------
    #[test]
    fn test_3d_cmp_ax_zero() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // CMP AX, 0x1234
        cpu.mem[0..3].copy_from_slice(&[0x3D, 0x34, 0x12]);
        cpu.ax = 0x1234;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x1234); // Register unchanged
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.contains(CpuFlags::CARRY), "CF should be clear");
        assert!(cpu.flag.contains(CpuFlags::ZERO), "ZF should be set");
        assert!(!cpu.flag.contains(CpuFlags::SIGN), "SF should be clear");
    }

    #[test]
    fn test_3d_cmp_ax_unsigned_underflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // CMP AX, 0x0002
        cpu.mem[0..3].copy_from_slice(&[0x3D, 0x02, 0x00]);
        cpu.ax = 0x0001;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0001); // Register unchanged
        assert_eq!(cpu.ip, 3);
        assert!(
            cpu.flag.contains(CpuFlags::CARRY),
            "CF=1 (0x0001 < 0x0002 unsigned)"
        );
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(
            cpu.flag.contains(CpuFlags::SIGN),
            "SF=1 (0xFFFF is negative)"
        );
    }

    #[test]
    fn test_3d_cmp_ax_signed_negative() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // CMP AX, 0x8000
        cpu.mem[0..3].copy_from_slice(&[0x3D, 0x00, 0x80]);
        cpu.ax = 0x0001; // Compare 1 vs -32768 (signed)
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x0001); // Register unchanged
        assert_eq!(cpu.ip, 3);
        assert!(
            cpu.flag.contains(CpuFlags::CARRY),
            "CF=1 (0x0001 < 0x8000 unsigned)"
        );
        assert!(!cpu.flag.contains(CpuFlags::ZERO), "ZF should be clear");
        assert!(
            cpu.flag.contains(CpuFlags::SIGN),
            "SF=1 (0x8001 is negative)"
        );
    }

    // ----------------------------
    // 0x72: JB (CF=1)
    // ----------------------------
    #[test]
    fn test_72_jb_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x72, 0x02]); // JB +2
        cpu.flag.set(CpuFlags::CARRY, true);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04); // 0x00 + 2 + 0x02
    }

    // ----------------------------
    // 0x73: JNB (CF=0)
    // ----------------------------
    #[test]
    fn test_73_jnb_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x73, 0xFE]); // JNB -2
        cpu.flag.set(CpuFlags::CARRY, false);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x00); // 0x00 + 2 - 2 (signed)
    }

    // ----------------------------
    // 0x74: JZ (ZF=1)
    // ----------------------------
    #[test]
    fn test_74_jz_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x74, 0x04]); // JZ +4
        cpu.flag.set(CpuFlags::ZERO, true);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x06); // 0x00 + 2 + 0x04
    }

    // ----------------------------
    // 0x75: JNZ (ZF=0)
    // ----------------------------
    #[test]
    fn test_75_jnz_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x75, 0x04]); // JNZ +4
        cpu.flag.set(CpuFlags::ZERO, false);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x06);
    }

    // ----------------------------
    // 0x76: JBE (CF=1 || ZF=1)
    // ----------------------------
    #[test]
    fn test_76_jbe_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x76, 0x02]); // JBE +2
        cpu.flag.set(CpuFlags::CARRY, true);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x77: JNBE (CF=0 && ZF=0)
    // ----------------------------
    #[test]
    fn test_77_jnbe_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x77, 0x02]); // JNBE +2
        cpu.flag.set(CpuFlags::CARRY, false);
        cpu.flag.set(CpuFlags::ZERO, false);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x78: JS (SF=1)
    // ----------------------------
    #[test]
    fn test_78_js_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x78, 0x02]); // JS +2
        cpu.flag.set(CpuFlags::SIGN, true);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x79: JNS (SF=0)
    // ----------------------------
    #[test]
    fn test_79_jns_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x79, 0x02]); // JNS +2
        cpu.flag.set(CpuFlags::SIGN, false);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x78: JS (SF=1) - Jump Not Taken
    // ----------------------------
    #[test]
    fn test_78_js_not_taken() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x78, 0x02]); // JS +2
        cpu.flag.set(CpuFlags::SIGN, false); // SF=0
        cpu.do_cycle();

        // Should NOT jump - IP advances by 2 bytes
        assert_eq!(cpu.ip, 0x02, "IP should advance past instruction");
    }

    // ----------------------------
    // 0x77: JNBE (CF=0 & ZF=0) - Jump Not Taken (CF=1 case)
    // ----------------------------
    #[test]
    fn test_77_jnbe_not_taken_cf() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x77, 0x04]); // JNBE +4
        cpu.flag.set(CpuFlags::CARRY, true); // CF=1 blocks jump
        cpu.flag.set(CpuFlags::ZERO, false);
        cpu.do_cycle();

        // Should NOT jump - IP advances by 2 bytes
        assert_eq!(cpu.ip, 0x02, "IP should ignore jump when CF=1");
    }

    // ----------------------------
    // 0x77: JNBE (CF=0 & ZF=0) - Jump Not Taken (ZF=1 case)
    // ----------------------------
    #[test]
    fn test_77_jnbe_not_taken_zf() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x77, 0x04]); // JNBE +4
        cpu.flag.set(CpuFlags::CARRY, false);
        cpu.flag.set(CpuFlags::ZERO, true); // ZF=1 blocks jump
        cpu.do_cycle();

        // Should NOT jump - IP advances by 2 bytes
        assert_eq!(cpu.ip, 0x02, "IP should ignore jump when ZF=1");
    }

    // ----------------------------
    // 0x70: JO (Jump if OF=1)
    // ----------------------------
    #[test]
    fn test_70_jo_overflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x70, 0x02]); // JO +2
        cpu.flag.set(CpuFlags::OVERFLOW, true);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04); // 0x00 + 2 + 0x02
    }

    // ----------------------------
    // 0x71: JNO (Jump if OF=0)
    // ----------------------------
    #[test]
    fn test_71_jno_no_overflow() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x71, 0x02]); // JNO +2
        cpu.flag.set(CpuFlags::OVERFLOW, false);
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x7A: JP/JPE (Jump if PF=1)
    // ----------------------------
    #[test]
    fn test_7a_jp_parity_even() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x7A, 0x02]); // JP +2
        cpu.flag.set(CpuFlags::PARITY, true); // Even parity (e.g., 0x03 = 0b00000011)
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x7B: JNP/JPO (Jump if PF=0)
    // ----------------------------
    #[test]
    fn test_7b_jnp_parity_odd() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x7B, 0x02]); // JNP +2
        cpu.flag.set(CpuFlags::PARITY, false); // Odd parity (e.g., 0x01 = 0b00000001)
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x7C: JL/JNGE (SF ≠ OF)
    // ----------------------------
    #[test]
    fn test_7c_jl_signed_less() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x7C, 0x02]); // JL +2
        cpu.flag.set(CpuFlags::SIGN, true); // Negative result
        cpu.flag.set(CpuFlags::OVERFLOW, false); // No overflow
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x7D: JNL/JGE (SF = OF)
    // ----------------------------
    #[test]
    fn test_7d_jnl_signed_not_less() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x7D, 0x02]); // JNL +2
        cpu.flag.set(CpuFlags::SIGN, false); // Positive result
        cpu.flag.set(CpuFlags::OVERFLOW, false); // No overflow
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x7E: JLE/JNG (ZF=1 OR SF≠OF)
    // ----------------------------
    #[test]
    fn test_7e_jle_zero_case() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x7E, 0x02]); // JLE +2
        cpu.flag.set(CpuFlags::ZERO, true); // Equality case
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x7F: JNLE/JG (ZF=0 AND SF=OF)
    // ----------------------------
    #[test]
    fn test_7f_jg_signed_greater() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..2].copy_from_slice(&[0x7F, 0x02]); // JG +2
        cpu.flag.set(CpuFlags::ZERO, false); // Non-zero result
        cpu.flag.set(CpuFlags::SIGN, false); // Positive result
        cpu.flag.set(CpuFlags::OVERFLOW, false); // No overflow
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x04);
    }

    // ----------------------------
    // 0x8C: MOV r/m16, Sreg
    // ----------------------------
    #[test]
    fn test_8c_mov_ax_ds() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // MOV AX, DS
        cpu.mem[0..2].copy_from_slice(&[0x8C, 0xD8]); // ModR/M: 11 11 000 (DS → AX)
        cpu.ds = 0x1234;
        cpu.do_cycle();

        assert_eq!(cpu.ax, 0x1234);
        assert_eq!(cpu.ip, 2);
    }

    #[test]
    fn test_8c_mov_mem_es() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // MOV [0x5678], ES
        cpu.mem[0..4].copy_from_slice(&[0x8C, 0x06, 0x78, 0x56]); // ModR/M: 00 00 110 (ES → [0x5678])
        cpu.es = 0xABCD;
        cpu.do_cycle();

        assert_eq!(read_word(&cpu.mem, 0x5678), 0xABCD);
        assert_eq!(cpu.ip, 4);
    }

    // ----------------------------
    // 0x8E: MOV Sreg, r/m16
    // ----------------------------
    #[test]
    fn test_8e_mov_ds_ax() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // MOV DS, AX
        cpu.mem[0..2].copy_from_slice(&[0x8E, 0xD8]); // ModR/M: 11 11 000 (AX → DS)
        cpu.ax = 0x5678;
        cpu.do_cycle();

        assert_eq!(cpu.ds, 0x5678);
        assert_eq!(cpu.ip, 2);
    }

    #[test]
    fn test_8e_mov_ss_mem() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // MOV SS, [0x1234]
        cpu.mem[0..4].copy_from_slice(&[0x8E, 0x16, 0x34, 0x12]); // ModR/M: 00 10 110 (SS ← [0x1234])
        write_word(&mut cpu.mem, 0x1234, 0xDEAD);
        cpu.do_cycle();

        assert_eq!(cpu.ss, 0xDEAD);
        assert_eq!(cpu.ip, 4);
    }

    // Edge Case: MOV CS, AX (invalid but technically encodable)
    #[test]
    fn test_8e_mov_cs_ax() {
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        // MOV CS, AX
        cpu.mem[0..2].copy_from_slice(&[0x8E, 0xC8]); // ModR/M: 11 01 000 (CS ← AX)
        cpu.ax = 0x1234;
        let original_cs = cpu.cs;
        cpu.do_cycle();

        // Most 8086 emulators ignore writes to CS via MOV
        assert_eq!(cpu.cs, original_cs, "CS should remain unchanged");
        assert_eq!(cpu.ip, 2);
    }

    // This is one big test that runs the program from: https://codegolf.stackexchange.com/questions/11880/emulate-an-intel-8086-cpu
    // and verifies it matches the reference output
    #[test]
    fn test_codegolf_reference_output() {
        let reference_output = include_bytes!(
            "../tests/codegolf_reference_program/test_codegolf_reference_output.bin"
        );
        let reference_program =
            include_bytes!("../tests/codegolf_reference_program/codegolf_reference_program");
        let mut cpu = Cpu::new_with_mem_size(TEST_MEM_SIZE);
        cpu.mem[0..reference_program.len()].copy_from_slice(reference_program);
        cpu.sp = 0x100;
        cpu.emulate();
        assert_eq!(cpu.mem[0x8000..0x87D0], *reference_output);
    }
}
