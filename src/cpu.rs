use crate::bits::Bits;
use std::mem;
use std::process;
static CARRY_FLAG: u16 = 0;
// static PARITY_FLAG: u16 = 2;
// static AUX_CARRY_FLAG: u16 = 4;
static ZERO_FLAG: u16 = 6;
static SIGN_FLAG: u16 = 7;
// static TRAP_FLAG: u16 = 8;
// static INTERRUPT_FLAG: u16 = 9;
// static DIRECTION_FLAG: u16 = 10;
// static OVERFLOW_FLAG: u16 = 11;

pub struct Cpu {
    // Registers
    ax: u16,
    bx: u16,
    cx: u16,
    dx: u16,
    si: u16,
    di: u16,
    bp: u16,
    sp: u16,
    ip: u16,

    flag: u16,
    mem: Box<[u8]>,
    debug: bool,
}

fn read_word(mem: &[u8], loc: usize) -> u16 {
    let b1 = mem[loc] as u16;
    let b2 = mem[loc + 1] as u16;
    b1 | (b2 << 8)
}

fn write_word(mem: &mut [u8], loc: usize, val: u16) {
    mem[loc] = val.get_low();
    mem[loc + 1] = val.get_high();
}

fn calc_word_sign_bit(val: u16) -> bool {
    (val as i16) < 0
}

fn calc_byte_sign_bit(val: u8) -> bool {
    (val as i8) < 0
}

fn calc_word_carry_bit(a: u16, b: u16) -> bool {
    let dword_val = (a as u32) + (b as u32);
    (dword_val & 0xFF0000) != 0
}

fn calc_byte_carry_bit(a: u8, b: u8) -> bool {
    let dword_val = (a as u16) + (b as u16);
    (dword_val & 0xFF00) != 0
}

fn calc_word_borrow_bit(a: u16, b: u16) -> bool {
    let dword_val = (a as u32).wrapping_sub(b as u32);
    (dword_val & 0xFF0000) != 0
}

fn calc_byte_borrow_bit(a: u8, b: u8) -> bool {
    let dword_val = (a as u16).wrapping_sub(b as u16);
    (dword_val & 0xFF00) != 0
}

fn calc_add_word_flags(flags: &mut u16, left: u16, right: u16, result: u16) {
    flags.set_bit(CARRY_FLAG, calc_word_carry_bit(left, right));
    flags.set_bit(SIGN_FLAG, calc_word_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == 0);
}

fn calc_add_byte_flags(flags: &mut u16, left: u8, right: u8, result: u8) {
    flags.set_bit(CARRY_FLAG, calc_byte_carry_bit(left, right));
    flags.set_bit(SIGN_FLAG, calc_byte_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == 0);
}

fn calc_sub_word_flags(flags: &mut u16, left: u16, right: u16, result: u16) {
    flags.set_bit(CARRY_FLAG, calc_word_borrow_bit(left, right));
    flags.set_bit(SIGN_FLAG, calc_word_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == 0);
}

fn calc_sub_byte_flags(flags: &mut u16, left: u8, right: u8, result: u8) {
    flags.set_bit(CARRY_FLAG, calc_byte_borrow_bit(left, right));
    flags.set_bit(SIGN_FLAG, calc_byte_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == 0);
}

fn inc_reg(reg: &mut u16, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_add(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_word_sign_bit(*reg));
    1
}

fn dec_reg(reg: &mut u16, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_sub(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_word_sign_bit(*reg));
    1
}

fn inc_byte(reg: &mut u8, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_add(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_byte_sign_bit(*reg));
    1
}

fn dec_byte(reg: &mut u8, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_sub(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_byte_sign_bit(*reg));
    1
}

fn push_reg(mem: &mut [u8], sp: &mut u16, reg: u16) -> u16 {
    *sp -= 2;
    write_word(mem, *sp as usize, reg);
    1
}

fn pop_reg(mem: &[u8], sp: &mut u16, reg: &mut u16) -> u16 {
    *reg = read_word(mem, *sp as usize);
    *sp += 2;
    1
}

fn mov_reg_imm_word(reg: &mut u16, imm: u16) -> u16 {
    *reg = imm;
    3
}
fn mov_reg_imm_byte(reg: &mut u16, imm: u8, high: bool) -> u16 {
    if high {
        (*reg).set_high(imm);
    } else {
        (*reg).set_low(imm);
    }
    2
}

fn parse_mod_rm_byte(modrm: u8) -> (u8, u8, u8) {
    // Mod R/M byte format
    // 00 | 000 | 000
    // Mod  Reg   R/M
    let id_mod = (modrm & 0xC0) >> 6;
    let id_reg = (modrm & 0x38) >> 3;
    let id_rm = modrm & 0x07;
    (id_mod, id_reg, id_rm)
}

impl Cpu {
    pub fn new(debug: bool, mem: Box<[u8]>) -> Cpu {
        Cpu {
            ax: 0,
            bx: 0,
            cx: 0,
            dx: 0,
            si: 0,
            di: 0,
            bp: 0,
            sp: 0x100,
            ip: 0,
            flag: 0,
            mem,
            debug,
        }
    }

    #[allow(unused)]
    pub fn new_with_mem_size(debug: bool, mem_size: usize) -> Cpu {
        Cpu::new(debug, vec![0; mem_size].into_boxed_slice())
    }

    pub fn emulate(&mut self) {
        loop {
            // println!("--------------");
            // println!("starting cycle");
            // self.dump_registers();
            // println!("--------------");
            self.do_cycle();
        }
    }

    pub fn dump_registers(&self) {
        println!(
            "ax: 0x{:X}    bx: 0x{:X}
cx: 0x{:X}    dx: 0x{:X}
si: 0x{:X}    di: 0x{:X}
bp: 0x{:X}    sp: 0x{:X}
ip: 0x{:X}",
            self.ax, self.bx, self.cx, self.dx, self.si, self.di, self.bp, self.sp, self.ip
        );
    }

    pub fn halt_cpu(&self) -> ! {
        println!("----------------------");
        println!("CPU halted");
        self.dump_registers();
        process::exit(0);
    }

    pub fn dump_video_ram(&self) {
        let mut j = 0;
        for i in 0x8000..0xA000 {
            let val = self.mem[i as usize];
            if val == 0 {
                print!(" ");
            } else {
                let c = char::from(val);
                print!("{}", c);
            }

            if j == 25 {
                println!();
                j = 0;
            } else {
                j += 1;
            }
        }
    }

    fn cpu_panic(&self, msg: &str) -> ! {
        println!("----------------------");
        self.dump_video_ram();
        self.dump_registers();
        panic!("{}", msg);
    }

    #[allow(unused)]
    fn cpu_debug_msg(&self, msg: &'static str) {
        if self.debug {
            println!("{}", msg);
        }
    }

    fn get_reg_word_code(&self, reg_code: u8) -> u16 {
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

    fn set_reg_word_code(&mut self, reg_code: u8, value: u16) {
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

    fn get_reg_byte_code(&self, reg_code: u8) -> u8 {
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

    fn set_reg_byte_code(&mut self, reg_code: u8, value: u8) {
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

    fn read_rm_address(&self, id_mod: u8, id_rm: u8) -> (u16, u16) {
        // Operand is a memory address.
        let ip = self.ip as usize;
        let mut ip_increment = 2;
        let mut address = match id_rm {
            0x00 => self.bx + self.si,
            0x01 => self.bx + self.di,
            0x02 => self.bp + self.si,
            0x03 => self.bp + self.di,
            0x04 => self.si,
            0x05 => self.di,
            0x06 if id_mod == 0 => read_word(&self.mem, ip + 2),
            0x06 => self.bp,
            0x07 => self.bx,
            _ => self.cpu_panic("do_word_inst: failed to parse R/M bits of mod R/M"),
        };

        // bp uses 16 bit displacement
        if id_rm == 0x06 && id_mod == 0 {
            ip_increment = 4;
        }

        if id_mod == 0x01 {
            let dc = self.mem[ip + 2] as i16;
            let mut signed_address = address as i16;
            signed_address += dc;
            address = signed_address as u16;
            ip_increment = 4;
        } else if id_mod == 0x02 {
            let dw = read_word(&self.mem, ip + 2) as i16;
            let mut signed_address = address as i16;
            signed_address += dw;
            address = signed_address as u16;
            ip_increment = 4;
        }
        (address, ip_increment)
    }

    // TODO: It shouldn't be hard to generalize do_byte_inst to accomodate this use case too
    // Only difference is we don't map 'reg' to a register value. grp2 instructions have specific
    // uses for the 'reg' value.
    fn do_grp2_inst<F>(&mut self, op: F) -> u16 
    where
        F: Fn(&mut u8, u8, &mut u16),
    {
        let ip = self.ip as usize;
        let modrm_byte = self.mem[ip + 1];
        let (id_mod, id_reg, id_rm) = parse_mod_rm_byte(modrm_byte);

        if id_mod < 0x03 {
            let (address, ip_increment) = self.read_rm_address(id_mod, id_rm);
            let mut rm = self.mem[address as usize];
            //let mut reg = self.get_reg_byte_code(id_reg);

            op(&mut rm, id_reg, &mut self.flag);

            self.mem[address as usize] = rm;
            //self.set_reg_byte_code(id_reg, reg);
            ip_increment
        } else {
            // Both operands are registers. Get their current values and pass to operation.
            let mut rm = self.get_reg_byte_code(id_rm);

            op(&mut rm, id_reg, &mut self.flag);

            // Update the register values.
            //self.set_reg_byte_code(id_reg, reg);
            self.set_reg_byte_code(id_rm, rm);
            2
        }
    }

    fn do_byte_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u8, &mut u8, &mut u16),
    {
        let ip = self.ip as usize;
        let modrm_byte = self.mem[ip + 1];
        let (id_mod, id_reg, id_rm) = parse_mod_rm_byte(modrm_byte);

        if id_mod < 0x03 {
            let (address, ip_increment) = self.read_rm_address(id_mod, id_rm);
            let mut rm = self.mem[address as usize];
            let mut reg = self.get_reg_byte_code(id_reg);

            op(&mut rm, &mut reg, &mut self.flag);

            self.mem[address as usize] = rm;
            self.set_reg_byte_code(id_reg, reg);
            ip_increment
        } else {
            // Both operands are registers. Get their current values and pass to operation.
            let mut reg = self.get_reg_byte_code(id_reg);
            let mut rm = self.get_reg_byte_code(id_rm);

            op(&mut rm, &mut reg, &mut self.flag);

            // TODO we have to look up the code twice. a bit inefficient.
            // Update the register values.
            self.set_reg_byte_code(id_reg, reg);
            self.set_reg_byte_code(id_rm, rm);
            2
        }
    }

    fn do_opext_inst(&mut self, word_inst: bool, imm_byte: bool) -> u16 {
        let ip = self.ip as usize;
        let modrm_byte = self.mem[ip + 1];
        let (id_mod, id_reg, id_rm) = parse_mod_rm_byte(modrm_byte);

        let byte_op: Box<dyn Fn(&mut u8, u8, &mut u16)> = match id_reg {
            0x00 => Box::new(|rm: &mut u8, imm: u8, flag: &mut u16| {
                // self.cpu_debug_msg("ADD Eb, Ib");
                let old = *rm;
                *rm = old.wrapping_add(imm);
                calc_add_byte_flags(flag, old, imm, *rm);
            }),
            0x01 => Box::new(|rm: &mut u8, imm: u8, flag: &mut u16| {
                // self.cpu_debug_msg("OR Eb, Ib");
                *rm |= imm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*rm));
            }),
            0x02 => Box::new(|rm: &mut u8, imm: u8, flag: &mut u16| {
                // self.cpu_debug_msg("ADC Eb, Ib");
                let old = *rm;
                let val = old.wrapping_add(imm);
                let c = flag.get_bit(CARRY_FLAG) as u8;
                *rm = val.wrapping_add(c);
                calc_add_byte_flags(flag, old, imm, *rm); // check?
            }),
            0x03 => Box::new(|rm: &mut u8, imm: u8, flag: &mut u16| {
                //self.cpu_debug_msg("SBB Eb, Ib");
                let old = *rm;
                let c = flag.get_bit(CARRY_FLAG) as u8;
                let val = imm.wrapping_add(c);
                *rm = val.wrapping_sub(c);
                calc_sub_byte_flags(flag, old, val, *rm); // check?
            }),
            0x04 => Box::new(|rm: &mut u8, imm: u8, flag: &mut u16| {
                //self.cpu_debug_msg("AND Eb, Ib");
                *rm &= imm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*rm));
            }),
            0x05 => Box::new(|rm: &mut u8, imm: u8, flag: &mut u16| {
                //self.cpu_debug_msg("SUB Eb, Ib");
                let old = *rm;
                *rm = old.wrapping_sub(imm);
                calc_sub_byte_flags(flag, old, imm, *rm);
            }),
            0x06 => Box::new(|rm: &mut u8, imm: u8, flag: &mut u16| {
                //self.cpu_debug_msg("XOR Eb, Ib");
                *rm ^= imm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*rm));
            }),
            0x07 => Box::new(|rm: &mut u8, imm: u8, flag: &mut u16| {
                //self.cpu_debug_msg("CMP Eb, Ib");
                let val = (*rm).wrapping_sub(imm);
                calc_sub_byte_flags(flag, *rm, imm, val);
            }),
            reg => self.cpu_panic(&format!(
                "Opext byte instruction: failed to parse REG: {}",
                reg
            )),
        };

        let word_op: Box<dyn Fn(&mut u16, u16, &mut u16)> = match id_reg {
            0x00 => Box::new(|rm: &mut u16, imm: u16, flag: &mut u16| {
                let old = *rm;
                *rm = old.wrapping_add(imm);
                calc_add_word_flags(flag, old, imm, *rm);
            }),
            0x01 => Box::new(|rm: &mut u16, imm: u16, flag: &mut u16| {
                *rm |= imm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*rm));
            }),
            0x02 => Box::new(|rm: &mut u16, imm: u16, flag: &mut u16| {
                let old = *rm;
                let val = old.wrapping_add(imm);
                let c = flag.get_bit(CARRY_FLAG) as u16;
                *rm = val.wrapping_add(c);
                calc_add_word_flags(flag, old, imm, *rm); // check?
            }),
            0x03 => Box::new(|rm: &mut u16, imm: u16, flag: &mut u16| {
                let old = *rm;
                let c = flag.get_bit(CARRY_FLAG) as u16;
                let val = imm.wrapping_add(c);
                *rm = val.wrapping_sub(c);
                calc_sub_word_flags(flag, old, val, *rm); // check?
            }),
            0x04 => Box::new(|rm: &mut u16, imm: u16, flag: &mut u16| {
                *rm &= imm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*rm));
            }),
            0x05 => Box::new(|rm: &mut u16, imm: u16, flag: &mut u16| {
                let old = *rm;
                *rm = old.wrapping_sub(imm);
                calc_sub_word_flags(flag, old, imm, *rm);
            }),
            0x06 => Box::new(|rm: &mut u16, imm: u16, flag: &mut u16| {
                *rm ^= imm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*rm));
            }),
            0x07 => Box::new(|rm: &mut u16, imm: u16, flag: &mut u16| {
                let val = (*rm).wrapping_sub(imm);
                calc_sub_word_flags(flag, *rm, imm, val);
            }),
            reg => self.cpu_panic(&format!(
                "Opext word instruction: failed to parse REG: {}",
                reg
            )),
        };

        if id_mod < 0x03 {
            // R/M is memory.
            let (address, ip_increment) = self.read_rm_address(id_mod, id_rm);

            if word_inst && !imm_byte {
                let imm = read_word(&self.mem, ip + 2);
                let mut rm = read_word(&self.mem, address as usize);
                word_op(&mut rm, imm, &mut self.flag);
                write_word(&mut self.mem, address as usize, rm);
            }
            if word_inst && imm_byte {
                let imm_byte = self.mem[ip + 2] as i8;
                let imm = imm_byte as i16;
                let mut rm = read_word(&self.mem, address as usize);
                word_op(&mut rm, imm as u16, &mut self.flag);
                write_word(&mut self.mem, address as usize, rm);
                // TODO check ip increment for this case..
            } else {
                let imm = self.mem[ip + 2];
                let mut rm = self.mem[address as usize];
                byte_op(&mut rm, imm, &mut self.flag);
                self.mem[address as usize] = rm;
            }
            ip_increment
        } else {
            // R/M is a register

            if word_inst && !imm_byte {
                let imm = read_word(&self.mem, ip + 2);
                let mut rm = self.get_reg_word_code(id_rm);
                word_op(&mut rm, imm, &mut self.flag);
                self.set_reg_word_code(id_rm, rm);
                4
            } else if word_inst && imm_byte {
                // NOTE byte is sign extended!
                let imm_byte = self.mem[ip + 2] as i8;
                let imm = imm_byte as i16; // sign extend
                let mut rm = self.get_reg_word_code(id_rm);
                word_op(&mut rm, imm as u16, &mut self.flag);
                self.set_reg_word_code(id_rm, rm);
                return 3;
            } else {
                let imm = self.mem[ip + 2];
                let mut rm = self.get_reg_byte_code(id_rm);
                byte_op(&mut rm, imm, &mut self.flag);
                self.set_reg_byte_code(id_rm, rm);
                return 2;
            }
        }
    }

    fn do_word_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u16, &mut u16, &mut u16),
    {
        let ip = self.ip as usize;
        let modrm_byte = self.mem[ip + 1];
        let (id_mod, id_reg, id_rm) = parse_mod_rm_byte(modrm_byte);

        if id_mod < 0x03 {
            let (address, ip_increment) = self.read_rm_address(id_mod, id_rm);

            let mut rm_value = read_word(&self.mem, address as usize);
            let mut reg = self.get_reg_word_code(id_reg);

            op(&mut rm_value, &mut reg, &mut self.flag);

            write_word(&mut self.mem, address as usize, rm_value);
            self.set_reg_word_code(id_reg, reg);

            ip_increment
        } else {
            // Both operands are registers. Get their current values and pass to operation.
            let mut reg = self.get_reg_word_code(id_reg);
            let mut rm = self.get_reg_word_code(id_rm);

            op(&mut rm, &mut reg, &mut self.flag);

            // TODO we have to look up the code twice. a bit inefficient.
            // Update the register values.
            self.set_reg_word_code(id_reg, reg);
            self.set_reg_word_code(id_rm, rm);

            2
        }
    }

    // TODO: again another function that should be possible to generalize
    fn do_rm_imm_word_inst<F>(&mut self, op: F) -> u16 
    where
        F: Fn(&mut u16, u16, &mut u16),
    {
        let ip = self.ip as usize;
        let modrm_byte = self.mem[ip + 1];
        let (id_mod, _, id_rm) = parse_mod_rm_byte(modrm_byte);
        //println!("ip: {}, modrm_byte: {:X}, mod: {:X}, rm: {:X}", ip, modrm_byte, id_mod, id_rm);
        if id_mod < 0x03 {
            let (address, ip_increment) = self.read_rm_address(id_mod, id_rm);
            let mut rm_value = read_word(&self.mem, address as usize);
            let imm = read_word(&self.mem, ip + ip_increment as usize);

            op(&mut rm_value, imm, &mut self.flag);

            write_word(&mut self.mem, address as usize, rm_value);

            ip_increment + 2 // adding two bytes for the immediate value
        } else {
            // Operand is a register
            let mut rm = self.get_reg_word_code(id_rm);
            // Immmediate value is 2 bytes after ip
            let imm = read_word(&self.mem, ip + 2);
            //println!("rm: {:X}, imm: {:X}", rm, imm);

            op(&mut rm, imm, &mut self.flag);

            // Update the register values.
            self.set_reg_word_code(id_rm, rm);

            4
        }
    }

    fn do_imm_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(u16, &[u8], &mut u16, &mut u16),
    {
        op(self.ip, &mut self.mem, &mut self.ax, &mut self.flag);
        2
    }

    fn do_sp_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut [u8], &mut u16),
    {
        op(&mut self.mem, &mut self.sp);
        1
    }

    // I don't know what to call this
    fn do_ip_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&[u8], &mut u16, u16),
    {
        op(&self.mem, &mut self.ip, self.flag);
        2
    }

    pub fn do_cycle(&mut self) {
        let opcode = self.mem[self.ip as usize];
        let ip_increment = match opcode {
            0x00 => self.do_byte_inst(|rm, reg, flag| {
                let old = *rm;
                *rm = old.wrapping_add(*reg);
                calc_add_byte_flags(flag, old, *reg, *rm);
            }),
            0x01 => self.do_word_inst(|rm, reg, flag| {
                let old = *rm;
                *rm = old.wrapping_add(*reg);
                calc_add_word_flags(flag, old, *reg, *rm);
            }),
            0x02 => self.do_byte_inst(|rm, reg, flag| {
                let old = *reg;
                *reg = old.wrapping_add(*rm);
                calc_add_byte_flags(flag, old, *rm, *reg);
            }),
            0x03 => self.do_word_inst(|rm, reg, flag| {
                let old = *reg;
                *reg = old.wrapping_add(*rm);
                calc_add_word_flags(flag, old, *rm, *reg);
            }),
            0x04 => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = mem[(ip + 1) as usize];
                let al = ax.get_low();
                let val = al + imm; // TODO wrapping add
                ax.set_low(val);
                calc_add_byte_flags(flag, al, imm, val);
            }),
            0x05 => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let old_ax = *ax;
                let val = old_ax + imm; // TODO wrapping add
                *ax = val;
                calc_add_word_flags(flag, old_ax, imm, val);
            }),
            // 0x06 =>
            // 0x07 =>
            0x08 => self.do_byte_inst(|rm, reg, flag| {
                *rm |= *reg;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*rm));
                // todo clear overflow flag
            }),
            0x09 => self.do_word_inst(|rm, reg, flag| {
                *rm |= *reg;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*rm));
                // todo clear overflow flag
            }),
            0x0A => self.do_byte_inst(|rm, reg, flag| {
                *reg |= *rm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *reg == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*reg));
                // todo clear overflow flag
            }),
            0x0B => self.do_word_inst(|rm, reg, flag| {
                *reg |= *rm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *reg == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*reg));
                // todo clear overflow flag
            }),
            0x0C => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = mem[(ip + 1) as usize];
                let val = ax.get_low() | imm;
                ax.set_low(val);
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, val == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(val));
                // todo clear overflow flag
            }),
            0x0D => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let val = *ax | imm;
                *ax = val;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, val == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(val));
                // todo clear overflow flag
            }),
            0x10 => self.do_byte_inst(|rm, reg, flag| {
                let old = *rm;
                let val = old.wrapping_add(*reg);
                let c = flag.get_bit(CARRY_FLAG) as u8;
                *rm = val.wrapping_add(c);
                calc_add_byte_flags(flag, old, *reg, *rm); // check?
            }),
            0x11 => self.do_word_inst(|rm, reg, flag| {
                let old = *rm;
                let val = old.wrapping_add(*reg);
                let c = flag.get_bit(CARRY_FLAG) as u16;
                *rm = val.wrapping_add(c);
                calc_add_word_flags(flag, old, *reg, *rm); // check?
            }),
            0x12 => self.do_byte_inst(|rm, reg, flag| {
                let old = *reg;
                let val = old.wrapping_add(*rm);
                let c = flag.get_bit(CARRY_FLAG) as u8;
                *reg = val.wrapping_add(c);
                calc_add_byte_flags(flag, old, *rm, *reg); // check?
            }),
            0x13 => self.do_word_inst(|rm, reg, flag| {
                let old = *reg;
                let val = old.wrapping_add(*rm);
                let c = flag.get_bit(CARRY_FLAG) as u16;
                *reg = val.wrapping_add(c);
                calc_add_word_flags(flag, old, *rm, *reg); // check?
            }),
            0x14 => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = mem[(ip + 1) as usize];
                let c = flag.get_bit(CARRY_FLAG) as u8;
                let old = ax.get_low();
                let val = old + imm + c; // TODO wrapping add
                ax.set_low(val);
                calc_add_byte_flags(flag, old, imm + c, val); // check?
            }),
            0x15 => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let c = flag.get_bit(CARRY_FLAG) as u16;
                let old = *ax;
                let val = old + imm + c; // TODO wrapping add
                *ax = val;
                calc_add_word_flags(flag, old, imm + c, val); // check?
            }),
            0x18 => self.do_byte_inst(|rm, reg, flag| {
                let old = *rm;
                let c = flag.get_bit(CARRY_FLAG) as u8;
                let val = (*reg).wrapping_add(c);
                *rm = old.wrapping_sub(val);
                calc_sub_byte_flags(flag, old, val, *rm); // check?
            }),
            0x19 => self.do_word_inst(|rm, reg, flag| {
                let old = *rm;
                let c = flag.get_bit(CARRY_FLAG) as u16;
                let val = (*reg).wrapping_add(c);
                *rm = old.wrapping_sub(val);
                calc_sub_word_flags(flag, old, val, *rm); // check?
            }),
            0x1A => self.do_byte_inst(|rm, reg, flag| {
                let old = *reg;
                let c = flag.get_bit(CARRY_FLAG) as u8;
                let val = (*rm).wrapping_add(c);
                *reg = old.wrapping_sub(val);
                calc_sub_byte_flags(flag, old, val, *reg); // check?
            }),
            0x1B => self.do_word_inst(|rm, reg, flag| {
                let old = *reg;
                let c = flag.get_bit(CARRY_FLAG) as u16;
                let val = (*rm).wrapping_add(c);
                *reg = old.wrapping_sub(val);
                calc_sub_word_flags(flag, old, val, *reg); // check?
            }),
            0x1C => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = mem[(ip + 1) as usize];
                let c = flag.get_bit(CARRY_FLAG) as u8;
                let old = ax.get_low();
                let val = old - (imm + c); // TODO wrapping add
                ax.set_low(val);
                calc_add_byte_flags(flag, old, imm + c, val); // check?
            }),
            0x1D => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let c = flag.get_bit(CARRY_FLAG) as u16;
                let old = *ax;
                let val = old + (imm + c); // TODO wrapping add
                *ax = val;
                calc_add_word_flags(flag, old, imm + c, val); // check?
            }),
            0x20 => self.do_byte_inst(|rm, reg, flag| {
                *rm &= *reg;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*rm));
                // todo clear overflow flag
            }),
            0x21 => self.do_word_inst(|rm, reg, flag| {
                *rm &= *reg;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*rm));
                // todo clear overflow flag
            }),
            0x22 => self.do_byte_inst(|rm, reg, flag| {
                *reg &= *rm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *reg == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*reg));
                // todo clear overflow flag
            }),
            0x23 => self.do_word_inst(|rm, reg, flag| {
                *reg &= *rm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *reg == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*reg));
                // todo clear overflow flag
            }),
            0x24 => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = mem[(ip + 1) as usize];
                let val = ax.get_low() & imm;
                ax.set_low(val);
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, val == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(val));
                // todo clear overflow flag
            }),
            0x25 => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let val = *ax & imm;
                *ax = val;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, val == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(val));
                // todo clear overflow flag
            }),
            0x28 => self.do_byte_inst(|rm, reg, flag| {
                let old = *rm;
                *rm = old.wrapping_sub(*reg);
                calc_sub_byte_flags(flag, old, *reg, *rm);
            }),
            0x29 => self.do_word_inst(|rm, reg, flag| {
                let old = *rm;
                *rm = old.wrapping_sub(*reg);
                calc_sub_word_flags(flag, old, *reg, *rm);
            }),
            0x2A => self.do_byte_inst(|rm, reg, flag| {
                let old = *reg;
                *reg = old.wrapping_sub(*rm);
                calc_sub_byte_flags(flag, old, *rm, *reg);
            }),
            0x2B => self.do_word_inst(|rm, reg, flag| {
                let old = *reg;
                *reg = old.wrapping_sub(*rm);
                calc_sub_word_flags(flag, old, *rm, *reg);
            }),
            0x2C => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = mem[(ip + 1) as usize];
                let al = ax.get_low();
                let val = al - imm; // TODO wrapping sub
                ax.set_low(val);
                calc_sub_byte_flags(flag, al, imm, val);
            }),
            0x2D => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let old_ax = *ax;
                let val = old_ax - imm; // TODO wrapping add
                *ax = val;
                calc_sub_word_flags(flag, old_ax, imm, val);
            }),
            0x30 => self.do_byte_inst(|rm, reg, flag| {
                *rm ^= *reg;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*rm));
                // todo clear overflow flag
            }),
            0x31 => self.do_word_inst(|rm, reg, flag| {
                *rm ^= *reg;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *rm == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*rm));
                // todo clear overflow flag
            }),
            0x32 => self.do_byte_inst(|rm, reg, flag| {
                *reg ^= *rm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *reg == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(*reg));
                // todo clear overflow flag
            }),
            0x33 => self.do_word_inst(|rm, reg, flag| {
                *reg ^= *rm;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, *reg == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(*reg));
                // todo clear overflow flag
            }),
            0x34 => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = mem[(ip + 1) as usize];
                let val = ax.get_low() ^ imm;
                ax.set_low(val);
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, val == 0);
                flag.set_bit(SIGN_FLAG, calc_byte_sign_bit(val));
                // todo clear overflow flag
            }),
            0x35 => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let val = *ax ^ imm;
                *ax = val;
                flag.set_bit(CARRY_FLAG, false);
                flag.set_bit(ZERO_FLAG, val == 0);
                flag.set_bit(SIGN_FLAG, calc_word_sign_bit(val));
                // todo clear overflow flag
            }),
            0x38 => self.do_byte_inst(|rm, reg, flag| {
                let val = (*rm).wrapping_sub(*reg);
                calc_sub_byte_flags(flag, *rm, *reg, val);
            }),
            0x39 => self.do_word_inst(|rm, reg, flag| {
                let val = (*rm).wrapping_sub(*reg);
                calc_sub_word_flags(flag, *rm, *reg, val);
            }),
            0x3A => self.do_byte_inst(|rm, reg, flag| {
                let val = (*reg).wrapping_sub(*rm);
                calc_sub_byte_flags(flag, *rm, *reg, val);
            }),
            0x3B => self.do_word_inst(|rm, reg, flag| {
                let val = (*reg).wrapping_sub(*rm);
                calc_sub_word_flags(flag, *rm, *reg, val);
            }),
            0x3C => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = mem[(ip + 1) as usize];
                let val = ax.get_low().wrapping_sub(imm);
                calc_sub_byte_flags(flag, ax.get_low(), imm, val);
            }),
            0x3D => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let val = (*ax).wrapping_sub(imm);
                calc_sub_word_flags(flag, *ax, imm, val);
            }),
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
            0x50 => push_reg(&mut self.mem, &mut self.sp, self.ax),
            0x51 => push_reg(&mut self.mem, &mut self.sp, self.cx),
            0x52 => push_reg(&mut self.mem, &mut self.sp, self.dx),
            0x53 => push_reg(&mut self.mem, &mut self.sp, self.bx),
            0x54 => self.do_sp_inst(|mem: &mut [u8], sp: &mut u16| {
                // handle SP as a special case to make borrow checker happy
                // TODO: 286 handles this differently..
                *sp -= 2;
                write_word(mem, *sp as usize, *sp);
            }),
            0x55 => push_reg(&mut self.mem, &mut self.sp, self.bp),
            0x56 => push_reg(&mut self.mem, &mut self.sp, self.si),
            0x57 => push_reg(&mut self.mem, &mut self.sp, self.di),
            0x58 => pop_reg(&self.mem, &mut self.sp, &mut self.ax),
            0x59 => pop_reg(&self.mem, &mut self.sp, &mut self.cx),
            0x5A => pop_reg(&self.mem, &mut self.sp, &mut self.dx),
            0x5B => pop_reg(&self.mem, &mut self.sp, &mut self.bx),
            0x5C => self.do_sp_inst(|mem: &mut [u8], sp: &mut u16| {
                // handle SP as a special case to make borrow checker happy
                // TODO: 286 handles this differently..
                *sp = read_word(mem, *sp as usize);
                *sp += 2;
            }),
            0x5D => pop_reg(&self.mem, &mut self.sp, &mut self.bp),
            0x5E => pop_reg(&self.mem, &mut self.sp, &mut self.si),
            0x5F => pop_reg(&self.mem, &mut self.sp, &mut self.di),
            // TODO this can be generalized a bit...
            0x72 => self.do_ip_inst(|mem: &[u8], ip: &mut u16, flag: u16| {
                if flag.get_bit(CARRY_FLAG) {
                    let mut sip = *ip as i16;
                    sip += (mem[(*ip + 1) as usize] as i8) as i16;
                    *ip = sip as u16;
                }
            }),
            0x74 => self.do_ip_inst(|mem: &[u8], ip: &mut u16, flag: u16| {
                if flag.get_bit(ZERO_FLAG) {
                    let mut sip = *ip as i16;
                    sip += (mem[(*ip + 1) as usize] as i8) as i16;
                    *ip = sip as u16;
                }
            }),
            0x75 => self.do_ip_inst(|mem: &[u8], ip: &mut u16, flag: u16| {
                if !flag.get_bit(ZERO_FLAG) {
                    let mut sip = *ip as i16;
                    sip += (mem[(*ip + 1) as usize] as i8) as i16;
                    *ip = sip as u16;
                }
            }),
            0x76 => self.do_ip_inst(|mem: &[u8], ip: &mut u16, flag: u16| {
                if flag.get_bit(CARRY_FLAG) || flag.get_bit(ZERO_FLAG) {
                    let mut sip = *ip as i16;
                    sip += (mem[(*ip + 1) as usize] as i8) as i16;
                    *ip = sip as u16;
                }
            }),
            0x77 => self.do_ip_inst(|mem: &[u8], ip: &mut u16, flag: u16| {
                if !flag.get_bit(CARRY_FLAG) && !flag.get_bit(ZERO_FLAG) {
                    let mut sip = *ip as i16;
                    sip += (mem[(*ip + 1) as usize] as i8) as i16;
                    *ip = sip as u16;
                }
            }),
            0x78 => self.do_ip_inst(|mem: &[u8], ip: &mut u16, flag: u16| {
                if flag.get_bit(SIGN_FLAG) {
                    let mut sip = *ip as i16;
                    sip += (mem[(*ip + 1) as usize] as i8) as i16;
                    *ip = sip as u16;
                }
            }),
            0x79 => self.do_ip_inst(|mem: &[u8], ip: &mut u16, flag: u16| {
                if !flag.get_bit(SIGN_FLAG) {
                    let mut sip = *ip as i16;
                    sip += (mem[(*ip + 1) as usize] as i8) as i16;
                    *ip = sip as u16;
                }
            }),
            0x80 => self.do_opext_inst(false, false),
            0x81 => self.do_opext_inst(true, false),
            0x82 => self.do_opext_inst(false, false),
            0x83 => self.do_opext_inst(true, true),
            0x86 => self.do_byte_inst(|rm, reg, _flag| {
                mem::swap(rm, reg);
            }),
            0x87 => self.do_word_inst(|rm, reg, _flag| {
                mem::swap(rm, reg);
            }),
            0x88 => self.do_byte_inst(|rm, reg, _flag| {
                *rm = *reg;
            }),
            0x89 => self.do_word_inst(|rm, reg, _flag| {
                *rm = *reg;
            }),
            0x8A => self.do_byte_inst(|rm, reg, _flag| {
                *reg = *rm;
            }),
            0x8B => self.do_word_inst(|rm, reg, _flag| {
                *reg = *rm;
            }),
            0x90 => 1,
            0xB0 => mov_reg_imm_byte(&mut self.ax, self.mem[(self.ip + 1) as usize], false),
            0xB1 => mov_reg_imm_byte(&mut self.cx, self.mem[(self.ip + 1) as usize], false),
            0xB2 => mov_reg_imm_byte(&mut self.dx, self.mem[(self.ip + 1) as usize], false),
            0xB3 => mov_reg_imm_byte(&mut self.bx, self.mem[(self.ip + 1) as usize], false),
            0xB4 => mov_reg_imm_byte(&mut self.ax, self.mem[(self.ip + 1) as usize], true),
            0xB5 => mov_reg_imm_byte(&mut self.cx, self.mem[(self.ip + 1) as usize], true),
            0xB6 => mov_reg_imm_byte(&mut self.dx, self.mem[(self.ip + 1) as usize], true),
            0xB7 => mov_reg_imm_byte(&mut self.bx, self.mem[(self.ip + 1) as usize], true),
            0xB8 => mov_reg_imm_word(&mut self.ax, read_word(&self.mem, (self.ip + 1) as usize)),
            0xB9 => mov_reg_imm_word(&mut self.cx, read_word(&self.mem, (self.ip + 1) as usize)),
            0xBA => mov_reg_imm_word(&mut self.dx, read_word(&self.mem, (self.ip + 1) as usize)),
            0xBB => mov_reg_imm_word(&mut self.bx, read_word(&self.mem, (self.ip + 1) as usize)),
            0xBC => mov_reg_imm_word(&mut self.sp, read_word(&self.mem, (self.ip + 1) as usize)),
            0xBD => mov_reg_imm_word(&mut self.bp, read_word(&self.mem, (self.ip + 1) as usize)),
            0xBE => mov_reg_imm_word(&mut self.si, read_word(&self.mem, (self.ip + 1) as usize)),
            0xBF => mov_reg_imm_word(&mut self.di, read_word(&self.mem, (self.ip + 1) as usize)),
            0xC3 => (|mem, ip: &mut u16, sp: &mut u16| {
                // RET
                pop_reg(mem, sp, ip);
                0
            })(&self.mem, &mut self.ip, &mut self.sp),
            0xC7 => self.do_rm_imm_word_inst(|rm, imm, _flags| {
                *rm = imm;
            }),
            0xE8 => (|mem: &mut [u8], ip: &mut u16, sp: &mut u16| {
                // CALL
                let return_address = *ip + 3; // next instruction after this one (3 bytes after)
                push_reg(mem, sp, return_address);
                // add the displacement to IP
                *ip += read_word(mem, (*ip as usize) + 1);
                3
            })(&mut self.mem, &mut self.ip, &mut self.sp),
            0xE9 => (|mem, ip: &mut u16| {
                *ip += read_word(mem, (*ip as usize) + 1);
                3
            })(&mut self.mem, &mut self.ip),
            0xEB => (|mem: &mut [u8], ip: &mut u16| {
                let sval = mem[(*ip as usize) + 1] as i16;
                let sip = (*ip as i16) + sval;
                *ip = sip as u16;
                2
            })(&mut self.mem, &mut self.ip),
            0xF4 => self.halt_cpu(),
            0xF8 => (|flag: &mut u16| {
                flag.set_bit(CARRY_FLAG, false);
                1
            })(&mut self.flag),
            0xF9 => (|flag: &mut u16| {
                flag.set_bit(CARRY_FLAG, true);
                1
            })(&mut self.flag),
            0xFE => self.do_grp2_inst(|rm, reg, flags| {
                let _ = match reg {
                    0x00 => inc_byte(rm, flags),
                    0x01 => dec_byte(rm, flags),
                    _ => panic!("inc/dec: unexpected reg value")
                };
            }),

            inst => self.cpu_panic(&format!("Unknown instruction: 0x{:X}", inst)),
        };
        self.ip += ip_increment;
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::*;
    use crate::Cpu;

    const TEST_MEM_SIZE: usize = 65535;

    #[test]
    fn cpu_sanity() {
        // NOP
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.mem[0] = 0x90;
        cpu.do_cycle();
        assert_eq!(cpu.ip, 1);
        assert_eq!(cpu.ax, 0);
        assert_eq!(cpu.bx, 0);
        assert_eq!(cpu.cx, 0);
        assert_eq!(cpu.dx, 0);
        assert_eq!(cpu.bp, 0);
        assert_eq!(cpu.sp, 0x0100);
        assert_eq!(cpu.flag, 0);

        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 69;
        cpu.cx = 42;
        cpu.dx = 1;
        cpu.ip = 0;
        cpu.mem[0] = 0x01; // ADD
        cpu.mem[1] = 0b11000010; // ADD DX, AX
        cpu.mem[2] = 0x01; // ADD
        cpu.mem[3] = 0b00001110; // ADD [0x102], CX
        cpu.mem[4] = 102; // Address
        cpu.mem[5] = 0;

        // Flag tests
        cpu.mem[6] = 0x01;
        cpu.mem[7] = 0b11000010; // ADD DX, AX
        cpu.mem[8] = 0x01;
        cpu.mem[9] = 0b11000010; // ADD DX, AX
        cpu.mem[10] = 0x01;
        cpu.mem[11] = 0b11000010; // ADD DX, AX
        cpu.mem[102] = 42;
        cpu.do_cycle();
        assert_eq!(cpu.dx, 70);
        assert_eq!(cpu.ip, 2);
        assert!(!cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(CARRY_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));
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
        assert!(cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(CARRY_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));

        // sign flag test
        cpu.dx = 32766;
        cpu.ax = 10;
        cpu.do_cycle();
        assert_eq!(cpu.dx, 32776);
        assert_eq!(cpu.ax, 10);
        assert!(!cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(CARRY_FLAG));
        assert!(cpu.flag.get_bit(SIGN_FLAG));

        cpu.dx = 65534;
        cpu.do_cycle();
        assert_eq!(cpu.dx, 8);
        assert_eq!(cpu.ax, 10);
        assert!(!cpu.flag.get_bit(ZERO_FLAG));
        assert!(cpu.flag.get_bit(CARRY_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));

        // ADD AX, BX
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0x1234;
        cpu.bx = 0x5678;
        cpu.flag.set_bit(CARRY_FLAG, true);
        cpu.mem[0] = 0x03;
        cpu.mem[1] = 0xc3;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x68AC);
        assert_eq!(cpu.bx, 0x5678);
        assert_eq!(cpu.ip, 0x02);
        assert!(!cpu.flag.get_bit(CARRY_FLAG));

        // MOV [BX+SI+0x1234], AX
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0xA55A;
        cpu.bx = 0x1000;
        cpu.si = 0x2000;
        cpu.mem[0x4234] = 0;
        cpu.mem[0] = 0x89;
        cpu.mem[1] = 0x80;
        cpu.mem[2] = 0x34;
        cpu.mem[3] = 0x12;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0xA55A);
        assert_eq!(cpu.bx, 0x1000);
        assert_eq!(cpu.si, 0x2000);
        assert_eq!(cpu.ip, 0x04);
        assert_eq!(read_word(&cpu.mem, 0x4234), 0xA55A);
    }

    #[test]
    fn cpu_byte_sanity() {
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 27;
        cpu.mem[0] = 0x00;
        cpu.mem[1] = 0x06;
        cpu.mem[2] = 0x40;
        cpu.mem[0x40] = 10;
        cpu.do_cycle();
        assert_eq!(cpu.mem[0x40], 37);
        assert!(!cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(CARRY_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));

        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.cx = 245;
        cpu.mem[0] = 0x00;
        cpu.mem[1] = 0x0E;
        cpu.mem[2] = 0x40;
        cpu.mem[0x40] = 15;
        cpu.do_cycle();
        assert_eq!(cpu.mem[0x40], 4);
        assert!(!cpu.flag.get_bit(ZERO_FLAG));
        assert!(cpu.flag.get_bit(CARRY_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));

        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.bx = 125;
        cpu.mem[0] = 0x00;
        cpu.mem[1] = 0x1E;
        cpu.mem[2] = 0x40;
        cpu.mem[0x40] = 10;
        cpu.do_cycle();
        assert_eq!(cpu.mem[0x40], 135);
        assert!(!cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(CARRY_FLAG));
        assert!(cpu.flag.get_bit(SIGN_FLAG));

        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.mem[0] = 0x04;
        cpu.mem[1] = 1;
        cpu.ax = 0x3445;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x3446);
    }

    #[test]
    fn cpu_mov_sanity() {
        // MOV AX, 0x1234
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.mem[0] = 0xB8;
        cpu.mem[1] = 0x34;
        cpu.mem[2] = 0x12;
        let old_flags = cpu.flag;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x1234);
        assert_eq!(cpu.ip, 0x03);
        assert_eq!(cpu.flag, old_flags);
        
        // MOV [0x1234], 0x5678
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.mem[0] = 0xC7;
        cpu.mem[1] = 0x06;
        cpu.mem[2] = 0x34;
        cpu.mem[3] = 0x12;
        cpu.mem[4] = 0x78;
        cpu.mem[5] = 0x56;
        let old_flags = cpu.flag;
        cpu.do_cycle();
        assert_eq!(read_word(&cpu.mem, 0x1234), 0x5678);
        assert_eq!(cpu.flag, old_flags);
        
        // MOV BX, 0x1234
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
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

        // MOV [0x1234], AX
        // A3 not implemented yet
        /*
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
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
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
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
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
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
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0xFFFF;
        cpu.bx = 0x0001;
        cpu.mem[0] = 0x01;
        cpu.mem[1] = 0xD8;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.get_bit(CARRY_FLAG));
        assert!(cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));

        // STC
        // ADC AX, BX
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0x0001;
        cpu.bx = 0x0001;
        cpu.mem[0] = 0xF9;
        cpu.mem[1] = 0x11;
        cpu.mem[2] = 0xD8;
        cpu.do_cycle();
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0003);
        assert_eq!(cpu.ip, 3);
        assert!(!cpu.flag.get_bit(CARRY_FLAG));

        // SUB AX, BX
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0x0001;
        cpu.bx = 0x0002;
        cpu.mem[0] = 0x29;
        cpu.mem[1] = 0xD8;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0xFFFF);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.get_bit(CARRY_FLAG));
        assert!(cpu.flag.get_bit(SIGN_FLAG));
        assert!(!cpu.flag.get_bit(ZERO_FLAG));
    }

    #[test]
    fn cpu_logical_op_sanity() {
        // AND AX, BX
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0xFF00;
        cpu.bx = 0x00FF;
        cpu.mem[0] = 0x21;
        cpu.mem[1] = 0xD8;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.bx, 0x00FF);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));
        assert!(!cpu.flag.get_bit(CARRY_FLAG));

        // XOR AX, BX
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0x1234;
        cpu.bx = 0x1234;
        cpu.mem[0] = 0x31;
        cpu.mem[1] = 0xD8;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.bx, 0x1234);
        assert_eq!(cpu.ip, 2);
        assert!(cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));
        assert!(!cpu.flag.get_bit(CARRY_FLAG));
    }

    #[test]
    fn cpu_inc_dec_sanity() {
        // INC AX
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0xFFFF;
        cpu.mem[0] = 0x40;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 1);
        assert!(cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));

        // DEC AX
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0x00001;
        cpu.mem[0] = 0x48;
        cpu.do_cycle();
        assert_eq!(cpu.ax, 0x0000);
        assert_eq!(cpu.ip, 1);
        assert!(cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));
        
        // INC BYTE PTR [0x1234]
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.mem[0] = 0xFE;
        cpu.mem[1] = 0x06;
        cpu.mem[2] = 0x34;
        cpu.mem[3] = 0x12;
        cpu.mem[0x1234] = 0xFF;
        cpu.do_cycle();
        assert_eq!(cpu.mem[0x1234], 0);
        assert!(cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));
        assert!(!cpu.flag.get_bit(CARRY_FLAG));
        
        // DEC AL
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0x6901;
        cpu.mem[0] = 0xFE;
        cpu.mem[1] = 0xC8;
        cpu.do_cycle();
        println!("{0:X}", cpu.ax);
        assert_eq!(cpu.ax, 0x6900);
        assert!(cpu.flag.get_bit(ZERO_FLAG));
        assert!(!cpu.flag.get_bit(SIGN_FLAG));
    }

    #[test]
    fn cpu_jmp_sanity() {
        // CMP AX, BX
        // JZ +2
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.ax = 0x1234;
        cpu.bx = 0x1234;
        cpu.mem[0] = 0x3B;
        cpu.mem[1] = 0xC3;
        cpu.mem[2] = 0x74;
        cpu.mem[3] = 0x02;
        cpu.do_cycle();
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x0006);
        assert!(cpu.flag.get_bit(ZERO_FLAG));

        // JMP +2
        cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.mem[0] = 0xEB;
        cpu.mem[1] = 0x02;
        cpu.do_cycle();
        assert_eq!(cpu.ip, 0x0004);
    }

    #[test]
    fn cpu_subroutine_sanity() {
        // CALL 0x0005
        // RET
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
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
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
        cpu.mem[0] = 0xF9;
        cpu.mem[1] = 0xF8;
        cpu.do_cycle();
        assert!(cpu.flag.get_bit(CARRY_FLAG));
        cpu.do_cycle();
        assert!(!cpu.flag.get_bit(CARRY_FLAG));
    }

    #[test]
    fn mem_read_write_test() {
        let mut mem = vec![0; TEST_MEM_SIZE];
        mem[0x44] = 0xFF;
        mem[0x45] = 0x13;
        mem[0x46] = 0x33;
        mem[0x47] = 0x12;

        assert_eq!(read_word(&mem, 0x45), 0x3313);
        write_word(&mut mem, 0x45, 0x1014);
        assert_eq!(mem[0x44], 0xFF);
        assert_eq!(mem[0x45], 0x14);
        assert_eq!(mem[0x46], 0x10);
        assert_eq!(mem[0x47], 0x12);
        assert_eq!(read_word(&mem, 0x45), 0x1014);
    }

    #[test]
    fn calc_word_sign_bit_test() {
        assert!(calc_word_sign_bit(0xFFFF));
        assert!(!calc_word_sign_bit(0x0000));
        assert!(calc_word_sign_bit(0x8000));
        assert!(!calc_word_sign_bit(0x1000));
        assert!(!calc_word_sign_bit(0x0001));
        assert!(!calc_word_sign_bit(0x0008));
        assert!(!calc_word_sign_bit(0x0080));
        assert!(!calc_word_sign_bit(0x0800));
    }

    #[test]
    fn calc_byte_sign_bit_test() {
        assert!(!calc_byte_sign_bit(0x01));
        assert!(!calc_byte_sign_bit(0x08));
        assert!(calc_byte_sign_bit(0x80));
        assert!(!calc_byte_sign_bit(0x10));
    }
}
