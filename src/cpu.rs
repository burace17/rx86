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

fn inc_reg(reg: &mut u16) -> u16 {
    *reg = (*reg).wrapping_add(1);
    1
}

fn dec_reg(reg: &mut u16) -> u16 {
    *reg = (*reg).wrapping_sub(1);
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
            0x06 if id_mod > 0 => self.bp,
            0x07 => self.bx,
            _ => self.cpu_panic("do_word_inst: failed to parse R/M bits of mod R/M"),
        };

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

    fn do_byte_inst<F>(&mut self, op: F) -> u16
    where
        F: Fn(&mut u8, &mut u8, &mut u16),
    {
        let ip = self.ip as usize;
        let modrm_byte = self.mem[ip + 1];

        // Mod R/M byte format
        // 00 | 000 | 000
        // Mod  Reg   R/M
        let id_mod = (modrm_byte & 0xC0) >> 6;
        let id_reg = (modrm_byte & 0x38) >> 3;
        let id_rm = modrm_byte & 0x07;
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

        // Mod R/M byte format
        // 00 | 000 | 000
        // Mod  Reg   R/M
        let id_mod = (modrm_byte & 0xC0) >> 6;
        let id_reg = (modrm_byte & 0x38) >> 3;
        let id_rm = modrm_byte & 0x07;

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

        // Mod R/M byte format
        // 00 | 000 | 000
        // Mod  Reg   R/M
        let id_mod = (modrm_byte & 0xC0) >> 6;
        let id_reg = (modrm_byte & 0x38) >> 3;
        let id_rm = modrm_byte & 0x07;

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
                let val = ax.get_low() - imm; // TODO wrapping
                calc_sub_byte_flags(flag, ax.get_low(), imm, val);
            }),
            0x3D => self.do_imm_inst(|ip, mem: &[u8], ax: &mut u16, flag: &mut u16| {
                let imm = read_word(mem, (ip + 1) as usize);
                let val = *ax - imm; // TODO wrapping
                calc_sub_word_flags(flag, *ax, imm, val);
            }),
            0x40 => inc_reg(&mut self.ax), // TODO set flags...
            0x41 => inc_reg(&mut self.cx),
            0x42 => inc_reg(&mut self.dx),
            0x43 => inc_reg(&mut self.bx),
            0x44 => inc_reg(&mut self.sp),
            0x45 => inc_reg(&mut self.bp),
            0x46 => inc_reg(&mut self.si),
            0x47 => inc_reg(&mut self.di),
            0x48 => dec_reg(&mut self.ax),
            0x49 => dec_reg(&mut self.cx),
            0x4A => dec_reg(&mut self.dx),
            0x4B => dec_reg(&mut self.bx),
            0x4C => dec_reg(&mut self.sp),
            0x4D => dec_reg(&mut self.bp),
            0x4E => dec_reg(&mut self.si),
            0x4F => dec_reg(&mut self.di),
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
                *ip = read_word(mem, *sp as usize);
                *sp += 2;
                0
            })(&self.mem, &mut self.ip, &mut self.sp),
            0xE8 => (|mem: &mut [u8], ip: &mut u16, sp: &mut u16| {
                *sp -= 2;
                let old = *ip;
                write_word(mem, *sp as usize, old + 3);
                *ip += read_word(mem, (old as usize) + 1);
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

            inst => self.cpu_panic(&format!("Unknown instruction: 0x{:X}", inst)),
        };
        self.ip += ip_increment;
    }
}

trait Bits {
    fn get_low(&self) -> u8;
    fn get_high(&self) -> u8;
    fn set_low(&mut self, value: u8);
    fn set_high(&mut self, value: u8);
    fn get_bit(&self, n: u16) -> bool;
    fn set_bit(&mut self, n: u16, val: bool);
}

impl Bits for u16 {
    fn get_low(&self) -> u8 {
        (self & 0x00FF) as u8
    }
    fn get_high(&self) -> u8 {
        ((self & 0xFF00) >> 8) as u8
    }
    fn set_low(&mut self, value: u8) {
        *self &= !0 << 8;
        *self |= value as u16;
    }
    fn set_high(&mut self, value: u8) {
        *self &= !0 >> 8;
        *self |= (value as u16) << 8;
    }
    fn get_bit(&self, n: u16) -> bool {
        ((1 << n) & self) != 0
    }
    fn set_bit(&mut self, n: u16, val: bool) {
        if val {
            *self |= 1 << n;
        } else {
            *self &= !(1 << n);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::*;
    use crate::Cpu;

    const TEST_MEM_SIZE: usize = 65535;

    #[test]
    fn cpu_sanity() {
        let mut cpu = Cpu::new_with_mem_size(false, TEST_MEM_SIZE);
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
    fn traits_test() {
        let mut test: u16 = 0x41F4;
        assert_eq!(test.get_low(), 0xF4);
        assert_eq!(test.get_high(), 0x41);
        test.set_low(0xEF);
        assert_eq!(test, 0x41EF);
        test.set_high(0xBE);
        assert_eq!(test, 0xBEEF);
        assert_eq!(test.get_low(), 0xEF);
        assert_eq!(test.get_high(), 0xBE);

        test = 0b0101;
        assert!(test.get_bit(0));
        assert!(!test.get_bit(1));
        assert!(test.get_bit(2));

        test.set_bit(0, false);
        assert_eq!(test, 4);
        assert!(!test.get_bit(0));
        test.set_bit(2, false);
        assert_eq!(test, 0);
        assert!(!test.get_bit(2));
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
