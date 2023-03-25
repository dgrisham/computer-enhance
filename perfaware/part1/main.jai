#import "Basic";
#import "File";

main :: () {
    args := get_command_line_arguments();
    if args.count < 2 {
        print_err("usage: main <input_assembly_file>\n");
        exit(1);
    }
    infile := args[1];

    asm_result, error := decode_file_to_string(infile);
    if error {
        print_err("error decoding binary file: %\n", error);
    }

    print(asm_result);

    outfile := tprint("%.disassembly", infile);
    if !write_entire_file(outfile, asm_result) {
        print_err("Error writing output to file '%'", outfile);
    }
}

decode_file_to_string :: (infile: string, debug := false) -> string, string {
    file, success := file_open(infile);
    if !success {
        return "", "Failed to open file for reading";
    }
    defer file_close(*file);

    length := file_length(file);
    buffer := cast(*u8) alloc(length);

    success = file_read(file, buffer, length);
    if !success {
        return "", "Failed to read file contents";
    }

    asm_bytes: []u8;
    asm_bytes.data = buffer;
    asm_bytes.count = length;

    // for asm_bytes if debug print("%: %\n", it_index, formatInt(it, base=2, minimum_digits=8));

    builder: String_Builder;

    print_to_builder(*builder, "; % disassembly:\n", infile);
    append(*builder, "bits 16\n");

    instruction_count := 0;
    while asm_bytes.count > 0 {
        instruction: string;
        defer {
            print_to_builder(*builder, instruction);
            instruction_count += 1;
            if debug print("%: %\n", instruction_count, instruction);
        }

        h: u8 = consume_byte(*asm_bytes);
        if debug print("instruction: %\n", formatInt(h, base=2, minimum_digits=8));

        if (h & 0xfc) == {
          case opcode_mov_rm_to_from_register;
            reg_is_dest := h & 0x02 == 2;
            is_word_length := h & 0x01 == 1;
            instruction = decode_rm_reg("mov", is_word_length, *asm_bytes, reg_is_dest, debug);
            continue;
          case opcode_add_rm_to_reg;
            reg_is_dest := h & 0x02 == 2;
            is_word_length := h & 0x01 == 1;
            instruction = decode_rm_reg("add", is_word_length, *asm_bytes, reg_is_dest, debug);
            continue;
          case opcode_sub_rm_from_reg;
            reg_is_dest := h & 0x02 == 2;
            is_word_length := h & 0x01 == 1;
            instruction = decode_rm_reg("sub", is_word_length, *asm_bytes, reg_is_dest, debug);
            continue;
          case opcode_cmp_rm_and_reg;
            reg_is_dest := h & 0x02 == 2;
            is_word_length := h & 0x01 == 1;
            instruction = decode_rm_reg("cmp", is_word_length, *asm_bytes, reg_is_dest, debug);
            continue;
        }

        if (h & 0xfe) == opcode_mov_immediate_to_rm {
            is_word_length := h & 0x01 == 1;
            instruction = decode_immediate_to_rm("mov", is_word_length, *asm_bytes, debug);
            continue;
        } else if (h & 0xfc) == opcode_immediate_rm_operation {
            is_word_length := h & 0x03 == 1; // sw == 01
            instruction = decode_opcode_immediate_rm_operation (is_word_length, *asm_bytes, debug);
            continue;
        }

        if (h & 0xfe) == opcode_mov_memory_to_accumulator {
            address: u16 = (cast(u16)(asm_bytes[1])) << 8 + asm_bytes[0];
            advance(*asm_bytes, 2);

            w := h & 0x01;
            reg_name := ifx w then "ax" else "al";
            instruction = tprint("mov %, [%]\n", reg_name, address);
            continue;
        }

        if (h & 0xfe) == {
          case opcode_add_immediate_to_accumulator;
            w := h & 0x01;

            immediate: s16;
            if w {
                immediate = (cast(s16)(asm_bytes[1])) << 8 + asm_bytes[0];
                advance(*asm_bytes, 2);
            } else {
                immediate = <<(cast(*s8)*consume_byte(*asm_bytes));
            }

            reg_name := ifx w then "ax" else "al";
            instruction = tprint("add %, %\n", reg_name, immediate);
            continue;
          case opcode_sub_immediate_from_accumulator;
            w := h & 0x01;

            immediate: s16;
            if w {
                immediate = (cast(s16)(asm_bytes[1])) << 8 + asm_bytes[0];
                advance(*asm_bytes, 2);
            } else {
                immediate = <<(cast(*s8)*consume_byte(*asm_bytes));
            }

            reg_name := ifx w then "ax" else "al";
            instruction = tprint("sub %, %\n", reg_name, immediate);
            continue;
          case opcode_cmp_immediate_and_accumulator;
            w := h & 0x01;

            immediate: s16;
            if w {
                immediate = (cast(s16)(asm_bytes[1])) << 8 + asm_bytes[0];
                advance(*asm_bytes, 2);
            } else {
                immediate = <<(cast(*s8)*consume_byte(*asm_bytes));
            }

            reg_name := ifx w then "ax" else "al";
            instruction = tprint("cmp %, %\n", reg_name, immediate);
            continue;
        }

        if (h & 0xf0) == opcode_mov_immediate_to_register {
            // deconstruct high byte
            w := h & 0x08 >> 3;
            reg := h & 0x07;
            if debug print("w: %, reg: %\n", formatInt(w, base=2, minimum_digits=1), formatInt(reg, base=2, minimum_digits=3));

            reg_name := ifx w then REG_WORD[reg] else REG_BYTE[reg];

            immediate: s16;
            if w {
                immediate = (cast(s16)(asm_bytes[1])) << 8 + asm_bytes[0];
                advance(*asm_bytes, 2);
            } else {
                immediate = <<(cast(*s8)*consume_byte(*asm_bytes));
            }

            instruction = tprint("mov %, %\n", reg_name, immediate);
        } else if (h & 0xfe) == opcode_mov_accumulator_to_memory {
            address: u16 = (cast(u16)(asm_bytes[1])) << 8 + asm_bytes[0];
            advance(*asm_bytes, 2);

            w := h & 0x01;
            reg_name := ifx w then "ax" else "al";
            instruction = tprint("mov [%], %\n", address, reg_name);
        } else {
            return "", "unimplemented opcode";
        }
    }

    return builder_to_string(*builder), "";
}

decode_rm_reg :: (operation: string, is_word_length: bool, asm_bytes: *[]u8, reg_is_dest: bool, debug: bool) -> string {
    l: u8 = consume_byte(asm_bytes);

    // deconstruct low byte
    mod : u8 = l >> 6;
    reg : u8 = (l & 0x38) >> 3;
    rm  : u8 = (l & 0x07);

    if debug print("mod: %, reg: %, rm: %\n",
        formatInt(mod, base=2, minimum_digits=2),
        formatInt(reg, base=2, minimum_digits=3),
        formatInt(rm, base=2, minimum_digits=3),
    );

    rm_name := decode_rm(mod, rm, is_word_length, asm_bytes);
    reg_name := ifx is_word_length then REG_WORD[reg] else REG_BYTE[reg];

    dst_name := ifx reg_is_dest then reg_name else rm_name;
    src_name := ifx reg_is_dest then rm_name  else reg_name;

    return tprint("% %, %\n", operation, dst_name, src_name);
}

decode_immediate_to_rm :: (operation: string, is_word_length: bool, asm_bytes: *[]u8, debug: bool) -> string #expand {
    l: u8 = consume_byte(asm_bytes);

    // deconstruct low byte
    mod: u8 = l >> 6;
    reg: u8 = (l & 0x38) >> 3;
    rm: u8 = (l & 0x07);

    if reg != 0 { // TODO: should the reg check be part of the instruction condition?
        `return "", "malformed instruction";
    }

    if debug print("mod: %, reg: %, rm: %\n",
        formatInt(mod, base=2, minimum_digits=2),
        formatInt(reg, base=2, minimum_digits=3),
        formatInt(rm, base=2, minimum_digits=3),
    );

    dst_name := decode_rm(mod, rm, is_word_length, asm_bytes);

    src_value: string;
    if is_word_length {
        immediate: s16 = (cast(s16)(get_byte(asm_bytes, 1))) << 8 + get_byte(asm_bytes, 0);
        advance(asm_bytes, 2);
        src_value = tprint("word %", immediate);
    } else {
        immediate: s8 = <<(cast(*s8)*consume_byte(asm_bytes));
        src_value = tprint("byte %", immediate);
    }

    return tprint("% %, %\n", operation, dst_name, src_value);
}

decode_opcode_immediate_rm_operation  :: (is_word_length: bool, asm_bytes: *[]u8, debug: bool) -> string #expand {
    l: u8 = consume_byte(asm_bytes);

    // deconstruct low byte
    mod: u8 = l >> 6;
    reg: u8 = (l & 0x38) >> 3;
    rm: u8 = (l & 0x07);

    operation: string;
    if reg == { // TODO: should the reg check be part of the instruction condition?
      case 0x0;
        operation = "add";
      case 0x5;
        operation = "sub";
      case 0x7;
        operation = "cmp";
      case;
        `return "", "encountered bad reg value when decoding immediate op";
    }

    if debug print("mod: %, reg: %, rm: %\n",
        formatInt(mod, base=2, minimum_digits=2),
        formatInt(reg, base=2, minimum_digits=3),
        formatInt(rm, base=2, minimum_digits=3),
    );

    dst_name := decode_rm(mod, rm, is_word_length, asm_bytes);

    src_value: string;
    if is_word_length {
        immediate: s16 = (cast(s16)(get_byte(asm_bytes, 1))) << 8 + get_byte(asm_bytes, 0);
        advance(asm_bytes, 2);
        src_value = tprint("word %", immediate);
    } else {
        immediate: s8 = <<(cast(*s8)*consume_byte(asm_bytes));
        src_value = tprint("byte %", immediate);
    }

    return tprint("% %, %\n", operation, dst_name, src_value);
}

decode_rm :: (mod: u8, rm: u8, is_word_length: bool, asm_bytes: *[]u8) -> string {
    rm_name: string;

    if mod == {
      case 0x00;
        if rm == {
          case 0x0; // bx + si
            rm_name = "[bx + si]";
          case 0x1; // bx + di
            rm_name = "[bx + di]";
          case 0x2; // bp + si
            rm_name = "[bp + si]";
          case 0x3; // bp + di
            rm_name = "[bp + di]";
          case 0x4; // si
            rm_name = "[si]";
          case 0x5; // di
            rm_name = "[di]";
          case 0x6; // D16
            rm_name = tprint("[%]", (cast(s16)get_byte(asm_bytes, 1)) << 8 + get_byte(asm_bytes, 0));
            advance(asm_bytes, 2);
          case 0x7; // bx
            rm_name = "[bx]";
        }
      case 0x01; // low disp
        displacement := <<cast(*s8)*consume_byte(asm_bytes);

        sign := ifx displacement > 0 then "+" else "-";
        magnitude := ifx displacement > 0 then displacement else -displacement;
        if rm == {
          case 0x0; // bx + si + D8
            rm_name = tprint("[bx + si % %]", sign, magnitude);
          case 0x1; // bx + di + D8
            rm_name = tprint("[bx + di % %]", sign, magnitude);
          case 0x2; // bp + si + D8
            rm_name = tprint("[bp + si % %]", sign, magnitude);
          case 0x3; // bp + di + D8
            rm_name = tprint("[bp + di % %]", sign, magnitude);
          case 0x4; // si + D8
            rm_name = ifx magnitude then tprint("[si % %]", sign, magnitude) else "[si]";
          case 0x5; // di + D8
            rm_name = ifx magnitude then tprint("[di % %]", sign, magnitude) else "[di]";
          case 0x6; // bp + D8
            rm_name = ifx magnitude then tprint("[bp % %]", sign, magnitude) else "[bp]";
          case 0x7; // bx + D8
            rm_name = ifx magnitude then tprint("[bx % %]", sign, magnitude) else "[bx]";
        }
      case 0x02; // low + high disp
        displacement: s16 = (cast(s16)(get_byte(asm_bytes, 1))) << 8 + get_byte(asm_bytes, 0);
        advance(asm_bytes, 2);

        sign := ifx displacement > 0 then "+" else "-";
        magnitude := ifx displacement > 0 then displacement else -displacement;
        if rm == {
          case 0x0; // bx + si + D16
            rm_name = tprint("[bx + si % %]", sign, magnitude);
          case 0x1; // bx + di + D16
            rm_name = tprint("[bx + di % %]", sign, magnitude);
          case 0x2; // bp + si + D16
            rm_name = tprint("[bp + si % %]", sign, magnitude);
          case 0x3; // bp + di + D16
            rm_name = tprint("[bp + di % %]", sign, magnitude);
          case 0x4; // si + D16
            rm_name = ifx displacement then tprint("[si % %]", sign, magnitude) else "[si]";
          case 0x5; // di + D16
            rm_name = ifx displacement then tprint("[di % %]", sign, magnitude) else "[di]";
          case 0x6; // bp + D16
            rm_name = ifx displacement then tprint("[bp % %]", sign, magnitude) else "[bp]";
          case 0x7; // bx + D16
            rm_name = ifx displacement then tprint("[bx % %]", sign, magnitude) else "[bx]";
        }
      case 0x03;
        rm_name = ifx is_word_length then REG_WORD[rm] else REG_BYTE[rm];
    }

    return rm_name;
}

opcode_immediate_rm_operation : u8 : 0x80;

opcode_add_rm_to_reg                : u8 : 0x00;
// opcode_add_immediate_to_rm          : u8 : 0x80;
opcode_add_immediate_to_accumulator : u8 : 0x04;

opcode_sub_rm_from_reg                : u8 : 0x28;
// opcode_sub_immediate_from_rm          : u8 : 0x80; // SAME AS ADD IMM TO RM !!! REG = 101
opcode_sub_immediate_from_accumulator : u8 : 0x2c;

opcode_cmp_rm_and_reg                : u8 : 0x38;
// opcode_cmp_immediate_and_rm          : u8 : 0x80; // SAME AS ADD IMM TO RM !!! REG = 111
opcode_cmp_immediate_and_accumulator : u8 : 0x3c;

opcode_mov_immediate_to_rm      : u8 : 0xc6;
opcode_mov_immediate_to_register: u8 : 0xb0;
opcode_mov_rm_to_from_register  : u8 : 0x88;
opcode_mov_memory_to_accumulator: u8 : 0xa0;
opcode_mov_accumulator_to_memory: u8 : 0xa2;

advance :: inline (a: *[]u8, amount: s64 = 1) {
    assert(amount >= 0);
    assert(a.count >= amount);
    a.count -= amount;
    a.data  += amount;
}

consume_byte :: inline (a: *[]u8) -> u8 {
    assert(a.count >= 1);

    val := (<<a)[0];
    a.count -= 1;
    a.data  += 1;
    return val;
}

get_byte :: inline (a: *[]u8, index: s64) -> u8 {
    assert(index >= 0);
    assert(index < a.count);
    return (<<a)[index];
}

print_err :: inline (format_string: string, args: .. Any) -> bytes_printed: s64 {
    return print(format_string, args, to_standard_error=true);
}

REG_BYTE :: string.[
    "al",
    "cl",
    "dl",
    "bl",
    "ah",
    "ch",
    "dh",
    "bh",
];

REG_WORD :: string.[
    "ax",
    "cx",
    "dx",
    "bx",
    "sp",
    "bp",
    "si",
    "di",
];
