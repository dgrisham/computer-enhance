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

    words: []u8;
    words.data = buffer;
    words.count = length;

    // for words if debug print("%: %\n", it_index, formatInt(it, base=2, minimum_digits=8));

    builder: String_Builder;

    print_to_builder(*builder, "; % disassembly:\n", infile);
    append(*builder, "bits 16\n");

    i := 0;
    instruction_count := 0;
    while i < words.count {
        if debug print("byte: %\n", i);
        h: u8 = words[i];
        i += 1;

        if debug print("instruction: %\n", formatInt(h, base=2, minimum_digits=8));

        instruction: string;
        if (h & 0xfc) == opcode_mov_rm_to_from_register {
            l: u8 = words[i];
            i += 1;

            // deconstruct high byte
            d := h & 0x02 >> 1;
            w := h & 0x01;

            // deconstruct low byte
            mod: u8 = l >> 6;
            reg: u8 = (l & 0x38) >> 3;
            rm: u8 = (l & 0x07);

            if debug print("mod: %, reg: %, rm: %\n", formatInt(mod, base=2, minimum_digits=2), formatInt(reg, base=2, minimum_digits=3), formatInt(rm, base=2, minimum_digits=3));

            reg_name := ifx w then REG_WORD[reg] else REG_BYTE[reg];
            other_name: string; // bad name
            if mod == {
              case 0x00;
                if rm == {
                  case 0x0; // bx + si
                    other_name = "[bx + si]";
                  case 0x1; // bx + di
                    other_name = "[bx + di]";
                  case 0x2; // bp + si
                    other_name = "[bp + si]";
                  case 0x3; // bp + di
                    other_name = "[bp + di]";
                  case 0x4; // si
                    other_name = "[si]";
                  case 0x5; // di
                    other_name = "[di]";
                  case 0x6; // D16
                    other_name = tprint("[%]", (cast(s16)(words[i+1])) << 8 + words[i]);
                    i += 2;
                  case 0x7; // bx
                    other_name = "[bx]";
                }
              case 0x01; // low disp
                displacement := <<cast(*s8)*words[i];
                i += 1;

                sign := ifx displacement > 0 then "+" else "-";
                magnitude := ifx displacement > 0 then displacement else -displacement;
                if rm == {
                  case 0x0; // bx + si + D8
                    other_name = tprint("[bx + si % %]", sign, magnitude);
                  case 0x1; // bx + di + D8
                    other_name = tprint("[bx + di % %]", sign, magnitude);
                  case 0x2; // bp + si + D8
                    other_name = tprint("[bp + si % %]", sign, magnitude);
                  case 0x3; // bp + di + D8
                    other_name = tprint("[bp + di % %]", sign, magnitude);
                  case 0x4; // si + D8
                    other_name = ifx magnitude then tprint("[si % %]", sign, magnitude) else "[si]";
                  case 0x5; // di + D8
                    other_name = ifx magnitude then tprint("[di % %]", sign, magnitude) else "[di]";
                  case 0x6; // bp + D8
                    other_name = ifx magnitude then tprint("[bp % %]", sign, magnitude) else "[bp]";
                  case 0x7; // bx + D8
                    other_name = ifx magnitude then tprint("[bx % %]", sign, magnitude) else "[bx]";
                }
              case 0x02; // low + high disp
                displacement: s16 = (cast(s16)(words[i+1])) << 8 + words[i];
                i += 2;

                sign := ifx displacement > 0 then "+" else "-";
                magnitude := ifx displacement > 0 then displacement else -displacement;
                if rm == {
                  case 0x0; // bx + si + D16
                    other_name = tprint("[bx + si % %]", sign, magnitude);
                  case 0x1; // bx + di + D16
                    other_name = tprint("[bx + di % %]", sign, magnitude);
                  case 0x2; // bp + si + D16
                    other_name = tprint("[bp + si % %]", sign, magnitude);
                  case 0x3; // bp + di + D16
                    other_name = tprint("[bp + di % %]", sign, magnitude);
                  case 0x4; // si + D16
                    other_name = ifx displacement then tprint("[si % %]", sign, magnitude) else "[si]";
                  case 0x5; // di + D16
                    other_name = ifx displacement then tprint("[di % %]", sign, magnitude) else "[di]";
                  case 0x6; // bp + D16
                    other_name = ifx displacement then tprint("[bp % %]", sign, magnitude) else "[bp]";
                  case 0x7; // bx + D16
                    other_name = ifx displacement then tprint("[bx % %]", sign, magnitude) else "[bx]";
                }
              case 0x03;
                other_name = ifx w then REG_WORD[rm] else REG_BYTE[rm];
            }

            src_name := ifx d then other_name else reg_name;
            dst_name := ifx d then reg_name   else other_name;

            instruction = tprint("mov %, %\n", dst_name, src_name);
        } else if (h & 0xfe) == opcode_mov_immediate_to_register_or_memory {
            l: u8 = words[i];
            i += 1;

            // deconstruct low byte
            mod: u8 = l >> 6;
            reg: u8 = (l & 0x38) >> 3;
            rm: u8 = (l & 0x07);

            if reg != 0 { // TODO: should the reg check be part of the instruction condition?
                return "", "malformed instruction";
            }

            w := h & 0x01;

            if debug print("mod: %, reg: %, rm: %\n", formatInt(mod, base=2, minimum_digits=2), formatInt(reg, base=2, minimum_digits=3), formatInt(rm, base=2, minimum_digits=3));

            dst_name: string;
            if mod == {
              case 0x00;
                if rm == {
                  case 0x0; // bx + si
                    dst_name = "[bx + si]";
                  case 0x1; // bx + di
                    dst_name = "[bx + di]";
                  case 0x2; // bp + si
                    dst_name = "[bp + si]";
                  case 0x3; // bp + di
                    dst_name = "[bp + di]";
                  case 0x4; // si
                    dst_name = "[si]";
                  case 0x5; // di
                    dst_name = "[di]";
                  case 0x6; // D16
                    dst_name = tprint("[%]", (words[i+1] << 8) + words[i]);
                    i += 2;
                  case 0x7; // bx
                    dst_name = "[bx]";
                }
              case 0x01; // low disp
                displacement := <<cast(*s8)*words[i];
                i += 1;

                sign := ifx displacement > 0 then "+" else "-";
                magnitude := ifx displacement > 0 then displacement else -displacement;
                if rm == {
                  case 0x0; // bx + si + D8
                    dst_name = tprint("[bx + si % %]", sign, magnitude);
                  case 0x1; // bx + di + D8
                    dst_name = tprint("[bx + di % %]", sign, magnitude);
                  case 0x2; // bp + si + D8
                    dst_name = tprint("[bp + si % %]", sign, magnitude);
                  case 0x3; // bp + di + D8
                    dst_name = tprint("[bp + di % %]", sign, magnitude);
                  case 0x4; // si + D8
                    dst_name = ifx magnitude then tprint("[si % %]", sign, magnitude) else "[si]";
                  case 0x5; // di + D8
                    dst_name = ifx magnitude then tprint("[di % %]", sign, magnitude) else "[di]";
                  case 0x6; // bp + D8
                    dst_name = ifx magnitude then tprint("[bp % %]", sign, magnitude) else "[bp]";
                  case 0x7; // bx + D8
                    dst_name = ifx magnitude then tprint("[bx % %]", sign, magnitude) else "[bx]";
                }
              case 0x02; // low + high disp
                displacement: s16 = (cast(s16)(words[i+1])) << 8 + words[i];
                i += 2;

                sign := ifx displacement > 0 then "+" else "-";
                magnitude := ifx displacement > 0 then displacement else -displacement;
                if rm == {
                  case 0x0; // bx + si + D16
                    dst_name = tprint("[bx + si % %]", sign, magnitude);
                  case 0x1; // bx + di + D16
                    dst_name = tprint("[bx + di % %]", sign, magnitude);
                  case 0x2; // bp + si + D16
                    dst_name = tprint("[bp + si % %]", sign, magnitude);
                  case 0x3; // bp + di + D16
                    dst_name = tprint("[bp + di % %]", sign, magnitude);
                  case 0x4; // si + D16
                    dst_name = ifx displacement then tprint("[si % %]", sign, magnitude) else "[si]";
                  case 0x5; // di + D16
                    dst_name = ifx displacement then tprint("[di % %]", sign, magnitude) else "[di]";
                  case 0x6; // bp + D16
                    dst_name = ifx displacement then tprint("[bp % %]", sign, magnitude) else "[bp]";
                  case 0x7; // bx + D16
                    dst_name = ifx displacement then tprint("[bx % %]", sign, magnitude) else "[bx]";
                }
              case 0x03;
                dst_name = ifx w then REG_WORD[rm] else REG_BYTE[rm];
            }

            src_value: string;
            if w {
                immediate: s16 = (cast(s16)(words[i+1])) << 8 + words[i];
                i += 2;

                src_value = tprint("word %", immediate);
            } else {
                immediate: s8 = <<(cast(*s8)*words[i]);
                i += 1;

                src_value = tprint("byte %", immediate);
            }

            instruction = tprint("mov %, %\n", dst_name, src_value);
            // instruction = "unhandled";
        } else if (h & 0xf0) == opcode_mov_immediate_to_register {
            // deconstruct high byte
            w := h & 0x08 >> 3;
            reg := h & 0x07;
            if debug print("w: %, reg: %\n", formatInt(w, base=2, minimum_digits=1), formatInt(reg, base=2, minimum_digits=3));

            reg_name := ifx w then REG_WORD[reg] else REG_BYTE[reg];

            immediate: s16;
            if w {
                immediate = words[i];
                i += 1;

                immediate += (cast(s16)words[i]) << 8;
                i += 1;
            } else {
                immediate_byte: s8 = <<(cast(*s8)*words[i]);
                i += 1;

                immediate = immediate_byte;
            }

            instruction = tprint("mov %, %\n", reg_name, immediate);
        } else if (h & 0xfe) == opcode_mov_memory_to_accumulator {
            address: u16 = (cast(u16)(words[i+1])) << 8 + words[i];
            i += 2;
            instruction = tprint("mov ax, [%]\n", address);
        } else if (h & 0xfe) == opcode_mov_accumulator_to_memory {
            address: u16 = (cast(u16)(words[i+1])) << 8 + words[i];
            i += 2;
            instruction = tprint("mov [%], ax\n", address);
        } else {
            return "", "opcode not recognized";
        }

        print_to_builder(*builder, instruction);
        instruction_count += 1;
        if debug print("%: %\n", instruction_count, instruction);
    }

    return builder_to_string(*builder), "";
}

opcode_mov_immediate_to_register: u8 : 0xb0;
opcode_mov_immediate_to_register_or_memory: u8 : 0xc6;
opcode_mov_rm_to_from_register: u8 : 0x88;
opcode_mov_memory_to_accumulator: u8 : 0xa0;
opcode_mov_accumulator_to_memory: u8 : 0xa2;

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
