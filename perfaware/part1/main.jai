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

            if debug print("mod: %, rm: %\n", formatInt(mod, base=2, minimum_digits=2), formatInt(rm, base=2, minimum_digits=3));

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
                    other_name = tprint("[%]", (words[i+1] << 8) + words[i]);
                    i += 2;
                  case 0x7; // bx
                    other_name = "[bx]";
                }
              case 0x01; // low disp
                displacement := words[i];
                i += 1;
                if rm == {
                  case 0x0; // bx + si + D8
                    other_name = tprint("[bx + si + %]", displacement);
                  case 0x1; // bx + di + D8
                    other_name = tprint("[bx + di + %]", displacement);
                  case 0x2; // bp + si + D8
                    other_name = tprint("[bp + si + %]", displacement);
                  case 0x3; // bp + di + D8
                    other_name = tprint("[bp + di + %]", displacement);
                  case 0x4; // si + D8
                    other_name = ifx displacement then tprint("[bx + %]", displacement) else "[si]";
                  case 0x5; // di + D8
                    other_name = ifx displacement then tprint("[si + %]", displacement) else "[di]";
                  case 0x6; // bp + D8
                    other_name = ifx displacement then tprint("[di + %]", displacement) else "[bp]";
                  case 0x7; // bx + D8
                    other_name = ifx displacement then tprint("[bp + %]", displacement) else "[bx]";
                }
              case 0x02; // low + high disp
                displacement := (words[i+1] << 8) + words[i];
                i += 2;

                if rm == {
                  case 0x0; // bx + si + D16
                    other_name = tprint("[bx + si + %]", displacement);
                  case 0x1; // bx + di + D16
                    other_name = tprint("[bx + di + %]", displacement);
                  case 0x2; // bp + si + D16
                    other_name = tprint("[bp + si + %]", displacement);
                  case 0x3; // bp + di + D16
                    other_name = tprint("[bp + di + %]", displacement);
                  case 0x4; // si + D16
                    other_name = ifx displacement then tprint("[bx + %]", displacement) else "[si]";
                  case 0x5; // di + D16
                    other_name = ifx displacement then tprint("[si + %]", displacement) else "[di]";
                  case 0x6; // bp + D16
                    other_name = ifx displacement then tprint("[di + %]", displacement) else "[bp]";
                  case 0x7; // bx + D16
                    other_name = ifx displacement then tprint("[bp + %]", displacement) else "[bx]";
                }
              case 0x03;
                other_name = ifx w then REG_WORD[rm]   else REG_BYTE[rm];
            }

            src_name := ifx d then other_name else reg_name;
            dst_name := ifx d then reg_name   else other_name;

            instruction = tprint("mov %, %\n", dst_name, src_name);
        } else if (h & 0xf0) == opcode_mov_immediate_to_register {
            // deconstruct high byte
            w := h & 0x08 >> 3;
            reg := h & 0x07;
            if debug print("w: %, reg: %\n", formatInt(w, base=2, minimum_digits=1), formatInt(reg, base=2, minimum_digits=3));

            reg_name := ifx w then REG_WORD[reg] else REG_BYTE[reg];

            immediate: u16 = words[i];
            i += 1;

            if w {
                immediate += (cast(u16)words[i]) << 8;
                i += 1;
            }

            instruction = tprint("mov %, %\n", reg_name, immediate);
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
opcode_mov_rm_to_from_register: u8 : 0x88;

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
