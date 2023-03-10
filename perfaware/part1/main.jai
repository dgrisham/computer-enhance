#import "Basic";
#import "File";

main :: () {
    args := get_command_line_arguments();
    if args.count < 2 {
        print_err("usage: main <input_assembly_file>\n");
        exit(1);
    }
    infile := args[1];

    file, success := file_open(infile);
    if !success {
        print("Failed to open file for reading\n");
        return;
    }
    defer file_close(*file);

    length := file_length(file);
    buffer := cast(*u8) alloc(length);

    success = file_read(file, buffer, length);
    if !success {
        print("Failed to read file contents\n");
        return;
    }

    words: []u8;
    words.data = buffer;
    words.count = length;

    builder: String_Builder;

    print_to_builder(*builder, "; % disassembly:\n", infile);
    append(*builder, "bits 16\n");

    i := 0;
    while i < words.count {
        print("byte: %\n", i);
        h: u8 = words[i];
        i += 1;

        print("instruction: %\n", formatInt(h, base=2, minimum_digits=8));

        instruction: string;
        if (h & 0xfc) == opcode_mov {
            l: u8 = words[i];
            i += 1;

            // deconstruct high byte
            d := h & 0x02 >> 1;
            w := h & 0x01;

            // deconstruct low byte
            mod: u8 = l >> 6;
            reg: u8 = (l & 0x38) >> 3;
            rm: u8 = (l & 0x07);

            print("mod: %, rm: %\n", formatInt(mod, base=2, minimum_digits=2), formatInt(rm, base=2, minimum_digits=3));

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
                    other_name = tprint("[si + %]", displacement);
                  case 0x5; // di + D8
                    other_name = tprint("[di + %]", displacement);
                  case 0x6; // bp + D8
                    other_name = tprint("[bp + %]", displacement);
                  case 0x7; // bx + D8
                    other_name = tprint("[bx + %]", displacement);
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
                    other_name = tprint("[si + %]", displacement);
                  case 0x5; // di + D16
                    other_name = tprint("[di + %]", displacement);
                  case 0x6; // bp + D16
                    other_name = tprint("[bp + %]", displacement);
                  case 0x7; // bx + D16
                    other_name = tprint("[bx + %]", displacement);
                }
              case 0x03;
                other_name = ifx w then REG_WORD[rm]   else REG_BYTE[rm];
            }

            src_name := ifx d then other_name else reg_name;
            dst_name := ifx d then reg_name   else other_name;

            instruction = tprint("mov %, %\n", dst_name, src_name);
        } else if (h & 0xf0) == opcode_immediate_to_register {
            // deconstruct high byte
            w := h & 0x08 >> 3;
            reg := h & 0x07;
            print("w: %, reg: %\n", formatInt(w, base=2, minimum_digits=1), formatInt(reg, base=2, minimum_digits=3));

            if w {
                i += 1;
            }

            data := words[i];
            i += 1;

            instruction = "unhandled\n";
        } else {
            print_err("opcode not recognized\n");
            exit(1);
        }

        print_to_builder(*builder, instruction);
        print("\n");
    }

    asm_result := builder_to_string(*builder);
    print(asm_result);

    outfile := tprint("%.disassembly", infile);
    if !write_entire_file(outfile, asm_result) {
        print_err("Error writing output to file '%'", outfile);
    }
}

opcode_immediate_to_register: u8 : 0xb0;

opcode_mov: u8 : 0x88;
mov_handler :: (l: u8, h: u8) -> string {

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
