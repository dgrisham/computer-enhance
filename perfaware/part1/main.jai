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
    buffer := cast(*u16) alloc(length);

    success = file_read(file, buffer, length);
    if !success {
        print("Failed to read file contents\n");
        return;
    }

    words: []u16;
    words.data = buffer;
    words.count = length / 2;

    builder: String_Builder;

    print_to_builder(*builder, "; % disassembly:\n", infile);
    append(*builder, "bits 16\n");

    for word: words {
        h: u8 = cast(u8)(word & 0x00ff);
        l: u8 = cast(u8)(word >> 8);

        // print("instruction %: % %\n", it_index, formatInt(h, base=2, minimum_digits=8), formatInt(l, base=2, minimum_digits=8));

        opcode_mov: u8 : 0x22;
        if (h >> 2) == opcode_mov {
            // deconstruct high byte
            d: u8 = h & 0x2 >> 1;
            w: u8 = h & 0x1;

            // deconstruct low byte
            mod: u8 = l >> 6;
            reg: u8 = (l & 0x3f) >> 3;
            rm: u8 = (l & 0x07);

            if mod != 0x3 {
                print_err("mod value for mov is not 11! Didn't expect that\n");
                exit(1);
            }

            dst_name, src_name: string;
            if d == 1 {
                dst_name = ifx w then REG_WORD[reg] else REG_BYTE[reg];
                src_name = ifx w then RM_WORD[rm]   else RM_BYTE[rm];
            } else {
                dst_name = ifx w then RM_WORD[rm]   else RM_BYTE[rm];
                src_name = ifx w then REG_WORD[reg] else REG_BYTE[reg];
            }

            print_to_builder(*builder, "mov %, %\n", dst_name, src_name);
        } else {
            print_err("opcode is not a mov! Didn't expect that\n");
            exit(1);
        }
    }

    asm_result := builder_to_string(*builder);
    print(asm_result);

    outfile := tprint("%.disassembly", infile);
    if !write_entire_file(outfile, asm_result) {
        print_err("Error writing output to file '%'", outfile);
    }
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

RM_BYTE :: string.[
    "al",
    "cl",
    "dl",
    "bl",
    "ah",
    "ch",
    "dh",
    "bh",
];

RM_WORD :: string.[
    "ax",
    "cx",
    "dx",
    "bx",
    "sp",
    "bp",
    "si",
    "di",
];
