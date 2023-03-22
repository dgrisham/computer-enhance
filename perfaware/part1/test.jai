#import "Basic";
#import "File";
#import "String";

TESTING :: true;

#load "main.jai";

tests :: () {
	print("\nRUNNING TESTS...\n\n");

	print("test_listing_0037_single_register_move\n");
	print("--------------------------------------\n\n");
	test_listing_0037_single_register_move();

	print("test_listing_0038_many_register_move\n");
	print("------------------------------------\n\n");
	test_listing_0038_many_register_move();

	print("test_listing_0039_more_movs\n");
	print("---------------------------\n\n");
	test_listing_0039_more_movs();

	print("test_listing_0040_challenge_movs\n");
	print("--------------------------------\n\n");
	test_listing_0040_challenge_movs();

	print("PASSED!\n\n");
}

test_listing_0037_single_register_move :: () {
	reference, success := read_entire_file("./listing_0037_single_register_mov.asm");
	assert(success);
	ref := sanitize_asm_string(reference);

	output, error := decode_file_to_string("./listing_0037_single_register_mov");
	assert(!error, error);
	out := sanitize_asm_string(output);

	assert(ref == out, diff(ref, out));
}

test_listing_0038_many_register_move :: () {
	reference, success := read_entire_file("./listing_0038_many_register_mov.asm");
	assert(success);
	ref := sanitize_asm_string(reference);

	output, error := decode_file_to_string("./listing_0038_many_register_mov");
	assert(!error, error);
	out := sanitize_asm_string(output);

	assert(ref == out, diff(ref, out));
}

test_listing_0039_more_movs :: () {
	reference, success := read_entire_file("./listing_0039_more_movs.asm");
	assert(success);
	ref := sanitize_asm_string(reference);

	output, error := decode_file_to_string("./listing_0039_more_movs");
	assert(!error, error);
	out := sanitize_asm_string(output);

	assert(ref == out, diff(ref, out));
}

test_listing_0040_challenge_movs :: () {
	reference, success := read_entire_file("./listing_0040_challenge_movs.asm");
	assert(success);
	ref := sanitize_asm_string(reference);

	output, error := decode_file_to_string("./listing_0040_challenge_movs", true);
	assert(!error, error);
	out := sanitize_asm_string(output);

	assert(ref == out, diff(ref, out));
}

diff :: (s1: string, s2: string) -> string {
    s1_lines := split(s1, cast(u8)(#char "\n"));
    s2_lines := split(s2, cast(u8)(#char "\n"));
    // if s1_lines.count != s2_lines.count return tprint("Expected % lines, got %", s1_lines.count, s2_lines.count);

    max_lhs_length := 0;
    for s1_lines {
        max_lhs_length = max(max_lhs_length, it.count);
    }

    num_lines := max(s1_lines.count, s2_lines.count);
    diff_lines := NewArray(num_lines, string, false, temp);
    for i: 0..num_lines-1 {
        if i >= s1_lines.count {
            diff_lines[i] = tprint("%: +%", i, s2_lines[i]);
        } else if i >= s2_lines.count {
            diff_lines[i] = tprint("%: -%", i, s1_lines[i]);
        } else if s1_lines[i] != s2_lines[i] {
            num_spaces := max_lhs_length - s1_lines[i].count + 4;

            builder: String_Builder;
            init_string_builder(*builder, s1_lines[i].count + num_spaces + s2_lines[i].count);
            print_to_builder(*builder, "%: -%", i, s1_lines[i]);
            for 0..(num_spaces - cast(s64)(i >= 10)) append(*builder, " ");
            print_to_builder(*builder, "+%", s2_lines[i]);
            diff_lines[i] = builder_to_string(*builder);
        } else {
            diff_lines[i] = tprint("%: %", i, s1_lines[i]);
        }
    }

    return join(..diff_lines, "\n", allocator=temp);
}

sanitize_asm_string :: (asm: string) -> string{
	lines := split(asm, "\n");
	builder: String_Builder;
	init_string_builder(*builder);
	for lines {
		line := trim(it, " \t\r\n");
		if line.count < 1 continue;
		if line[0] == #char ";"{
			continue;
		}
		append(*builder, line);
		append(*builder, "\n");
	}
	str := builder_to_string(*builder);
	reset(*builder);
	return str;
}

#run tests();
