#import "Basic";
#import "File";

main :: () {
    file, success := file_open("./listing_0037_single_register_mov");
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

    print("successfully read in file, % bytes\n", length);

    compiled_data: []u16;
    compiled_data.data = buffer;
    compiled_data.count = length / 2;

    for b: compiled_data {
        l: u8 = cast(u8)(b >> 8);
        h: u8 = cast(u8)(b & 0x00ff);

        // print("full: %\n", formatInt(b, base=2, minimum_digits=2));
        print("low: %\n", formatInt(l, base=2, minimum_digits=8));
        print("high: %\n", formatInt(h, base=2, minimum_digits=8));
    }
    // print("file contents: '%'\n", <<buffer);
}
