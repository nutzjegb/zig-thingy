const std = @import("std");
const lexer = @import("lexer.zig");

pub fn main() !void {
    // TODO: Add comment
    const data =
        \\
        \\print("How many fibonacci numbers do you want?😊")
        \\let num = input()
        \\
        \\def myFunc(num) {
        \\  let a = 0
        \\  let b = 1
        \\  while(num > 0) {
        \\    print(a)
        \\    let c = a + b
        \\    a = b
        \\    b = c
        \\    num = num - 1
        \\  }
        \\}
        \\
        \\print(myFunc(num))
        \\
    ;

    _ = try lexer.parse_source(std.heap.page_allocator, data);
}

test "simple test" {
    std.debug.assert(true);
}
