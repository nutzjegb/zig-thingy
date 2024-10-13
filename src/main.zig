const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");

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

    var tokens = try lexer.parse_source(std.heap.page_allocator, data);
    defer tokens.clearAndFree();

    var result = try parser.parse_tokens(std.heap.page_allocator, tokens);
    defer result.deinit();
}

test "simple test" {
    try std.testing.expect(true);
}
