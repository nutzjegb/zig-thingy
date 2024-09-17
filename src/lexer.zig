const std = @import("std");
// const unicode = @import("std").unicode;

const Operator = enum {
    EQUALS,
    LESS_THAN,
    GREATER_THAN,
    NOT_EQUALS,
    LESS_OR_EQUAL,
    GREATER_OR_EQUAL,
};

pub const TokenType = union(enum) {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    SEMICOLON,
    MINUS,
    PLUS,
    ASSIGNMENT,
    NOT,
    OPERATOR: Operator,
    IDENTIFIER: []const u8,
    STRING: []const u8,
    NUM: usize,
    KEYWORD: KeyWord,
    EOF,
};
pub const TokenTypeTag = std.meta.Tag(TokenType);

const LexerToken = struct {
    char: u8,
    pos: usize,
    line: usize,
    col: usize,
    EOF: bool = false,
};

pub const Token = struct {
    line: usize,
    col: usize,

    token_type: TokenType,
};

pub const KeyWord = enum {
    IF,
    LET,
    DEF,
    PRINT,
    INPUT,
    WHILE,
    AND,
    OR,
    UNDEFINED,
    CLASS,
    SELF,
};
const Keywords = [_][]const u8{
    "if",
    "let",
    "def",
    "print",
    "input",
    "while",
    "and",
    "or",
    "undefined",
    "class",
    "self",
};
fn is_keyword(string: []const u8) bool {
    for (Keywords) |keyword| {
        if (std.mem.eql(u8, string, keyword)) {
            return true;
        }
    }
    return false;
}
fn parse_keyword(string: []const u8) !KeyWord {
    var buf: [32]u8 = undefined;
    const keyword = std.ascii.upperString(&buf, string);
    return std.meta.stringToEnum(KeyWord, keyword).?;
}

const Lexer = struct {
    source: []const u8,
    current_char: LexerToken,
    prev_char: ?LexerToken,

    pub fn init(source: []const u8) !Lexer {
        return Lexer{
            .source = source,
            .current_char = LexerToken{ .char = source[0], .col = 1, .line = 1, .pos = 0 },
            .prev_char = null,
        };
    }

    fn createToken(self: Lexer, token_type: TokenType) Token {
        const prev_pos = self.prev_char.?;
        return Token{ .line = prev_pos.line, .col = prev_pos.col, .token_type = token_type };
    }

    fn next_char(self: *Lexer) void {
        self.prev_char = self.current_char;

        if (self.current_char.pos + 1 >= self.source.len) {
            self.current_char.char = '\x00';
            self.current_char.line = 0;
            self.current_char.col = 0;
            self.current_char.pos = self.source.len;
            self.current_char.EOF = true;
            return;
        }

        self.current_char.pos += 1;
        self.current_char.char = self.source[self.current_char.pos];

        if (self.current_char.char == '\n') {
            self.current_char.line += 1;
            self.current_char.col = 0;
        } else {
            self.current_char.col += 1;
        }
    }

    fn skip_ignoreable(self: *Lexer) void {
        while (true) {
            switch (self.current_char.char) {
                '\n', ' ', '\t' => self.next_char(),
                else => return,
            }
        }
    }

    fn handle_string(self: *Lexer) !Token {
        const start_pos = self.current_char;
        while (true) {
            switch (self.current_char.char) {
                '\\' => return error.Todo,
                '"' => {
                    self.next_char();
                    const word = self.source[start_pos.pos..self.prev_char.?.pos];
                    return Token{
                        .token_type = TokenType{ .STRING = word },
                        .line = start_pos.line,
                        .col = start_pos.col,
                    };
                },
                else => self.next_char(),
            }
            if (self.current_char.EOF) {
                return error.unterminated_string;
            }
        }
    }

    fn handle_num(self: *Lexer) !Token {
        const start_pos = self.prev_char.?;
        while (std.ascii.isDigit(self.current_char.char)) {
            self.next_char();
        }
        const word = self.source[start_pos.pos..self.current_char.pos];
        const num = try std.fmt.parseInt(usize, word, 10);
        return Token{
            .token_type = TokenType{ .NUM = num },
            .line = start_pos.line,
            .col = start_pos.col,
        };
    }

    fn handle_literal(self: *Lexer) !Token {
        const start_pos = self.prev_char.?;
        while (std.ascii.isAlphanumeric(self.current_char.char)) {
            self.next_char();
        }
        const word = self.source[start_pos.pos..self.current_char.pos];

        if (is_keyword(word)) {
            const keyword = try parse_keyword(word);
            return Token{
                .token_type = TokenType{ .KEYWORD = keyword },
                .line = start_pos.line,
                .col = start_pos.col,
            };
        } else {
            return Token{
                .token_type = TokenType{ .IDENTIFIER = word },
                .line = start_pos.line,
                .col = start_pos.col,
            };
        }
    }

    fn handle_operator(self: *Lexer) !Token {
        const prev_char = self.prev_char.?.char;
        switch (self.current_char.char) {
            '=' => {
                self.next_char();
                switch (prev_char) {
                    '<' => return self.createToken(TokenType{ .OPERATOR = .LESS_OR_EQUAL }),
                    '>' => return self.createToken(TokenType{ .OPERATOR = .GREATER_OR_EQUAL }),
                    '!' => return self.createToken(TokenType{ .OPERATOR = .NOT_EQUALS }),
                    '=' => return self.createToken(TokenType{ .OPERATOR = .EQUALS }),
                    else => return error.ParserError,
                }
            },
            else => {
                switch (prev_char) {
                    '<' => return self.createToken(TokenType{ .OPERATOR = .LESS_THAN }),
                    '>' => return self.createToken(TokenType{ .OPERATOR = .GREATER_THAN }),
                    '!' => return self.createToken(.NOT),
                    '=' => return self.createToken(.ASSIGNMENT),
                    else => return error.ParserError,
                }
            },
        }
    }

    pub fn get_token(self: *Lexer) !Token {
        // Ignore comments and spaces, empty lines..
        // TODO: remove, but in loop below?
        self.skip_ignoreable();
        // std.debug.print("current char '{c}' i:{d} {d}:{d}\n", .{ self.current_char.char, self.current_char.pos, self.current_char.line, self.current_char.col });

        // Load next char
        self.next_char();
        if (self.prev_char.?.EOF) {
            return self.createToken(TokenType.EOF);
        }
        const char = self.prev_char.?.char;
        switch (char) {
            '(' => return self.createToken(TokenType.LEFT_PAREN),
            ')' => return self.createToken(TokenType.RIGHT_PAREN),
            '{' => return self.createToken(TokenType.LEFT_BRACE),
            '}' => return self.createToken(TokenType.RIGHT_BRACE),
            '+' => return self.createToken(TokenType.PLUS),
            '-' => return self.createToken(TokenType.MINUS),
            ',' => return self.createToken(TokenType.COMMA),
            ';' => return self.createToken(TokenType.SEMICOLON),
            '.' => return self.createToken(TokenType.DOT),
            '"' => return self.handle_string(),
            '<', '>', '!', '=' => return self.handle_operator(),
            else => {
                if (std.ascii.isAlphabetic(char)) {
                    return self.handle_literal();
                }
                if (std.ascii.isDigit(char)) {
                    return self.handle_num();
                }
                return error.Todo;
            },
        }
    }
};

pub fn print_token(token: Token) void {
    // std.debug.print("Token {d}:{d} {s}", .{ token.line, token.col, @tagName(token.token_type) });

    switch (token.token_type) {
        .STRING => |v| std.debug.print("token: {s} '{s}'\n", .{ @tagName(token.token_type), v }),
        .IDENTIFIER => |v| std.debug.print("token: {s} '{s}'\n", .{ @tagName(token.token_type), v }),
        .OPERATOR => |v| std.debug.print("token: {s} {s}\n", .{ @tagName(token.token_type), @tagName(v) }),
        .KEYWORD => |v| std.debug.print("token: {s} {s}\n", .{ @tagName(token.token_type), @tagName(v) }),
        .NUM => |v| std.debug.print("token: {s} {d}\n", .{ @tagName(token.token_type), v }),
        else => std.debug.print("token: {s}\n", .{@tagName(token.token_type)}),
    }
}

pub fn parse_source(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(Token) {
    var token_list = std.ArrayList(Token).init(allocator);
    errdefer token_list.clearAndFree();

    var lexer = try Lexer.init(source);
    while (true) {
        const token = try lexer.get_token();
        print_token(token);
        try token_list.append(token);

        if (token.token_type == TokenType.EOF) {
            break;
        }
    }
    return token_list;
}

// fn lexer(data: []const u8) !void {
//     var iterator = (try std.unicode.Utf8View.init(data)).iterator();
//     while (iterator.nextCodepoint()) |codepoint| {
//         // std.debug.print("got codepoint {s}\n", .{codepoint});
//         if ('=' == codepoint) {
//             std.debug.print(" Got equal!", .{});
//         }
//     }
// }

test "simple test" {
    const data = "let a = 0";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqual(.LET, tokens.items[0].token_type.KEYWORD);
    try std.testing.expectEqualStrings("a", tokens.items[1].token_type.IDENTIFIER);
    try std.testing.expectEqual(.ASSIGNMENT, tokens.items[2].token_type);
    try std.testing.expectEqual(0, tokens.items[3].token_type.NUM);

    try std.testing.expectEqual(.EOF, tokens.items[4].token_type);
    try std.testing.expectEqual(5, tokens.items.len);
}

test "nums" {
    const data = "1234 0 56";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqual(1234, tokens.items[0].token_type.NUM);
    try std.testing.expectEqual(0, tokens.items[1].token_type.NUM);
    try std.testing.expectEqual(56, tokens.items[2].token_type.NUM);

    try std.testing.expectEqual(.EOF, tokens.items[3].token_type);
    try std.testing.expectEqual(4, tokens.items.len);
}

test "keywords" {
    const data = "if If and undefined";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqual(.IF, tokens.items[0].token_type.KEYWORD);
    try std.testing.expectEqualStrings("If", tokens.items[1].token_type.IDENTIFIER);
    try std.testing.expectEqual(.AND, tokens.items[2].token_type.KEYWORD);
    try std.testing.expectEqual(.UNDEFINED, tokens.items[3].token_type.KEYWORD);

    try std.testing.expectEqual(.EOF, tokens.items[4].token_type);
    try std.testing.expectEqual(5, tokens.items.len);
}

test "string" {
    const data = "\"my string\"";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqualStrings("my string", tokens.items[0].token_type.STRING);
}

test "empty string" {
    const data = "\"\"";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqualStrings("", tokens.items[0].token_type.STRING);
}

test "unterminated string" {
    const data = "\"my string";

    try std.testing.expectError(error.unterminated_string, parse_source(std.testing.allocator, data));
}

test "operators" {
    const data = "< <= = == ! != >= >";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqual(.LESS_THAN, tokens.items[0].token_type.OPERATOR);
    try std.testing.expectEqual(.LESS_OR_EQUAL, tokens.items[1].token_type.OPERATOR);
    try std.testing.expectEqual(.ASSIGNMENT, tokens.items[2].token_type);
    try std.testing.expectEqual(.EQUALS, tokens.items[3].token_type.OPERATOR);
    try std.testing.expectEqual(.NOT, tokens.items[4].token_type);
    try std.testing.expectEqual(.NOT_EQUALS, tokens.items[5].token_type.OPERATOR);
    try std.testing.expectEqual(.GREATER_OR_EQUAL, tokens.items[6].token_type.OPERATOR);
    try std.testing.expectEqual(.GREATER_THAN, tokens.items[7].token_type.OPERATOR);

    try std.testing.expectEqual(.EOF, tokens.items[8].token_type);
    try std.testing.expectEqual(9, tokens.items.len);
}

test "unknown operators" {
    const data = "=!1";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqual(.ASSIGNMENT, tokens.items[0].token_type);
    try std.testing.expectEqual(.NOT, tokens.items[1].token_type);
    try std.testing.expectEqual(1, tokens.items[2].token_type.NUM);

    try std.testing.expectEqual(.EOF, tokens.items[3].token_type);
    try std.testing.expectEqual(4, tokens.items.len);
}

test "func" {
    const data = "def myFunc(1, \"abc\", 3)";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqual(.DEF, tokens.items[0].token_type.KEYWORD);
    try std.testing.expectEqualStrings("myFunc", tokens.items[1].token_type.IDENTIFIER);
    try std.testing.expectEqual(.LEFT_PAREN, tokens.items[2].token_type);
    try std.testing.expectEqual(1, tokens.items[3].token_type.NUM);
    try std.testing.expectEqual(.COMMA, tokens.items[4].token_type);
    try std.testing.expectEqualStrings("abc", tokens.items[5].token_type.STRING);
    try std.testing.expectEqual(.COMMA, tokens.items[6].token_type);
    try std.testing.expectEqual(3, tokens.items[7].token_type.NUM);
    try std.testing.expectEqual(.RIGHT_PAREN, tokens.items[8].token_type);

    try std.testing.expectEqual(.EOF, tokens.items[9].token_type);
    try std.testing.expectEqual(10, tokens.items.len);
}

test "line numbers" {
    const data =
        \\let a = 0
        \\let b = 5
        \\print(a + b)
    ;

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    try std.testing.expectEqual(.LET, tokens.items[0].token_type.KEYWORD);
    try std.testing.expectEqual(1, tokens.items[0].line);
    try std.testing.expectEqual(1, tokens.items[0].col);
    try std.testing.expectEqualStrings("a", tokens.items[1].token_type.IDENTIFIER);
    try std.testing.expectEqual(1, tokens.items[1].line);
    try std.testing.expectEqual(5, tokens.items[1].col);
    try std.testing.expectEqual(.ASSIGNMENT, tokens.items[2].token_type);
    try std.testing.expectEqual(1, tokens.items[2].line);
    try std.testing.expectEqual(7, tokens.items[2].col);
    try std.testing.expectEqual(0, tokens.items[3].token_type.NUM);
    try std.testing.expectEqual(1, tokens.items[3].line);
    try std.testing.expectEqual(9, tokens.items[3].col);

    try std.testing.expectEqual(.LET, tokens.items[4].token_type.KEYWORD);
    try std.testing.expectEqual(2, tokens.items[4].line);
    try std.testing.expectEqual(1, tokens.items[4].col);
    try std.testing.expectEqualStrings("b", tokens.items[5].token_type.IDENTIFIER);
    try std.testing.expectEqual(2, tokens.items[5].line);
    try std.testing.expectEqual(5, tokens.items[5].col);

    try std.testing.expectEqual(.PRINT, tokens.items[8].token_type.KEYWORD);
    try std.testing.expectEqual(3, tokens.items[8].line);
    try std.testing.expectEqual(1, tokens.items[8].col);
    try std.testing.expectEqual(.LEFT_PAREN, tokens.items[9].token_type);
    try std.testing.expectEqual(3, tokens.items[9].line);
    try std.testing.expectEqual(6, tokens.items[9].col);
    try std.testing.expectEqualStrings("a", tokens.items[10].token_type.IDENTIFIER);
    try std.testing.expectEqual(3, tokens.items[10].line);
    try std.testing.expectEqual(7, tokens.items[10].col);
    try std.testing.expectEqual(.PLUS, tokens.items[11].token_type);
    try std.testing.expectEqual(3, tokens.items[11].line);
    try std.testing.expectEqual(9, tokens.items[11].col);
    try std.testing.expectEqualStrings("b", tokens.items[12].token_type.IDENTIFIER);
    try std.testing.expectEqual(3, tokens.items[12].line);
    try std.testing.expectEqual(11, tokens.items[12].col);
    try std.testing.expectEqual(.RIGHT_PAREN, tokens.items[13].token_type);
    try std.testing.expectEqual(3, tokens.items[13].line);
    try std.testing.expectEqual(12, tokens.items[13].col);

    try std.testing.expectEqual(.EOF, tokens.items[14].token_type);
    try std.testing.expectEqual(15, tokens.items.len);
}
