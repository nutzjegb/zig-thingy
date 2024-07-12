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
};
const Keywords = [_][]const u8{
    "if",
    "let",
    "def",
    "print",
    "input",
    "while",
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
        const start_pos = self.prev_char.?;
        while (true) {
            switch (self.current_char.char) {
                '\\' => return error.Todo,
                '"' => {
                    self.next_char();
                    const word = self.source[start_pos.pos..self.current_char.pos];
                    return Token{
                        .token_type = TokenType{ .STRING = word },
                        .line = start_pos.line,
                        .col = start_pos.col,
                    };
                },
                else => self.next_char(),
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
                switch (prev_char) {
                    '<' => return self.createToken(TokenType{ .OPERATOR = Operator.LESS_OR_EQUAL }),
                    '>' => return self.createToken(TokenType{ .OPERATOR = Operator.GREATER_OR_EQUAL }),
                    '!' => return self.createToken(TokenType{ .OPERATOR = Operator.NOT_EQUALS }),
                    '=' => return self.createToken(TokenType{ .OPERATOR = Operator.EQUALS }),
                    else => return error.ParserError,
                }
            },
            else => {
                switch (prev_char) {
                    '<' => return self.createToken(TokenType{ .OPERATOR = Operator.LESS_THAN }),
                    '>' => return self.createToken(TokenType{ .OPERATOR = Operator.GREATER_THAN }),
                    '!' => return self.createToken(TokenType.NOT),
                    '=' => return self.createToken(TokenType.ASSIGNMENT),
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
            ',' => return self.createToken(Token.COMMA),
            ';' => return self.createToken(TokenType.SEMICOLON),
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
        .STRING => |v| std.debug.print("token: {s} {s}\n", .{ @tagName(token.token_type), v }),
        .IDENTIFIER => |v| std.debug.print("token: {s} '{s}'\n", .{ @tagName(token.token_type), v }),
        .OPERATOR => |v| std.debug.print("token: {s} {s}\n", .{ @tagName(token.token_type), @tagName(v) }),
        .KEYWORD => |v| std.debug.print("token: {s} {s}\n", .{ @tagName(token.token_type), @tagName(v) }),
        .NUM => |v| std.debug.print("token: {s} {d}\n", .{ @tagName(token.token_type), v }),
        else => std.debug.print("token: {s}\n", .{@tagName(token.token_type)}),
    }
}

pub fn parse_source(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(Token) {
    var token_list = std.ArrayList(Token).init(allocator);
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

    std.debug.assert(tokens.items[0].token_type.KEYWORD == KeyWord.LET);
    std.debug.assert(tokens.items[1].token_type == TokenType.IDENTIFIER);
    std.debug.assert(tokens.items[2].token_type.OPERATOR == Operator.ASSIGNMENT);
    std.debug.assert(tokens.items[3].token_type.NUM == 0);
    std.debug.assert(tokens.items[4].token_type == TokenType.EOF);
}

test "nums" {
    const data = "1234 0 56";

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    std.debug.assert(tokens.items[0].token_type.NUM == 1234);
    std.debug.assert(tokens.items[1].token_type.NUM == 0);
    std.debug.assert(tokens.items[2].token_type.NUM == 56);
}

test "line numbers" {
    const data =
        \\let a = 0
        \\let b = 5
        \\print(a + b)
    ;

    var tokens = try parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    std.debug.assert(tokens.items[0].token_type.KEYWORD == KeyWord.LET);
    std.debug.assert(tokens.items[0].line == 1);
    std.debug.assert(tokens.items[0].col == 1);
    std.debug.assert(tokens.items[1].token_type == TokenType.IDENTIFIER);
    std.debug.assert(tokens.items[1].line == 1);
    std.debug.assert(tokens.items[1].col == 5);
    std.debug.assert(tokens.items[2].token_type.OPERATOR == Operator.ASSIGNMENT);
    std.debug.assert(tokens.items[2].line == 1);
    std.debug.assert(tokens.items[2].col == 7);
    std.debug.assert(tokens.items[3].token_type.NUM == 0);
    std.debug.assert(tokens.items[3].line == 1);
    std.debug.assert(tokens.items[3].col == 9);

    std.debug.assert(tokens.items[4].token_type.KEYWORD == KeyWord.LET);
    std.debug.assert(tokens.items[4].line == 2);
    std.debug.assert(tokens.items[4].col == 1);
    std.debug.assert(tokens.items[5].token_type == TokenType.IDENTIFIER);
    std.debug.assert(tokens.items[5].line == 2);
    std.debug.assert(tokens.items[5].col == 5);

    std.debug.assert(tokens.items[8].token_type.KEYWORD == KeyWord.PRINT);
    std.debug.assert(tokens.items[8].line == 3);
    std.debug.assert(tokens.items[8].col == 1);
    std.debug.assert(tokens.items[9].token_type == TokenType.LEFT_PAREN);
    std.debug.assert(tokens.items[9].line == 3);
    std.debug.assert(tokens.items[9].col == 6);
    std.debug.assert(tokens.items[10].token_type == TokenType.IDENTIFIER);
    std.debug.assert(tokens.items[10].line == 3);
    std.debug.assert(tokens.items[10].col == 7);
    std.debug.assert(tokens.items[11].token_type == TokenType.PLUS);
    std.debug.assert(tokens.items[11].line == 3);
    std.debug.assert(tokens.items[11].col == 9);
    std.debug.assert(tokens.items[12].token_type == TokenType.IDENTIFIER);
    std.debug.assert(tokens.items[12].line == 3);
    std.debug.assert(tokens.items[12].col == 11);
    std.debug.assert(tokens.items[13].token_type == TokenType.RIGHT_PAREN);
    std.debug.assert(tokens.items[13].line == 3);
    std.debug.assert(tokens.items[13].col == 12);
}
