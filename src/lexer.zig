const std = @import("std");
const unicode = @import("std").unicode;

// TODO: bad name?
const Operator = enum {
    ASSIGNMENT,
    EQUALS,
    LESS_THAN,
    GREATER_THAN,
    NOT_EQUALS,
    NOT,
    LESS_OR_EQUAL,
    GREATER_OR_EQUAL,
};

const TokenType = union(enum) {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    MINUS,
    PLUS,
    OPERATOR: Operator,
    IDENTIFIER: []const u8,
    STRING: []const u8,
    NUM: usize,
    KEYWORD: []const u8,
    EOF,
};

const LexerToken = struct {
    char: u8,
    pos: usize,
    line: usize,
    col: usize,
};

const Token = struct {
    line: usize,
    col: usize,

    token_type: TokenType,
};

const Keywords = [_][]const u8{
    "let",
    "def",
    "print",
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
        return Token{ .line = self.current_char.line, .col = self.current_char.col, .token_type = token_type };
    }

    fn next_char(self: *Lexer) !void {
        if (self.current_char.pos + 1 == self.source.len) {
            return error.EOF;
        }

        self.prev_char = self.current_char;
        self.current_char.pos += 1;
        self.current_char.char = self.source[self.current_char.pos];

        if (self.current_char.char == '\n') {
            self.current_char.line += 1;
            self.current_char.col = 0;
        } else {
            self.current_char.col += 1;
        }
    }

    fn skip_ignoreable(self: *Lexer) !void {
        while (true) {
            switch (self.current_char.char) {
                '\n', ' ', '\t' => try self.next_char(),
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
                    try self.next_char();
                    const word = self.source[start_pos.pos..self.current_char.pos];
                    return Token{
                        .token_type = TokenType{ .STRING = word },
                        .line = start_pos.line,
                        .col = start_pos.col,
                    };
                },
                else => try self.next_char(),
            }
        }
    }

    fn handle_num(self: *Lexer) !Token {
        const start_pos = self.prev_char.?;
        while (std.ascii.isDigit(self.current_char.char)) {
            try self.next_char();
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
            try self.next_char();
        }
        const word = self.source[start_pos.pos..self.current_char.pos];

        if (is_keyword(word)) {
            return Token{
                .token_type = TokenType{ .KEYWORD = word },
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
        switch (self.current_char.pos) {
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
                    '!' => return self.createToken(TokenType{ .OPERATOR = Operator.NOT }),
                    '=' => return self.createToken(TokenType{ .OPERATOR = Operator.ASSIGNMENT }),
                    else => return error.ParserError,
                }
            },
        }
    }

    pub fn get_token(self: *Lexer) !Token {
        // Ignore comments and spaces, empty lines..
        // TODO: remove, but in loop below?
        self.skip_ignoreable() catch |err| {
            if (err == error.EOF) {
                return Token{ .col = 0, .line = 0, .token_type = TokenType.EOF };
            }
            return err;
        };
        // std.debug.print("current char '{c}' i:{d} {d}:{d}\n", .{ self.current_char.char, self.current_char.pos, self.current_char.line, self.current_char.col });

        // Load next char
        self.next_char() catch |err| {
            if (err == error.EOF) {
                return Token{ .col = 0, .line = 0, .token_type = TokenType.EOF };
            }
            return err;
        };
        const char = self.prev_char.?.char;
        switch (char) {
            '(' => return self.createToken(TokenType.LEFT_PAREN),
            ')' => return self.createToken(TokenType.RIGHT_PAREN),
            '{' => return self.createToken(TokenType.LEFT_BRACE),
            '}' => return self.createToken(TokenType.RIGHT_BRACE),
            '+' => return self.createToken(TokenType.PLUS),
            '-' => return self.createToken(TokenType.MINUS),
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

        // Keyword or expression
        // TODO: switch?
        //bug next_char should be after token creation
        // if (self.current_char == '(') {
        //     self.next_char();
        //     return self.createToken(TokenType.LEFT_PAREN);
        // } else if (self.current_char == ')') {
        //     self.next_char();
        //     return self.createToken(TokenType.RIGHT_PAREN);
        // } else if (self.current_char == '<') {
        //     self.next_char();
        //     return self.createToken(TokenType.LEFT_BRACKET);
        // } else if (self.current_char == '>') {
        //     self.next_char();
        //     return self.createToken(TokenType.RIGHT_BRACKET);
        // } else if (self.current_char == '{') {
        //     self.next_char();
        //     return self.createToken(TokenType.LEFT_BRACE);
        // } else if (self.current_char == '}') {
        //     self.next_char();
        //     return self.createToken(TokenType.RIGHT_BRACE);
        // } else if (self.current_char == '=') {
        //     self.next_char();
        //     return self.createToken(TokenType.EQUALS);
        // } else if (self.current_char == '=') {
        //     self.next_char();
        //     return self.createToken(TokenType.EQUALS);
        // } else if (self.current_char == '"') {
        //     const pos = self.current_pos;
        //     return Token{ .token_type = TokenType{ .STRING = self.get_quoted_word() }, .pos = pos };
        // } else if (std.ascii.isAlphanumeric(self.current_char)) {
        //     const start_pos = self.current_pos;
        //     while (std.ascii.isAlphanumeric(self.peek())) {
        //         self.next_char();
        //     }
        //     self.next_char();
        //     const word = self.source[start_pos..self.current_pos];

        //     if (is_keyword(word)) {
        //         return Token{
        //             .token_type = TokenType{ .KEYWORD = word },
        //             .pos = start_pos,
        //         };
        //     } else {
        //         return Token{
        //             .token_type = TokenType{ .IDENTIFIER = word },
        //             .pos = start_pos,
        //         };
        //     }
        // }
        return error.Stuk;
    }
};

pub fn parse_source(allocator: std.mem.Allocator, source: []const u8) !std.ArrayList(Token) {
    var token_list = std.ArrayList(Token).init(allocator);
    var lexer = try Lexer.init(source);
    while (true) {
        const token = try lexer.get_token();
        switch (token.token_type) {
            .STRING, .IDENTIFIER, .KEYWORD => |v| std.debug.print("token: {s} '{s}'\n", .{ @tagName(token.token_type), v }),
            .OPERATOR => |v| std.debug.print("token: {s} {s}\n", .{ @tagName(token.token_type), @tagName(v) }),
            .NUM => |v| std.debug.print("token: {s} {d}\n", .{ @tagName(token.token_type), v }),
            else => std.debug.print("token: {s}\n", .{@tagName(token.token_type)}),
        }
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
    std.debug.assert(true);
}
