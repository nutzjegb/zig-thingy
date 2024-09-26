const std = @import("std");
const lexer = @import("lexer.zig");
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const TokenTypeTag = @import("lexer.zig").TokenTypeTag;
const KeyWord = @import("lexer.zig").KeyWord;

const ParserError = error{
    UnexpectedToken,
    Todo,
};

fn is_token_type(token: Token, comptime token_type: TokenTypeTag) bool {
    return @as(TokenTypeTag, token.token_type) == token_type;
}

// const Sum

const Print = struct {
    val: []const u8,
};

const Parser = struct {
    tokens: std.ArrayList(Token),
    cur_token: Token,
    idx: usize,
    // cur_token: Token,

    pub fn init(tokens: std.ArrayList(Token)) Parser {
        return Parser{
            .tokens = tokens,
            .cur_token = tokens.items[0],
            .idx = 0,
        };
    }

    fn next_token(self: *Parser) void {
        self.idx += 1;
        self.cur_token = self.tokens.items[self.idx];
    }

    fn expect(self: *Parser, comptime token_type: TokenTypeTag) !void {
        if (!is_token_type(self.cur_token, token_type)) {
            return error.UnexpectedToken;
        }
        self.next_token();
    }

    fn accept(self: *Parser, comptime token_type: TokenTypeTag) bool {
        if (is_token_type(self.cur_token, token_type)) {
            self.next_token();
            return true;
        }
        return false;
    }

    fn accept2(self: *Parser, comptime token_type: TokenTypeTag) ?TokenType {
        if (is_token_type(self.cur_token, token_type)) {
            const r = self.cur_token.token_type;
            self.next_token();
            return r;
        }
        return null;
    }

    fn handle_print(self: *Parser) !void {
        // var to_print: []const u8 = undefined;
        // try self.expect(.LEFT_PAREN);
        // switch (self.cur_token.token_type) {
        //     .STRING => |v| to_print = v,
        //     .NUM => |v| {
        //         var buf: [32]u8 = undefined;
        //         to_print = try std.fmt.bufPrint(&buf, "{d}", .{v});
        //     },
        //     else => return error.UnexpectedToken,
        // }
        // self.next_token();
        // try self.expect(.RIGHT_PAREN);
        try self.handle_paren_expr();
    }

    fn handle_let(self: *Parser) !void {
        try self.expect(.IDENTIFIER);
        try self.expect(.ASSIGNMENT);
        try self.handle_expr();
    }

    fn handle_statement(self: *Parser) !void {
        std.debug.print("in handle statement, token {s}\n", .{@tagName(self.cur_token.token_type)});
        // statement ::=
        //      "PRINT" "(" (expression | string | number) ")"
        //      | "IF" "(" comparison ")" "{" {statement} "}"
        //      | "WHILE" "(" comparison ")" "{" {statement} "}"
        //      | "LET" identifier "=" expression
        //      | "DEF" identifier "(" list? ")" "{" {statement} "}"
        //      | identifier "=" (expression | string | number)
        //      | identifier (expression | string | number)

        // statement
        //    : 'if' paren_expr statement
        //    | 'while' paren_expr statement
        //    | '{' statement* '}'
        //    | expr

        // paren_expr
        //    : '(' expr ')'

        // expr
        //    : sum
        //    | id '=' expr

        // sum
        //    : term
        //    | sum '+' term
        //    | sum '-' term

        // term
        //    : id
        //    | integer
        //    | paren_expr

        // switch (self.cur_token.token_type) {
        //     TokenType.KEYWORD => |keyword| {
        //         switch (keyword) {
        //             KeyWord.PRINT => try self.handle_print(),
        //             else => return error.Todo,
        //         }
        //     },
        //     else => return error.Todo,
        // }
        if (self.accept2(.KEYWORD)) |token| {
            switch (token.KEYWORD) {
                .PRINT => try self.handle_print(),
                .LET => try self.handle_let(),
                else => return error.Todo,
            }
        } else {
            return error.Todo;
        }
    }

    fn handle_paren_expr(self: *Parser) !void {
        try self.expect(.LEFT_PAREN);
        try self.handle_expr();
        try self.expect(.RIGHT_PAREN);
    }

    fn handle_expr(self: *Parser) !void {
        // _ = self;
        // return error.Todo;

        // TODO: XXX
        // check if we a identifier, number or string
        // then a + - or != or something
        // or a function call?
        _ = self;
        return error.FixMe;
    }

    fn handle_program(self: *Parser) !void {
        //TODO: create root of the ast or something

        while (self.cur_token.token_type != TokenType.EOF) {
            try self.handle_statement();
        }
    }

    pub fn parse_tokens(self: *Parser) !void {
        self.handle_program() catch |err| {
            std.debug.print("Error parsing token on line {}:{}\n", .{ self.cur_token.line, self.cur_token.col });
            lexer.print_token(self.cur_token);
            return err;
        };
    }
};

pub fn parse_tokens(tokens: std.ArrayList(Token)) !void {
    var parser = Parser.init(tokens);
    try parser.parse_tokens();
}
