const std = @import("std");
const lexer = @import("lexer.zig");
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const TokenTypeTag = @import("lexer.zig").TokenTypeTag;
const KeyWord = @import("lexer.zig").KeyWord;
const Operator = @import("lexer.zig").Operator;

const ParserError = error{
    UnexpectedToken,
    Todo,
    Overflow,
    InvalidCharacter,
    OutOfMemory,
    NoSpaceLeft,
};

// fn is_type(token: Token, comptime T: type) bool {
//     return @as(T, token.token_type) == T;
// }
fn is_token_type(token: Token, comptime token_type: TokenTypeTag) bool {
    return @as(TokenTypeTag, token.token_type) == token_type;
}

// const Sum

// const Print = struct {
//     val: []const u8,
// };
const ExprBinary = struct {
    left: *const Expr,
    operator: Operator,
    right: *const Expr,
    //allocator: std.mem.Allocator,

    //fn init(allocator: std.mem.Allocator) ExprBinary {

    //}

    // fn cleanup(self: *Parser) void {
    //     self.left.
    // }
};
const ExprUnary = struct {
    operator: Operator,
    right: *const Expr,
};
const Expr = union(enum) {
    LITERAL: []const u8,
    BINARY: ExprBinary,
    UNARY: ExprUnary,
};

const StmtType = enum {
    PRINT,
    EXPRESSION,
};

const Stmt = struct {
    expressions: std.ArrayList(Expr),
    expression_type: StmtType,

    fn init(allocator: std.mem.Allocator, expression_type: StmtType) Stmt {
        return Stmt{
            .expressions = std.ArrayList(Expr).init(allocator),
            .expression_type = expression_type,
        };
    }

    pub fn print(allocator: std.mem.Allocator, expression: Expr) ParserError!Stmt {
        var stmt = Stmt.init(allocator, .PRINT);
        try stmt.expressions.append(expression);
        return stmt;
    }

    pub fn cleanup(self: *Stmt) void {
        for (self.expressions) |expr| {
            expr.clearAndFree();
        }
        self.expressions.clearAndFree();
    }
};

const Parser = struct {
    tokens: std.ArrayList(Token),
    cur_token: Token,
    idx: usize,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) Parser {
        return Parser{
            .tokens = tokens,
            .cur_token = tokens.items[0],
            .idx = 0,
            .allocator = allocator,
        };
    }

    fn alloc_expr(self: *Parser) ParserError!*Stmt {
        return self.allocator.create(Expr);
    }

    fn next_token(self: *Parser) void {
        self.idx += 1;
        self.cur_token = self.tokens.items[self.idx];
    }

    fn expect(self: *Parser, comptime token_type: TokenTypeTag) ParserError!void {
        if (!self.match(token_type)) {
            return error.UnexpectedToken;
        }
        self.next_token();
    }
    fn expect_keyword(self: *Parser, comptime keyword: KeyWord) ParserError!void {
        if (!self.match_keyword(keyword)) {
            return error.UnexpectedToken;
        }
        self.next_token();
    }

    fn match(self: *Parser, comptime token: TokenTypeTag) bool {
        return is_token_type(self.cur_token, token);
    }
    fn match_keyword(self: *Parser, comptime keyword: KeyWord) bool {
        if (self.match(.KEYWORD)) {
            return (self.cur_token.token_type.KEYWORD == keyword);
        }
        return false;
    }

    fn print_statement(self: *Parser) ParserError!Stmt {
        try self.expect_keyword(.PRINT);
        try self.expect(.LEFT_PAREN);
        const expr = try self.expression();
        try self.expect(.RIGHT_PAREN);

        return Stmt.print(self.allocator, expr);
    }

    fn accept(self: *Parser, comptime token_type: TokenTypeTag) ?TokenType {
        if (is_token_type(self.cur_token, token_type)) {
            const r = self.cur_token.token_type;
            self.next_token();
            return r;
        }
        return null;
    }

    fn statement(self: *Parser) ParserError!Stmt {
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

        std.debug.print("in statement, token {s}\n", .{@tagName(self.cur_token.token_type)});

        //if (match(FOR)) return forStatement();
        //if (match(IF)) return ifStatement();
        if (self.match_keyword(.PRINT)) return self.print_statement();
        //if (match(RETURN)) return returnStatement();
        //if (match(WHILE)) return whileStatement();
        //if (match(LEFT_BRACE)) return new Stmt.Block(block());
        // return expressionStatement();
        return error.Todo;
    }

    fn expression(self: *Parser) ParserError!Expr {
        // TODO: a lot
        return self.equality();
    }

    fn equality(self: *Parser) ParserError!Expr {
        var expr = try self.comparison();

        while (true) {
            if (self.match(.OPERATOR)) {
                const operator = self.cur_token.token_type.OPERATOR;
                if (operator == .EQUALS or operator == .NOT_EQUALS) {
                    self.next_token();

                    const right = try self.comparison();
                    expr = Expr{ .BINARY = .{ .left = &expr, .operator = operator, .right = &right } };
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return expr;
    }

    fn comparison(self: *Parser) ParserError!Expr {
        var expr = try self.term();

        while (true) {
            if (self.match(.OPERATOR)) {
                const operator = self.cur_token.token_type.OPERATOR;
                self.next_token();

                switch (operator) {
                    .GREATER_THAN => {
                        const right = try self.term();
                        expr = Expr{ .BINARY = .{ .left = &expr, .operator = operator, .right = &right } };
                    },
                    else => return error.Todo,
                }
            } else {
                break;
            }
        }
        return expr;
    }

    fn term(self: *Parser) ParserError!Expr {
        var expr = try self.factor();

        while (true) {
            if (self.match(.MINUS) or self.match(.PLUS)) {
                const operator = self.cur_token.token_type.OPERATOR;
                self.next_token();

                const right = try self.factor();
                expr = Expr{ .BINARY = .{ .left = &expr, .operator = operator, .right = &right } };
            } else {
                break;
            }
        }
        return expr;
    }

    fn factor(self: *Parser) ParserError!Expr {
        var expr = try self.unary();

        while (true) {
            if (self.match(.SLASH) or self.match(.STAR)) {
                const operator = self.cur_token.token_type.OPERATOR;
                self.next_token();

                const right = try self.unary();
                expr = Expr{ .BINARY = .{ .left = &expr, .operator = operator, .right = &right } };
            } else {
                break;
            }
        }
        return expr;
    }

    fn unary(self: *Parser) ParserError!Expr {
        if (self.match(.MINUS) or self.match(.NOT)) {
            const operator = self.cur_token.token_type.OPERATOR;
            self.next_token();

            const right = try self.unary();
            return Expr{ .UNARY = .{ .operator = operator, .right = &right } };
        }
        return self.primary();
    }

    fn primary(self: *Parser) ParserError!Expr {
        // if (match(FALSE)) return Expr.literal(false);
        // if (match(TRUE)) return Expr.literal(true);
        // if (match(NIL)) return Expr.literal(null);

        if (self.accept(.NUM)) |token| {
            const max_len = 20;
            var buf: [max_len]u8 = undefined;
            const numAsString = try std.fmt.bufPrint(&buf, "{}", .{token.NUM});
            return Expr{ .LITERAL = numAsString };
        }
        if (self.accept(.STRING)) |token| {
            return Expr{ .LITERAL = token.STRING };
        }
        // if identifier
        if (self.match(.LEFT_PAREN)) {
            try self.expect(.LEFT_PAREN);
            const expr = try self.expression();
            try self.expect(.RIGHT_PAREN);
            return expr;
        }
        return error.Todo;
    }

    fn declaration(self: *Parser) ParserError!Stmt {
        std.debug.print("in declaration, token {s}\n", .{@tagName(self.cur_token.token_type)});

        // TODO: check
        // class
        // function
        // assignment

        return self.statement();
    }

    // fn handle_paren_expr(self: *Parser) !void {
    //     try self.expect(.LEFT_PAREN);
    //     try self.handle_expr();
    //     try self.expect(.RIGHT_PAREN);
    // }

    pub fn parse_tokens(self: *Parser) ParserError!std.ArrayList(Stmt) {
        var statements = std.ArrayList(Stmt).init(self.allocator);
        errdefer statements.clearAndFree();

        while (self.cur_token.token_type != TokenType.EOF) {
            const stmt = self.declaration() catch |err| {
                std.debug.print("Error parsing token on line {}:{}\n", .{ self.cur_token.line, self.cur_token.col });
                lexer.print_token(self.cur_token);
                return err;
            };
            try statements.append(stmt);
        }
        return statements;
    }
};

pub fn parse_tokens(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) ParserError!std.ArrayList(Stmt) {
    var parser = Parser.init(allocator, tokens);
    return parser.parse_tokens();
}

fn test_token_parser(data: []const u8) ParserError!void {
    var tokens = try lexer.parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    var statements = try parse_tokens(std.testing.allocator, tokens);
    defer statements.clearAndFree();
}

test "addition" {
    const data = "a + b + c";

    _ = try test_token_parser(data);
}
