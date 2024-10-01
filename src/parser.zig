const std = @import("std");
const lexer = @import("lexer.zig");
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const TokenTypeTag = @import("lexer.zig").TokenTypeTag;
const KeyWord = @import("lexer.zig").KeyWord;
const Operator = @import("lexer.zig").Operator;

const ParserError = error{
    UnexpectedToken,
    ExpectedExpression,
    ExpectedIdentifier,
    SyntaxError,
    Todo,
    Overflow,
    InvalidCharacter,
    OutOfMemory,
    NoSpaceLeft,
};

fn is_token_type(token: Token, comptime token_type: TokenTypeTag) bool {
    return @as(TokenTypeTag, token.token_type) == token_type;
}

const ExprBinary = struct {
    left: *const Expr,
    operator: Operator,
    right: *const Expr,
};
const Unary = enum {
    NEGATIVE,
    NOT,
};
const ExprUnary = struct {
    operator: Unary,
    right: *const Expr,
};
const ExprLiteral = union(enum) {
    STR: []const u8,
    IDENTIFIER: []const u8,
    BOOL: bool,
    NUM: usize,
    UNDEFINED,
};
const Expr = union(enum) {
    LITERAL: ExprLiteral,
    BINARY: ExprBinary,
    UNARY: ExprUnary,
};

const StmtType = union(enum) {
    PRINT,
    EXPRESSION,
    DECLARATION: []const u8,
};

const Stmt = struct {
    expressions: std.ArrayList(*Expr),
    statement_type: StmtType,
};

// TODO: purge as it is not really useful
fn init_statement(allocator: std.mem.Allocator, statement_type: StmtType) Stmt {
    return Stmt{
        .expressions = std.ArrayList(*Expr).init(allocator),
        .statement_type = statement_type,
    };
}

const ParserResult = struct {
    statements: std.ArrayList(Stmt),
    area_allocator: *std.heap.ArenaAllocator,
    org_allocator: std.mem.Allocator,

    pub fn deinit(self: *ParserResult) void {
        self.area_allocator.deinit();
        self.org_allocator.destroy(self.area_allocator);
    }
};

const Parser = struct {
    tokens: ?std.ArrayList(Token),
    cur_token: ?Token,
    idx: usize,
    allocator: std.mem.Allocator,
    arena_allocator: *std.heap.ArenaAllocator,
    org_allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Parser {
        const arena_allocator = try allocator.create(std.heap.ArenaAllocator);
        arena_allocator.* = std.heap.ArenaAllocator.init(allocator);
        const new_allocator = arena_allocator.allocator();

        return Parser{
            .tokens = null,
            .cur_token = null,
            .idx = 0,
            .arena_allocator = arena_allocator,
            .allocator = new_allocator,
            .org_allocator = allocator,
        };
    }

    fn advance_token(self: *Parser) void {
        self.idx += 1;
        self.cur_token = self.tokens.?.items[self.idx];
    }

    fn expect(self: *Parser, comptime token_type: TokenTypeTag) ParserError!void {
        if (!self.match(token_type)) {
            return error.UnexpectedToken;
        }
        self.advance_token();
    }
    fn expect_keyword(self: *Parser, comptime keyword: KeyWord) ParserError!void {
        if (!self.match_keyword(keyword)) {
            return error.UnexpectedToken;
        }
        self.advance_token();
    }

    fn match(self: *Parser, comptime token: TokenTypeTag) bool {
        return is_token_type(self.cur_token.?, token);
    }
    fn match_keyword(self: *Parser, comptime keyword: KeyWord) bool {
        if (self.match(.KEYWORD)) {
            return (self.cur_token.?.token_type.KEYWORD == keyword);
        }
        return false;
    }

    fn accept(self: *Parser, comptime token_type: TokenTypeTag) ?TokenType {
        if (self.match(token_type)) {
            const r = self.cur_token.?.token_type;
            self.advance_token();
            return r;
        }
        return null;
    }
    fn accept_keyword(self: *Parser, comptime keyword: KeyWord) ?TokenType {
        if (self.match_keyword(keyword)) {
            const r = self.cur_token.?.token_type;
            self.advance_token();
            return r;
        }
        return null;
    }

    fn print_statement(self: *Parser) ParserError!Stmt {
        try self.expect_keyword(.PRINT);
        try self.expect(.LEFT_PAREN);
        const expr = try self.expression();
        try self.expect(.RIGHT_PAREN);

        var stmt = init_statement(self.allocator, .PRINT);

        try stmt.expressions.append(expr);
        return stmt;
    }

    fn expression_statement(self: *Parser) ParserError!Stmt {
        const expr = try self.expression();

        var stmt = init_statement(self.allocator, .EXPRESSION);

        try stmt.expressions.append(expr);
        return stmt;
    }

    fn var_declaration(self: *Parser) ParserError!Stmt {
        try self.expect_keyword(.LET);
        if (self.accept(.IDENTIFIER)) |token| {
            try self.expect(.ASSIGNMENT);
            const expr = try self.expression();

            const stmt_type = StmtType{ .DECLARATION = token.IDENTIFIER };
            var stmt = init_statement(self.allocator, stmt_type);
            try stmt.expressions.append(expr);
            return stmt;
        } else {
            return error.ExpectedIdentifier;
        }
    }

    fn statement(self: *Parser) ParserError!Stmt {
        //if (match(FOR)) return forStatement();
        //if (match(IF)) return ifStatement();
        if (self.match_keyword(.PRINT)) return self.print_statement();
        //if (match(RETURN)) return returnStatement();
        //if (match(WHILE)) return whileStatement();
        //if (match(LEFT_BRACE)) return new Stmt.Block(block());
        return self.expression_statement();
    }

    fn expression(self: *Parser) ParserError!*Expr {
        // TODO: a lot
        return self.equality();
    }

    fn equality(self: *Parser) ParserError!*Expr {
        var expr = try self.comparison();

        while (true) {
            if (self.match(.OPERATOR)) {
                const operator = self.cur_token.?.token_type.OPERATOR;
                if (operator == .EQUALS or operator == .NOT_EQUALS) {
                    self.advance_token();

                    const right = try self.comparison();

                    const new_expr = try self.allocator.create(Expr);
                    new_expr.* = .{ .BINARY = .{ .left = expr, .operator = operator, .right = right } };
                    expr = new_expr;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return expr;
    }

    fn comparison(self: *Parser) ParserError!*Expr {
        var expr = try self.term();

        while (true) {
            if (self.match(.OPERATOR)) {
                const operator = self.cur_token.?.token_type.OPERATOR;
                self.advance_token();

                switch (operator) {
                    .GREATER_THAN, .GREATER_OR_EQUAL, .LESS_OR_EQUAL, .LESS_THAN => {
                        const right = try self.term();

                        const new_expr = try self.allocator.create(Expr);
                        new_expr.* = .{ .BINARY = .{ .left = expr, .operator = operator, .right = right } };
                        expr = new_expr;
                    },
                    else => return error.UnexpectedToken,
                }
            } else {
                break;
            }
        }
        return expr;
    }

    fn term(self: *Parser) ParserError!*Expr {
        var expr = try self.factor();

        while (true) {
            if (self.match(.MINUS)) {
                self.advance_token();

                const right = try self.factor();
                const new_expr = try self.allocator.create(Expr);
                new_expr.* = .{ .BINARY = .{ .left = expr, .operator = .SUBTRACTION, .right = right } };
                expr = new_expr;
            } else if (self.match(.PLUS)) {
                self.advance_token();

                const right = try self.factor();
                const new_expr = try self.allocator.create(Expr);
                new_expr.* = .{ .BINARY = .{ .left = expr, .operator = .ADDITION, .right = right } };
                expr = new_expr;
            } else {
                break;
            }
        }
        return expr;
    }

    fn factor(self: *Parser) ParserError!*Expr {
        var expr = try self.unary();

        while (true) {
            if (self.match(.STAR)) {
                const operator = Operator.MULTIPLICATION;
                self.advance_token();

                const right = try self.unary();
                const new_expr = try self.allocator.create(Expr);
                new_expr.* = .{ .BINARY = .{ .left = expr, .operator = operator, .right = right } };
                expr = new_expr;
            } else if (self.match(.SLASH)) {
                const operator = Operator.DIVIDE;
                self.advance_token();

                const right = try self.unary();
                const new_expr = try self.allocator.create(Expr);
                new_expr.* = .{ .BINARY = .{ .left = expr, .operator = operator, .right = right } };
                expr = new_expr;
            } else {
                break;
            }
        }
        return expr;
    }

    fn unary(self: *Parser) ParserError!*Expr {
        if (self.match(.MINUS)) {
            self.advance_token();

            const right = try self.unary();
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .UNARY = .{ .operator = .NEGATIVE, .right = right } };
            return expr;
        } else if (self.match(.NOT)) {
            self.advance_token();

            const right = try self.unary();
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .UNARY = .{ .operator = .NOT, .right = right } };
            return expr;
        }
        return self.primary();
    }

    fn primary(self: *Parser) ParserError!*Expr {
        if (self.accept_keyword(.TRUE)) |_| {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .LITERAL = .{ .BOOL = true } };
            return expr;
        }
        if (self.accept_keyword(.FALSE)) |_| {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .LITERAL = .{ .BOOL = false } };
            return expr;
        }
        if (self.accept_keyword(.UNDEFINED)) |_| {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .LITERAL = .UNDEFINED };
            return expr;
        }

        if (self.accept(.NUM)) |token| {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .LITERAL = .{ .NUM = token.NUM } };
            return expr;
        }
        if (self.accept(.STRING)) |token| {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .LITERAL = .{ .STR = token.STRING } };
            return expr;
        }
        if (self.accept(.IDENTIFIER)) |token| {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .LITERAL = .{ .IDENTIFIER = token.IDENTIFIER } };
            return expr;
        }

        if (self.match(.LEFT_PAREN)) {
            try self.expect(.LEFT_PAREN);
            const expr = try self.expression();
            try self.expect(.RIGHT_PAREN);
            return expr;
        }
        return error.ExpectedExpression;
    }

    fn declaration(self: *Parser) ParserError!Stmt {
        // TODO: check
        // class
        // function

        if (self.match_keyword(.LET)) return self.var_declaration();

        return self.statement();
    }

    pub fn parse_tokens(self: *Parser, tokens: std.ArrayList(Token)) ParserError!ParserResult {
        // Cleanup to be sure
        self.arena_allocator.deinit();
        errdefer self.arena_allocator.deinit();

        var statements = std.ArrayList(Stmt).init(self.allocator);

        self.tokens = tokens;
        self.cur_token = tokens.items[0];

        while (self.cur_token.?.token_type != TokenType.EOF) {
            const stmt = self.declaration() catch |err| {
                std.debug.print("Error parsing token on line {}:{}\n", .{ self.cur_token.?.line, self.cur_token.?.col });
                lexer.print_token(self.cur_token.?);
                return err;
            };
            try statements.append(stmt);
        }

        const result = ParserResult{
            .area_allocator = self.arena_allocator,
            .statements = statements,
            .org_allocator = self.org_allocator,
        };
        return result;
    }
};

pub fn parse_tokens(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) ParserError!ParserResult {
    var parser = try Parser.init(allocator);
    errdefer parser.org_allocator.destroy(parser.arena_allocator);

    return parser.parse_tokens(tokens);
}

fn parse_data(data: []const u8) !ParserResult {
    var tokens = try lexer.parse_source(std.testing.allocator, data);
    defer tokens.clearAndFree();

    return parse_tokens(std.testing.allocator, tokens);
}

fn operator_to_str(op: Operator) []const u8 {
    return switch (op) {
        .ADDITION => "+",
        .SUBTRACTION => "-",
        .GREATER_OR_EQUAL => ">=",
        .GREATER_THAN => ">",
        .LESS_OR_EQUAL => "<=",
        .LESS_THAN => "<",
        .EQUALS => "=",
        .NOT_EQUALS => "!=",
        .MULTIPLICATION => "*",
        .DIVIDE => "/",
    };
}

fn expr_to_str(allocator: std.mem.Allocator, expr: *const Expr) ![]u8 {
    return switch (expr.*) {
        .LITERAL => {
            return try switch (expr.LITERAL) {
                .BOOL => std.fmt.allocPrint(allocator, "{}", .{expr.LITERAL.BOOL}),
                .NUM => std.fmt.allocPrint(allocator, "{}", .{expr.LITERAL.NUM}),
                .STR => std.fmt.allocPrint(allocator, "\"{s}\"", .{expr.LITERAL.STR}),
                .IDENTIFIER => std.fmt.allocPrint(allocator, "{s}", .{expr.LITERAL.IDENTIFIER}),
                .UNDEFINED => std.fmt.allocPrint(allocator, "undefined", .{}),
            };
        },
        .BINARY => {
            const left = try expr_to_str(allocator, expr.BINARY.left);
            const right = try expr_to_str(allocator, expr.BINARY.right);

            const res = try std.fmt.allocPrint(allocator, "{s} {s} ({s})", .{ left, operator_to_str(expr.BINARY.operator), right });
            allocator.free(left);
            allocator.free(right);
            return res;
        },
        .UNARY => {
            const op = switch (expr.UNARY.operator) {
                .NOT => "!",
                .NEGATIVE => "-",
            };
            const right = try expr_to_str(allocator, expr.UNARY.right);
            const res = try std.fmt.allocPrint(allocator, "{s}({s})", .{ op, right });
            allocator.free(right);
            return res;
        },
    };
}

fn print_stmt(stmt: Stmt) !void {
    switch (stmt.statement_type) {
        .DECLARATION => std.debug.print("LET {s} ", .{stmt.statement_type.DECLARATION}),
        .EXPRESSION => std.debug.print("EXPR ", .{}),
        .PRINT => std.debug.print("PRINT ", .{}),
    }

    std.debug.print("(", .{});
    for (stmt.expressions.items) |expr| {
        const to_print = try expr_to_str(std.testing.allocator, expr);
        std.debug.print("{s}", .{to_print});
        std.testing.allocator.free(to_print);
    }
    std.debug.print(")\n", .{});
}

test "invalid token" {
    const data = "1 ~ a + -3";

    var result = try parse_data(data);
    defer result.deinit();

    for (result.statements.items) |stmt| {
        try print_stmt(stmt);
    }
}

test "unexpected token" {
    const data = "1 + a + +3";

    const result = parse_data(data);
    try std.testing.expectError(ParserError.ExpectedExpression, result);
}

fn expect_one(result: ParserResult, comptime expected_stmt: anytype) !*Expr {
    try std.testing.expectEqual(1, result.statements.items.len);
    const stmt = result.statements.items[0];

    const testje = "b";
    if (@TypeOf(testje) == @TypeOf(expected_stmt)) {
        try std.testing.expectEqualStrings(expected_stmt, stmt.statement_type.DECLARATION);
    } else {
        try std.testing.expectEqual(expected_stmt, stmt.statement_type);
    }
    try std.testing.expectEqual(1, stmt.expressions.items.len);

    return stmt.expressions.items[0];
}

test "assignment" {
    const data = "let a = 5";

    var result = try parse_data(data);
    defer result.deinit();

    const expr = try expect_one(result, "a");
    const got = try expr_to_str(std.testing.allocator, expr);
    defer std.testing.allocator.free(got);

    try std.testing.expectEqualStrings("5", got);
}

test "math" {
    const data = "1 + a * (-3 + 6)";
    const expected = "1 + (a * (-(3) + (6)))";

    //     int z, x=5, y=-10, a=4, b=2, c=3;
    // z = -c + x - y * b / sub_one(a);
    // z == 8

    var result = try parse_data(data);
    defer result.deinit();

    const expr = try expect_one(result, StmtType.EXPRESSION);
    const got = try expr_to_str(std.testing.allocator, expr);
    defer std.testing.allocator.free(got);

    try std.testing.expectEqualStrings(expected, got);
}
