const std = @import("std");
const testing = std.testing;
// const expectEqualSlices = testing.expectEqualSlices;
// const expectError = testing.expectError;

const Lexer = @import("lexer.zig");
const Token = @import("token.zig");
const Keyword = Token.Keyword;

// fn expectEqual(comptime T: type, expected: T, actual: T) !void {
//     try testing.expectEqual(expected, actual);
// }

fn expectSyntaxError(string: []const u8, message: []const u8) !void {
    var lexer = Lexer.new(string);
    try testing.expectError(error.SyntaxError, lexer.nextToken());
    try testing.expectEqualSlices(u8, message, lexer.error_message.?);
}

fn expectTokenType(string: []const u8, token_type: Token.Kind) !void {
    var lexer = Lexer.new(string);
    const token = try lexer.nextToken();
    try testing.expectEqual(token_type, token.type);
}

fn expectKeyword(string: []const u8, keyword: Keyword) !void {
    var lexer = Lexer.new(string);
    const token = try lexer.nextToken();
    try testing.expectEqual(Token.Kind.ident, token.type);
    try testing.expectEqual(Token.Value.keyword, token.value);
    try testing.expectEqual(keyword, token.value.keyword);
}

fn expectTokenString(string: []const u8, token_type: Token.Kind, token_value: []const u8) !void {
    var lexer = Lexer.new(string);
    const token = try lexer.nextToken();
    try testing.expectEqual(token_type, token.type);
    try testing.expectEqual(Token.Value.string, token.value);
    // std.debug.print("\nExpected: {s}\nActual: {s}\n", .{token_value, token.value.string});
    try testing.expectEqualSlices(u8, token_value, token.value.string);
}

fn expectTokenBuffer(string: []const u8, token_type: Token.Kind, token_value: []const u8) !void {
    var lexer = Lexer.new(string);
    const token = try lexer.nextToken();
    try testing.expectEqual(token_type, token.type);
    try testing.expectEqual(Token.Value.buffer, token.value);
    try testing.expectEqualSlices(u8, token_value, token.value.buffer.items);
}

fn testIdents(idents: []const []const u8) !void {
    for (idents) |ident| {
        try expectTokenString(ident, .ident, ident);
    }
}

fn expectChar(string: []const u8, value: u8) !void {
    var lexer = Lexer.new(string);
    const token = try lexer.nextToken();
    try testing.expectEqual(Token.Kind.char, token.type);
    try testing.expectEqual(Token.Value.char, token.value);
    try testing.expectEqual(value, token.value.char);
}

fn expectOperator(string: []const u8, token_type: Token.Kind) !void {
    var lexer = Lexer.new(string);
    lexer.slash_is_regex = false;
    const token = try lexer.nextToken();
    try testing.expectEqual(token_type, token.type);
}

// zig fmt: off
test "eof" { try expectTokenType("", .eof); }
test "space" { try expectTokenType(" ", .space); }
test "tab" { try expectTokenType("\t", .space); }
test "newline" { try expectTokenType("\n", .newline); }
test "multiple newlines" { try expectTokenType("\n\n\n", .newline); }
test "underscore" { try expectTokenType("_", .underscore); }

test "def" { try expectKeyword("def", .def); }
test "if" { try expectKeyword("if", .@"if"); }
test "else" { try expectKeyword("else", .@"else"); }
test "elsif" { try expectKeyword("elsif", .elsif); }
test "end" { try expectKeyword("end", .end); }
test "true" { try expectKeyword("true", .true); }
test "false" { try expectKeyword("false", .false); }
test "class" { try expectKeyword("class", .class); }
test "module" { try expectKeyword("module", .module); }
test "include" { try expectKeyword("include", .include); }
test "extend" { try expectKeyword("extend", .extend); }
test "while" { try expectKeyword("while", .@"while"); }
test "until" { try expectKeyword("until", .until); }
test "nil" { try expectKeyword("nil", .nil); }
test "do" { try expectKeyword("do", .do); }
test "yield" { try expectKeyword("yield", .yield); }
test "return" { try expectKeyword("return", .@"return"); }
test "unless" { try expectKeyword("unless", .unless); }
test "next" { try expectKeyword("next", .next); }
test "break" { try expectKeyword("break", .@"break"); }
test "begin" { try expectKeyword("begin", .begin); }
test "lib" { try expectKeyword("lib", .lib); }
test "fun" { try expectKeyword("fun", .fun); }
test "type" { try expectKeyword("type", .type); }
test "struct" { try expectKeyword("struct", .@"struct"); }
test "union" { try expectKeyword("union", .@"union"); }
test "enum" { try expectKeyword("enum", .@"enum"); }
test "macro" { try expectKeyword("macro", .macro); }
test "out" { try expectKeyword("out", .out); }
test "require" { try expectKeyword("require", .require); }
test "case" { try expectKeyword("case", .case); }
test "when" { try expectKeyword("when", .when); }
test "select" { try expectKeyword("select", .select); }
test "then" { try expectKeyword("then", .then); }
test "of" { try expectKeyword("of", .of); }
test "abstract" { try expectKeyword("abstract", .abstract); }
test "rescue" { try expectKeyword("rescue", .rescue); }
test "ensure" { try expectKeyword("ensure", .ensure); }
test "alias" { try expectKeyword("alias", .alias); }
test "pointerof" { try expectKeyword("pointerof", .pointerof); }
test "sizeof" { try expectKeyword("sizeof", .sizeof); }
test "instance_sizeof" { try expectKeyword("instance_sizeof", .instance_sizeof); }
test "offsetof" { try expectKeyword("offsetof", .offsetof); }
test "as" { try expectKeyword("as", .as); }
test "typeof" { try expectKeyword("typeof", .typeof); }
test "for" { try expectKeyword("for", .@"for"); }
test "in" { try expectKeyword("in", .in); }
test "with" { try expectKeyword("with", .with); }
test "self" { try expectKeyword("self", .self); }
test "super" { try expectKeyword("super", .super); }
test "private" { try expectKeyword("private", .private); }
test "protected" { try expectKeyword("protected", .protected); }
test "asm" { try expectKeyword("asm", .@"asm"); }
test "uninitialized" { try expectKeyword("uninitialized", .uninitialized); }
test "annotation" { try expectKeyword("annotation", .annotation); }
test "verbatim" { try expectKeyword("verbatim", .verbatim); }
test "is_a?" { try expectKeyword("is_a?", .is_a_question); }
test "as?" { try expectKeyword("as?", .as_question); }
test "nil?" { try expectKeyword("nil?", .nil_question); }
test "responds_to?" { try expectKeyword("responds_to?", .responds_to_question); }

test "idents" {
    try testIdents(&[_][]const u8{
        "ident", "something", "with_underscores", "with_1", "foo?", "bar!", "fooBar",
        // "❨╯°□°❩╯︵┻━┻" // TODO
    });
}

test "idents ending with question mark" {
    try testIdents(&[_][]const u8{
        "def?", "if?", "else?", "elsif?", "end?", "true?", "false?", "class?", "while?",
        "do?", "yield?", "return?", "unless?", "next?", "break?", "begin?"
    });
}

test "idents ending with exclamation mark" {
    try testIdents(&[_][]const u8{
        "def!", "if!", "else!", "elsif!", "end!", "true!", "false!", "class!", "while!",
        "nil!", "do!", "yield!", "return!", "unless!", "next!", "break!", "begin!"
    });
}

// TODO: numbers

test "char" { try expectChar("'a'", 'a'); }
test "alert char" { try expectChar("'\\a'", std.ascii.control_code.bel); }
test "backspace char" { try expectChar("'\\b'", std.ascii.control_code.bs); }
test "newline char" { try expectChar("'\\n'", '\n'); }
test "tab char" { try expectChar("'\\t'", '\t'); }
test "vertical tab char" { try expectChar("'\\v'", std.ascii.control_code.vt); }
test "form feed char" { try expectChar("'\\f'", std.ascii.control_code.ff); }
test "carriage return char" { try expectChar("'\\r'", '\r'); }
test "null char" { try expectChar("'\\0'", std.ascii.control_code.nul); }
// TODO: duplicate test
test "null char" { try expectChar("'\\0'", std.ascii.control_code.nul); }
test "single quote char" { try expectChar("'\\''", '\''); }
test "backslash char" { try expectChar("'\\\\'", '\\'); }

test "unterminated char" {
    try expectSyntaxError("'", "unterminated char literal");
}

test "unterminated char escape sequence" {
    try expectSyntaxError("'\\", "unterminated char literal");
}

test "=" { try expectOperator("=", .op_eq); }
test "<" { try expectOperator("<", .op_lt); }
test "<=" { try expectOperator("<=", .op_lt_eq); }
test ">" { try expectOperator(">", .op_gt); }
test ">=" { try expectOperator(">=", .op_gt_eq); }
test "+" { try expectOperator("+", .op_plus); }
test "-" { try expectOperator("-", .op_minus); }
test "*" { try expectOperator("*", .op_star); }
test "/" { try expectOperator("/", .op_slash); }
test "//" { try expectOperator("//", .op_slash_slash); }
test "(" { try expectOperator("(", .op_lparen); }
test ")" { try expectOperator(")", .op_rparen); }
test "==" { try expectOperator("==", .op_eq_eq); }
test "!=" { try expectOperator("!=", .op_bang_eq); }
test "=~" { try expectOperator("=~", .op_eq_tilde); }
test "!" { try expectOperator("!", .op_bang); }
test "," { try expectOperator(",", .op_comma); }
test "." { try expectOperator(".", .op_period); }
test ".." { try expectOperator("..", .op_period_period); }
test "..." { try expectOperator("...", .op_period_period_period); }
test "&&" { try expectOperator("&&", .op_amp_amp); }
test "||" { try expectOperator("||", .op_bar_bar); }
test "|" { try expectOperator("|", .op_bar); }
test "{" { try expectOperator("{", .op_lcurly); }
test "}" { try expectOperator("}", .op_rcurly); }
test "?" { try expectOperator("?", .op_question); }
test ":" { try expectOperator(":", .op_colon); }
test "+=" { try expectOperator("+=", .op_plus_eq); }
test "-=" { try expectOperator("-=", .op_minus_eq); }
test "*=" { try expectOperator("*=", .op_star_eq); }
test "/=" { try expectOperator("/=", .op_slash_eq); }
test "%=" { try expectOperator("%=", .op_percent_eq); }
test "//=" { try expectOperator("//=", .op_slash_slash_eq); }
test "&=" { try expectOperator("&=", .op_amp_eq); }
test "|=" { try expectOperator("|=", .op_bar_eq); }
test "^=" { try expectOperator("^=", .op_caret_eq); }
test "**=" { try expectOperator("**=", .op_star_star_eq); }
test "<<" { try expectOperator("<<", .op_lt_lt); }
test ">>" { try expectOperator(">>", .op_gt_gt); }
test "%" { try expectOperator("%", .op_percent); }
test "&" { try expectOperator("&", .op_amp); }
test "|" { try expectOperator("|", .op_bar); }
test "^" { try expectOperator("^", .op_caret); }
test "**" { try expectOperator("**", .op_star_star); }
test "<<=" { try expectOperator("<<=", .op_lt_lt_eq); }
test ">>=" { try expectOperator(">>=", .op_gt_gt_eq); }
test "~" { try expectOperator("~", .op_tilde); }
test "[]" { try expectOperator("[]", .op_lsquare_rsquare); }
test "[]=" { try expectOperator("[]=", .op_lsquare_rsquare_eq); }
test "[" { try expectOperator("[", .op_lsquare); }
test "]" { try expectOperator("]", .op_rsquare); }
test "::" { try expectOperator("::", .op_colon_colon); }
test "<=>" { try expectOperator("<=>", .op_lt_eq_gt); }
test "=>" { try expectOperator("=>", .op_eq_gt); }
test "||=" { try expectOperator("||=", .op_bar_bar_eq); }
test "&&=" { try expectOperator("&&=", .op_amp_amp_eq); }
test "===" { try expectOperator("===", .op_eq_eq_eq); }
test ";" { try expectOperator(";", .op_semicolon); }
test "->" { try expectOperator("->", .op_minus_gt); }
test "[]?" { try expectOperator("[]?", .op_lsquare_rsquare_question); }
test "{%" { try expectOperator("{%", .op_lcurly_percent); }
test "{{" { try expectOperator("{{", .op_lcurly_lcurly); }
test "%}" { try expectOperator("%}", .op_percent_rcurly); }
test "@[" { try expectOperator("@[", .op_at_lsquare); }
test "!~" { try expectOperator("!~", .op_bang_tilde); }
test "&+" { try expectOperator("&+", .op_amp_plus); }
test "&-" { try expectOperator("&-", .op_amp_minus); }
test "&*" { try expectOperator("&*", .op_amp_star); }
test "&**" { try expectOperator("&**", .op_amp_star_star); }
test "&+=" { try expectOperator("&+=", .op_amp_plus_eq); }
test "&-=" { try expectOperator("&-=", .op_amp_minus_eq); }
test "&*=" { try expectOperator("&*=", .op_amp_star_eq); }

test "!@foo" { try expectTokenType("!@foo", .op_bang); }
test "+@foo" { try expectTokenType("+@foo", .op_plus); }
test "-@foo" { try expectTokenType("-@foo", .op_minus); }
test "&+@foo" { try expectTokenType("&+@foo", .op_amp_plus); }
test "&-@foo" { try expectTokenType("&-@foo", .op_amp_minus); }

test "const" { try expectTokenString("Foo", .@"const", "Foo"); }
test "instance var" { try expectTokenString("@foo", .instance_var, "@foo"); }
test "class var" { try expectTokenString("@@foo", .class_var, "@@foo"); }

test "globals" {
    for ([_][]const u8{"$foo", "$FOO", "$_foo", "$foo123"}) |global| {
        try expectTokenString(global, .global, global);
    }
}

test "symbols" {
    for ([_][]const u8{
        ":foo", ":foo!", ":foo?", ":foo=", ":\"foo\"", ":+", ":-", ":*", ":/", "://", // TODO: ":かたな"
        ":==", ":<", ":<=", ":>", ":>=", ":!", ":!=", ":=~", ":!~", ":&", ":|",
        ":^", ":~", ":**", ":>>", ":<<", ":%", ":[]", ":[]?", ":[]=", ":<=>", ":===",
        ":&+", ":&-", ":&*", ":&**"
    }) |symbol| {
        const value = if (symbol[1] == '"') symbol[2..symbol.len - 1] else symbol[1..];
        try expectTokenString(symbol, .symbol, value);
    }
}
