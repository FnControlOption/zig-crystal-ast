const Token = @This();
const std = @import("std");
const ArrayList = std.ArrayList;
const FormatOptions = std.fmt.FormatOptions;
const Location = @import("location.zig");
const NumberKind = @import("ast.zig").NumberKind;

pub const Keyword = enum {
    abstract,
    alias,
    annotation,
    as,
    as_question,
    @"asm",
    begin,
    @"break",
    case,
    class,
    def,
    do,
    @"else",
    elsif,
    end,
    ensure,
    @"enum",
    extend,
    false,
    @"for",
    fun,
    @"if",
    in,
    include,
    instance_sizeof,
    is_a_question,
    lib,
    macro,
    module,
    next,
    nil,
    nil_question,
    of,
    offsetof,
    out,
    pointerof,
    private,
    protected,
    require,
    rescue,
    responds_to_question,
    @"return",
    select,
    self,
    sizeof,
    @"struct",
    super,
    then,
    true,
    type,
    typeof,
    uninitialized,
    @"union",
    unless,
    until,
    verbatim,
    when,
    @"while",
    with,
    yield,

    pub fn toString(keyword: Keyword) []const u8 {
        return switch (keyword) {
            .as_question => "as?",
            .is_a_question => "is_a?",
            .nil_question => "nil?",
            .responds_to_question => "responds_to?",
            else => @tagName(keyword),
        };
    }

    pub fn format(keyword: Keyword, comptime fmt: []const u8, opt: FormatOptions, writer: anytype) !void {
        _ = opt;
        _ = fmt;
        try writer.writeAll(keyword.toString());
    }
};

pub const Kind = enum {
    eof,
    space,
    newline,

    ident,
    @"const",
    instance_var,
    class_var,

    char,
    string,
    symbol,
    number,

    underscore,
    comment,

    delimiter_start,
    delimiter_end,

    string_array_start,
    interpolation_start,
    symbol_array_start,
    string_array_end,

    global,
    global_match_data_index,

    magic_dir,
    magic_end_line,
    magic_file,
    magic_line,

    macro_literal,
    macro_expression_start,
    macro_control_start,
    macro_var,
    macro_end,

    // the following operator kinds should be sorted by their codepoints

    // zig fmt: off
    op_bang,                     // !
    op_bang_eq,                  // !=
    op_bang_tilde,               // !~
    op_dollar_question,          // $?
    op_dollar_tilde,             // $~
    op_percent,                  // %
    op_percent_eq,               // %=
    op_percent_rcurly,           // %}
    op_amp,                      // &
    op_amp_amp,                  // &&
    op_amp_amp_eq,               // &&=
    op_amp_star,                 // &*
    op_amp_star_star,            // &**
    op_amp_star_eq,              // &*=
    op_amp_plus,                 // &+
    op_amp_plus_eq,              // &+=
    op_amp_minus,                // &-
    op_amp_minus_eq,             // &-=
    op_amp_eq,                   // &=
    op_lparen,                   // (
    op_rparen,                   // )
    op_star,                     // *
    op_star_star,                // **
    op_star_star_eq,             // **=
    op_star_eq,                  // *=
    op_plus,                     // +
    op_plus_eq,                  // +=
    op_comma,                    // ,
    op_minus,                    // -
    op_minus_eq,                 // -=
    op_minus_gt,                 // ->
    op_period,                   // .
    op_period_period,            // ..
    op_period_period_period,     // ...
    op_slash,                    // /
    op_slash_slash,              // //
    op_slash_slash_eq,           // //=
    op_slash_eq,                 // /=
    op_colon,                    // :
    op_colon_colon,              // ::
    op_semicolon,                // ;
    op_lt,                       // <
    op_lt_lt,                    // <<
    op_lt_lt_eq,                 // <<=
    op_lt_eq,                    // <=
    op_lt_eq_gt,                 // <=>
    op_eq,                       // =
    op_eq_eq,                    // ==
    op_eq_eq_eq,                 // ===
    op_eq_gt,                    // =>
    op_eq_tilde,                 // =~
    op_gt,                       // >
    op_gt_eq,                    // >=
    op_gt_gt,                    // >>
    op_gt_gt_eq,                 // >>=
    op_question,                 // ?
    op_at_lsquare,               // @[
    op_lsquare,                  // [
    op_lsquare_rsquare,          // []
    op_lsquare_rsquare_eq,       // []=
    op_lsquare_rsquare_question, // []?
    op_rsquare,                  // ]
    op_caret,                    // ^
    op_caret_eq,                 // ^=
    op_grave,                    // `
    op_lcurly,                   // {
    op_lcurly_percent,           // {%
    op_lcurly_lcurly,            // {{
    op_bar,                      // |
    op_bar_eq,                   // |=
    op_bar_bar,                  // ||
    op_bar_bar_eq,               // ||=
    op_rcurly,                   // }
    op_tilde,                    // ~
    // zig fmt: on

    pub fn toString(kind: Kind) []const u8 {
        return switch (kind) {
            .op_bang => "!",
            .op_bang_eq => "!=",
            .op_bang_tilde => "!~",
            .op_dollar_question => "$?",
            .op_dollar_tilde => "$~",
            .op_percent => "%",
            .op_percent_eq => "%=",
            .op_percent_rcurly => "%}",
            .op_amp => "&",
            .op_amp_amp => "&&",
            .op_amp_amp_eq => "&&=",
            .op_amp_star => "&*",
            .op_amp_star_star => "&**",
            .op_amp_star_eq => "&*=",
            .op_amp_plus => "&+",
            .op_amp_plus_eq => "&+=",
            .op_amp_minus => "&-",
            .op_amp_minus_eq => "&-=",
            .op_amp_eq => "&=",
            .op_lparen => "(",
            .op_rparen => ")",
            .op_star => "*",
            .op_star_star => "**",
            .op_star_star_eq => "**=",
            .op_star_eq => "*=",
            .op_plus => "+",
            .op_plus_eq => "+=",
            .op_comma => ",",
            .op_minus => "-",
            .op_minus_eq => "-=",
            .op_minus_gt => "->",
            .op_period => ".",
            .op_period_period => "..",
            .op_period_period_period => "...",
            .op_slash => "/",
            .op_slash_slash => "//",
            .op_slash_slash_eq => "//=",
            .op_slash_eq => "/=",
            .op_colon => ":",
            .op_colon_colon => "::",
            .op_semicolon => ";",
            .op_lt => "<",
            .op_lt_lt => "<<",
            .op_lt_lt_eq => "<<=",
            .op_lt_eq => "<=",
            .op_lt_eq_gt => "<=>",
            .op_eq => "=",
            .op_eq_eq => "==",
            .op_eq_eq_eq => "===",
            .op_eq_gt => "=>",
            .op_eq_tilde => "=~",
            .op_gt => ">",
            .op_gt_eq => ">=",
            .op_gt_gt => ">>",
            .op_gt_gt_eq => ">>=",
            .op_question => "?",
            .op_at_lsquare => "@[",
            .op_lsquare => "[",
            .op_lsquare_rsquare => "[]",
            .op_lsquare_rsquare_eq => "[]=",
            .op_lsquare_rsquare_question => "[]?",
            .op_rsquare => "]",
            .op_caret => "^",
            .op_caret_eq => "^=",
            .op_grave => "`",
            .op_lcurly => "{",
            .op_lcurly_percent => "{%",
            .op_lcurly_lcurly => "{{",
            .op_bar => "|",
            .op_bar_eq => "|=",
            .op_bar_bar => "||",
            .op_bar_bar_eq => "||=",
            .op_rcurly => "}",
            .op_tilde => "~",

            .magic_dir => "__DIR__",
            .magic_end_line => "__END_LINE__",
            .magic_file => "__FILE__",
            .magic_line => "__LINE__",

            else => @tagName(kind),
        };
    }

    pub fn format(kind: Kind, comptime fmt: []const u8, opt: FormatOptions, writer: anytype) !void {
        _ = opt;
        _ = fmt;
        try writer.writeAll(kind.toString());
    }

    pub fn isOperator(kind: Kind) bool {
        return @enumToInt(kind) >= @enumToInt(Kind.op_bang) and
            @enumToInt(kind) <= @enumToInt(Kind.op_tilde);
    }

    pub fn isAssignmentOperator(kind: Kind) bool {
        return switch (kind) {
            // zig fmt: off
            .op_plus_eq, .op_minus_eq, .op_star_eq, .op_slash_eq, .op_slash_slash_eq,
            .op_percent_eq, .op_bar_eq, .op_amp_eq, .op_caret_eq, .op_star_star_eq,
            .op_lt_lt_eq, .op_gt_gt_eq, .op_bar_bar_eq, .op_amp_amp_eq, .op_amp_plus_eq,
            .op_amp_minus_eq, .op_amp_star_eq
            // zig fmt: on
            => true,
            else => false,
        };
    }

    pub fn isMagic(kind: Kind) bool {
        return switch (kind) {
            .magic_dir, .magic_end_line, .magic_file, .magic_line => true,
            else => false,
        };
    }
};

pub const Value = union(enum) {
    char: u8,
    // utf8: []const u8,
    string: []const u8,
    keyword: Keyword,
    none,

    pub fn isAnyKeyword(value: Value) bool {
        return value == .keyword;
    }

    pub fn isKeyword(value: Value, keyword: Keyword) bool {
        return value == .keyword and value.keyword == keyword;
    }

    pub fn isString(value: Value, string: []const u8) bool {
        return value == .string and std.mem.eql(u8, value.string, string);
    }
};

type: Kind = .eof,
value: Value = .none,
number_kind: NumberKind = .i32,
line_number: usize = 0,
column_number: usize = 0,
filename: ?[]const u8 = null,
delimiter_state: DelimiterState = .{},
macro_state: MacroState = .{},
passed_backslash_newline: bool = false,
doc_buffer: ?ArrayList(u8) = null,
raw: []const u8 = "",
start: usize = 0,
invalid_escape: bool = false,

_location: ?Location = null,

pub const MacroState = struct {
    whitespace: bool = true,
    nest: i32 = 0,
    control_nest: i32 = 0,
    delimiter_state: ?DelimiterState = null,
    beginning_of_line: bool = true,
    yields: bool = false,
    comment: bool = false,
    // heredocs = null,

    // pub fn default() MacroState {
    //     return .{
    //         .whitespace = true,
    //         .nest = 0,
    //         .control_nest = 0,
    //         // .delimiter_state = null,
    //         .beginning_of_line = true,
    //         .yields = false,
    //         .comment = false,
    //         // .heredocs = null,
    //     };
    // }
};

pub const DelimiterKind = enum {
    string,
    regex,
    string_array,
    symbol_array,
    command,
    heredoc,
};

pub const Delimiters = union(DelimiterKind) {
    pub const Pair = struct { nest: u8, end: u8 };

    string: Pair,
    regex: Pair,
    string_array: Pair,
    symbol_array: Pair,
    command: Pair,
    heredoc: []const u8,

    pub fn of(
        comptime kind: DelimiterKind,
        nest_: u8,
        end_: u8,
    ) Delimiters {
        return @unionInit(
            Delimiters,
            @tagName(kind),
            Pair{ .nest = nest_, .end = end_ },
        );
    }

    pub fn nest(delimiters: Delimiters) ?u8 {
        return switch (delimiters) {
            .heredoc => null,
            inline else => |pair| pair.nest,
        };
    }

    pub fn end(delimiters: Delimiters) ?u8 {
        return switch (delimiters) {
            .heredoc => null,
            inline else => |pair| pair.end,
        };
    }
};

pub const DelimiterState = struct {
    delimiters: Delimiters = Delimiters.of(.string, 0, 0),
    open_count: i32 = 0,
    heredoc_indent: i32 = 0,
    allow_escapes: bool = true,

    // pub fn default() DelimiterState {
    //     return .{
    //         .delimiters = Delimiters.of(.string, 0, 0),
    //         .open_count = 0,
    //         .heredoc_indent = 0,
    //         .allow_escapes = true,
    //     };
    // }

    // pub fn withOpenCountDelta(state: DelimiterState, delta: i32) DelimiterState {
    //     return .{
    //         .delimiters = state.delimiters,
    //         .open_count = state.open_count + delta,
    //         .heredoc_indent = state.heredoc_indent,
    //         .allow_escapes = state.allow_escapes,
    //     };
    // }

    // pub fn withHeredocIndent(state: DelimiterState, indent: i32) DelimiterState {
    //     return .{
    //         .delimiters = state.delimiters,
    //         .open_count = state.open_count,
    //         .heredoc_indent = indent,
    //         .allow_escapes = state.allow_escapes,
    //     };
    // }
};

pub fn doc(token: *const Token) ?[]const u8 {
    return if (token.doc_buffer) |doc_buffer|
        doc_buffer.items
    else
        null;
}

pub fn location(token: *Token) Location {
    if (token._location) |loc| {
        return loc;
    }
    const loc = Location.new(token.filename, token.line_number, token.column_number);
    token._location = loc;
    return loc;
}

pub fn isAnyKeyword(token: *const Token) bool {
    return token.type == .ident and token.value.isAnyKeyword();
}

pub fn isKeyword(token: *const Token, keyword: Keyword) bool {
    return token.type == .ident and token.value.isKeyword(keyword);
}

pub fn nameToString(token: *const Token) []const u8 {
    return switch (token.value) {
        .keyword => |keyword| keyword.toString(),
        .string => |string| string,
        else => unreachable,
    };
}

pub fn toString(token: *const Token, writer: anytype) !void {
    return switch (token.value) {
        .char => |char| writer.writeByte(char),
        // .utf8 => |encoded| writer.writeAll(encoded),
        .string => |string| writer.writeAll(string),
        .keyword => |keyword| writer.writeAll(keyword.toString()),
        .none => writer.writeAll(token.type.toString()),
    };
}

pub fn format(token: *const Token, comptime fmt: []const u8, opt: FormatOptions, writer: anytype) !void {
    _ = fmt;
    _ = opt;
    return token.toString(writer);
}

pub fn copyFrom(self: *Token, other: Token) void {
    self.type = other.type;
    self.value = other.value;
    self.number_kind = other.number_kind;
    self.line_number = other.line_number;
    self.column_number = other.column_number;
    self.filename = other.filename;
    self.delimiter_state = other.delimiter_state;
    self.macro_state = other.macro_state;
    self.doc_buffer = other.doc_buffer;
}

// zig fmt: off
pub fn main() void {
    const p = @import("std").debug.print;
    p("{}\n", .{Keyword.@"else"});
    p("{s}\n", .{Keyword.responds_to_question.toString()});
    p("{} {} {}\n", .{Kind.eof.isOperator(), Kind.op_bang.isOperator(), Kind.op_tilde.isOperator()});
    p("{}\n", .{Kind.magic_dir.isMagic()});
    p("{}\n", .{Kind.op_percent_eq.isAssignmentOperator()});
    p("{}\n", .{Kind.op_percent.isAssignmentOperator()});
    p("{}\n", .{MacroState{}});
    p("{}\n", .{DelimiterState{}});
    p("{}\n", .{Delimiters.of(.regex, '{', '}')});
    // p("{}\n", .{(DelimiterState{}).withOpenCountDelta(3)});
    // p("{}\n", .{(DelimiterState{}).withHeredocIndent(5)});
    p("{}\n", .{Token{}});
    var token = Token{};
    token.type = .ident;
    token.value = .{ .keyword = .abstract };
    p("{}\n", .{token.isAnyKeyword()});
    p("{}\n", .{token.isKeyword(.abstract)});
    var token2 = Token{};
    token2.copyFrom(token);
    p("{}\n", .{token2.isKeyword(.abstract)});
    p("{}\n", .{token.location()});
}
