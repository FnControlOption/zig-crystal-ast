const std = @import("std");
const NumberKind = @import("ast.zig").NumberKind;

type: Type = .EOF,
value: union(enum) {
    char: u8,
    string: []const u8,
    symbol: []const u8,
    nil: void,
} = .nil,
number_kind: NumberKind = .I32,
line_number: i32 = 0,
column_number: i32 = 0,
filename: ?[]const u8 = null,
delimiter_state: DelimiterState = .{},
macro_state: MacroState = .{},
passed_backslash_newline: bool = false,
doc_buffer: ?void = null, // ?IO::Memory
raw: []const u8 = "",
start: i32 = 0,
invalid_escape: bool = false,
location: ?void = null, // ?Location

pub const MacroState = struct {
    whitespace: bool = true,
    nest: i32 = 0,
    control_nest: i32 = 0,
    delimiter_state: ?DelimiterState = null,
    beginning_of_line: bool = true,
    yields: bool = false,
    comment: bool = false,
    heredocs: ?[]DelimiterState = null,
};

pub const DelimiterKind = enum {
    STRING,
    REGEX,
    STRING_ARRAY,
    SYMBOL_ARRAY,
    COMMAND,
    HEREDOC,
};

pub const DelimiterState = struct {
    kind: DelimiterKind = .STRING,
    nest: union(enum) { char: u8, string: []const u8 } = .{ .char = 0 },
    end: union(enum) { char: u8, string: []const u8 } = .{ .char = 0 },
    open_count: i32 = 0,
    heredoc_indent: i32 = 0,
    allow_escapes: bool = true,

    pub fn withOpenCountDelta(self: @This(), delta: i32) @This() {
        return .{
            .kind = self.kind,
            .nest = self.nest,
            .end = self.end,
            .open_count = self.open_count + delta,
            .heredoc_indent = self.heredoc_indent,
            .allow_escapes = self.allow_escapes,
        };
    }

    pub fn withHeredocIndent(self: @This(), indent: i32) @This() {
        return .{
            .kind = self.kind,
            .nest = self.nest,
            .end = self.end,
            .open_count = self.open_count,
            .heredoc_indent = indent,
            .allow_escapes = self.allow_escapes,
        };
    }
};

pub const Type = enum {
    // cd crystal-lang/crystal
    // rg --pcre2 -o '(?<=type = :|next_char :).*' src/compiler/crystal/syntax/lexer.cr | sort | uniq

    ExclamationMark,              // "!"
    ExclamationMarkEqual,         // "!="
    ExclamationMarkTilde,         // "!~"
    DollarQuestionMark,           // "$?"
    DollarTilde,                  // "$~"
    Percent,                      // "%"
    PercentEqual,                 // "%="
    PercentRBrace,                // "%}"
    Ampersand,                    // "&"
    Ampersand2,                   // "&&"
    Ampersand2Equal,              // "&&="
    AmpersandAsterisk,            // "&*"
    AmpersandAsterisk2,           // "&**"
    AmpersandAsteriskEqual,       // "&*="
    AmpersandPlus,                // "&+"
    AmpersandPlusEqual,           // "&+="
    AmpersandMinus,               // "&-"
    AmpersandMinusEqual,          // "&-="
    AmpersandEqual,               // "&="
    LParen,                       // "("
    RParen,                       // ")"
    Asterisk,                     // "*"
    Asterisk2,                    // "**"
    Asterisk2Equal,               // "**="
    AsteriskEqual,                // "*="
    Plus,                         // "+"
    PlusEqual,                    // "+="
    Comma,                        // ","
    Minus,                        // "-"
    MinusEqual,                   // "-="
    MinusArrow,                   // "->"
    Dot,                          // "."
    Dot2,                         // ".."
    Dot3,                         // "..."
    Slash,                        // "/"
    Slash2,                       // "//"
    Slash2Equal,                  // "//="
    SlashEqual,                   // "/="
    Colon,                        // ":"
    Colon2,                       // "::"
    Semicolon,                    // ";"
    LArrow,                       // "<"
    LArrow2,                      // "<<"
    LArrow2Equal,                 // "<<="
    LArrowEqual,                  // "<="
    LArrowEqualRArrow,            // "<=>"
    Equal,                        // "="
    Equal2,                       // "=="
    Equal3,                       // "==="
    EqualArrow,                   // "=>"
    EqualTilde,                   // "=~"
    RArrow,                       // ">"
    RArrowEqual,                  // ">="
    RArrow2,                      // ">>"
    RArrow2Equal,                 // ">>="
    QuestionMark,                 // "?"
    AtLBracket,                   // "@["
    LBracket,                     // "["
    LBracketRBracket,             // "[]"
    LBracketRBracketEqual,        // "[]="
    LBracketRBracketQuestionMark, // "[]?"
    RBracket,                     // "]"
    Caret,                        // "^"
    CaretEqual,                   // "^="
    Backtick,                     // "`"
    LBrace,                       // "{"
    LBracePercent,                // "{%"
    LBrace2,                      // "{{"
    Pipe,                         // "|"
    PipeEqual,                    // "|="
    Pipe2,                        // "||"
    Pipe2Equal,                   // "||="
    RBrace,                       // "}"
    Tilde,                        // "~"

    CHAR,
    COMMENT,
    CONST,
    DELIMITER_END,
    DELIMITER_START,
    EOF,
    GLOBAL,
    GLOBAL_MATCH_DATA_INDEX,
    IDENT,
    INTERPOLATION_START,
    MACRO_CONTROL_START,
    MACRO_END,
    MACRO_EXPRESSION_START,
    MACRO_LITERAL,
    MACRO_VAR,
    NEWLINE,
    NUMBER,
    SPACE,
    STRING,
    STRING_ARRAY_END,
    STRING_ARRAY_START,
    SYMBOL,
    SYMBOL_ARRAY_START,
    UNDERSCORE,
    __DIR__,
    __END_LINE__,
    __FILE__,
    __LINE__,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s}", .{self.toString()});
    }

    pub fn toString(self: @This()) []const u8 {
        return switch (self) {
            .ExclamationMark,              => "!",
            .ExclamationMarkEqual,         => "!=",
            .ExclamationMarkTilde,         => "!~",
            .DollarQuestionMark,           => "$?",
            .DollarTilde,                  => "$~",
            .Percent,                      => "%",
            .PercentEqual,                 => "%=",
            .PercentRBrace,                => "%}",
            .Ampersand,                    => "&",
            .Ampersand2,                   => "&&",
            .Ampersand2Equal,              => "&&=",
            .AmpersandAsterisk,            => "&*",
            .AmpersandAsterisk2,           => "&**",
            .AmpersandAsteriskEqual,       => "&*=",
            .AmpersandPlus,                => "&+",
            .AmpersandPlusEqual,           => "&+=",
            .AmpersandMinus,               => "&-",
            .AmpersandMinusEqual,          => "&-=",
            .AmpersandEqual,               => "&=",
            .LParen,                       => "(",
            .RParen,                       => ")",
            .Asterisk,                     => "*",
            .Asterisk2,                    => "**",
            .Asterisk2Equal,               => "**=",
            .AsteriskEqual,                => "*=",
            .Plus,                         => "+",
            .PlusEqual,                    => "+=",
            .Comma,                        => ",",
            .Minus,                        => "-",
            .MinusEqual,                   => "-=",
            .MinusArrow,                   => "->",
            .Dot,                          => ".",
            .Dot2,                         => "..",
            .Dot3,                         => "...",
            .Slash,                        => "/",
            .Slash2,                       => "//",
            .Slash2Equal,                  => "//=",
            .SlashEqual,                   => "/=",
            .Colon,                        => ":",
            .Colon2,                       => "::",
            .Semicolon,                    => ";",
            .LArrow,                       => "<",
            .LArrow2,                      => "<<",
            .LArrow2Equal,                 => "<<=",
            .LArrowEqual,                  => "<=",
            .LArrowEqualRArrow,            => "<=>",
            .Equal,                        => "=",
            .Equal2,                       => "==",
            .Equal3,                       => "===",
            .EqualArrow,                   => "=>",
            .EqualTilde,                   => "=~",
            .RArrow,                       => ">",
            .RArrowEqual,                  => ">=",
            .RArrow2,                      => ">>",
            .RArrow2Equal,                 => ">>=",
            .QuestionMark,                 => "?",
            .AtLBracket,                   => "@[",
            .LBracket,                     => "[",
            .LBracketRBracket,             => "[]",
            .LBracketRBracketEqual,        => "[]=",
            .LBracketRBracketQuestionMark, => "[]?",
            .RBracket,                     => "]",
            .Caret,                        => "^",
            .CaretEqual,                   => "^=",
            .Backtick,                     => "`",
            .LBrace,                       => "{",
            .LBracePercent,                => "{%",
            .LBrace2,                      => "{{",
            .Pipe,                         => "|",
            .PipeEqual,                    => "|=",
            .Pipe2,                        => "||",
            .Pipe2Equal,                   => "||=",
            .RBrace,                       => "}",
            .Tilde,                        => "~",

            .CHAR                          => "CHAR",
            .COMMENT                       => "COMMENT",
            .CONST                         => "CONST",
            .DELIMITER_END                 => "DELIMITER_END",
            .DELIMITER_START               => "DELIMITER_START",
            .EOF                           => "EOF",
            .GLOBAL                        => "GLOBAL",
            .GLOBAL_MATCH_DATA_INDEX       => "GLOBAL_MATCH_DATA_INDEX",
            .IDENT                         => "IDENT",
            .INTERPOLATION_START           => "INTERPOLATION_START",
            .MACRO_CONTROL_START           => "MACRO_CONTROL_START",
            .MACRO_END                     => "MACRO_END",
            .MACRO_EXPRESSION_START        => "MACRO_EXPRESSION_START",
            .MACRO_LITERAL                 => "MACRO_LITERAL",
            .MACRO_VAR                     => "MACRO_VAR",
            .NEWLINE                       => "NEWLINE",
            .NUMBER                        => "NUMBER",
            .SPACE                         => "SPACE",
            .STRING                        => "STRING",
            .STRING_ARRAY_END              => "STRING_ARRAY_END",
            .STRING_ARRAY_START            => "STRING_ARRAY_START",
            .SYMBOL                        => "SYMBOL",
            .SYMBOL_ARRAY_START            => "SYMBOL_ARRAY_START",
            .UNDERSCORE                    => "UNDERSCORE",
            .__DIR__                       => "__DIR__",
            .__END_LINE__                  => "__END_LINE__",
            .__FILE__                      => "__FILE__",
            .__LINE__                      => "__LINE__",
        };
    }
};

const print = std.debug.print;
const c_allocator = std.heap.c_allocator;

const utils = @import("utils.zig");
const inspect = utils.inspect;
const pp = utils.pp;
const p = utils.p;
const xprint = utils.xprint;

pub fn main() void {
    p(Type.__LINE__);
    p(Type.__LINE__.toString());
    p(Type.__LINE__.toString() ++ "bar");
    p(@This() {});
}
