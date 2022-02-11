const std = @import("std");

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

pub fn main() void {
    @import("std").debug.print("{}\n", .{Type.__LINE__});
    @import("std").debug.print("{s}\n", .{Type.__LINE__.toString() ++ "bar"});
}
