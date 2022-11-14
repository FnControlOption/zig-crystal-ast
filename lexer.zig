const Lexer = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Location = @import("location.zig");
const Token = @import("token.zig");
const Keyword = Token.Keyword;

const ast = @import("ast.zig");
const StringInterpolation = ast.StringInterpolation;
const StringLiteral = ast.StringLiteral;

allocator: Allocator,

doc_enabled: bool = false,
comments_enabled: bool = false,
count_whitespace: bool = false,
wants_raw: bool = false,
slash_is_regex: bool = true,
wants_def_or_macro_name: bool = false,
string: []const u8,
current_pos: usize = 0,
token: Token = .{},
// temp_token: Token = .{},
line_number: usize = 1,
column_number: usize = 1,
wants_symbol: bool = true,
filename: ?[]const u8 = "",
wants_regex: bool = true,
comment_is_doc: bool = true,

// delimiter_state_stack = [],
// macro_curly_count: i32 = 0,

stacked: bool = false,
stacked_filename: ?[]const u8 = "",
stacked_line_number: usize = 1,
stacked_column_number: usize = 1,

_token_end_location: ?Location = null,
// string_pool: StringPool = .{},

heredocs: ArrayList(Heredoc),
consuming_heredocs: bool = false,

// macro_expansion_pragmas = null,

// warnings: WarningCollection = .{},

error_message: ?[]const u8 = null,

pub const Heredoc = struct {
    delimiter_state: Token.DelimiterState,
    node: *StringInterpolation,
};

fn new(string: []const u8) Lexer {
    const allocator = std.heap.page_allocator; // TODO
    return init(allocator, string);
}

pub fn init(allocator: Allocator, string: []const u8) Lexer {
    return .{
        .allocator = allocator,
        .string = string,
        .heredocs = ArrayList(Heredoc).init(allocator),
    };
}

pub fn nextToken(lexer: *Lexer) !Token {
    const token = try lexer._nextToken();

    if (token.type == .newline and
        !lexer.consuming_heredocs and
        lexer.heredocs.items.len > 0)
    {
        try lexer.consumeHeredocs();
    }

    return token;
}

pub fn consumeHeredocs(lexer: *Lexer) !void {
    lexer.consuming_heredocs = true;
    std.mem.reverse(Heredoc, lexer.heredocs.items);
    while (lexer.heredocs.items.len > 0) {
        const heredoc = lexer.heredocs.pop();
        try lexer.consumeHeredoc(heredoc);
    }
    lexer.consuming_heredocs = false;
}

const StringPiece = struct {
    value: union(enum) {
        string: []const u8,
        node: ast.Node,
    },
    line_number: usize,
};

pub fn consumeHeredoc(lexer: *Lexer, heredoc: Heredoc) !void {
    _ = lexer;
    _ = heredoc;
    return error.Unimplemented;
    // const node = heredoc.node;
    // var delimiter_state = heredoc.delimiter_state;
    // // nextStringToken(delimiter_state);
    // // delimiter_state = lexer.token.delimiter_state;

    // var pieces = ArrayList(StringPiece).init(lexer.allocator);
    // var has_interpolation = false;

    // // consumeDelimiter(pieces, delimiter_state, has_interpolation);

    // if (has_interpolation) {
    //     // combineInterpolationPieces(pieces, delimiter_state, &node.expressions);
    // } else {
    //     // const string = combinePieces(pieces, delimiter_state);
    //     // const exp = StringLiteral.new(lexer.allocator, string);
    //     // exp.setLocation(node.location);
    //     // exp.setEndLocation(token_end_location);
    //     // node.expressions.append(exp);
    // }
    // _ = pieces;
    // _ = node;
    // _ = delimiter_state;
}

pub fn _nextToken(lexer: *Lexer) !Token {
    // Check previous token:
    if (lexer.token.type == .newline or lexer.token.type == .eof) {
        // 1) After a newline or at the start of the stream (:EOF), a following
        //    comment can be a doc comment.
        lexer.comment_is_doc = true;
    } else if (lexer.token.type != .space) {
        // 2) Any non-space token prevents a following comment from being a doc
        //    comment.
        lexer.comment_is_doc = false;
    }

    lexer.resetToken();

    // Skip comments
    while (lexer.currentChar() == '#') {
        var start = lexer.current_pos;

        // Check #<loc:...> pragma comment
        if (lexer.nextCharNoColumnIncrement() == '<' and
            lexer.nextCharNoColumnIncrement() == 'l' and
            lexer.nextCharNoColumnIncrement() == 'o' and
            lexer.nextCharNoColumnIncrement() == 'c' and
            lexer.nextCharNoColumnIncrement() == ':')
        {
            lexer.skipCharNoColumnIncrement();
            try lexer.consumeLocPragma();
            start = lexer.current_pos;
        } else {
            if (lexer.doc_enabled and lexer.comment_is_doc) {
                try lexer.consumeDoc();
            } else if (lexer.comments_enabled) {
                return lexer.consumeComment(start);
            } else {
                lexer.skipComment();
            }
        }
    }

    var start = lexer.current_pos;

    // pragmas

    var reset_regex_flags = true;

    switch (lexer.currentChar()) {
        0 => {
            lexer.token.type = .eof;
        },
        ' ', '\t' => {
            try lexer.consumeWhitespace();
            reset_regex_flags = false;
        },
        '\\' => {
            switch (lexer.nextChar()) {
                '\r', '\n' => {
                    _ = try lexer.handleCrlfOrLf();
                    lexer.incrLineNumber();
                    lexer.token.passed_backslash_newline = true;
                    try lexer.consumeWhitespace();
                    reset_regex_flags = false;
                },
                else => {
                    try lexer.unknownToken();
                },
            }
        },
        '\n' => {
            lexer.token.type = .newline;
            lexer.skipChar();
            lexer.incrLineNumber();
            reset_regex_flags = false;
            try lexer.consumeNewlines();
        },
        '\r' => {
            if (lexer.nextChar() == '\n') {
                lexer.skipChar();
                lexer.token.type = .newline;
                lexer.incrLineNumber();
                reset_regex_flags = false;
                try lexer.consumeNewlines();
            } else {
                try lexer.raise("expected '\\n' after '\\r'");
            }
        },
        '=' => {
            switch (lexer.nextChar()) {
                '=' => {
                    switch (lexer.nextChar()) {
                        '=' => {
                            lexer.skipTokenChar(.op_eq_eq_eq);
                        },
                        else => {
                            lexer.token.type = .op_eq_eq;
                        },
                    }
                },
                '>' => {
                    lexer.skipTokenChar(.op_eq_gt);
                },
                '~' => {
                    lexer.skipTokenChar(.op_eq_tilde);
                },
                else => {
                    lexer.token.type = .op_eq;
                },
            }
        },
        '!' => {
            switch (lexer.nextChar()) {
                '=' => {
                    lexer.skipTokenChar(.op_bang_eq);
                },
                '~' => {
                    lexer.skipTokenChar(.op_bang_tilde);
                },
                else => {
                    lexer.token.type = .op_bang;
                },
            }
        },
        '<' => {
            switch (lexer.nextChar()) {
                '=' => {
                    switch (lexer.nextChar()) {
                        '>' => {
                            lexer.skipTokenChar(.op_lt_eq_gt);
                        },
                        else => {
                            lexer.token.type = .op_lt_eq;
                        },
                    }
                },
                '<' => {
                    switch (lexer.nextChar()) {
                        '=' => {
                            lexer.skipTokenChar(.op_lt_lt_eq);
                        },
                        '-' => {
                            // TODO: refactor Crystal implementation
                            try lexer.consumeHeredocStart(start);
                        },
                        else => {
                            lexer.token.type = .op_lt_lt;
                        },
                    }
                },
                else => {
                    lexer.token.type = .op_lt;
                },
            }
        },
        '>' => {
            switch (lexer.nextChar()) {
                '=' => {
                    lexer.skipTokenChar(.op_gt_eq);
                },
                '>' => {
                    switch (lexer.nextChar()) {
                        '=' => {
                            lexer.skipTokenChar(.op_gt_gt_eq);
                        },
                        else => {
                            lexer.token.type = .op_gt_gt;
                        },
                    }
                },
                else => {
                    lexer.token.type = .op_gt;
                },
            }
        },
        '+' => {
            lexer.token.start = start; // TODO: what is this for?
            switch (lexer.nextChar()) {
                '=' => {
                    lexer.skipTokenChar(.op_plus_eq);
                },
                '0'...'9' => {
                    try lexer.scanNumber(start, false);
                },
                '+' => {
                    try lexer.raise("postfix increment is not supported, use `exp += 1`");
                },
                else => {
                    lexer.token.type = .op_plus;
                },
            }
        },
        '-' => {
            lexer.token.start = start; // TODO: what is this for?
            switch (lexer.nextChar()) {
                '=' => {
                    lexer.skipTokenChar(.op_minus_eq);
                },
                '>' => {
                    lexer.skipTokenChar(.op_minus_gt);
                },
                '0'...'9' => {
                    try lexer.scanNumber(start, true);
                },
                '-' => {
                    try lexer.raise("postfix decrement is not supported, use `exp -= 1`");
                },
                else => {
                    lexer.token.type = .op_minus;
                },
            }
        },
        '*' => {
            switch (lexer.nextChar()) {
                '=' => {
                    lexer.skipTokenChar(.op_star_eq);
                },
                '*' => {
                    switch (lexer.nextChar()) {
                        '=' => {
                            lexer.skipTokenChar(.op_star_star_eq);
                        },
                        else => {
                            lexer.token.type = .op_star_star;
                        },
                    }
                },
                else => {
                    lexer.token.type = .op_star;
                },
            }
        },
        '/' => {
            const line = lexer.line_number;
            const column = lexer.column_number;
            _ = line;
            _ = column; // TODO: clean up Crystal impl
            const char = lexer.nextChar();
            if ((lexer.wants_def_or_macro_name or !lexer.slash_is_regex) and char == '/') {
                switch (lexer.nextChar()) {
                    '=' => {
                        lexer.skipTokenChar(.op_slash_slash_eq);
                    },
                    else => {
                        lexer.token.type = .op_slash_slash;
                    },
                }
            } else if (!lexer.slash_is_regex and char == '=') {
                lexer.skipTokenChar(.op_slash_eq);
            } else if (lexer.wants_def_or_macro_name) {
                lexer.token.type = .op_slash;
            } else if (lexer.slash_is_regex) {
                lexer.delimiterStartNoAdvance(.{
                    .delimiters = Token.Delimiters.of(.regex, '/', '/'),
                }, start);
            } else if (std.ascii.isWhitespace(char) or char == 0) {
                lexer.token.type = .op_slash;
            } else if (lexer.wants_regex) {
                lexer.delimiterStartNoAdvance(.{
                    .delimiters = Token.Delimiters.of(.regex, '/', '/'),
                }, start);
            } else {
                lexer.token.type = .op_slash;
            }
        },
        '%' => {
            if (lexer.wants_def_or_macro_name) {
                lexer.skipTokenChar(.op_percent);
            } else {
                switch (lexer.nextChar()) {
                    '=' => {
                        lexer.skipTokenChar(.op_percent_eq);
                    },
                    '(', '<', '[', '{', '|' => |char| {
                        lexer.delimiterStart(.{
                            .delimiters = Token.Delimiters.of(.string, char, closingChar(char)),
                        }, start);
                    },
                    'i' => {
                        switch (lexer.peekNextChar()) {
                            '(', '<', '[', '{', '|' => |char| {
                                lexer.skipChar();
                                lexer.skipTokenChar(.symbol_array_start);
                                lexer.setTokenRawFromStart(start);
                                lexer.token.delimiter_state = .{
                                    .delimiters = Token.Delimiters.of(.symbol_array, char, closingChar(char)),
                                };
                            },
                            else => {
                                lexer.token.type = .op_percent;
                            },
                        }
                    },
                    'q' => {
                        switch (lexer.peekNextChar()) {
                            '(', '<', '[', '{', '|' => |char| {
                                lexer.skipChar();
                                lexer.delimiterStart(.{
                                    .delimiters = Token.Delimiters.of(.string, char, closingChar(char)),
                                    .allow_escapes = false,
                                }, start);
                            },
                            else => {
                                lexer.token.type = .op_percent;
                            },
                        }
                    },
                    'Q' => {
                        switch (lexer.peekNextChar()) {
                            '(', '<', '[', '{', '|' => |char| {
                                lexer.skipChar();
                                lexer.delimiterStart(.{
                                    .delimiters = Token.Delimiters.of(.string, char, closingChar(char)),
                                }, start);
                            },
                            else => {
                                lexer.token.type = .op_percent;
                            },
                        }
                    },
                    'r' => {
                        switch (lexer.nextChar()) {
                            '(', '<', '[', '{', '|' => |char| {
                                lexer.delimiterStart(.{
                                    .delimiters = Token.Delimiters.of(.regex, char, closingChar(char)),
                                }, start);
                            },
                            else => {
                                try lexer.raise("unknown %r char");
                            },
                        }
                    },
                    'x' => {
                        switch (lexer.nextChar()) {
                            '(', '<', '[', '{', '|' => |char| {
                                lexer.delimiterStart(.{
                                    .delimiters = Token.Delimiters.of(.command, char, closingChar(char)),
                                }, start);
                            },
                            else => {
                                try lexer.raise("unknown %x char");
                            },
                        }
                    },
                    'w' => {
                        switch (lexer.peekNextChar()) {
                            '(', '<', '[', '{', '|' => |char| {
                                lexer.skipChar();
                                lexer.skipTokenChar(.string_array_start);
                                lexer.setTokenRawFromStart(start);
                                lexer.token.delimiter_state = .{
                                    .delimiters = Token.Delimiters.of(.string_array, char, closingChar(char)),
                                };
                            },
                            else => {
                                lexer.token.type = .op_percent;
                            },
                        }
                    },
                    '}' => {
                        lexer.skipTokenChar(.op_percent_rcurly);
                    },
                    else => {
                        lexer.token.type = .op_percent;
                    },
                }
            }
        },
        '(' => lexer.skipTokenChar(.op_lparen),
        ')' => lexer.skipTokenChar(.op_rparen),
        '{' => {
            switch (lexer.nextChar()) {
                '%' => {
                    lexer.skipTokenChar(.op_lcurly_percent);
                },
                '{' => {
                    lexer.skipTokenChar(.op_lcurly_lcurly);
                },
                else => {
                    lexer.token.type = .op_lcurly;
                },
            }
        },
        '}' => lexer.skipTokenChar(.op_rcurly),
        '[' => {
            switch (lexer.nextChar()) {
                ']' => {
                    switch (lexer.nextChar()) {
                        '=' => {
                            lexer.skipTokenChar(.op_lsquare_rsquare_eq);
                        },
                        '?' => {
                            lexer.skipTokenChar(.op_lsquare_rsquare_question);
                        },
                        else => {
                            lexer.token.type = .op_lsquare_rsquare;
                        },
                    }
                },
                else => {
                    lexer.token.type = .op_lsquare;
                },
            }
        },
        ']' => lexer.skipTokenChar(.op_rsquare),
        ',' => lexer.skipTokenChar(.op_comma),
        '?' => lexer.skipTokenChar(.op_question),
        ';' => {
            reset_regex_flags = false;
            lexer.skipTokenChar(.op_semicolon);
        },
        ':' => {
            switch (lexer.nextChar()) {
                ':' => {
                    lexer.skipTokenChar(.op_colon_colon);
                },
                else => |char| {
                    if (lexer.wants_symbol) {
                        // TODO: backport this additional parameter to Crystal?
                        try lexer.consumeSymbol(char, start);
                    } else {
                        lexer.token.type = .op_colon;
                    }
                },
            }
        },
        '~' => {
            lexer.skipTokenChar(.op_tilde);
        },
        '.' => {
            const line = lexer.line_number;
            const column = lexer.column_number;
            switch (lexer.nextChar()) {
                '.' => {
                    switch (lexer.nextChar()) {
                        '.' => {
                            lexer.skipTokenChar(.op_period_period_period);
                        },
                        else => {
                            lexer.token.type = .op_period_period;
                        },
                    }
                },
                else => |char| {
                    if (std.ascii.isDigit(char)) {
                        try lexer.raiseAt(".1 style number literal is not supported, put 0 before dot", line, column);
                    }
                    lexer.token.type = .op_period;
                },
            }
        },
        '&' => {
            switch (lexer.nextChar()) {
                '&' => {
                    switch (lexer.nextChar()) {
                        '=' => {
                            lexer.skipTokenChar(.op_amp_amp_eq);
                        },
                        else => {
                            lexer.token.type = .op_amp_amp;
                        },
                    }
                },
                '=' => {
                    lexer.skipTokenChar(.op_amp_eq);
                },
                '+' => {
                    switch (lexer.nextChar()) {
                        '=' => {
                            lexer.skipTokenChar(.op_amp_plus_eq);
                        },
                        else => {
                            lexer.token.type = .op_amp_plus;
                        },
                    }
                },
                '-' => {
                    // Check if '>' comes after '&-', making it '&->'.
                    // We want to parse that like '&(->...)',
                    // so we only return '&' for now.
                    if (lexer.peekNextChar() == '>') {
                        lexer.token.type = .op_amp;
                    } else {
                        switch (lexer.nextChar()) {
                            '=' => {
                                lexer.skipTokenChar(.op_amp_minus_eq);
                            },
                            else => {
                                lexer.token.type = .op_amp_minus;
                            },
                        }
                    }
                },
                '*' => {
                    switch (lexer.nextChar()) {
                        '*' => {
                            lexer.skipTokenChar(.op_amp_star_star);
                        },
                        '=' => {
                            lexer.skipTokenChar(.op_amp_star_eq);
                        },
                        else => {
                            lexer.token.type = .op_amp_star;
                        },
                    }
                },
                else => {
                    lexer.token.type = .op_amp;
                },
            }
        },
        '|' => {
            switch (lexer.nextChar()) {
                '|' => {
                    switch (lexer.nextChar()) {
                        '=' => {
                            lexer.skipTokenChar(.op_bar_bar_eq);
                        },
                        else => {
                            lexer.token.type = .op_bar_bar;
                        },
                    }
                },
                '=' => {
                    lexer.skipTokenChar(.op_bar_eq);
                },
                else => {
                    lexer.token.type = .op_bar;
                },
            }
        },
        '^' => {
            switch (lexer.nextChar()) {
                '=' => {
                    lexer.skipTokenChar(.op_caret_eq);
                },
                else => {
                    lexer.token.type = .op_caret;
                },
            }
        },
        '\'' => {
            try lexer.consumeCharLiteral();
        },
        '`' => {
            if (lexer.wants_def_or_macro_name) {
                lexer.skipChar();
                lexer.token.type = .op_grave;
            } else {
                lexer.delimiterStart(.{
                    .delimiters = Token.Delimiters.of(.command, '`', '`'),
                }, start);
            }
        },
        '"' => {
            lexer.delimiterStart(.{
                .delimiters = Token.Delimiters.of(.string, '"', '"'),
            }, start);
        },
        '0'...'9' => {
            try lexer.scanNumber(start, false);
        },
        '@' => {
            start = lexer.current_pos; // TODO: redundant?
            switch (lexer.nextChar()) {
                '[' => {
                    lexer.skipTokenChar(.op_at_lsquare);
                },
                else => |char| {
                    // TODO: backport improvement to Crystal
                    if (char == '@') {
                        lexer.skipChar();
                        try lexer.consumeVariable(.class_var, start);
                    } else {
                        try lexer.consumeVariable(.instance_var, start);
                    }
                },
            }
        },
        '$' => {
            start = lexer.current_pos; // TODO: redundant?
            switch (lexer.nextChar()) {
                '~' => {
                    lexer.skipTokenChar(.op_dollar_tilde);
                },
                '?' => {
                    lexer.skipTokenChar(.op_dollar_question);
                },
                else => |char| {
                    if (std.ascii.isDigit(char)) {
                        lexer.consumeGlobalMatchDataIndex();
                    } else {
                        // TODO: backport improvement to Crystal
                        try lexer.consumeVariable(.global, start);
                    }
                },
            }
        },
        'a' => {
            switch (lexer.nextChar()) {
                'b' => {
                    if (lexer.charSequence("stract")) {
                        return lexer.checkIdentOrKeyword(.abstract, start);
                    }
                },
                'l' => {
                    if (lexer.charSequence("ias")) {
                        return lexer.checkIdentOrKeyword(.alias, start);
                    }
                },
                's' => {
                    const peek = lexer.peekNextChar();
                    switch (peek) {
                        'm' => {
                            lexer.skipChar();
                            return lexer.checkIdentOrKeyword(.@"asm", start);
                        },
                        '?' => {
                            lexer.skipChar();
                            lexer.skipChar();
                            lexer.token.type = .ident;
                            lexer.token.value = .{ .keyword = .as_question };
                            return lexer.token;
                        },
                        else => {
                            return lexer.checkIdentOrKeyword(.as, start);
                        },
                    }
                },
                'n' => {
                    if (lexer.charSequence("notation")) {
                        return lexer.checkIdentOrKeyword(.annotation, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'b' => {
            switch (lexer.nextChar()) {
                'e' => {
                    if (lexer.charSequence("gin")) {
                        return lexer.checkIdentOrKeyword(.begin, start);
                    }
                },
                'r' => {
                    if (lexer.charSequence("eak")) {
                        return lexer.checkIdentOrKeyword(.@"break", start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'c' => {
            switch (lexer.nextChar()) {
                'a' => {
                    if (lexer.charSequence("se")) {
                        return lexer.checkIdentOrKeyword(.case, start);
                    }
                },
                'l' => {
                    if (lexer.charSequence("ass")) {
                        return lexer.checkIdentOrKeyword(.class, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'd' => {
            switch (lexer.nextChar()) {
                'e' => {
                    if (lexer.nextChar() == 'f') {
                        return lexer.checkIdentOrKeyword(.def, start);
                    }
                },
                'o' => return lexer.checkIdentOrKeyword(.do, start),
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'e' => {
            switch (lexer.nextChar()) {
                'l' => {
                    switch (lexer.nextChar()) {
                        's' => {
                            switch (lexer.nextChar()) {
                                'e' => return lexer.checkIdentOrKeyword(.@"else", start),
                                'i' => {
                                    if (lexer.nextChar() == 'f') {
                                        return lexer.checkIdentOrKeyword(.elsif, start);
                                    }
                                },
                                else => {
                                    // scanIdent
                                },
                            }
                        },
                        else => {
                            // scanIdent
                        },
                    }
                },
                'n' => {
                    switch (lexer.nextChar()) {
                        'd' => {
                            return lexer.checkIdentOrKeyword(.end, start);
                        },
                        's' => {
                            if (lexer.charSequence("ure")) {
                                return lexer.checkIdentOrKeyword(.ensure, start);
                            }
                        },
                        'u' => {
                            if (lexer.nextChar() == 'm') {
                                return lexer.checkIdentOrKeyword(.@"enum", start);
                            }
                        },
                        else => {
                            // scanIdent
                        },
                    }
                },
                'x' => {
                    if (lexer.charSequence("tend")) {
                        return lexer.checkIdentOrKeyword(.extend, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'f' => {
            switch (lexer.nextChar()) {
                'a' => {
                    if (lexer.charSequence("lse")) {
                        return lexer.checkIdentOrKeyword(.false, start);
                    }
                },
                'o' => {
                    if (lexer.nextChar() == 'r') {
                        return lexer.checkIdentOrKeyword(.@"for", start);
                    }
                },
                'u' => {
                    if (lexer.nextChar() == 'n') {
                        return lexer.checkIdentOrKeyword(.fun, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'i' => {
            switch (lexer.nextChar()) {
                'f' => {
                    return lexer.checkIdentOrKeyword(.@"if", start);
                },
                'n' => {
                    if (isIdentPartOrEnd(lexer.peekNextChar())) {
                        switch (lexer.nextChar()) {
                            'c' => {
                                if (lexer.charSequence("lude")) {
                                    return lexer.checkIdentOrKeyword(.include, start);
                                }
                            },
                            's' => {
                                if (lexer.charSequence("tance_sizeof")) {
                                    return lexer.checkIdentOrKeyword(.instance_sizeof, start);
                                }
                            },
                            else => {
                                // scanIdent
                            },
                        }
                    } else {
                        lexer.skipChar();
                        lexer.token.type = .ident;
                        lexer.token.value = .{ .keyword = .in };
                        return lexer.token;
                    }
                },
                's' => {
                    if (lexer.charSequence("_a?")) {
                        return lexer.checkIdentOrKeyword(.is_a_question, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'l' => {
            switch (lexer.nextChar()) {
                'i' => {
                    if (lexer.nextChar() == 'b') {
                        return lexer.checkIdentOrKeyword(.lib, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'm' => {
            switch (lexer.nextChar()) {
                'a' => {
                    if (lexer.charSequence("cro")) {
                        return lexer.checkIdentOrKeyword(.macro, start);
                    }
                },
                'o' => {
                    switch (lexer.nextChar()) {
                        'd' => {
                            if (lexer.charSequence("ule")) {
                                return lexer.checkIdentOrKeyword(.module, start);
                            }
                        },
                        else => {
                            // scanIdent
                        },
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'n' => {
            switch (lexer.nextChar()) {
                'e' => {
                    if (lexer.charSequence("xt")) {
                        return lexer.checkIdentOrKeyword(.next, start);
                    }
                },
                'i' => {
                    switch (lexer.nextChar()) {
                        'l' => {
                            if (lexer.peekNextChar() == '?') {
                                lexer.skipChar();
                                return lexer.checkIdentOrKeyword(.nil_question, start);
                            } else {
                                return lexer.checkIdentOrKeyword(.nil, start);
                            }
                        },
                        else => {
                            // scanIdent
                        },
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'o' => {
            switch (lexer.nextChar()) {
                'f' => {
                    if (lexer.peekNextChar() == 'f') {
                        lexer.skipChar();
                        if (lexer.charSequence("setof")) {
                            return lexer.checkIdentOrKeyword(.offsetof, start);
                        }
                    } else {
                        return lexer.checkIdentOrKeyword(.of, start);
                    }
                },
                'u' => {
                    if (lexer.nextChar() == 't') {
                        return lexer.checkIdentOrKeyword(.out, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'p' => {
            switch (lexer.nextChar()) {
                'o' => {
                    if (lexer.charSequence("interof")) {
                        return lexer.checkIdentOrKeyword(.pointerof, start);
                    }
                },
                'r' => {
                    switch (lexer.nextChar()) {
                        'i' => {
                            if (lexer.charSequence("vate")) {
                                return lexer.checkIdentOrKeyword(.private, start);
                            }
                        },
                        'o' => {
                            if (lexer.charSequence("tected")) {
                                return lexer.checkIdentOrKeyword(.protected, start);
                            }
                        },
                        else => {
                            // scanIdent
                        },
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'r' => {
            switch (lexer.nextChar()) {
                'e' => {
                    switch (lexer.nextChar()) {
                        's' => {
                            switch (lexer.nextChar()) {
                                'c' => {
                                    if (lexer.charSequence("ue")) {
                                        return lexer.checkIdentOrKeyword(.rescue, start);
                                    }
                                },
                                'p' => {
                                    if (lexer.charSequence("onds_to?")) {
                                        return lexer.checkIdentOrKeyword(.responds_to_question, start);
                                    }
                                },
                                else => {
                                    // scanIdent
                                },
                            }
                        },
                        't' => {
                            if (lexer.charSequence("urn")) {
                                return lexer.checkIdentOrKeyword(.@"return", start);
                            }
                        },
                        'q' => {
                            if (lexer.charSequence("uire")) {
                                return lexer.checkIdentOrKeyword(.require, start);
                            }
                        },
                        else => {
                            // scanIdent
                        },
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        's' => {
            switch (lexer.nextChar()) {
                'e' => {
                    if (lexer.nextChar() == 'l') {
                        switch (lexer.nextChar()) {
                            'e' => {
                                if (lexer.charSequence("ct")) {
                                    return lexer.checkIdentOrKeyword(.select, start);
                                }
                            },
                            'f' => {
                                return lexer.checkIdentOrKeyword(.self, start);
                            },
                            else => {
                                // scanIdent
                            },
                        }
                    }
                },
                'i' => {
                    if (lexer.charSequence("zeof")) {
                        return lexer.checkIdentOrKeyword(.sizeof, start);
                    }
                },
                't' => {
                    if (lexer.charSequence("ruct")) {
                        return lexer.checkIdentOrKeyword(.@"struct", start);
                    }
                },
                'u' => {
                    if (lexer.charSequence("per")) {
                        return lexer.checkIdentOrKeyword(.super, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        't' => {
            switch (lexer.nextChar()) {
                'h' => {
                    if (lexer.charSequence("en")) {
                        return lexer.checkIdentOrKeyword(.then, start);
                    }
                },
                'r' => {
                    if (lexer.charSequence("ue")) {
                        return lexer.checkIdentOrKeyword(.true, start);
                    }
                },
                'y' => {
                    if (lexer.charSequence("pe")) {
                        if (lexer.peekNextChar() == 'o') {
                            lexer.skipChar();
                            if (lexer.nextChar() == 'f') {
                                return lexer.checkIdentOrKeyword(.typeof, start);
                            }
                        } else {
                            return lexer.checkIdentOrKeyword(.type, start);
                        }
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'u' => {
            if (lexer.nextChar() == 'n') {
                switch (lexer.nextChar()) {
                    'i' => {
                        switch (lexer.nextChar()) {
                            'o' => {
                                if (lexer.nextChar() == 'n') {
                                    return lexer.checkIdentOrKeyword(.@"union", start);
                                }
                            },
                            'n' => {
                                if (lexer.charSequence("itialized")) {
                                    return lexer.checkIdentOrKeyword(.uninitialized, start);
                                }
                            },
                            else => {
                                // scanIdent
                            },
                        }
                    },
                    'l' => {
                        if (lexer.charSequence("ess")) {
                            return lexer.checkIdentOrKeyword(.unless, start);
                        }
                    },
                    't' => {
                        if (lexer.charSequence("il")) {
                            return lexer.checkIdentOrKeyword(.until, start);
                        }
                    },
                    else => {
                        // scanIdent
                    },
                }
            }
            _ = lexer.scanIdent(start);
        },
        'v' => {
            if (lexer.charSequence("erbatim")) {
                return lexer.checkIdentOrKeyword(.verbatim, start);
            }
            _ = lexer.scanIdent(start);
        },
        'w' => {
            switch (lexer.nextChar()) {
                'h' => {
                    switch (lexer.nextChar()) {
                        'e' => {
                            if (lexer.nextChar() == 'n') {
                                return lexer.checkIdentOrKeyword(.when, start);
                            }
                        },
                        'i' => {
                            if (lexer.charSequence("le")) {
                                return lexer.checkIdentOrKeyword(.@"while", start);
                            }
                        },
                        else => {
                            // scanIdent
                        },
                    }
                },
                'i' => {
                    if (lexer.charSequence("th")) {
                        return lexer.checkIdentOrKeyword(.with, start);
                    }
                },
                else => {
                    // scanIdent
                },
            }
            _ = lexer.scanIdent(start);
        },
        'y' => {
            if (lexer.charSequence("ield")) {
                return lexer.checkIdentOrKeyword(.yield, start);
            }
            _ = lexer.scanIdent(start);
        },
        '_' => {
            switch (lexer.nextChar()) {
                '_' => {
                    // TODO: Crystal implementation calls scan_ident twice
                    switch (lexer.nextChar()) {
                        'D' => {
                            if (lexer.charSequence("IR__")) {
                                if (!isIdentPartOrEnd(lexer.peekNextChar())) {
                                    lexer.skipChar();
                                    lexer.token.type = .magic_dir;
                                    return lexer.token;
                                }
                            }
                        },
                        'E' => {
                            if (lexer.charSequence("ND_LINE__")) {
                                if (!isIdentPartOrEnd(lexer.peekNextChar())) {
                                    lexer.skipChar();
                                    lexer.token.type = .magic_end_line;
                                    return lexer.token;
                                }
                            }
                        },
                        'F' => {
                            if (lexer.charSequence("ILE__")) {
                                if (!isIdentPartOrEnd(lexer.peekNextChar())) {
                                    lexer.skipChar();
                                    lexer.token.type = .magic_file;
                                    return lexer.token;
                                }
                            }
                        },
                        'L' => {
                            if (lexer.charSequence("INE__")) {
                                if (!isIdentPartOrEnd(lexer.peekNextChar())) {
                                    lexer.skipChar();
                                    lexer.token.type = .magic_line;
                                    return lexer.token;
                                }
                            }
                        },
                        else => {
                            // scanIdent
                        },
                    }
                },
                else => |char| {
                    if (!isIdentPart(char)) {
                        lexer.token.type = .underscore;
                        return lexer.token;
                    }
                },
            }
            _ = lexer.scanIdent(start);
        },
        else => |char| {
            if (std.ascii.isUpper(char)) {
                start = lexer.current_pos; // TODO: redundant?
                while (isIdentPart(lexer.nextChar())) {
                    // Nothing to do
                }
                lexer.token.type = .@"const";
                lexer.token.value = .{ .string = lexer.stringRange(start) };
            } else if (isIdentStart(char)) {
                lexer.skipChar();
                _ = lexer.scanIdent(start);
            } else {
                try lexer.unknownToken();
            }
        },
    }

    if (reset_regex_flags) {
        lexer.wants_regex = true;
        lexer.slash_is_regex = false;
    }

    return lexer.token;
}

pub fn tokenEndLocation(lexer: *Lexer) Location {
    if (lexer._token_end_location) |loc| {
        return loc;
    }
    const loc = Location.new(lexer.filename, lexer.line_number, lexer.column_number - 1);
    lexer._token_end_location = loc;
    return loc;
}

pub fn consumeComment(lexer: *Lexer, start_pos: usize) Token {
    lexer.skipComment();
    lexer.token.type = .comment;
    lexer.token.value = .{ .string = lexer.stringRange(start_pos) };
    return lexer.token;
}

pub fn consumeDoc(lexer: *Lexer) !void {
    var char = lexer.currentChar();
    var start_pos = lexer.current_pos;

    // Ignore first whitespace after comment, like in `# some doc`
    if (char == ' ') {
        char = lexer.nextChar();
        start_pos = lexer.current_pos;
    }

    lexer.skipComment();

    if (lexer.token.doc_buffer) |*doc_buffer| {
        try doc_buffer.append('\n');
        try doc_buffer.appendSlice(lexer.stringRange(start_pos));
    } else {
        var doc_buffer = ArrayList(u8).init(lexer.allocator);
        try doc_buffer.appendSlice(lexer.stringRange(start_pos));
        lexer.token.doc_buffer = doc_buffer;
    }
}

pub fn skipComment(lexer: *Lexer) void {
    var char = lexer.currentChar();
    while (char != '\n' and char != 0) {
        char = lexer.nextCharNoColumnIncrement();
    }
}

pub fn consumeWhitespace(lexer: *Lexer) !void {
    const start_pos = lexer.current_pos;
    lexer.token.type = .space;
    lexer.skipChar();
    while (true) {
        switch (lexer.currentChar()) {
            ' ', '\t' => {
                lexer.skipChar();
            },
            '\\' => {
                switch (lexer.nextChar()) {
                    '\r', '\n' => {
                        _ = try lexer.handleCrlfOrLf();
                        lexer.skipChar();
                        lexer.incrLineNumber();
                        lexer.token.passed_backslash_newline = true;
                    },
                    else => {
                        try lexer.unknownToken();
                    },
                }
            },
            else => {
                break;
            },
        }
    }
    if (lexer.count_whitespace) {
        lexer.token.value = .{ .string = lexer.stringRange(start_pos) };
    }
}

pub fn consumeNewlines(lexer: *Lexer) !void {
    // If there are heredocs we don't freely consume newlines because
    // these will be part of the heredoc string
    if (lexer.heredocs.items.len > 0) {
        return;
    }

    if (lexer.count_whitespace) {
        return;
    }

    while (true) {
        switch (lexer.currentChar()) {
            '\n' => {
                lexer.skipCharNoColumnIncrement();
                lexer.incrLineNumberWithColumn(null);
                lexer.token.doc_buffer = null;
            },
            '\r' => {
                if (lexer.nextCharNoColumnIncrement() != '\n') {
                    try lexer.raise("expected '\\n' after '\\r'");
                }
                lexer.skipCharNoColumnIncrement();
                lexer.incrLineNumberWithColumn(null);
                lexer.token.doc_buffer = null;
            },
            else => {
                break;
            },
        }
    }
}

pub fn checkIdentOrKeyword(lexer: *Lexer, keyword: Keyword, start: usize) Token {
    if (isIdentPartOrEnd(lexer.peekNextChar())) {
        _ = lexer.scanIdent(start);
    } else {
        lexer.skipChar();
        lexer.token.type = .ident;
        lexer.token.value = .{ .keyword = keyword };
    }
    return lexer.token;
}

pub fn scanIdent(lexer: *Lexer, start: usize) Token {
    while (isIdentPart(lexer.currentChar())) {
        lexer.skipChar();
    }
    switch (lexer.currentChar()) {
        '?', '!' => {
            if (lexer.peekNextChar() != '=') {
                lexer.skipChar();
            }
        },
        else => {},
    }
    lexer.token.type = .ident;
    lexer.token.value = .{ .string = lexer.stringRange(start) };
    return lexer.token;
}

pub fn skipSymbolChar(lexer: *Lexer, value: []const u8, start: usize) void {
    lexer.skipChar();
    lexer.symbol(value, start);
}

pub fn symbol(lexer: *Lexer, value: []const u8, start: usize) void {
    lexer.token.type = .symbol;
    lexer.token.value = .{ .string = value };
    lexer.setTokenRawFromStart(start);
}

// genCheckIntFitsInSize
// raiseValueDoesntFitIn
// warnLargeUint64Literal

fn scanNumber(lexer: *Lexer, start: usize, negative: bool) !void {
    // TODO
    lexer.token.type = .number;
    while (std.ascii.isDigit(lexer.nextChar())) {}
    lexer.setTokenRawFromStart(start);
    lexer.token.value = .{ .string = lexer.stringRange(start) };
    lexer.token.number_kind = .i32;
    _ = negative;
}

// fn consumeNumberSuffix
// nextStringToken
// fn nextStringTokenNoescape
// fn checkHeredocEnd

fn raiseUnterminatedQuoted(
    lexer: *Lexer,
    delimiter_state: Token.DelimiterState,
) !void {
    return lexer.raise(switch (delimiter_state.delimiters) {
        .command => "Unterminated command literal",
        .regex => "Unterminated regular expression",
        .heredoc => |delimiter| try std.fmt.allocPrint(
            lexer.allocator,
            "Unterminated heredoc: can't find \"{s}\" anywhere before the end of file",
            .{delimiter},
        ),
        .string => "Unterminated string literal",
        .string_array, .symbol_array => unreachable,
    });
}

// nextMacroToken
// lookahead
// peekAhead

const skipMacroWhitespace = MacroLexer.skipMacroWhitespace;
const checkMacroOpeningKeyword = MacroLexer.checkMacroOpeningKeyword;
const checkHeredocStart = MacroLexer.checkHeredocStart;

pub fn consumeOctalEscape(lexer: *Lexer, first_char: u8) !u8 {
    var value = first_char - '0';
    var count: usize = 1;
    while (count <= 3) : (count += 1) {
        const char = lexer.peekNextChar();
        if ('0' <= char and char < '8') {
            lexer.skipChar();
        } else {
            break;
        }
        if (@mulWithOverflow(u8, value, 8, &value) or
            @addWithOverflow(u8, value, char - '0', &value))
        {
            try lexer.raise("octal value too big");
        }
    }
    return value;
}

pub fn consumeCharUnicodeEscape(lexer: *Lexer) !u21 {
    if (lexer.peekNextChar() == '{') {
        lexer.skipChar();
        return try lexer.consumeBracedUnicodeEscape(false);
    } else {
        return try lexer.consumeNonBracedUnicodeEscape();
    }
}

pub fn consumeStringHexEscape(lexer: *Lexer) !u8 {
    const high = std.fmt.parseInt(u4, &[1]u8{lexer.nextChar()}, 16) catch {
        try lexer.raise("invalid hex escape");
        unreachable;
    };

    const low = std.fmt.parseInt(u4, &[1]u8{lexer.nextChar()}, 16) catch {
        try lexer.raise("invalid hex escape");
        unreachable;
    };

    return (@intCast(u8, high) << 4) | low;
}

pub fn consumeStringUnicodeEscape(lexer: *Lexer, buffer: *ArrayList(u8)) !void {
    if (lexer.peekNextChar() == '{') {
        lexer.skipChar();
        try lexer.consumeStringUnicodeBraceEscape(buffer);
    } else {
        const codepoint = try lexer.consumeNonBracedUnicodeEscape();
        var encoded: [4]u8 = undefined;
        const length = try std.unicode.utf8Encode(codepoint, &encoded);
        try buffer.appendSlice(encoded[0..length]);
    }
}

pub fn consumeStringUnicodeBraceEscape(lexer: *Lexer, buffer: *ArrayList(u8)) !void {
    while (true) {
        const codepoint = try lexer.consumeBracedUnicodeEscape(true);
        var encoded: [4]u8 = undefined;
        const length = try std.unicode.utf8Encode(codepoint, &encoded);
        try buffer.appendSlice(encoded[0..length]);
        if (lexer.currentChar() != ' ') break;
    }
}

pub fn consumeNonBracedUnicodeEscape(lexer: *Lexer) !u16 {
    var codepoint: u16 = 0;
    var count: usize = 0;
    while (count < 4) : (count += 1) {
        if (charToHex(lexer.nextChar())) |hex_value| {
            codepoint = 16 * codepoint + hex_value;
        } else {
            try lexer.expectedHexadecimalCharacterInUnicodeEscape();
        }
    }
    if (0xD800 <= codepoint and codepoint <= 0xDFFF) {
        try lexer.raise("invalid unicode codepoint (surrogate half)");
    }
    return codepoint;
}

pub fn consumeBracedUnicodeEscape(lexer: *Lexer, allow_spaces: bool) !u21 {
    var codepoint: u21 = 0;
    var found_curly = false;
    var found_space = false;
    var found_digit = false;
    var char: u8 = 0;

    var count: usize = 0;
    while (count < 6) : (count += 1) {
        char = lexer.nextChar();
        switch (char) {
            '}' => {
                found_curly = true;
                break;
            },
            ' ' => {
                if (allow_spaces) {
                    found_space = true;
                    break;
                } else {
                    try lexer.expectedHexadecimalCharacterInUnicodeEscape();
                }
            },
            else => {
                if (charToHex(char)) |hex_value| {
                    if (@mulWithOverflow(u21, 16, codepoint, &codepoint) or
                        @addWithOverflow(u21, codepoint, hex_value, &codepoint))
                    {
                        try lexer.raise("invalid unicode codepoint (too large)");
                    }
                    found_digit = true;
                } else {
                    try lexer.expectedHexadecimalCharacterInUnicodeEscape();
                }
            },
        }
    }

    if (!found_digit) {
        try lexer.expectedHexadecimalCharacterInUnicodeEscape();
    } else if (codepoint > 0x10FFFF) {
        try lexer.raise("invalid unicode codepoint (too large)");
    } else if (0xD800 <= codepoint and codepoint <= 0xDFFF) {
        try lexer.raise("invalid unicode codepoint (surrogate half)");
    }

    if (!found_space) {
        if (!found_curly) {
            char = lexer.nextChar();
        }

        if (char != '}') {
            try lexer.raise("expected '}' to close unicode escape");
        }
    }

    return codepoint;
}

pub fn expectedHexadecimalCharacterInUnicodeEscape(lexer: *Lexer) !void {
    try lexer.raise("expected hexadecimal character in unicode escape");
}

// stringTokenEscapeValue

pub fn delimiterStart(lexer: *Lexer, state: Token.DelimiterState, start: usize) void {
    lexer.skipChar();
    lexer.delimiterStartNoAdvance(state, start);
}

pub fn delimiterStartNoAdvance(lexer: *Lexer, state: Token.DelimiterState, start: usize) void {
    lexer.token.type = .delimiter_start;
    lexer.token.delimiter_state = state;
    lexer.setTokenRawFromStart(start);
}

pub const nextStringArrayToken = StringLexer.nextStringArrayToken;

// fn charToHex(char: u8) ?u8 {
//     return switch (char) {
//         '0' ... '9' => char - '0',
//         'a' ... 'f' => 10 + (char - 'a'),
//         'A' ... 'F' => 10 + (char - 'A'),
//         else => null
//     };
// }

pub fn charToHex(char: u8) ?u4 {
    return switch (char) {
        // zig fmt: off
        '0' => 0, '1' => 1, '2' => 2, '3' => 3, '4' => 4,
        '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9,
        'a', 'A' => 10, 'b', 'B' => 11, 'c', 'C' => 12,
        'd', 'D' => 13, 'e', 'E' => 14, 'f', 'F' => 15,
        // zig fmt: on
        else => null,
    };
}

pub fn consumeLocPragma(lexer: *Lexer) !void {
    switch (lexer.currentChar()) {
        '"' => {
            // skip '"'
            lexer.skipCharNoColumnIncrement();

            const filename_pos = lexer.current_pos;

            while (true) {
                switch (lexer.currentChar()) {
                    '"' => break,
                    0 => try lexer.raise("unexpected end of file in loc pragma"),
                    else => lexer.skipCharNoColumnIncrement(),
                }
            }

            lexer.incrColumnNumberBy(lexer.current_pos - filename_pos + 7); // == "#<loc:\"".size
            const filename = lexer.stringRange(filename_pos);

            // skip '"'
            lexer.skipChar();

            if (lexer.currentChar() != ',') {
                try lexer.raise("expected ',' in loc pragma after filename");
            }
            lexer.skipChar();

            var line_number: usize = 0;
            while (true) {
                switch (lexer.currentChar()) {
                    '0'...'9' => {
                        line_number = 10 * line_number + (lexer.currentChar() - '0');
                    },
                    ',' => {
                        lexer.skipChar();
                        break;
                    },
                    else => {
                        try lexer.raise("expected digit or ',' in loc pragma for line number");
                    },
                }
                lexer.skipChar();
            }

            var column_number: usize = 0;
            while (true) {
                switch (lexer.currentChar()) {
                    '0'...'9' => {
                        column_number = 10 * column_number + (lexer.currentChar() - '0');
                    },
                    '>' => {
                        lexer.skipChar();
                        break;
                    },
                    else => {
                        try lexer.raise("expected digit or '>' in loc pragma for column_number number");
                    },
                }
                lexer.skipChar();
            }

            lexer.setLocation(filename, line_number, column_number);
        },
        'p' => {
            switch (lexer.nextCharNoColumnIncrement()) {
                'o' => {
                    if (lexer.nextCharNoColumnIncrement() != 'p' or
                        lexer.nextCharNoColumnIncrement() != '>')
                    {
                        try lexer.raise("expected #<loc:push>, #<loc:pop> or #<loc:\"...\">");
                    }

                    // skip '>'
                    lexer.skipCharNoColumnIncrement();

                    lexer.incrColumnNumberBy(10); // == "#<loc:pop>".size

                    lexer.popLocation();
                },
                'u' => {
                    if (lexer.nextCharNoColumnIncrement() != 's' or
                        lexer.nextCharNoColumnIncrement() != 'h' or
                        lexer.nextCharNoColumnIncrement() != '>')
                    {
                        try lexer.raise("expected #<loc:push>, #<loc:pop> or #<loc:\"...\">");
                    }

                    // skip '>'
                    lexer.skipCharNoColumnIncrement();

                    lexer.incrColumnNumberBy(11); // == "#<loc:push>".size

                    lexer.token.line_number = lexer.line_number;
                    lexer.token.column_number = lexer.column_number;
                    lexer.pushLocation();
                },
                else => {
                    try lexer.raise("expected #<loc:push>, #<loc:pop> or #<loc:\"...\">");
                },
            }
        },
        else => {
            try lexer.raise("expected #<loc:push>, #<loc:pop> or #<loc:\"...\">");
        },
    }
}

fn consumeHeredocStart(lexer: *Lexer, start: usize) !void {
    var has_single_quote = false;
    var found_closing_single_quote = false;

    if (lexer.nextChar() == '\'') {
        has_single_quote = true;
        lexer.skipChar();
    }

    if (!isIdentPart(lexer.currentChar())) {
        try lexer.raise("heredoc identifier starts with invalid character");
    }

    const start_here = lexer.current_pos;
    var end_here: usize = 0;

    while (true) {
        const char = lexer.nextChar();
        if (char == '\r') {
            if (lexer.peekNextChar() == '\n') {
                end_here = lexer.current_pos;
                lexer.skipChar();
                break;
            } else {
                try lexer.raise("expecting '\\n' after '\\r'");
            }
        } else if (char == '\n') {
            end_here = lexer.current_pos;
            break;
        } else if (isIdentPart(char)) {
            // ok
        } else if (char == 0) {
            try lexer.raise("Unexpected EOF on heredoc identifier");
        } else {
            if (char == '\'' and has_single_quote) {
                found_closing_single_quote = true;
                end_here = lexer.current_pos;
                lexer.skipChar();
                break;
            } else if (has_single_quote) {
                // wait until another quote
            } else {
                end_here = lexer.current_pos;
                break;
            }
        }
    }

    if (has_single_quote and !found_closing_single_quote) {
        try lexer.raise("expecting closing single quote");
    }

    const here = lexer.stringRange2(start_here, end_here);

    lexer.delimiterStartNoAdvance(.{
        .delimiters = .{ .heredoc = here },
        .allow_escapes = !has_single_quote,
    }, start);
}

fn consumeSymbol(lexer: *Lexer, first_char: u8, start: usize) !void {
    switch (first_char) {
        '+' => {
            lexer.skipSymbolChar("+", start);
        },
        '-' => {
            lexer.skipSymbolChar("-", start);
        },
        '*' => {
            if (lexer.nextChar() == '*') {
                lexer.skipSymbolChar("**", start);
            } else {
                lexer.symbol("*", start);
            }
        },
        '/' => {
            if (lexer.nextChar() == '/') {
                lexer.skipSymbolChar("//", start);
            } else {
                lexer.symbol("/", start);
            }
        },
        '=' => {
            switch (lexer.nextChar()) {
                '=' => {
                    if (lexer.nextChar() == '=') {
                        lexer.skipSymbolChar("===", start);
                    } else {
                        lexer.symbol("==", start);
                    }
                },
                '~' => {
                    lexer.skipSymbolChar("=~", start);
                },
                else => {
                    try lexer.unknownToken();
                },
            }
        },
        '!' => {
            switch (lexer.nextChar()) {
                '=' => {
                    lexer.skipSymbolChar("!=", start);
                },
                '~' => {
                    lexer.skipSymbolChar("!~", start);
                },
                else => {
                    lexer.symbol("!", start);
                },
            }
        },
        '<' => {
            switch (lexer.nextChar()) {
                '=' => {
                    if (lexer.nextChar() == '>') {
                        lexer.skipSymbolChar("<=>", start);
                    } else {
                        lexer.symbol("<=", start);
                    }
                },
                '<' => {
                    lexer.skipSymbolChar("<<", start);
                },
                else => {
                    lexer.symbol("<", start);
                },
            }
        },
        '>' => {
            switch (lexer.nextChar()) {
                '=' => {
                    lexer.skipSymbolChar(">=", start);
                },
                '>' => {
                    lexer.skipSymbolChar(">>", start);
                },
                else => {
                    lexer.symbol(">", start);
                },
            }
        },
        '&' => {
            switch (lexer.nextChar()) {
                '+' => {
                    lexer.skipSymbolChar("&+", start);
                },
                '-' => {
                    lexer.skipSymbolChar("&-", start);
                },
                '*' => {
                    switch (lexer.nextChar()) {
                        '*' => {
                            lexer.skipSymbolChar("&**", start);
                        },
                        else => {
                            lexer.symbol("&*", start);
                        },
                    }
                },
                else => {
                    lexer.symbol("&", start);
                },
            }
        },
        '|' => {
            lexer.skipSymbolChar("|", start);
        },
        '^' => {
            lexer.skipSymbolChar("^", start);
        },
        '~' => {
            lexer.skipSymbolChar("~", start);
        },
        '%' => {
            lexer.skipSymbolChar("%", start);
        },
        '[' => {
            if (lexer.nextChar() == ']') {
                switch (lexer.nextChar()) {
                    '=' => {
                        lexer.skipSymbolChar("[]=", start);
                    },
                    '?' => {
                        lexer.skipSymbolChar("[]?", start);
                    },
                    else => {
                        lexer.symbol("[]", start);
                    },
                }
            } else {
                try lexer.unknownToken();
            }
        },
        '"' => {
            try lexer.consumeQuotedSymbol(start);
        },
        else => |char| {
            if (isIdentStart(char)) {
                while (isIdentPart(lexer.nextChar())) {
                    // Nothing to do
                }
                switch (lexer.currentChar()) {
                    '?' => {
                        lexer.skipChar();
                    },
                    '!', '=' => {
                        if (lexer.peekNextChar() != '=') {
                            lexer.skipChar();
                        }
                    },
                    else => {},
                }
                lexer.token.type = .symbol;
                lexer.token.value = .{ .string = lexer.stringRange(start + 1) };
                lexer.setTokenRawFromStart(start);
            } else {
                lexer.token.type = .op_colon;
            }
        },
    }
}

fn consumeQuotedSymbol(lexer: *Lexer, start: usize) !void {
    const line = lexer.line_number;
    const column = lexer.column_number;
    var buffer = ArrayList(u8).init(lexer.allocator);
    // var buffer: ArrayList(u8) = undefined;
    // var found_escape = false;
    while (true) {
        switch (lexer.nextChar()) {
            '\\' => {
                // if (!found_escape) {
                //     found_escape = true;
                //     buffer = ArrayList(u8).init(lexer.allocator);
                //     try buffer.appendSlice(lexer.stringRange(start + 2));
                // }
                switch (lexer.nextChar()) {
                    'a' => {
                        try buffer.append(std.ascii.control_code.bel); // '\a'
                    },
                    'b' => {
                        try buffer.append(std.ascii.control_code.bs); // '\b'
                    },
                    'n' => {
                        try buffer.append('\n');
                    },
                    'r' => {
                        try buffer.append('\r');
                    },
                    't' => {
                        try buffer.append('\t');
                    },
                    'v' => {
                        try buffer.append(std.ascii.control_code.vt); // '\v'
                    },
                    'f' => {
                        try buffer.append(std.ascii.control_code.ff); // '\f'
                    },
                    'e' => {
                        try buffer.append(std.ascii.control_code.esc); // '\e'
                    },
                    'x' => {
                        try buffer.append(try lexer.consumeStringHexEscape());
                    },
                    'u' => {
                        try lexer.consumeStringUnicodeEscape(&buffer);
                    },
                    '0'...'7' => |char| {
                        try buffer.append(try lexer.consumeOctalEscape(char));
                    },
                    '\n' => {
                        lexer.incrLineNumberWithColumn(null);
                        try buffer.append('\n');
                    },
                    0 => {
                        try lexer.raiseAt("unterminated quoted symbol", line, column);
                    },
                    else => |char| {
                        try buffer.append(char);
                    },
                }
            },
            '"' => {
                break;
            },
            0 => {
                try lexer.raiseAt("unterminated quoted symbol", line, column);
            },
            else => |char| {
                try buffer.append(char);
                // if (found_escape) {
                //     try buffer.append(char);
                // }
            },
        }
    }

    lexer.token.type = .symbol;
    lexer.token.value = .{ .buffer = buffer };
    // if (found_escape) {
    //     lexer.token.value = .{ .buffer = buffer };
    // } else {
    //     lexer.token.value = .{ .string = lexer.stringRange(start + 2) };
    // }
    lexer.skipChar();
    lexer.setTokenRawFromStart(start);
}

fn consumeCharLiteral(lexer: *Lexer) !void {
    const start = lexer.current_pos;
    const line = lexer.line_number;
    const column = lexer.column_number;
    lexer.token.type = .char;
    switch (lexer.nextChar()) {
        '\\' => {
            switch (lexer.nextChar()) {
                '\\' => {
                    lexer.token.value = .{ .char = '\\' };
                },
                '\'' => {
                    lexer.token.value = .{ .char = '\'' };
                },
                'a' => {
                    lexer.token.value = .{ .char = std.ascii.control_code.bel }; // '\a'
                },
                'b' => {
                    lexer.token.value = .{ .char = std.ascii.control_code.bs }; // '\b'
                },
                'e' => {
                    lexer.token.value = .{ .char = std.ascii.control_code.esc }; // '\e'
                },
                'f' => {
                    lexer.token.value = .{ .char = std.ascii.control_code.ff }; // '\f'
                },
                'n' => {
                    lexer.token.value = .{ .char = '\n' };
                },
                'r' => {
                    lexer.token.value = .{ .char = '\r' };
                },
                't' => {
                    lexer.token.value = .{ .char = '\t' };
                },
                'v' => {
                    lexer.token.value = .{ .char = std.ascii.control_code.vt }; // '\v'
                },
                'u' => {
                    const codepoint = try lexer.consumeCharUnicodeEscape();
                    lexer.token.value = .{ .utf8 = codepoint };
                },
                '0' => {
                    lexer.token.value = .{ .char = std.ascii.control_code.nul }; // '\0'
                },
                0 => {
                    try lexer.raiseAt("unterminated char literal", line, column);
                },
                else => |char| {
                    const message = try std.fmt.allocPrint(
                        lexer.allocator,
                        "invalid char escape sequence '\\{c}'",
                        .{char},
                    );
                    try lexer.raiseAt(message, line, column);
                },
            }
        },
        '\'' => {
            try lexer.raiseAt("invalid empty char literal (did you mean '\\''?)", line, column);
        },
        0 => {
            try lexer.raiseAt("unterminated char literal", line, column);
        },
        else => |char| {
            lexer.token.value = .{ .char = char };
        },
    }
    if (lexer.nextChar() != '\'') {
        try lexer.raiseAt("unterminated char literal, use double quotes for strings", line, column);
    }
    lexer.skipChar();
    lexer.setTokenRawFromStart(start);
}

fn consumeVariable(lexer: *Lexer, token_type: Token.Kind, start: usize) !void {
    if (isIdentStart(lexer.currentChar())) {
        while (isIdentPart(lexer.nextChar())) {
            // Nothing to do
        }
        lexer.token.type = token_type;
        lexer.token.value = .{ .string = lexer.stringRange(start) };
    } else {
        try lexer.unknownToken();
    }
}

fn consumeGlobalMatchDataIndex(lexer: *Lexer) void {
    const start = lexer.current_pos;
    if (lexer.currentChar() == '0') {
        lexer.skipChar();
    } else {
        while (std.ascii.isDigit(lexer.nextChar())) {
            // Nothing to do
        }
        if (lexer.currentChar() == '?') lexer.skipChar();
    }
    lexer.token.type = .global_match_data_index;
    lexer.token.value = .{ .string = lexer.stringRange(start) };
}

pub fn setLocation(lexer: *Lexer, filename: []const u8, line_number: usize, column_number: usize) void {
    lexer.filename = filename;
    lexer.token.filename = filename;

    lexer.line_number = line_number;
    lexer.token.line_number = line_number;

    lexer.column_number = column_number;
    lexer.token.column_number = column_number;
}

pub fn popLocation(lexer: *Lexer) void {
    if (lexer.stacked) {
        lexer.stacked = false;

        lexer.filename = lexer.stacked_filename;
        lexer.token.filename = lexer.stacked_filename;

        lexer.line_number = lexer.stacked_line_number;
        lexer.token.line_number = lexer.stacked_line_number;

        lexer.column_number = lexer.stacked_column_number;
        lexer.token.column_number = lexer.stacked_column_number;
    }
}

pub fn pushLocation(lexer: *Lexer) void {
    if (!lexer.stacked) {
        lexer.stacked = true;
        lexer.stacked_filename = lexer.filename;
        lexer.stacked_line_number = lexer.line_number;
        lexer.stacked_column_number = lexer.column_number;
    }
}

pub fn incrColumnNumber(lexer: *Lexer) void {
    lexer.incrColumnNumberBy(1);
}

pub fn incrColumnNumberBy(lexer: *Lexer, d: usize) void {
    lexer.column_number += d;
    if (lexer.stacked) {
        lexer.stacked_column_number += d;
    }
}

pub fn incrLineNumber(lexer: *Lexer) void {
    lexer.incrLineNumberWithColumn(1);
}

pub fn incrLineNumberWithColumn(lexer: *Lexer, column_number: ?usize) void {
    lexer.line_number += 1;
    if (column_number) |c| lexer.column_number = c;
    if (lexer.stacked) {
        lexer.stacked_line_number += 1;
        if (column_number) |c| lexer.stacked_column_number = c;
    }
}

pub inline fn skipCharNoColumnIncrement(lexer: *Lexer) void {
    lexer.current_pos += 1;
}

pub inline fn nextCharNoColumnIncrement(lexer: *Lexer) u8 {
    lexer.skipCharNoColumnIncrement();
    return lexer.currentChar();
}

pub inline fn skipChar(lexer: *Lexer) void {
    lexer.incrColumnNumber();
    lexer.skipCharNoColumnIncrement();
}

pub inline fn nextChar(lexer: *Lexer) u8 {
    lexer.skipChar();
    return lexer.currentChar();
}

pub fn nextCharCheckLine(lexer: *Lexer) u8 {
    const char = lexer.nextCharNoColumnIncrement();
    if (char == '\n') {
        lexer.incrLineNumber();
    } else {
        lexer.incrColumnNumber();
    }
    return char;
}

pub inline fn skipTokenChar(lexer: *Lexer, token_type: Token.Kind) void {
    lexer.skipChar();
    lexer.token.type = token_type;
}

pub fn resetToken(lexer: *Lexer) void {
    lexer.token.value = .nil;
    lexer.token.line_number = lexer.line_number;
    lexer.token.column_number = lexer.column_number;
    lexer.token.filename = lexer.filename;
    lexer.token._location = null;
    lexer.token.passed_backslash_newline = false;
    lexer.token.doc_buffer = null;
    lexer.token.invalid_escape = false;
    lexer._token_end_location = null;
}

pub fn skipTokenAndSpace(lexer: *Lexer) !void {
    _ = try lexer.nextToken();
    try lexer.skipSpace();
}

pub fn skipTokenAndSpaceOrNewline(lexer: *Lexer) !void {
    _ = try lexer.nextToken();
    try lexer.skipSpaceOrNewline();
}

pub fn skipTokenAndStatementEnd(lexer: *Lexer) !void {
    _ = try lexer.nextToken();
    try lexer.skipStatementEnd();
}

pub fn nextTokenNeverASymbol(lexer: *Lexer) !Token {
    lexer.wants_symbol = false;
    const token = try lexer.nextToken();
    lexer.wants_symbol = true;
    return token;
}

pub fn currentChar(lexer: Lexer) u8 {
    if (lexer.current_pos < lexer.string.len) {
        return lexer.string[lexer.current_pos];
    } else {
        return 0;
    }
}

pub fn peekNextChar(lexer: Lexer) u8 {
    if (lexer.current_pos + 1 < lexer.string.len) {
        return lexer.string[lexer.current_pos + 1];
    } else {
        return 0;
    }
}

pub fn stringRange(lexer: Lexer, start_pos: usize) []const u8 {
    return lexer.stringRange2(start_pos, lexer.current_pos);
}

pub fn stringRange2(lexer: Lexer, start_pos: usize, end_pos: usize) []const u8 {
    return lexer.string[start_pos..end_pos];
}

// stringRangeFromPool
// stringRangeFromPool2
// sliceRange
// sliceRange2

pub fn isIdentStart(char: u8) bool {
    return std.ascii.isAlphabetic(char) or char == '_' or char > 0x9F;
}

pub fn isIdentPart(char: u8) bool {
    return isIdentStart(char) or std.ascii.isDigit(char);
}

pub fn isIdent(name: []const u8) bool {
    return name.len > 0 and isIdentStart(name[0]);
}

pub fn isSetter(name: []const u8) bool {
    return isIdent(name) and name[name.len - 1] == '=';
}

pub fn isIdentPartOrEnd(char: u8) bool {
    return isIdentPart(char) or char == '?' or char == '!';
}

const peekNotIdentPartOrEndNextChar = MacroLexer.peekNotIdentPartOrEndNextChar;

pub fn closingChar(char: u8) u8 {
    return switch (char) {
        '<' => '>',
        '(' => ')',
        '[' => ']',
        '{' => '}',
        else => char,
    };
}

pub fn skipSpace(lexer: *Lexer) !void {
    while (lexer.token.type == .space) {
        _ = try lexer.nextToken();
    }
}

pub fn skipSpaceOrNewline(lexer: *Lexer) !void {
    while (true) {
        switch (lexer.token.type) {
            .space, .newline => _ = try lexer.nextToken(),
            else => break,
        }
    }
}

pub fn skipStatementEnd(lexer: *Lexer) !void {
    while (true) {
        switch (lexer.token.type) {
            .space, .newline, .op_semicolon => {
                _ = try lexer.nextToken();
            },
            else => {
                break;
            },
        }
    }
}

pub fn handleCrlfOrLf(lexer: *Lexer) !bool {
    const isCarriageReturn = lexer.currentChar() == '\r';
    if (isCarriageReturn) {
        if (lexer.nextChar() != '\n') {
            try lexer.raise("expecting '\\n' after '\\r'");
        }
    }
    return isCarriageReturn;
}

inline fn charSequence(lexer: *Lexer, comptime chars: []const u8) bool {
    comptime var i = 0;
    inline while (i < chars.len) : (i += 1) {
        @setEvalBranchQuota(2000);
        if (lexer.nextChar() != chars[i]) {
            return false;
        }
    }
    return true;
}

pub fn unknownToken(lexer: *Lexer) !void {
    switch (lexer.currentChar()) {
        '\n' => try lexer.raise("unknown token: '\\n'"),
        else => |c| {
            const message = try std.fmt.allocPrint(
                lexer.allocator,
                "unknown token: '{s}'",
                .{std.fmt.fmtSliceEscapeUpper(&[1]u8{c})},
            );
            try lexer.raise(message);
        },
    }
}

pub fn setTokenRawFromStart(lexer: *Lexer, start: usize) void {
    if (lexer.wants_raw) {
        lexer.token.raw = lexer.stringRange(start);
    }
}

pub fn raise(lexer: *Lexer, message: []const u8) !void {
    return lexer.raiseAt(
        message,
        lexer.line_number,
        lexer.column_number,
    );
}

pub fn raiseAt(
    lexer: *Lexer,
    message: []const u8,
    line_number: usize,
    column_number: usize,
) !void {
    lexer.error_message = message;
    // zig fmt: off
    _ = line_number; _ = column_number; // TODO
    // zig fmt: on
    return error.SyntaxError;
}

pub fn raiseFor(
    lexer: *Lexer,
    message: []const u8,
    token: Token,
) !void {
    return lexer.raiseAt(
        message,
        token.line_number,
        token.column_number,
    );
    // TODO: token.filename
}

pub fn raiseLoc(
    lexer: *Lexer,
    message: []const u8,
    location: Location,
) !void {
    return lexer.raiseAt(
        message,
        location.line_number,
        location.column_number,
    );
    // TODO: location.filename
}

const MacroLexer = struct {
    fn skipMacroWhitespace(lexer: *Lexer) void {
        while (std.ascii.isWhitespace(lexer.currentChar())) {
            if (lexer.currentChar() == '\n') {
                lexer.incrLineNumberWithColumn(0);
            }
            lexer.skipChar();
        }
    }

    fn checkMacroOpeningKeyword(lexer: *Lexer, beginning_of_line: bool, is_abstract_def: *bool) bool {
        switch (lexer.currentChar()) {
            'a' => {
                switch (lexer.nextChar()) {
                    'b' => {
                        if (lexer.charSequence("stract") and std.ascii.isWhitespace(lexer.nextChar())) {
                            switch (lexer.nextChar()) {
                                'd' => {
                                    if (lexer.charSequence("ef") and lexer.peekNotIdentPartOrEndNextChar()) {
                                        is_abstract_def.* = true;
                                        return true;
                                    }
                                },
                                'c' => {
                                    return lexer.charSequence("lass") and lexer.peekNotIdentPartOrEndNextChar();
                                },
                                's' => {
                                    return lexer.charSequence("truct") and lexer.peekNotIdentPartOrEndNextChar();
                                },
                                else => {
                                    return false;
                                },
                            }
                        }
                    },
                    'n' => {
                        return lexer.charSequence("notation") and lexer.peekNotIdentPartOrEndNextChar();
                    },
                    else => {
                        return false;
                    },
                }
            },
            'b' => {
                return lexer.charSequence("egin") and lexer.peekNotIdentPartOrEndNextChar();
            },
            'c' => {
                switch (lexer.nextChar()) {
                    'a' => {
                        return lexer.charSequence("se") and lexer.peekNotIdentPartOrEndNextChar();
                    },
                    'l' => {
                        return lexer.charSequence("ass") and lexer.peekNotIdentPartOrEndNextChar();
                    },
                    else => {
                        return false;
                    },
                }
            },
            'd' => {
                switch (lexer.nextChar()) {
                    'o' => {
                        return lexer.peekNotIdentPartOrEndNextChar();
                    },
                    'e' => {
                        return lexer.nextChar() == 'f' and lexer.peekNotIdentPartOrEndNextChar();
                    },
                    else => {
                        return false;
                    },
                }
            },
            'f' => {
                return lexer.charSequence("un") and lexer.peekNotIdentPartOrEndNextChar();
            },
            'i' => {
                return beginning_of_line and lexer.nextChar() == 'f' and lexer.peekNotIdentPartOrEndNextChar();
            },
            'l' => {
                return lexer.charSequence("ib") and lexer.peekNotIdentPartOrEndNextChar();
            },
            'm' => {
                switch (lexer.nextChar()) {
                    'a' => {
                        return lexer.charSequence("cro") and lexer.peekNotIdentPartOrEndNextChar();
                    },
                    'o' => {
                        return lexer.charSequence("dule") and lexer.peekNotIdentPartOrEndNextChar();
                    },
                    else => {
                        return false;
                    },
                }
            },
            's' => {
                switch (lexer.nextChar()) {
                    'e' => {
                        return lexer.charSequence("lect") and lexer.peekNotIdentPartOrEndNextChar();
                    },
                    't' => {
                        return lexer.charSequence("ruct") and lexer.peekNotIdentPartOrEndNextChar();
                    },
                    else => {
                        return false;
                    },
                }
            },
            'u' => {
                if (lexer.nextChar() == 'n') {
                    switch (lexer.nextChar()) {
                        'i' => {
                            return lexer.charSequence("on") and lexer.peekNotIdentPartOrEndNextChar();
                        },
                        'l' => {
                            return beginning_of_line and lexer.charSequence("ess") and lexer.peekNotIdentPartOrEndNextChar();
                        },
                        't' => {
                            return beginning_of_line and lexer.charSequence("il") and lexer.peekNotIdentPartOrEndNextChar();
                        },
                        else => {
                            return false;
                        },
                    }
                }
            },
            'w' => {
                return beginning_of_line and lexer.charSequence("hile") and lexer.peekNotIdentPartOrEndNextChar();
            },
            else => {
                return false;
            },
        }
        return false;
    }

    fn checkHeredocStart(lexer: *Lexer) ?Token.DelimiterState {
        if (lexer.currentChar() != '<' or !lexer.charSequence("<-")) {
            return null;
        }

        var has_single_quote = false;
        var found_closing_single_quote = false;

        if (lexer.nextChar() == '\'') {
            has_single_quote = true;
            lexer.skipChar();
        }

        if (!isIdentPart(lexer.currentChar())) {
            return null;
        }

        const start_here = lexer.current_pos;
        var end_here: usize = 0;

        while (true) {
            const char = lexer.nextChar();
            if (char == '\r') {
                if (lexer.peekNextChar() == '\n') {
                    end_here = lexer.current_pos;
                    lexer.skipChar();
                    break;
                } else {
                    return null;
                }
            } else if (char == '\n') {
                end_here = lexer.current_pos;
                break;
            } else if (isIdentPart(char)) {
                // ok
            } else if (char == 0) {
                return null;
            } else if (char == '\'' and has_single_quote) {
                found_closing_single_quote = true;
                end_here = lexer.current_pos;
                lexer.skipChar();
                break;
            } else if (has_single_quote) {
                // wait until another quote
            } else {
                end_here = lexer.current_pos;
                break;
            }
        }

        if (has_single_quote and !found_closing_single_quote) {
            return null;
        }

        const here = lexer.stringRange2(start_here, end_here);
        return .{
            .delimiters = .{ .heredoc = here },
            .allow_escapes = !has_single_quote,
        };
    }

    fn peekNotIdentPartOrEndNextChar(lexer: *Lexer) bool {
        const next_char = lexer.peekNextChar();
        if (isIdentPartOrEnd(next_char) or next_char == ':') {
            return false;
        }
        lexer.skipChar();
        return true;
    }
};

const StringLexer = struct {
    fn nextStringArrayToken(lexer: *Lexer) !Token {
        lexer.resetToken();

        while (true) {
            const char = lexer.currentChar();
            if (char == '\n') {
                lexer.skipChar();
                lexer.incrLineNumberWithColumn(1); // TODO: same as incrLineNumber
            } else if (std.ascii.isWhitespace(char)) {
                lexer.skipChar();
            } else {
                break;
            }
        }

        var delimiter_end: u8 = undefined;
        var delimiter_nest: u8 = undefined;
        switch (lexer.token.delimiter_state.delimiters) {
            .string_array => |delimiters| {
                delimiter_end = delimiters.end;
                delimiter_nest = delimiters.nest;
            },
            .symbol_array => |delimiters| {
                delimiter_end = delimiters.end;
                delimiter_nest = delimiters.nest;
            },
            else => unreachable,
        }

        if (lexer.currentChar() == delimiter_end) {
            const start = lexer.current_pos;
            lexer.skipChar();
            lexer.setTokenRawFromStart(start);
            lexer.token.type = .string_array_end;
            return lexer.token;
        }

        const start = lexer.current_pos;
        var sub_start = start;
        var value = ArrayList(u8).init(lexer.allocator);

        var escaped = false;
        while (true) {
            const char = lexer.currentChar();
            if (char == 0) {
                break; // raise is handled by parser
            } else if (char == delimiter_end) {
                if (!escaped) {
                    if (lexer.token.delimiter_state.open_count == 0) {
                        break;
                    } else {
                        lexer.token.delimiter_state.open_count -= 1;
                    }
                }
            } else if (char == delimiter_nest) {
                if (!escaped) {
                    lexer.token.delimiter_state.open_count += 1;
                }
            } else if (std.ascii.isWhitespace(char)) {
                if (!escaped) break;
            } else if (escaped) {
                try value.append('\\');
            }

            escaped = lexer.currentChar() == '\\';
            if (escaped) {
                try value.appendSlice(lexer.stringRange2(sub_start, lexer.current_pos));
                sub_start = lexer.current_pos + 1;
            }

            lexer.skipChar();
        }

        if (start == lexer.current_pos) {
            lexer.token.type = .eof;
            return lexer.token;
        }

        try value.appendSlice(lexer.stringRange2(sub_start, lexer.current_pos));

        lexer.token.type = .string;
        lexer.token.value = .{ .buffer = value };
        lexer.setTokenRawFromStart(start);

        return lexer.token;
    }
};

// zig fmt: off
pub fn main() !void {
    // const p = @import("std").debug.print;
    const p = struct {
        fn f(fmt: []const u8, args: anytype) void {
            _ = fmt;
            _ = args;
        }
    }.f;
    const assert = @import("std").debug.assert;
    const escape = std.fmt.fmtSliceEscapeUpper;

    var lexer = Lexer.new("foo");
    p("{}\n", .{@TypeOf(try lexer.nextToken())});
    p("{}\n", .{lexer.tokenEndLocation()});
    p("{s}\n", .{lexer.stringRange2(0, 3)});
    p("{c} {c} {s}\n", .{lexer.nextChar(), lexer.nextCharCheckLine(), lexer.stringRange(0)});
    lexer = Lexer.new("#<loc:push>");
    var token = try lexer.nextToken();
    p("{} {}\n", .{@TypeOf(token), token.location()});
    lexer = Lexer.new("#<loc:\"foo\",1,2>");
    token = try lexer.nextToken();
    p("{} {}\n", .{@TypeOf(token), token.location()});
    lexer = Lexer.new("#<loc:push");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new("#<loc:\"foo\",1,2");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    // p("{}\n", .{std.ascii.isPrint('\n')}); // => false
    // p("{}\n", .{std.ascii.isPrint(0)}); // => false
    // p("{}\n", .{std.ascii.isPrint('a')}); // => true
    // p("{}\n", .{std.ascii.isPrint(' ')}); // => true
    // p("{}\n", .{std.ascii.isPrint(':')}); // => true
    // p("{s}\n", .{escape(&[1]u8{'\n'})}); // => \x0A
    // p("{}\n", .{"\\x0A".len}); // => 4
    lexer = Lexer.new("f\n");
    if (lexer.unknownToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer.skipChar();
    if (lexer.unknownToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new("\r");
    if (lexer.handleCrlfOrLf()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new("\r\n");
    p("{}\n", .{try lexer.handleCrlfOrLf()});
    p("{c}\n", .{closingChar('<')});
    p("{} {} {} {}\n", .{isIdentStart(160), isIdentStart('a'), isIdentStart('_'), isIdentStart('1')});
    p("{} {}\n", .{isIdentPart('1'), isIdentPartOrEnd('1')});
    p("{} {}\n", .{isIdent("foo "), isSetter("foo =")});
    lexer = Lexer.new(";,a");
    p("{c} {} {c} {} {c}\n", .{
        lexer.currentChar(),
        lexer.peekNotIdentPartOrEndNextChar(),
        lexer.currentChar(),
        lexer.peekNotIdentPartOrEndNextChar(),
        lexer.currentChar(),
    });
    p("{?} {?} {?} {?}\n", .{charToHex('0'), charToHex('a'), charToHex('A'), charToHex('?')});
    lexer = Lexer.new("123");
    p("{}\n", .{try lexer.consumeOctalEscape('1')});
    lexer = Lexer.new("377");
    p("{}\n", .{try lexer.consumeOctalEscape('3')});
    lexer = Lexer.new("400");
    if (lexer.consumeOctalEscape('4')) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new(".FFFF");
    p("{}\n", .{try lexer.consumeNonBracedUnicodeEscape()});
    lexer = Lexer.new(".FFFZ");
    if (lexer.consumeNonBracedUnicodeEscape()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new(".FFFFFF}");
    if (lexer.consumeBracedUnicodeEscape(false)) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new(".10FFFF}");
    p("{}\n", .{try lexer.consumeBracedUnicodeEscape(false)});
    lexer = Lexer.new(".120000}");
    if (lexer.consumeBracedUnicodeEscape(false)) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});

    lexer = Lexer.new("# foobar");
    lexer.doc_enabled = true;
    lexer.comment_is_doc = true;
    token = try lexer.nextToken();
    p("{s}\n", .{token.doc_buffer.?.items});

    lexer = Lexer.new("# foobar");
    lexer.comments_enabled = true;
    token = try lexer.nextToken();
    p("{s}\n", .{token.value.string});

    lexer = Lexer.new("");
    token = try lexer.nextToken();
    p("{}\n", .{token.type});

    lexer = Lexer.new("Foo");
    token = try lexer.nextToken();
    p("{} {s}\n", .{token.type, token.value.string});

    lexer = Lexer.new("foo");
    token = try lexer.nextToken();
    p("{} {s}\n", .{token.type, token.value.string});

    lexer = Lexer.new("\x9F");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});

    // consumeWhitespace
    lexer = Lexer.new(" ");
    lexer.count_whitespace = true;
    token = try lexer.nextToken();
    p("{} \"{s}\"\n", .{token.type, token.value.string});

    lexer = Lexer.new("\t\\\n\\\r\n");
    lexer.count_whitespace = true;
    token = try lexer.nextToken();
    p("{} {s}\n", .{token.type, escape(token.value.string)});

    lexer = Lexer.new("\\\n\\\r\n");
    lexer.count_whitespace = true;
    token = try lexer.nextToken();
    p("{} {s}\n", .{token.type, escape(token.value.string)});

    // consumeNewlines
    lexer = Lexer.new("\n\r\n");
    lexer.count_whitespace = false;
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    lexer = Lexer.new("\n\r\n");
    lexer.count_whitespace = true;
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    lexer = Lexer.new("\r");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new("\n\r");
    // token = try lexer.nextToken();
    // p("{} {}\n", .{token.type, token.value});
    // if (true) unreachable;
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});

    // checkIdentOrKeyword
    lexer = Lexer.new("yield");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    for ([_][]const u8{
        "abstract", "alias", "asm", "as", "as?", "annotation",
        "begin", "break",
        "case", "class",
        "def", "do",
        "else", "elsif", "end", "ensure", "enum", "extend",
        "false", "for", "fun",
        "if", "include", "instance_sizeof", "is_a?",
        "lib",
        "macro", "module",
        "next", "nil?", "nil",
        "offsetof", "of", "out",
        "pointerof", "private", "protected",
        "rescue", "responds_to?", "return", "require",
        "select", "self", "sizeof", "struct", "super",
        "then", "true", "typeof", "type",
        "union", "uninitialized", "unless", "until",
        "verbatim",
        "when", "while", "with",
        "yield",
    }) |s| {
        lexer = Lexer.new(s);
        token = try lexer.nextToken();
        assert(token.type == .ident);
        assert(token.value == .keyword);
    }

    for ([_][]const u8{
        "abc", "bebe", "cafe", "draw", "elsa", "foo"
    }) |s| {
        lexer = Lexer.new(s);
        token = try lexer.nextToken();
        assert(token.type == .ident);
        assert(token.value == .string);
        assert(std.mem.eql(u8, s, token.value.string));
    }

    lexer = Lexer.new("__DIR__");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    lexer = Lexer.new("__END_LINE__");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    lexer = Lexer.new("__FILE__");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    lexer = Lexer.new("__LINE__");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    lexer = Lexer.new("_");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    lexer = Lexer.new("_foo");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.value});

    const Op = struct {
        string: []const u8, token_type: Token.Kind,
        fn init(string: []const u8, token_type: Token.Kind) @This() {
            return .{ .string = string, .token_type = token_type };
        }
    };
    const op = Op.init;

    for ([_]Op{
        op("!", .op_bang),
        op("!=", .op_bang_eq),
        op("!~", .op_bang_tilde),
        op("$?", .op_dollar_question),
        op("$~", .op_dollar_tilde),
        // op("%", .op_percent),
        // op("%=", .op_percent_eq),
        // op("%}", .op_percent_rcurly),
        op("&", .op_amp),
        op("&&", .op_amp_amp),
        op("&&=", .op_amp_amp_eq),
        op("&*", .op_amp_star),
        op("&**", .op_amp_star_star),
        op("&*=", .op_amp_star_eq),
        op("&+", .op_amp_plus),
        op("&+=", .op_amp_plus_eq),
        op("&-", .op_amp_minus),
        op("&-=", .op_amp_minus_eq),
        op("&=", .op_amp_eq),
        op("(", .op_lparen),
        op(")", .op_rparen),
        op("*", .op_star),
        op("**", .op_star_star),
        op("**=", .op_star_star_eq),
        op("*=", .op_star_eq),
        op("+", .op_plus),
        op("+=", .op_plus_eq),
        op(",", .op_comma),
        op("-", .op_minus),
        op("-=", .op_minus_eq),
        op("->", .op_minus_gt),
        op(".", .op_period),
        op("..", .op_period_period),
        op("...", .op_period_period_period),
        // op("/", .op_slash),
        // op("//", .op_slash_slash),
        // op("//=", .op_slash_slash_eq),
        // op("/=", .op_slash_eq),
        op(":", .op_colon),
        op("::", .op_colon_colon),
        op(";", .op_semicolon),
        op("<", .op_lt),
        op("<<", .op_lt_lt),
        op("<<=", .op_lt_lt_eq),
        op("<=", .op_lt_eq),
        op("<=>", .op_lt_eq_gt),
        op("=", .op_eq),
        op("==", .op_eq_eq),
        op("===", .op_eq_eq_eq),
        op("=>", .op_eq_gt),
        op("=~", .op_eq_tilde),
        op(">", .op_gt),
        op(">=", .op_gt_eq),
        op(">>", .op_gt_gt),
        op(">>=", .op_gt_gt_eq),
        op("?", .op_question),
        op("@[", .op_at_lsquare),
        op("[", .op_lsquare),
        op("[]", .op_lsquare_rsquare),
        op("[]=", .op_lsquare_rsquare_eq),
        op("[]?", .op_lsquare_rsquare_question),
        op("]", .op_rsquare),
        op("^", .op_caret),
        op("^=", .op_caret_eq),
        // op("`", .op_grave),
        op("{", .op_lcurly),
        op("{%", .op_lcurly_percent),
        op("{{", .op_lcurly_lcurly),
        op("|", .op_bar),
        op("|=", .op_bar_eq),
        op("||", .op_bar_bar),
        op("||=", .op_bar_bar_eq),
        op("}", .op_rcurly),
        op("~", .op_tilde),
    }) |o| {
        lexer = Lexer.new(o.string);
        lexer.wants_symbol = false; // for op_colon
        token = try lexer.nextToken();
        assert(token.type == o.token_type);
        // p("{}\n", .{token.type});
    }

    // consumeVariable
    lexer = Lexer.new("@foo");
    token = try lexer.nextToken();
    assert(token.type == .instance_var);
    assert(token.value == .string);
    p("{} {s}\n", .{token.type, token.value.string});

    lexer = Lexer.new("@@foo");
    token = try lexer.nextToken();
    assert(token.type == .class_var);
    assert(token.value == .string);
    p("{} {s}\n", .{token.type, token.value.string});

    lexer = Lexer.new("$foo");
    token = try lexer.nextToken();
    assert(token.type == .global);
    assert(token.value == .string);
    p("{} {s}\n", .{token.type, token.value.string});

    lexer = Lexer.new("$0");
    token = try lexer.nextToken();
    assert(token.type == .global_match_data_index);
    assert(token.value == .string);
    p("{} {s}\n", .{token.type, token.value.string});

    lexer = Lexer.new("$10?");
    token = try lexer.nextToken();
    assert(token.type == .global_match_data_index);
    assert(token.value == .string);
    p("{} {s}\n", .{token.type, token.value.string});

    // consumeCharLiteral
    lexer = Lexer.new("'a'");
    lexer.wants_raw = true;
    token = try lexer.nextToken();
    assert(token.type == .char);
    assert(token.value == .char);
    p("{} {c} {s}\n", .{token.type, token.value.char, token.raw});

    lexer = Lexer.new("''");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new("'");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new("'\\");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new("'\\Z");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});

    for ("\\'abefnrtv0") |c| {
        lexer = Lexer.new(&[4]u8{'\'', '\\', c, '\''});
        token = try lexer.nextToken();
        assert(token.type == .char);
        assert(token.value == .char);
        // p("{} {s}\n", .{token.type, escape(&[1]u8{token.value.char})});
    }

    // p("{any}\n", .{"あ"});

    // consumeCharUnicodeEscape
    for ([_][]const u8{
        "'\\u0041'", "'\\u0157'", "'\\u3042'", "'\\u{1F0DF}'"
    }) |s| {
        lexer = Lexer.new(s);
        lexer.wants_raw = true;
        token = try lexer.nextToken();
        assert(token.type == .char);
        assert(token.value == .utf8);
        var buf: [4]u8 = undefined;
        const length = try std.unicode.utf8Encode(token.value.utf8, &buf);
        const encoded = buf[0..length];
        p("{} {s} {s} {any} {}\n", .{token.type, encoded, token.raw, encoded, @as(std.meta.Tag(Token.Value), token.value)});
    }

    // consumeSymbol
    var it = std.mem.tokenize(u8,
        "+ - * ** / // == === =~ ! != !~ < <= <=> << > >= >> & &+ &- &* &** | ^ ~ % [] []= []? " ++
        "a a? a!", " ");
    while (it.peek()) |s| : (_ = it.next()) {
        const raw = try std.fmt.allocPrint(std.heap.page_allocator, ":{s}", .{s});
        lexer = Lexer.new(raw);
        lexer.wants_raw = true;
        token = try lexer.nextToken();
        assert(token.type == .symbol);
        assert(token.value == .string);
        assert(std.mem.eql(u8, s, token.value.string));
        assert(std.mem.eql(u8, raw, token.raw));
        // p("{} {s} {s}\n", .{token.type, token.value.string, token.raw});
    }

    lexer = Lexer.new(":1");
    token = try lexer.nextToken();
    assert(token.type == .op_colon);
    p("{}\n", .{token.type});

    lexer = Lexer.new(":a!=");
    token = try lexer.nextToken();
    assert(token.type == .symbol);
    assert(token.value == .string);
    assert(std.mem.eql(u8, "a", token.value.string));

    lexer = Lexer.new(":a==");
    token = try lexer.nextToken();
    assert(token.type == .symbol);
    assert(token.value == .string);
    assert(std.mem.eql(u8, "a", token.value.string));

    // consumeQuotedSymbol
    lexer = Lexer.new(":\"a\\z\\123\\\n\\xFF\\u3042\\u{1F0DF}\"");
    token = try lexer.nextToken();
    assert(token.type == .symbol);
    assert(token.value == .buffer);
    p("{} {s}\n", .{token.type, escape(token.value.buffer.items)});
    assert(std.mem.eql(u8, "azS\n\xFFあ🃟", token.value.buffer.items));

    lexer = Lexer.new("\"");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.delimiter_state.delimiters});
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .string);
    assert(token.delimiter_state.delimiters.string.nest == '"');
    assert(token.delimiter_state.delimiters.string.end == '"');

    lexer = Lexer.new("`");
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.delimiter_state.delimiters});
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .command);
    assert(token.delimiter_state.delimiters.command.nest == '`');
    assert(token.delimiter_state.delimiters.command.end == '`');

    // consumeHeredocStart
    lexer = Lexer.new("<<-EOS");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});
    lexer = Lexer.new("<<-'EOS\n");
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});

    lexer = Lexer.new("<<-EOS,");
    token = try lexer.nextToken();
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .heredoc);
    p("{} {}\n", .{token.type, token.delimiter_state.delimiters});
    assert(std.mem.eql(u8, "EOS", token.delimiter_state.delimiters.heredoc));

    lexer = Lexer.new("<<-'FOO BAR'");
    token = try lexer.nextToken();
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .heredoc);
    p("{} {}\n", .{token.type, token.delimiter_state.delimiters});
    assert(std.mem.eql(u8, "FOO BAR", token.delimiter_state.delimiters.heredoc));

    lexer = Lexer.new("//");
    lexer.wants_def_or_macro_name = true;
    lexer.slash_is_regex = true;
    lexer.wants_regex = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_slash_slash);

    lexer = Lexer.new("//=");
    lexer.wants_def_or_macro_name = false;
    lexer.slash_is_regex = false;
    lexer.wants_regex = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_slash_slash_eq);

    lexer = Lexer.new("/=");
    lexer.wants_def_or_macro_name = false;
    lexer.slash_is_regex = false;
    lexer.wants_regex = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_slash_eq);

    lexer = Lexer.new("/");
    lexer.wants_def_or_macro_name = true;
    lexer.slash_is_regex = true;
    lexer.wants_regex = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_slash);

    lexer = Lexer.new("/");
    lexer.wants_def_or_macro_name = false;
    lexer.slash_is_regex = true;
    lexer.wants_regex = false;
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.delimiter_state.delimiters});
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .regex);
    assert(token.delimiter_state.delimiters.regex.nest == '/');
    assert(token.delimiter_state.delimiters.regex.end == '/');

    lexer = Lexer.new("/\t");
    lexer.wants_def_or_macro_name = false;
    lexer.slash_is_regex = false;
    lexer.wants_regex = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_slash);

    lexer = Lexer.new("/foo");
    lexer.wants_def_or_macro_name = false;
    lexer.slash_is_regex = false;
    lexer.wants_regex = true;
    token = try lexer.nextToken();
    p("{} {}\n", .{token.type, token.delimiter_state.delimiters});
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .regex);
    assert(token.delimiter_state.delimiters.regex.nest == '/');
    assert(token.delimiter_state.delimiters.regex.end == '/');

    lexer = Lexer.new("/foo");
    lexer.wants_def_or_macro_name = false;
    lexer.slash_is_regex = false;
    lexer.wants_regex = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_slash);

    lexer = Lexer.new("%");
    lexer.wants_def_or_macro_name = true;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_percent);

    lexer = Lexer.new("%=");
    lexer.wants_def_or_macro_name = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_percent_eq);

    lexer = Lexer.new("%}");
    lexer.wants_def_or_macro_name = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_percent_rcurly);

    lexer = Lexer.new("%");
    lexer.wants_def_or_macro_name = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_percent);

    lexer = Lexer.new("%i");
    lexer.wants_def_or_macro_name = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_percent);

    lexer = Lexer.new("%i(");
    lexer.wants_def_or_macro_name = false;
    lexer.wants_raw = true;
    token = try lexer.nextToken();
    p("{} {} {s}\n", .{token.type, token.delimiter_state.delimiters, token.raw});
    assert(token.type == .symbol_array_start);
    assert(token.delimiter_state.delimiters == .symbol_array);
    assert(token.delimiter_state.delimiters.symbol_array.nest == '(');
    assert(token.delimiter_state.delimiters.symbol_array.end == ')');

    lexer = Lexer.new("%w");
    lexer.wants_def_or_macro_name = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_percent);

    lexer = Lexer.new("%w(");
    lexer.wants_def_or_macro_name = false;
    lexer.wants_raw = true;
    token = try lexer.nextToken();
    p("{} {} {s}\n", .{token.type, token.delimiter_state.delimiters, token.raw});
    assert(token.type == .string_array_start);
    assert(token.delimiter_state.delimiters == .string_array);
    assert(token.delimiter_state.delimiters.string_array.nest == '(');
    assert(token.delimiter_state.delimiters.string_array.end == ')');

    lexer = Lexer.new("%q");
    lexer.wants_def_or_macro_name = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_percent);

    lexer = Lexer.new("%q(");
    lexer.wants_def_or_macro_name = false;
    lexer.wants_raw = true;
    token = try lexer.nextToken();
    p("{} {} {s}\n", .{token.type, token.delimiter_state.delimiters, token.raw});
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .string);
    assert(token.delimiter_state.delimiters.string.nest == '(');
    assert(token.delimiter_state.delimiters.string.end == ')');
    assert(token.delimiter_state.allow_escapes == false);

    lexer = Lexer.new("%Q");
    lexer.wants_def_or_macro_name = false;
    token = try lexer.nextToken();
    p("{}\n", .{token.type});
    assert(token.type == .op_percent);

    lexer = Lexer.new("%Q(");
    lexer.wants_def_or_macro_name = false;
    lexer.wants_raw = true;
    token = try lexer.nextToken();
    p("{} {} {s}\n", .{token.type, token.delimiter_state.delimiters, token.raw});
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .string);
    assert(token.delimiter_state.delimiters.string.nest == '(');
    assert(token.delimiter_state.delimiters.string.end == ')');
    assert(token.delimiter_state.allow_escapes == true);

    lexer = Lexer.new("%r");
    lexer.wants_def_or_macro_name = false;
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});

    lexer = Lexer.new("%r(");
    lexer.wants_def_or_macro_name = false;
    lexer.wants_raw = true;
    token = try lexer.nextToken();
    p("{} {} {s}\n", .{token.type, token.delimiter_state.delimiters, token.raw});
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .regex);
    assert(token.delimiter_state.delimiters.regex.nest == '(');
    assert(token.delimiter_state.delimiters.regex.end == ')');

    lexer = Lexer.new("%x");
    lexer.wants_def_or_macro_name = false;
    if (lexer.nextToken()) |_| unreachable else |err| p("{} {?s}\n", .{err, lexer.error_message});

    lexer = Lexer.new("%x(");
    lexer.wants_def_or_macro_name = false;
    lexer.wants_raw = true;
    token = try lexer.nextToken();
    p("{} {} {s}\n", .{token.type, token.delimiter_state.delimiters, token.raw});
    assert(token.type == .delimiter_start);
    assert(token.delimiter_state.delimiters == .command);
    assert(token.delimiter_state.delimiters.command.nest == '(');
    assert(token.delimiter_state.delimiters.command.end == ')');

    // nextTokenNeverASymbol
    lexer = Lexer.new(":foo");
    token = try lexer.nextToken();
    assert(token.type == .symbol);
    assert(token.value == .string);
    assert(std.mem.eql(u8, "foo", token.value.string));

    lexer = Lexer.new(":foo");
    token = try lexer.nextTokenNeverASymbol();
    assert(token.type == .op_colon);

    // nextStringArrayToken
    lexer = Lexer.new("%w( foo  bar )");
    lexer.wants_raw = true;
    token = try lexer.nextToken();
    assert(token.type == .string_array_start);
    assert(std.mem.eql(u8, token.raw, "%w("));
    token = try lexer.nextStringArrayToken();
    assert(token.type == .string);
    assert(std.mem.eql(u8, token.raw, "foo"));
    assert(token.value == .buffer);
    assert(std.mem.eql(u8, token.value.buffer.items, "foo"));
    token = try lexer.nextStringArrayToken();
    assert(token.type == .string);
    assert(std.mem.eql(u8, token.raw, "bar"));
    assert(token.value == .buffer);
    assert(std.mem.eql(u8, token.value.buffer.items, "bar"));
    token = try lexer.nextStringArrayToken();
    assert(token.type == .string_array_end);
    assert(std.mem.eql(u8, token.raw, ")"));
    token = try lexer.nextStringArrayToken();
    assert(token.type == .eof);

    // checkHeredocStart
    lexer = Lexer.new("foo");
    assert(lexer.checkHeredocStart() == null);
    lexer = Lexer.new("<<-foo");
    assert(lexer.checkHeredocStart() == null);
    lexer = Lexer.new("<<-foo\r");
    assert(lexer.checkHeredocStart() == null);
    lexer = Lexer.new("<<-foo\n");
    var delimiter_state = lexer.checkHeredocStart().?;
    assert(std.mem.eql(u8, "foo", delimiter_state.delimiters.heredoc));
    lexer = Lexer.new("<<-'foo bar'");
    delimiter_state = lexer.checkHeredocStart().?;
    assert(std.mem.eql(u8, "foo bar", delimiter_state.delimiters.heredoc));

    // checkMacroOpeningKeyword
    lexer = Lexer.new("abstract def");
    var is_abstract_def = false;
    assert(lexer.checkMacroOpeningKeyword(true, &is_abstract_def));
    assert(is_abstract_def);

    for ([_][]const u8{
        "abstract class", "abstract struct", "annotation",
        "begin",
        "case", "class",
        "do", "def",
        "fun",
        "if",
        "lib",
        "macro", "module",
        "select", "struct",
        "union", "unless", "until",
        "while",
    }) |s| {
        lexer = Lexer.new(s);
        is_abstract_def = false;
        assert(lexer.checkMacroOpeningKeyword(true, &is_abstract_def));
        assert(is_abstract_def == false);
    }

    // raiseUnterminatedQuoted
    lexer = Lexer.new("foo");
    if (lexer.raiseUnterminatedQuoted(.{ .delimiters = .{ .heredoc = "bar" } })) |_| unreachable else |err| {
        assert(err == error.SyntaxError);
        assert(std.mem.eql(u8, lexer.error_message.?, blk: {
            break :blk "Unterminated heredoc: can't find " ++
                "\"bar\" anywhere before the end of file";
        }));
    }

    const delimiter_kinds = [_]Token.DelimiterKind{
        .command,
        .regex,
        .string
    };
    var messages = [_][]const u8{
        "Unterminated command literal",
        "Unterminated regular expression",
        "Unterminated string literal",
    };
    assert(delimiter_kinds.len == messages.len);
    inline for (delimiter_kinds) |kind, i| {
        lexer = Lexer.new("foo");
        if (lexer.raiseUnterminatedQuoted(.{ .delimiters = Token.Delimiters.of(kind, '`', '"') })) |_| unreachable else |err| {
            assert(err == error.SyntaxError);
            assert(std.mem.eql(u8, messages[i], lexer.error_message.?));
        }
    }
}
