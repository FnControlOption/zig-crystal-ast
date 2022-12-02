const Parser = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Utf8EncodeError = error{
    CodepointTooLarge,
    Utf8CannotEncodeSurrogateHalf,
};

const Lexer = @import("lexer.zig");
const Location = @import("location.zig");
const Token = @import("token.zig");
const Keyword = Token.Keyword;

const ast = @import("ast.zig");
const Node = ast.Node;
const Visibiity = ast.Visibility;

const Alias = ast.Alias;
const And = ast.And;
const Annotation = ast.Annotation;
const AnnotationDef = ast.AnnotationDef;
const Arg = ast.Arg;
const ArrayLiteral = ast.ArrayLiteral;
const Asm = ast.Asm;
const AsmOperand = ast.AsmOperand;
const Assign = ast.Assign;
const Block = ast.Block;
const BoolLiteral = ast.BoolLiteral;
const Break = ast.Break;
const CStructOrUnionDef = ast.CStructOrUnionDef;
const Call = ast.Call;
const Case = ast.Case;
const Cast = ast.Cast;
const CharLiteral = ast.CharLiteral;
const ClassDef = ast.ClassDef;
const ClassVar = ast.ClassVar;
const Def = ast.Def;
const DoubleSplat = ast.DoubleSplat;
const EnumDef = ast.EnumDef;
const ExceptionHandler = ast.ExceptionHandler;
const Expressions = ast.Expressions;
const Extend = ast.Extend;
const ExternalVar = ast.ExternalVar;
const FunDef = ast.FunDef;
const Generic = ast.Generic;
const Global = ast.Global;
const HashLiteral = ast.HashLiteral;
const If = ast.If;
const ImplicitObj = ast.ImplicitObj;
const Include = ast.Include;
const InstanceSizeOf = ast.InstanceSizeOf;
const InstanceVar = ast.InstanceVar;
const IsA = ast.IsA;
const LibDef = ast.LibDef;
const Macro = ast.Macro;
const MacroExpression = ast.MacroExpression;
const MacroFor = ast.MacroFor;
const MacroIf = ast.MacroIf;
const MacroLiteral = ast.MacroLiteral;
const MacroVar = ast.MacroVar;
const MacroVerbatim = ast.MacroVerbatim;
const MagicConstant = ast.MagicConstant;
const Metaclass = ast.Metaclass;
const ModuleDef = ast.ModuleDef;
const MultiAssign = ast.MultiAssign;
const NamedArgument = ast.NamedArgument;
const NamedTupleLiteral = ast.NamedTupleLiteral;
const Next = ast.Next;
const Nil = ast.Nil;
const NilableCast = ast.NilableCast;
const Nop = ast.Nop;
const Not = ast.Not;
const NumberLiteral = ast.NumberLiteral;
const OffsetOf = ast.OffsetOf;
const OpAssign = ast.OpAssign;
const Or = ast.Or;
const Out = ast.Out;
const Path = ast.Path;
const PointerOf = ast.PointerOf;
const ProcLiteral = ast.ProcLiteral;
const ProcNotation = ast.ProcNotation;
const ProcPointer = ast.ProcPointer;
const RangeLiteral = ast.RangeLiteral;
const ReadInstanceVar = ast.ReadInstanceVar;
const RegexLiteral = ast.RegexLiteral;
const Require = ast.Require;
const Rescue = ast.Rescue;
const RespondsTo = ast.RespondsTo;
const Return = ast.Return;
const Select = ast.Select;
const Self = ast.Self;
const SizeOf = ast.SizeOf;
const Splat = ast.Splat;
const StringInterpolation = ast.StringInterpolation;
const StringLiteral = ast.StringLiteral;
const SymbolLiteral = ast.SymbolLiteral;
const TupleLiteral = ast.TupleLiteral;
const TypeDeclaration = ast.TypeDeclaration;
const TypeDef = ast.TypeDef;
const TypeOf = ast.TypeOf;
const Underscore = ast.Underscore;
const UninitializedVar = ast.UninitializedVar;
const Union = ast.Union;
const Unless = ast.Unless;
const Until = ast.Until;
const Var = ast.Var;
const VisibilityModifier = ast.VisibilityModifier;
const When = ast.When;
const While = ast.While;
const Yield = ast.Yield;

const Unclosed = struct {
    name: []const u8,
    location: Location,

    fn init(name: []const u8, location: Location) Unclosed {
        return .{ .name = name, .location = location };
    }
};

lexer: Lexer,
visibility: ?Visibiity = null,
block_arg_name: ?[]const u8 = null,

var_scopes: ArrayList(StringHashMap(void)),
unclosed_stack: ArrayList(Unclosed),
calls_super: bool = false,
calls_initialize: bool = false,
calls_previous_def: bool = false,
uses_block_arg: bool = false,
is_macro_def: bool = false,
assigns_special_var: bool = false,
def_nest: usize = 0,
fun_nest: usize = 0,
type_nest: usize = 0,

call_args_start_locations: ArrayList(Location),
temp_arg_count: usize = 0,
in_macro_expression: bool = false,
stop_on_yield: usize = 0,
inside_c_struct: bool = false,
wants_doc: bool = false,
doc_enabeld: bool = false,
no_type_declaration: usize = 0,
inside_interpolation: bool = false,

stop_on_do: bool = false,
assigned_vars: ArrayList([]const u8),

fn new(string: []const u8) !Parser {
    const allocator = std.heap.page_allocator; // TODO: use ArenaAllocator
    return init(allocator, string);
}

pub fn init(allocator: Allocator, string: []const u8) !Parser {
    var parser = Parser{
        .lexer = Lexer.init(allocator, string),
        .var_scopes = ArrayList(StringHashMap(void)).init(allocator),
        .unclosed_stack = ArrayList(Unclosed).init(allocator),
        .call_args_start_locations = ArrayList(Location).init(allocator),
        .assigned_vars = ArrayList([]const u8).init(allocator),
    };
    try parser.createIsolatedVarScope();
    return parser;
}

pub fn parse(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    try lexer.skipTokenAndStatementEnd();

    const node = try parser.parseExpressions();
    try parser.check(.eof);
    return node;
}

// parse(mode: ParseMode)

pub fn parseExpressions(parser: *Parser) !Node {
    const old_stop_on_do = parser.replaceStopOnDo(false);
    defer _ = parser.replaceStopOnDo(old_stop_on_do);
    return parser.parseExpressionsInternal();
}

pub fn parseExpressionsInternal(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    if (parser.isEndToken()) {
        return Nop.node(allocator);
    }

    const exp = try parser.parseMultiAssign();

    lexer.slash_is_regex = true;
    try lexer.skipStatementEnd();

    if (parser.isEndToken()) {
        return exp;
    }

    var exps = ArrayList(Node).init(allocator);
    try exps.append(exp);

    while (true) {
        try exps.append(try parser.parseMultiAssign());
        try lexer.skipStatementEnd();
        if (parser.isEndToken()) break;
    }

    return Expressions.from(allocator, exps);
}

pub fn parseMultiAssign(parser: *Parser) !Node {
    // TODO: implement
    return parser.parseExpression();
}

// pub fn parseMultiAssign(parser: *Parser) !Node {
//     const lexer = &parser.lexer;
//     const allocator = lexer.allocator;
//     const location = lexer.token.location();
//
//     var lhs_splat_index: ?usize = null;
//     if (lexer.token.type == .op_star) {
//         lhs_splat_index = 0;
//         try lexer.skipTokenAndSpace();
//     }
//
//     var last = try parser.parseExpression();
//     try lexer.skipSpace();
//
//     const last_is_target = isMultiAssignTarget(last);
//
//     switch (lexer.token.type) {
//         .op_comma => {
//             _ = last_is_target;
//             return error.Unimplemented;
//         },
//         .newline, .op_semicolon => {
//             return error.Unimplemented;
//         },
//         else => {
//             return error.Unimplemented;
//         },
//     }
//
//     var exps = ArrayList(Node).init(allocator);
//     try exps.append(last);
//
//     var i: usize = 0;
//     var possible_assign_index: ?usize = null;
//
//     while (lexer.token.type == .op_comma) {
//         return error.Unimplemented;
//     }
//
//     if (possible_assign_index == null and isMultiAssignMiddle(last)) {
//         possible_assign_index = i;
//     }
//
//     if (possible_assign_index) |assign_index| {
//         var targets = ArrayList(Node).initCapacity(allocator, assign_index);
//         var target_index: usize = 0;
//         while (target_index < assign_index) : (target_index += 1) {
//             const exp = exps.items[target_index];
//             targets.append(parser.multiassignLeftHand(exp));
//         }
//
//         var assign_exp = exps.items[assign_index];
//         var values = ArrayList(Node).init(allocator);
//
//         switch (assign_exp) {
//             .assign => |assign| {
//                 targets.append(parser.multiassignLeftHand(assign.target));
//                 values.append(assign.value);
//             },
//             .call => |*call| {
//                 call.name = call.name[0 .. call.name.len - 1];
//                 targets.append(assign_exp);
//                 values.append(call.args.pop());
//             },
//             else => {
//                 return lexer.raise("BUG: multiassign index expression can only be Assign or Call");
//             },
//         }
//
//         values.appendSlice(exps.items[assign_index + 1 ..]);
//         if (values.items.len != 1) {
//             if (lhs_splat_index) {
//                 if (targets.items.len - 1 > values.items.len) {
//                     return lexer.raiseLoc("Multiple assignment count mismatch", location);
//                 }
//             } else {
//                 if (targets.items.len != values.items.len) {
//                     return lexer.raiseLoc("Multiple assignment count mismatch", location);
//                 }
//             }
//         }
//
//         const multi = try MultiAssign.node(allocator, targets, values);
//         multi.setLocation(location);
//         return error.Unimplemented;
//     } else {
//         return parser.unexpectedToken();
//     }
// }

// pub fn isMultiAssignTarget(exp: Node) bool {
//     switch (exp) {
//         .underscore, .@"var", .instance_var, .class_var, .global, .assign => {
//             return true;
//         },
//         .call => |call| {
//             if (call.has_parentheses) {
//                 return false;
//             }
//             return (call.args.items.len == 0 and call.named_args == null) or
//                 Lexer.isSetter(call.name) or
//                 std.mem.eql(u8, "[]", call.name) or
//                 std.mem.eql(u8, "[]=", call.name);
//         },
//         else => {
//             return false;
//         },
//     }
// }

// pub fn isMultiAssignMiddle(exp: Node) bool {
//     return switch (exp) {
//         .assign => true,
//         .call => |call| call.name[call.name.len - 1] == '=',
//         else => false,
//     };
// }

// pub fn multiassignLeftHand(parser: *Parser, exp: Node) Node {
//     const lexer = &parser.lexer;
//     const allocator = lexer.allocator;
//     var lhs = exp;
//     switch (exp) {
//         .path => |path| {
//             return lexer.raiseLoc("can't assign to constant in multiple assignment", path.location.?);
//         },
//         .call => |call| {
//             if (call.obj == null and call.args.items.len == 0) {
//                 var v = try Var.node(allocator, call.anem);
//                 v.copyLocation(lhs);
//                 lhs = v;
//             }
//         },
//     }
//     switch (lhs) {
//         .@"var" => |v| {
//             if (std.mem.eql(u8, v.name, "self")) {
//                 return lexer.raiseLoc("can't change the value of self", v.location.?);
//             }
//             parser.pushVar(v);
//         },
//     }
//     return lhs;
// }

pub fn parseExpression(parser: *Parser) !Node {
    // TODO: implement
    return parser.parseOpAssign();
}

// parseExpressionSuffix
// parseExpressionSuffix
// parseOpAssignNoControl

pub fn parseOpAssign(parser: *Parser) !Node {
    // TODO: implement
    return parser.parseQuestionColon();
}

pub fn parseQuestionColon(parser: *Parser) !Node {
    // TODO: implement
    return parser.parseRange();
}

pub fn parseRange(parser: *Parser) !Node {
    // TODO: implement
    return parser.parseOr();
}

// newRange

inline fn parseOperator(
    parser: *Parser,
    comptime options: struct {
        node: type,
        operators: []const Token.Kind,
        next_operator: []const u8,
        right_associative: ?[]const u8 = null,
    },
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const location = lexer.token.location();

    const parseLeft = @field(Parser, options.next_operator);
    const parseRight = @field(
        Parser,
        options.right_associative orelse options.next_operator,
    );

    var left = try parseLeft(parser);
    next_exp: while (true) {
        if (lexer.token.type == .space) {
            try lexer.skipToken();
            continue;
        }

        inline for (options.operators) |op| {
            if (lexer.token.type == op) {
                try parser.checkVoidValue(left, location);

                const method = op.toString();
                const name_location = lexer.token.location();

                lexer.slash_is_regex = true;
                try lexer.skipTokenAndSpaceOrNewline();
                const right = try parseRight(parser);
                if (options.node == Call) {
                    var args = ArrayList(Node).init(allocator);
                    try args.append(right);
                    left = try Call.node(allocator, left, method, args, .{
                        .name_location = name_location,
                    });
                } else {
                    left = try options.node.node(allocator, left, right);
                }
                left.setLocation(location);
                left.copyEndLocation(right);
                continue :next_exp;
            }
        }

        return left;
    }
}

pub fn parseOr(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parseAnd",
        .node = Or,
        .operators = &.{.op_bar_bar},
    });
}

pub fn parseAnd(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parseEquality",
        .node = And,
        .operators = &.{.op_amp_amp},
    });
}

pub fn parseEquality(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parseCmp",
        .node = Call,
        .operators = &.{ .op_lt, .op_lt_eq, .op_gt, .op_gt_eq, .op_lt_eq_gt },
    });
}

pub fn parseCmp(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parseLogicalOr",
        .node = Call,
        .operators = &.{ .op_eq_eq, .op_bang_eq, .op_eq_tilde, .op_bang_tilde, .op_eq_eq_eq },
    });
}

pub fn parseLogicalOr(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parseLogicalAnd",
        .node = Call,
        .operators = &.{ .op_bar, .op_caret },
    });
}

pub fn parseLogicalAnd(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parseShift",
        .node = Call,
        .operators = &.{.op_amp},
    });
}

pub fn parseShift(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parseAddOrSub",
        .node = Call,
        .operators = &.{ .op_lt_lt, .op_gt_gt },
    });
}

pub fn parseAddOrSub(parser: *Parser) !Node {
    // TODO: implement
    return parser.parseMulOrDiv();
}

pub fn parseMulOrDiv(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parsePow",
        .node = Call,
        .operators = &.{ .op_star, .op_slash, .op_slash_slash, .op_percent, .op_amp_star },
    });
}

pub fn parsePow(parser: *Parser) !Node {
    return parser.parseOperator(.{
        .next_operator = "parsePrefix",
        .node = Call,
        .operators = &.{ .op_star_star, .op_amp_star_star },
        .right_associative = "parsePow",
    });
}

pub fn parsePrefix(parser: *Parser) !Node {
    // TODO: implement
    return parser.parseAtomicWithMethod();
}

// AtomicWithMethodCheck

pub fn parseAtomicWithMethod(parser: *Parser) !Node {
    // TODO: implement
    return parser.parseAtomic();
}

// parseAtomicMethodSuffix
// parseAtomicMethodSuffixSpecial
// parseSingleArg
// parseIsA
// parseAs
// parseAsQuestion
// parseRespondsTo

pub fn parseRespondsToName(parser: *Parser) ![]const u8 {
    const lexer = &parser.lexer;

    if (lexer.token.type != .symbol) {
        return parser.unexpectedTokenMsg("expected symbol");
    }

    return lexer.token.value.string;
}

// parseNilQuestion
// parseNegationSuffix

pub fn parseAtomic(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const location = lexer.token.location();
    const atomic = try parser.parseAtomicWithoutLocation();
    if (atomic.location() == null)
        atomic.setLocation(location);
    return atomic;
}

pub fn parseAtomicWithoutLocation(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;
    switch (lexer.token.type) {
        // TODO
        .op_lsquare_rsquare => {
            return parser.parseEmptyArrayLiteral();
        },
        // TODO
        .char => {
            const value = lexer.token.value.char;
            const node = try CharLiteral.node(allocator, value);
            try parser.skipNodeToken(node);
            return node;
        },
        // TODO
        .string_array_start => {
            return parser.parseStringArray();
        },
        .symbol_array_start => {
            return parser.parseSymbolArray();
        },
        .symbol => {
            const value = lexer.token.value.string;
            const node = try SymbolLiteral.node(allocator, value);
            try parser.skipNodeToken(node);
            return node;
        },
        .global => {
            return lexer.raise("$global_variables are not supported, use @@class_variables instead");
        },
        .op_dollar_tilde, .op_dollar_question => |token_type| {
            const location = lexer.token.location();
            // TODO: backport to Crystal
            const name = token_type.toString();

            if (blk: {
                lexer.startPeekAhead();
                defer lexer.endPeekAhead();
                try lexer.skipTokenAndSpace();
                break :blk lexer.token.type == .op_eq;
            }) {
                const v = try Var.node(allocator, name);
                v.setLocation(location);
                try parser.pushVar(v);
                try parser.skipNodeToken(v);
                return v;
            } else {
                const node = try Global.node(allocator, name);
                node.setLocation(location);
                return node;
            }
        },
        .global_match_data_index => {
            if (blk: {
                lexer.startPeekAhead();
                defer lexer.endPeekAhead();
                try lexer.skipTokenAndSpace();
                break :blk lexer.token.type == .op_eq;
            }) {
                return lexer.raise("global match data cannot be assigned to");
            }

            var method: []const u8 = "[]";
            var value = lexer.token.value.string;
            if (value[value.len - 1] == '?') {
                method = "[]?";
                value = value[0 .. value.len - 1];
            }
            const location = lexer.token.location();
            // TODO: slightly inefficient
            const index = std.fmt.parseInt(i32, value, 10) catch |err| {
                switch (err) {
                    error.Overflow => {
                        return lexer.raise(try std.fmt.allocPrint(
                            allocator,
                            "Index ${s} doesn't fit in an Int32",
                            .{value},
                        ));
                    },
                    error.InvalidCharacter => return err,
                }
            };
            const receiver = try Global.node(allocator, "$~");
            receiver.setLocation(location);
            var args = ArrayList(Node).init(allocator);
            try args.append(try NumberLiteral.fromNumber(allocator, index));
            const node = try Call.node(allocator, receiver, method, args, .{});
            try parser.skipNodeToken(node);
            return node;
        },
        .magic_line => {
            const node = try MagicConstant.expandLineNode(allocator, lexer.token.location());
            try parser.skipNodeToken(node);
            return node;
        },
        .magic_end_line => {
            return lexer.raiseFor("__END_LINE__ can only be used in default parameter value", lexer.token);
        },
        // TODO
        .underscore => {
            const node = try Underscore.node(allocator);
            try parser.skipNodeToken(node);
            return node;
        },
        else => {
            return parser.unexpectedTokenInAtomic();
        },
    }
}

// checkTypeDeclaration
// parseTypeDeclaration

pub fn nextComesColonSpace(parser: *Parser) bool {
    if (parser.no_type_declaration == 0) return false;

    const lexer = &parser.lexer;

    const pos = lexer.current_pos;
    defer lexer.current_pos = pos;

    while (std.ascii.isWhitespace(lexer.currentChar())) {
        lexer.skipCharNoColumnIncrement();
    }
    if (lexer.currentChar() == ':') {
        lexer.skipCharNoColumnIncrement();
        return std.ascii.isWhitespace(lexer.currentChar());
    }
    return false;
}

// newNodeCheckTypeDeclaration
// newNodeCheckTypeDeclaration
// parseGenericOrCustomLiteral
// parseCustomLiteral
// checkNotInsideDef
// isInsideDef
// isInsideFun
// parseAnnotation
// parseBegin
// parseExceptionHandler
// SemicolonOrNewLine
// ConstOrDoubleColon
// parseRescue
// parseRescueTypes
// parseWhile
// parseUntil
// parseWhileOrUntil
// doesCallBlockArgFollow
// parseCallBlockArg
// parseCallBlockArgAfterDot
// parseClassDef
// isStatementEnd
// parseTypeVars
// parseModuleDef
// parseAnnotationDef
// parseParenthesizedExpression
// parseFunLiteral
// checkNotPipeBeforeProcLiteralBody
// parseFunLiteralParam
// parseFunPointer
// parseDelimiter
// fn combineInterpolationPieces
// fn combinePieces
// consumeDelimiter
// consumeRegexOptions
// consumeHeredocs
// consumeHeredoc
// needsHeredocIndentRemoved
// removeHeredocIndent
// fn addHeredocPiece
// fn addHeredocPiece
// removeHeredocFromLine
// parseStringWithoutInterpolation

pub fn parseStringArray(parser: *Parser) !Node {
    return parser.parseStringOrSymbolArray(StringLiteral, "String");
}

pub fn parseSymbolArray(parser: *Parser) !Node {
    return parser.parseStringOrSymbolArray(SymbolLiteral, "Symbol");
}

pub fn parseStringOrSymbolArray(
    parser: *Parser,
    comptime StringOrSymbolLiteral: type,
    elements_type: []const u8,
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    var strings = ArrayList(Node).init(allocator);

    while (true) {
        _ = try lexer.nextStringArrayToken();
        switch (lexer.token.type) {
            .string => {
                const value = lexer.token.value.string;
                const string = try StringOrSymbolLiteral.node(allocator, value);
                try strings.append(string);
            },
            .string_array_end => {
                try lexer.skipToken();
                break;
            },
            else => {
                var buffer = ArrayList(u8).init(allocator);
                try buffer.appendSlice("Unterminated ");
                for (elements_type) |c|
                    try buffer.append(std.ascii.toLower(c));
                try buffer.appendSlice(" array literal");
                return lexer.raise(buffer.items);
            },
        }
    }

    const of = try Path.global(allocator, elements_type);
    return ArrayLiteral.node(allocator, strings, .{ .of = of });
}

pub fn parseEmptyArrayLiteral(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const line = lexer.line_number;
    const column = lexer.token.column_number;

    try lexer.skipTokenAndSpace();
    if (lexer.token.isKeyword(.of)) {
        try lexer.skipTokenAndSpaceOrNewline();
        const of = try parser.parseBareProcType();
        const elements = ArrayList(Node).init(allocator);
        const node = try ArrayLiteral.node(allocator, elements, .{ .of = of });
        node.copyEndLocation(of);
        return node;
    } else {
        return lexer.raiseAt("for empty arrays use '[] of ElementType'", line, column);
    }
}

// parseArrayLiteral
// parseHashOrTupleLiteral
// parseHashLiteral

pub fn atNamedTupleStart(parser: *const Parser) bool {
    const lexer = &parser.lexer;
    return switch (lexer.token.type) {
        .ident, .@"const" => lexer.currentChar() == ':' and lexer.peekNextChar() != ':',
        else => false,
    };
}

// atStringLiteralStart
// parseTuple
// newHashLiteral
// parseNamedTuple
// parseNamedTuple
// parseRequire
// parseCase
// checkValidExhaustiveExpressionk
// addWhenExp
// isWhenExpConstant
// whenExpressionEnd
// parseWhenExpression
// parseSelect
// isValidSelectWhen
// parseInclude
// parseExtend
// parseIncludeOrExtend
// parseToDef
// parseDef
// prepareParseDef
// parseMacro
// parseMacroBody
// fn newMacroExpressions
// parseMacroVarExps
// checkMacroSkipWhitespace
// parsePercentMacroExpression
// parseMacroExpression
// checkMacroExpressionEnd
// parsePercentMacroControl
// parseMacroControl
// parseMacroIf
// parseExpressionInsideMacro
// parseDefHelper
// checkValidDefName
// checkValidDefOpName
// parseDefFreeVars
// computeBlockArgYields
// parseParam
// parseBlockParam
// parseParamName
// isInvalidInternalName
// parseIf
// parseIfAfterCondition
// parseUnless
// parseUnlessAfterCondition
// setVisibility
// parseVarOrCall
// nextComesPlusOrMinus

pub fn replaceStopOnDo(parser: *Parser, new_value: bool) bool {
    const old_stop_on_do = parser.stop_on_do;
    parser.stop_on_do = new_value;
    return old_stop_on_do;
}

// parseBlock
// parseCurlyBlock
// parseBlock2
// parseCallArgs
// parseCallArgsSpaceConsumed
// parseCallArgsNamedArgs
// parseNamedArgs
// parseCallArg
// parseOut
// parseGenericOrGlobalCall

pub fn parseBareProcType(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const first_type = try parser.parseTypeSplatUnionType();

    if (blk: {
        const has_another_type =
            lexer.token.type == .op_comma and
            try parser.atTypeStart(.{ .consume_newlines = true });
        break :blk lexer.token.type != .op_minus_gt and
            !has_another_type;
    }) {
        if (first_type == .splat) {
            return lexer.raiseLoc("invalid type splat", first_type.location().?);
        }
        return first_type;
    }

    var input_types = ArrayList(Node).init(allocator);
    try input_types.append(first_type);

    if (lexer.token.type != .op_minus_gt) {
        while (true) {
            try lexer.skipTokenAndSpaceOrNewline();
            try input_types.append(try parser.parseTypeSplatUnionType());
            const has_another_type =
                lexer.token.type == .op_comma and
                try parser.atTypeStart(.{ .consume_newlines = true });
            if (!has_another_type) break;
        }
    }

    return parser.parseProcTypeOutput(input_types, first_type.location());
}

pub fn parseUnionType(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const first_type = try parser.parseAtomicTypeWithSuffix();
    if (lexer.token.type != .op_bar) return first_type;

    var types = ArrayList(Node).init(allocator);
    try types.append(first_type);

    var last_type: Node = undefined;
    while (true) {
        try lexer.skipTokenAndSpaceOrNewline();
        last_type = try parser.parseAtomicTypeWithSuffix();
        try types.append(last_type);
        if (lexer.token.type != .op_bar) break;
    }

    const u = try Union.node(allocator, types);
    u.copyLocation(first_type);
    u.copyEndLocation(last_type);
    return u;
}

pub fn parseAtomicTypeWithSuffix(parser: *Parser) !Node {
    var t = try parser.parseAtomicType();
    try parser.parseTypeSuffix(&t);
    return t;
}

pub fn parseAtomicType(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;
    const location = lexer.token.location();

    switch (lexer.token.type) {
        .ident => {
            if (lexer.token.value.isKeyword(.self)) {
                try lexer.skipTokenAndSpace();
                const node = try Self.node(allocator);
                node.setLocation(location);
                return node;
            }
            if (lexer.token.value.isString("self?")) {
                try lexer.skipTokenAndSpace();
                const node = try Self.node(allocator);
                node.setLocation(location);
                return parser.makeNilableType(node);
            }
            if (lexer.token.value.isKeyword(.typeof)) {
                // TODO
            }
            return parser.unexpectedToken();
        },
        .underscore => {
            try lexer.skipTokenAndSpace();
            const node = try Underscore.node(allocator);
            node.setLocation(location);
            return node;
        },
        // TODO
        else => {
            return parser.unexpectedToken();
        },
    }
}

// parseUnionTypes
// parseGeneric
// parseGeneric

pub fn parsePath(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const location = lexer.token.location();

    var is_global = false;
    if (lexer.token.type == .op_colon_colon) {
        try lexer.skipTokenAndSpaceOrNewline();
        is_global = true;
    }

    _ = location; // TODO
    const path = try parser.parsePath2(is_global, lexer.token.location());
    try lexer.skipSpace();
    return path;
}

pub fn parsePath2(
    parser: *Parser,
    is_global: bool,
    location: Location,
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    var names = ArrayList([]const u8).init(allocator);
    try names.append(try parser.checkConst());

    var end_location = lexer.tokenEndLocation();

    lexer.wants_regex = false;
    try lexer.skipToken();
    while (lexer.token.type == .op_colon_colon) {
        try lexer.skipTokenAndSpaceOrNewline();
        try names.append(try parser.checkConst());
        end_location = lexer.tokenEndLocation();

        lexer.wants_regex = false;
        try lexer.skipToken();
    }

    const node = try Path.node(allocator, names, is_global);
    node.setLocation(location);
    node.setEndLocation(end_location);
    return node;
}

// parseTypeArgs
// parseNamedTypeArgs

pub fn consumeTypeSplatStar(parser: *Parser) !bool {
    const lexer = &parser.lexer;
    if (lexer.token.type == .op_star) {
        try lexer.skipTokenAndSpaceOrNewline();
        return true;
    } else {
        return false;
    }
}

pub fn parseTypeSplatUnionType(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const location = lexer.token.location();
    const splat = try parser.consumeTypeSplatStar();

    const t = try parser.parseUnionType();
    if (splat) {
        const node = try Splat.node(allocator, t);
        node.setLocation(location);
        return node;
    }
    return t;
}

const ParseTypeArgError = error{SyntaxError} || Allocator.Error || Utf8EncodeError;
pub fn parseTypeArg(parser: *Parser) ParseTypeArgError!Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    if (lexer.token.type == .number) {
        const value = lexer.token.value.string;
        const kind = lexer.token.number_kind;
        const num = try NumberLiteral.node(allocator, value, kind);
        num.setLocation(lexer.token.location());
        try lexer.skipTokenAndSpace();
        return num;
    }

    // TODO
    // switch (lexer.token.value) {
    //     .keyword => |keyword| {
    //         switch (keyword) {
    //             else => {},
    //         }
    //     },
    //     else => {},
    // }
    return parser.parseUnionType();
}

pub fn parseTypeSuffix(parser: *Parser, t: *Node) !void {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;
    while (true) {
        switch (lexer.token.type) {
            .op_period => {
                try lexer.skipTokenAndSpaceOrNewline();
                try parser.checkIdentKeyword(.class);
                try lexer.skipTokenAndSpace();
                const node = try Metaclass.node(allocator, t.*);
                node.copyLocation(t.*);
                t.* = node;
            },
            .op_question => {
                try lexer.skipTokenAndSpace();
                t.* = try parser.makeNilableType(t.*);
            },
            .op_star => {
                try lexer.skipTokenAndSpace();
                t.* = try parser.makePointerType(t.*);
            },
            .op_star_star => {
                try lexer.skipTokenAndSpace();
                t.* = try parser.makePointerType(t.*);
                t.* = try parser.makePointerType(t.*);
            },
            .op_lsquare => {
                try lexer.skipTokenAndSpaceOrNewline();
                const size = try parser.parseTypeArg();
                try lexer.skipSpaceOrNewline();
                try parser.check(.op_rsquare);
                try lexer.skipTokenAndSpace();
                t.* = try parser.makeStaticArrayType(t.*, size);
            },
            else => {
                return;
            },
        }
    }
}

pub fn parseProcTypeOutput(
    parser: *Parser,
    input_types: ArrayList(Node),
    location: ?Location,
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const has_output_type = try parser.atTypeStart(.{ .consume_newlines = false });

    try parser.check(.op_minus_gt);
    try lexer.skipTokenAndSpace();

    var output_type: ?Node = null;
    if (has_output_type) {
        try lexer.skipSpaceOrNewline();
        output_type = try parser.parseUnionType();
    }

    const node = try ProcNotation.node(allocator, input_types, output_type);
    node.setLocation(location);
    return node;
}

pub fn makeNilableType(parser: *const Parser, t: Node) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const n = try Path.global(allocator, "Nil");
    n.copyLocation(t);

    var types = ArrayList(Node).init(allocator);
    try types.append(t);
    try types.append(n);

    const u = try Union.node(allocator, types);
    u.copyLocation(t);
    return u;
}

// makeNilableExpression

pub fn makePointerType(parser: *const Parser, t: Node) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const pointer = try Path.global(allocator, "Pointer");
    pointer.copyLocation(t);

    var type_vars = ArrayList(Node).init(allocator);
    try type_vars.append(t);

    const generic = try Generic.node(allocator, pointer, type_vars, .{
        .suffix = .asterisk,
    });
    generic.copyLocation(t);
    return generic;
}

pub fn makeStaticArrayType(parser: *const Parser, t: Node, size: Node) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const static_array = try Path.global(allocator, "StaticArray");
    static_array.copyLocation(t);

    var type_vars = ArrayList(Node).init(allocator);
    try type_vars.append(t);
    try type_vars.append(size);

    const generic = try Generic.node(allocator, static_array, type_vars, .{
        .suffix = .bracket,
    });
    generic.copyLocation(t);
    return generic;
}

// makeTupleType
// makeNamedTupleType

pub fn atTypeStart(
    parser: *Parser,
    options: struct { consume_newlines: bool },
) !bool {
    const lexer = &parser.lexer;
    lexer.startPeekAhead();
    defer lexer.endPeekAhead();

    if (options.consume_newlines) {
        try lexer.skipTokenAndSpaceOrNewline();
    } else {
        try lexer.skipTokenAndSpace();
    }

    return parser.findTypeStart();
}

fn findTypeStart(parser: *Parser) !bool {
    const lexer = &parser.lexer;

    while (lexer.token.type == .op_lparen or
        lexer.token.type == .op_lcurly)
    {
        try lexer.skipTokenAndSpaceOrNewline();
    }

    // TODO: the below conditions are not complete, and there are many false-positive or true-negative examples.

    switch (lexer.token.type) {
        .ident => {
            if (parser.atNamedTupleStart()) {
                return false;
            }
            if (lexer.token.value.isKeyword(.typeof)) {
                return true;
            }
            if (lexer.token.value.isKeyword(.self) or
                lexer.token.value.isString("self?"))
            {
                try lexer.skipTokenAndSpace();
                return parser.findDelimiterOrTypeSuffix();
            }
            return false;
        },
        .@"const" => {
            if (parser.atNamedTupleStart()) {
                return false;
            }
            return parser.findTypePathStart();
        },
        .op_colon_colon => {
            try lexer.skipToken();
            return parser.findTypePathStart();
        },
        .underscore, .op_minus_gt => {
            return true;
        },
        .op_star => {
            try lexer.skipTokenAndSpaceOrNewline();
            return parser.findTypeStart();
        },
        else => {
            return false;
        },
    }
}

fn findTypePathStart(parser: *Parser) !bool {
    const lexer = &parser.lexer;

    while (lexer.token.type == .@"const") {
        try lexer.skipToken();
        if (lexer.token.type != .op_colon_colon) break;
        try lexer.skipTokenAndSpaceOrNewline();
    }

    try lexer.skipSpace();
    return parser.findDelimiterOrTypeSuffix();
}

fn findDelimiterOrTypeSuffix(parser: *Parser) !bool {
    const lexer = &parser.lexer;
    switch (lexer.token.type) {
        .op_period => {
            try lexer.skipTokenAndSpaceOrNewline();
            return lexer.token.isKeyword(.class);
        },
        .op_question, .op_star, .op_star_star => {
            try lexer.skipTokenAndSpace();
            return parser.findDelimiterOrTypeSuffix();
        },
        // zig fmt: off
        .op_minus_gt, .op_bar, .op_comma, .op_eq_gt, .newline, .eof,
        .op_eq, .op_semicolon, .op_lparen, .op_rparen, .op_lsquare, .op_rsquare
        // -> | , => \n EOF = ; ( ) [ ]
        // zig fmt: on
        => return true,
        else => return false,
    }
}

// parseTypeof
// parseVisibilityModifier
// parseAsm
// parseAsmOperands
// parseAsmOperand
// parseAsmClobbers
// parseAsmOptions
// parseYieldWithScope
// parseYield
// parseBreak
// parseReturn
// parseNext
// parseControlExpression
// parseLib
// parseLibBody
// fn parseLibBodyExpressions
// parseLibBodyExp
// parseLibBodyExpWithoutLocation
// parseFunDef
// parseAlias
// parsePointerof
// parseSizeof
// parseInstanceSizeof
// parseSizeof
// parseOffsetof

// pub fn parseTypeDef(parser: *Parser) Node {
//     const lexer = &parser.lexer;
//     try lexer.skipTokenAndSpaceOrNewline();
//     const name = try parser.checkConst();
//     const name_location = lexer.token.location();
//     try lexer.skipTokenAndSpaceOrNewline();
//     try parser.check(.op_eq);
//     try lexer.skipTokenAndSpaceOrNewline();
//     const type = parser.parseBareProcType();
// }

// parseCStructOrUnion
// parseCStructOrUnionBody
// fn parseCStructOrUnionBodyExpressions

// pub fn parseCStructOrUnionFields(
//     parser: *Parser,
//     exps: ArrayList(Node),
// ) void {
//     const lexer = &parser.lexer;
//     const allocator = lexer.allocator;
//
//     var vars = ArrayList(Node).init(allocator);
//
//     const first = try Var.node(allocator, identToString(lexer.token));
//     first.setLocation(lexer.token.location());
//     try vars.append(first);
//
//     try lexer.skipTokenAndSpaceOrNewline();
//
//     while (lexer.token.type == .op_comma) {
//         try lexer.skipTokenAndSpaceOrNewline(); // TODO: redundant?
//
//         const v = try Var.node(allocator, try parser.checkIdent());
//         v.setLocation(lexer.token.location());
//         try vars.append(v);
//
//         try lexer.skipTokenAndSpaceOrNewline();
//     }
//
//     try parser.check(.op_colon);
//     try lexer.skipTokenAndSpaceOrNewline();
//
//     const t = parser.parseBareProcType();
//
//     try lexer.skipStatementEnd();
//
//     for (vars) |v| {
//         var node = try TypeDeclaration.node(allocator, v, t);
//         node.copyLocation(v);
//         node.copyEndLocation(t);
//         try exps.append(node);
//     }
// }

// parseEnumDef
// parseEnumBody
// fn parseEnumBodyExpressions

pub fn skipNodeToken(parser: *Parser, node: Node) !void {
    const lexer = &parser.lexer;
    node.setEndLocation(lexer.tokenEndLocation());
    try lexer.skipToken();
}

pub fn isEndToken(parser: *Parser) bool {
    const lexer = &parser.lexer;
    switch (lexer.token.type) {
        .op_rcurly, .op_rsquare, .op_percent_rcurly, .eof => {
            return true;
        },
        else => {
            switch (lexer.token.value) {
                .keyword => |keyword| {
                    switch (keyword) {
                        .do, .end, .@"else", .elsif, .when, .in, .rescue, .ensure, .then => {
                            return !parser.nextComesColonSpace();
                        },
                        else => {
                            return false;
                        },
                    }
                },
                else => {
                    return false;
                },
            }
        },
    }
}

pub fn canBeAssigned(node: Node) bool {
    switch (node) {
        .@"var", .instance_var, .class_var, .path, .global, .underscore => {
            return true;
        },
        .call => |call| {
            // TODO: check if [] has parentheses?
            // a = [:foo] * 3; a[0] = :bar; a.[](1) = :baz; p a
            // a = [:foo] * 3; a[0] = :bar; a.[]=(1, :baz); p a
            return (call.obj == null and call.args.items.len == 0 and call.block == null) or std.mem.eql(u8, call.name, "[]");
        },
        else => {
            return false;
        },
    }
}

const DefOrMacroCheck1 = &[_]Token.Kind{
    // zig fmt: off
    .ident, .@"const", .op_grave,
    .op_lt_lt, .op_lt, .op_lt_eq, .op_eq_eq, .op_eq_eq_eq, .op_bang_eq, .op_eq_tilde,
    .op_bang_tilde, .op_gt_gt, .op_gt, .op_gt_eq, .op_plus, .op_minus, .op_star, .op_slash,
    .op_slash_slash, .op_bang, .op_tilde, .op_percent, .op_amp, .op_bar, .op_caret, .op_star_star,
    .op_lsquare_rsquare, .op_lsquare_rsquare_eq, .op_lsquare_rsquare_question, .op_lt_eq_gt,
    .op_amp_plus, .op_amp_minus, .op_amp_star, .op_amp_star_star,
    // zig fmt: on
};

pub fn consumeDefOrMacroName(parser: *Parser) ![]const u8 {
    const lexer = &parser.lexer;
    lexer.wants_def_or_macro_name = true;
    try lexer.skipTokenAndSpaceOrNewline();
    try parser.checkAny(DefOrMacroCheck1);
    lexer.wants_def_or_macro_name = false;
    return switch (lexer.token.type) {
        .ident, .@"const" => identToString(lexer.token),
        else => |token_type| token_type.toString(),
    };
}

pub fn consumeDefEqualsSignSkipSpace(parser: *Parser) !bool {
    const lexer = &parser.lexer;
    try lexer.skipToken();
    if (lexer.token.type == .op_eq) {
        try lexer.skipTokenAndSpace();
        return true;
    } else {
        try lexer.skipSpace();
        return false;
    }
}

pub fn createIsolatedVarScope(parser: *Parser) !void {
    const allocator = parser.lexer.allocator;
    const scope = StringHashMap(void).init(allocator);
    try parser.var_scopes.append(scope);
}

pub fn createLexicalVarScope(parser: *Parser) !void {
    const var_scopes = parser.var_scopes.items;
    const current_scope = try var_scopes[var_scopes.len - 1].clone();
    try parser.var_scopes.append(current_scope);
}

pub fn resetVarScope(parser: *Parser) void {
    _ = parser.var_scopes.pop();
}

pub fn pushVars(parser: *Parser, vars: ArrayList(Node)) !void {
    for (vars.items) |v| {
        try parser.pushVar(v);
    }
}

pub fn pushVar(parser: *Parser, node: Node) !void {
    const lexer = &parser.lexer;
    switch (node) {
        .@"var" => |v| {
            try parser.pushVarName(v.name);
        },
        .arg => |arg| {
            try parser.pushVarName(arg.name);
        },
        .type_declaration => |type_declaration| {
            switch (type_declaration.@"var") {
                .@"var" => |v| {
                    try parser.pushVarName(v.name);
                },
                .instance_var => |instance_var| {
                    try parser.pushVarName(instance_var.name);
                },
                else => {
                    return lexer.raise("can't happen");
                },
            }
        },
        else => {
            // Nothing
        },
    }
}

pub fn pushVarName(parser: *Parser, name: []const u8) !void {
    const var_scopes = parser.var_scopes.items;
    try var_scopes[var_scopes.len - 1].put(name, {});
}

pub fn isVarInScope(parser: *const Parser, name: []const u8) bool {
    const var_scopes = parser.var_scopes.items;
    return var_scopes[var_scopes.len - 1].contains(name);
}

pub fn open(parser: *Parser, symbol: []const u8) !void {
    const lexer = &parser.lexer;
    try parser.openAt(symbol, lexer.token.location());
}

pub fn openAt(parser: *Parser, symbol: []const u8, location: Location) !void {
    try parser.unclosed_stack.append(Unclosed.init(symbol, location));
}

pub fn close(parser: *Parser) void {
    _ = parser.unclosed_stack.pop();
}

pub fn checkVoidValue(parser: *Parser, exp: Node, location: Location) !void {
    const lexer = &parser.lexer;
    switch (exp) {
        .@"break", .next, .@"return" => {
            return lexer.raiseLoc("void value expression", location);
        },
        else => {},
    }
}

pub fn checkVoidExpressionKeyword(parser: *Parser) !void {
    const lexer = &parser.lexer;
    switch (lexer.token.value) {
        .keyword => |keyword| {
            switch (keyword) {
                .@"break", .next, .@"return" => {
                    if (!parser.nextComesColonSpace()) {
                        return lexer.raiseFor("void value expression", lexer.token);
                        // TODO: keyword.toString().len
                    }
                },
                else => {},
            }
        },
        else => {},
    }
}

pub fn checkAny(parser: *Parser, token_types: []const Token.Kind) !void {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;
    for (token_types) |token_type| {
        if (lexer.token.type == token_type) {
            return;
        }
    }
    var buffer = ArrayList(u8).init(allocator);
    try buffer.appendSlice("expecting any of these tokens: ");
    var first = true;
    for (token_types) |token_type| {
        if (!first) {
            try buffer.appendSlice(", ");
        }
        try buffer.appendSlice(token_type.toString());
        first = false;
    }
    try buffer.appendSlice(" (not '");
    try buffer.appendSlice(lexer.token.type.toString());
    try buffer.appendSlice("')");
    return lexer.raiseFor(buffer.items, lexer.token);
}

pub fn check(parser: *Parser, token_type: Token.Kind) !void {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;
    if (token_type != lexer.token.type) {
        const message = try std.fmt.allocPrint(
            allocator,
            "expecting token '{}', not '{}'",
            .{ token_type, lexer.token },
        );
        return lexer.raiseFor(message, lexer.token);
    }
}

pub fn checkIdentKeyword(parser: *Parser, value: Keyword) !void {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;
    if (!lexer.token.isKeyword(value)) {
        const message = try std.fmt.allocPrint(allocator, "expecting identifier '{}', not '{}'", .{ value, lexer.token });
        return lexer.raiseFor(message, lexer.token);
    }
}

pub fn checkIdent(parser: *Parser) ![]const u8 {
    const lexer = &parser.lexer;
    try parser.check(.ident);
    return identToString(lexer.token);
}

fn identToString(token: Token) []const u8 {
    return switch (token.value) {
        .keyword => |keyword| keyword.toString(),
        .string => |string| string,
        else => unreachable,
    };
}

pub fn checkConst(parser: *Parser) ![]const u8 {
    const lexer = &parser.lexer;
    try parser.check(.@"const");
    return lexer.token.value.string;
}

pub fn unexpectedToken(parser: *Parser) error{ SyntaxError, OutOfMemory } {
    return parser.unexpectedTokenMsg(null);
}

pub fn unexpectedTokenMsg(parser: *Parser, msg: ?[]const u8) error{ SyntaxError, OutOfMemory } {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;
    var buffer = ArrayList(u8).init(allocator);
    var writer = buffer.writer();
    try writer.writeAll("unexpected token: ");
    if (lexer.token.type == .eof) {
        try writer.writeAll("EOF");
    } else {
        try writer.writeByte('"');
        try lexer.token.toString(writer);
        try writer.writeByte('"');
    }
    if (msg) |m| {
        try writer.writeAll(" (");
        try writer.writeAll(m);
        try writer.writeByte(')');
    }
    return lexer.raiseFor(buffer.items, lexer.token);
}

pub fn unexpectedTokenInAtomic(parser: *Parser) error{ SyntaxError, OutOfMemory } {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;
    const unclosed_stack = parser.unclosed_stack.items;
    if (unclosed_stack.len > 0) {
        const unclosed = unclosed_stack[unclosed_stack.len - 1];
        const message = try std.fmt.allocPrint(
            allocator,
            "unterminated {s}",
            .{unclosed.name},
        );
        return lexer.raiseLoc(message, unclosed.location);
    }

    return parser.unexpectedToken();
}

pub fn isVar(parser: *const Parser, name: []const u8) bool {
    if (parser.in_macro_expression) return true;

    return std.mem.eql(u8, name, "self") or parser.isVarInScope(name);
}

pub fn pushVisibility(parser: *Parser, new_value: ?Visibiity) ?Visibiity {
    const old_visibility = parser.visibility;
    parser.visibility = new_value;
    return old_visibility;
}

pub fn resetVisibility(parser: *Parser, old_value: ?Visibiity) void {
    parser.visibility = old_value;
}

// nextToken

pub fn tempArgName(parser: *Parser) ![]const u8 {
    const allocator = parser.lexer.allocator;
    const arg_name = try std.fmt.allocPrint(allocator, "__arg{}", .{parser.temp_arg_count});
    parser.temp_arg_count += 1;
    return arg_name;
}

// zig fmt: off
pub fn main() !void {
    const p = @import("std").debug.print;
    const assert = @import("std").debug.assert;
    p("", .{});
    var parser = try Parser.new("foo");
    var lexer = &parser.lexer;
    assert(@TypeOf(parser) == Parser);

    assert(parser.temp_arg_count == 0);
    assert(std.mem.eql(u8, "__arg0", try parser.tempArgName()));
    assert(parser.temp_arg_count == 1);

    assert(parser.isVarInScope("bar") == false);
    try parser.pushVarName("bar");
    assert(parser.isVarInScope("bar"));

    assert(parser.isVarInScope("fizz") == false);
    assert(parser.isVarInScope("buzz") == false);
    try parser.pushVar(try Var.node(lexer.allocator, "fizz"));
    try parser.pushVars(blk: {
        var vars = ArrayList(Node).init(lexer.allocator);
        try vars.append(try Var.node(lexer.allocator, "buzz"));
        break :blk vars;
    });
    assert(parser.isVarInScope("fizz"));
    assert(parser.isVarInScope("buzz"));

    assert(parser.isVar("self"));
    assert(parser.isVar("bar"));
    assert(parser.isVar("fizz"));
    assert(parser.isVar("buzz"));
    assert(parser.isVar("foo") == false);
    parser.in_macro_expression = true;
    assert(parser.isVar("foo"));

    var var_scopes = parser.var_scopes.items;
    assert(var_scopes.len == 1);
    assert(var_scopes[var_scopes.len - 1].count() == 3);

    try parser.createIsolatedVarScope();
    var_scopes = parser.var_scopes.items;
    assert(var_scopes.len == 2);
    assert(var_scopes[var_scopes.len - 1].count() == 0);

    parser.resetVarScope();
    var_scopes = parser.var_scopes.items;
    assert(var_scopes.len == 1);
    assert(var_scopes[var_scopes.len - 1].count() == 3);

    try parser.createLexicalVarScope();
    var_scopes = parser.var_scopes.items;
    assert(var_scopes.len == 2);
    assert(var_scopes[var_scopes.len - 1].count() == 3);

    parser = try Parser.new("foo=");
    assert(
        std.mem.eql(u8, try parser.consumeDefOrMacroName(), "foo"),
    );
    assert(try parser.consumeDefEqualsSignSkipSpace());

    parser = try Parser.new("=");
    lexer = &parser.lexer;
    if (parser.consumeDefOrMacroName()) |_| unreachable else |err| {
        assert(err == error.SyntaxError);
        assert(std.mem.eql(u8, lexer.error_message.?, blk: {
            break :blk "expecting any of these tokens: " ++
                "ident, const, `, <<, <, <=, ==, ===, !=, =~, !~, " ++
                ">>, >, >=, +, -, *, /, //, !, ~, %, &, |, ^, **, " ++
                "[], []=, []?, <=>, &+, &-, &*, &** (not '=')";
        }));
    }

    assert(canBeAssigned(try Var.node(lexer.allocator, "foo")));
    assert(
        canBeAssigned(try Call.node(
            lexer.allocator,
            null,
            "foo",
            ArrayList(Node).init(lexer.allocator),
            .{},
        )),
    );
    assert(
        canBeAssigned(try Call.node(
            lexer.allocator,
            try Var.node(lexer.allocator, "foo"),
            "[]",
            blk: {
                var args = ArrayList(Node).init(lexer.allocator);
                try args.append(try Var.node(lexer.allocator, "bar"));
                // try args.append(try Var.node(lexer.allocator, "fizz"));
                break :blk args;
            },
            .{},
        )),
    );

    parser = try Parser.new("foo.bar");
    lexer = &parser.lexer;
    try lexer.skipToken();
    var token_end_location = lexer.tokenEndLocation();
    var node = try Var.node(lexer.allocator, "foo");
    try parser.skipNodeToken(node);
    assert(node.endLocation().?.compare(.eq, token_end_location));

    parser = try Parser.new("Foo");
    lexer = &parser.lexer;
    try lexer.skipToken();
    assert(std.mem.eql(u8, "Foo", try parser.checkConst()));

    parser = try Parser.new("foo");
    lexer = &parser.lexer;
    try lexer.skipToken();
    assert(std.mem.eql(u8, "foo", try parser.checkIdent()));

    parser = try Parser.new("type");
    lexer = &parser.lexer;
    try lexer.skipToken();
    assert(std.mem.eql(u8, "type", try parser.checkIdent()));

    parser = try Parser.new("type");
    lexer = &parser.lexer;
    try lexer.skipToken();
    try parser.checkIdentKeyword(.type);

    parser = try Parser.new("foo");
    lexer = &parser.lexer;
    try lexer.skipToken();
    if (parser.checkIdentKeyword(.fun)) |_| unreachable else |err| {
        assert(err == error.SyntaxError);
        assert(std.mem.eql(u8, lexer.error_message.?, blk: {
            break :blk "expecting identifier 'fun', not 'foo'";
        }));
    }

    parser = try Parser.new("break : ");
    parser.no_type_declaration = 0;
    lexer = &parser.lexer;
    try lexer.skipToken();
    if (parser.checkVoidExpressionKeyword()) |_| unreachable else |err| {
        assert(err == error.SyntaxError);
        assert(std.mem.eql(u8, lexer.error_message.?, blk: {
            break :blk "void value expression";
        }));
    }

    parser = try Parser.new("break : ");
    parser.no_type_declaration = 1;
    lexer = &parser.lexer;
    try lexer.skipToken();
    _ = try parser.checkVoidExpressionKeyword();

    if (parser.checkVoidValue(
        try Return.node(lexer.allocator),
        Location.new(null, 0, 0),
    )) |_| unreachable else |err| {
        assert(err == error.SyntaxError);
        assert(std.mem.eql(u8, lexer.error_message.?, blk: {
            break :blk "void value expression";
        }));
    }

    parser = try Parser.new("foo");
    lexer = &parser.lexer;
    try lexer.skipToken();
    try parser.open("fizz");
    assert(parser.unexpectedTokenInAtomic() == error.SyntaxError);
    assert(std.mem.eql(u8, lexer.error_message.?, blk: {
        break :blk "unterminated fizz";
    }));
    parser.close();
    assert(parser.unexpectedTokenInAtomic() == error.SyntaxError);
    assert(std.mem.eql(u8, lexer.error_message.?, blk: {
        break :blk "unexpected token: \"foo\"";
    }));

    parser = try Parser.new("foo");
    assert(parser.visibility == null);
    var old_visibility = parser.pushVisibility(.public);
    assert(old_visibility == null);
    assert(parser.visibility.? == .public);
    parser.resetVisibility(old_visibility);
    assert(parser.visibility == null);

    parser = try Parser.new("::Foo::Bar::Baz");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parsePath();
    assert(node.path.is_global);
    assert(
        node.endLocation().?.compare(.eq, Location.new("", 1, 15)),
    );
    var expected_names = &[_][]const u8{"Foo", "Bar", "Baz"};
    var actual_names = node.path.names.items;
    assert(expected_names.len == actual_names.len);
    var i: usize = 0;
    while (i < expected_names.len) : (i += 1) {
        assert(std.mem.eql(u8, expected_names[i], actual_names[i]));
    }

    parser = try Parser.new(":foo");
    lexer = &parser.lexer;
    try lexer.skipToken();
    assert(std.mem.eql(u8, "foo", try parser.parseRespondsToName()));

    // .string_array_start / parseStringArray
    parser = try Parser.new("%w[foo bar]");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .array_literal);
    var elements = node.array_literal.elements.items;
    assert(elements.len == 2);
    assert(elements[0] == .string_literal);
    assert(std.mem.eql(u8, "foo", elements[0].string_literal.value));
    assert(elements[1] == .string_literal);
    assert(std.mem.eql(u8, "bar", elements[1].string_literal.value));

    // .symbol_array_start / parseSymbolArray
    parser = try Parser.new("%i(foo bar)");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .array_literal);
    elements = node.array_literal.elements.items;
    assert(elements.len == 2);
    assert(elements[0] == .symbol_literal);
    assert(std.mem.eql(u8, "foo", elements[0].symbol_literal.value));
    assert(elements[1] == .symbol_literal);
    assert(std.mem.eql(u8, "bar", elements[1].symbol_literal.value));

    // .char
    parser = try Parser.new("'F'");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .char_literal);
    assert(node.char_literal.value == 'F');

    // .symbol
    parser = try Parser.new(":foo");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .symbol_literal);
    assert(std.mem.eql(u8, "foo", node.symbol_literal.value));

    // .global
    parser = try Parser.new("$foo");
    lexer = &parser.lexer;
    try lexer.skipToken();
    if (parser.parseAtomic()) |_| unreachable else |err| {
        assert(err == error.SyntaxError);
        assert(std.mem.eql(u8, lexer.error_message.?, blk: {
            break :blk "$global_variables are not supported, use @@class_variables instead";
        }));
    }

    // .op_dollar_tilde
    parser = try Parser.new("$~");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseAtomic();
    assert(node == .global);
    assert(std.mem.eql(u8, "$~", node.global.name));

    // .op_dollar_question
    parser = try Parser.new("$? =");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseAtomic();
    assert(node == .@"var");
    assert(std.mem.eql(u8, "$?", node.@"var".name));

    // .global_match_data_index
    parser = try Parser.new("$123");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .call);
    assert(node.call.obj.? == .global);
    assert(std.mem.eql(u8, "$~", node.call.obj.?.global.name));
    assert(std.mem.eql(u8, "[]", node.call.name));
    assert(node.call.args.items.len == 1);
    assert(node.call.args.items[0] == .number_literal);
    assert(std.mem.eql(u8, "123", node.call.args.items[0].number_literal.value));

    // .magic_line
    parser = try Parser.new("\n __LINE__");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .number_literal);
    assert(std.mem.eql(u8, "2", node.number_literal.value));

    // .underscore
    parser = try Parser.new("_");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .underscore);

    // parseAtomicType
    parser = try Parser.new("self");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseBareProcType();
    assert(node == .self);

    parser = try Parser.new("self?");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseBareProcType();
    assert(node == .@"union");
    assert(node.@"union".types.items.len == 2);
    assert(node.@"union".types.items[0] == .self);
    assert(node.@"union".types.items[1] == .path);
    assert(node.@"union".types.items[1].path.is_global);
    assert(node.@"union".types.items[1].path.names.items.len == 1);
    assert(std.mem.eql(u8, "Nil", node.@"union".types.items[1].path.names.items[0]));

    parser = try Parser.new("_");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseBareProcType();
    assert(node == .underscore);

    // makePointerType
    parser = try Parser.new("self*");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseBareProcType();
    assert(node == .generic);
    assert(node.generic.name == .path);
    assert(node.generic.name.path.is_global);
    assert(std.mem.eql(u8, "Pointer", node.generic.name.path.names.items[0]));
    assert(node.generic.type_vars.items[0] == .self);

    parser = try Parser.new("self**");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseBareProcType();
    assert(node == .generic);
    assert(node.generic.name == .path);
    assert(node.generic.name.path.is_global);
    assert(std.mem.eql(u8, "Pointer", node.generic.name.path.names.items[0]));
    node = node.generic.type_vars.items[0];
    assert(node == .generic);
    assert(node.generic.name == .path);
    assert(node.generic.name.path.is_global);
    assert(std.mem.eql(u8, "Pointer", node.generic.name.path.names.items[0]));
    assert(node.generic.type_vars.items[0] == .self);

    // makeStaticArrayType
    parser = try Parser.new("self[123]");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseBareProcType();
    assert(node == .generic);
    assert(node.generic.name == .path);
    assert(node.generic.name.path.is_global);
    assert(std.mem.eql(u8, "StaticArray", node.generic.name.path.names.items[0]));
    assert(node.generic.type_vars.items[0] == .self);
    assert(node.generic.type_vars.items[1] == .number_literal);

    // parseUnionType
    parser = try Parser.new("self | _");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseBareProcType();
    assert(node == .@"union");
    assert(node.@"union".types.items.len == 2);
    assert(node.@"union".types.items[0] == .self);
    assert(node.@"union".types.items[1] == .underscore);

    // parseProcTypeOutput
    parser = try Parser.new("self, _ -> self");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseBareProcType();
    assert(node == .proc_notation);
    assert(node.proc_notation.inputs.?.items.len == 2);
    assert(node.proc_notation.inputs.?.items[0] == .self);
    assert(node.proc_notation.inputs.?.items[1] == .underscore);
    assert(node.proc_notation.output.? == .self);

    // parseEmptyArrayLiteral
    parser = try Parser.new("[] of self");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .array_literal);

    // parseOperator
    parser = try Parser.new("'a' || 'b'");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .@"or");

    // p("{}\n", .{});

    // parser = Parser.new("");
    // var node = try parser.parse();
    // p("{}\n", .{node});
    // assert(node == .nop);
    // parser = Parser.new("\n; foo");
    // if (parser.parse()) |_| unreachable else |err| p("{} {?s}\n", .{err, parser.lexer.error_message});
    // parser = Parser.new("+=");
    // parser.stop_on_do = true;
    // if (parser.parse()) |_| unreachable else |err| p("{} {?s}\n", .{err, parser.lexer.error_message});
    // assert(parser.stop_on_do);
    // try parser.parse();
    // p("{}\n", .{parser.lexer.token.type});
    // assert(parser.lexer.token.type == .ident);
}
