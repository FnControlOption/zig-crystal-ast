const Parser = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Lexer = @import("lexer.zig");
const Location = @import("location.zig");
const Token = @import("token.zig");

const ast = @import("ast.zig");
const Node = ast.Node;
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
};

allocator: Allocator,

lexer: Lexer,
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
consume_heredocs: bool = false,
inside_interpolation: bool = false,

stop_on_do: bool = false,
assigned_vars: ArrayList([]const u8),

pub fn new(string: []const u8) !Parser {
    const allocator = std.heap.page_allocator; // TODO
    var var_scopes = ArrayList(StringHashMap(void)).init(allocator);
    try var_scopes.append(StringHashMap(void).init(allocator));
    return .{
        .allocator = allocator,
        .lexer = .{ .allocator = allocator, .string = string },
        .var_scopes = var_scopes,
        .unclosed_stack = ArrayList(Unclosed).init(allocator),
        .call_args_start_locations = ArrayList(Location).init(allocator),
        .assigned_vars = ArrayList([]const u8).init(allocator),
    };
}

pub fn parse(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    try lexer.skipTokenAndStatementEnd();

    const value = try parser.parseExpressions();
    try parser.check(.eof);
    return value;
}

// parse(mode: ParseMode)

fn parseExpressions(parser: *Parser) !Node {
    const old_stop_on_do = parser.stop_on_do;
    defer parser.stop_on_do = old_stop_on_do;
    parser.stop_on_do = false;
    return parser.parseExpressionsInternal();
}

fn parseExpressionsInternal(parser: *Parser) !Node {
    if (parser.isEndToken()) {
        return Nop.new(parser.allocator);
    }

    const exp = try parser.parseMultiAssign();

    const lexer = &parser.lexer;
    lexer.slash_is_regex = true;
    try lexer.skipStatementEnd();

    if (parser.isEndToken()) {
        return exp;
    }

    var exps = ArrayList(Node).init(parser.allocator);
    try exps.append(exp);

    while (true) {
        try exps.append(try parser.parseMultiAssign());
        try lexer.skipStatementEnd();
        if (parser.isEndToken()) break;
    }

    return Expressions.from(parser.allocator, exps);
}

fn parseMultiAssign(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const location = lexer.token.location();

    var lhs_splat_index: ?usize = null;
    if (lexer.token.type == .op_star) {
        lhs_splat_index = 0;
        try lexer.skipTokenAndSpace();
    }

    var last = try parser.parseExpression();
    try lexer.skipSpace();

    const last_is_target = isMultiAssignTarget(last);

    switch (lexer.token.type) {
        .op_comma => {
            _ = last_is_target;
            return error.Unimplemented;
        },
        .newline, .op_semicolon => {
            return error.Unimplemented;
        },
        else => {
            return error.Unimplemented;
        }
    }

    var exps = ArrayList(Node).init(parser.allocator);
    try exps.append(last);

    var i: usize = 0;
    var possible_assign_index: ?usize = null;

    while (lexer.token.type == .op_comma) {
        return error.Unimplemented;
    }

    if (possible_assign_index == null and isMultiAssignMiddle(last)) {
        possible_assign_index = i;
    }

    if (possible_assign_index) |assign_index| {
        var targets = ArrayList(Node).initCapacity(parser.allocator, assign_index);
        var target_index: usize = 0;
        while (target_index < assign_index) : (target_index += 1) {
            const exp = exps.items[target_index];
            targets.append(parser.multiassignLeftHand(exp));
        }

        var assign_exp = exps.items[assign_index];
        var values = ArrayList(Node).init(parser.allocator);

        switch (assign_exp) {
            .assign => |assign| {
                targets.append(parser.multiassignLeftHand(assign.target));
                values.append(assign.value);
            },
            .call => |*call| {
                call.name = call.name[0..call.name.len - 1];
                targets.append(assign_exp);
                values.append(call.args.pop());
            },
            else => {
                try lexer.raise("BUG: multiassign index expression can only be Assign or Call");
            }
        }

        values.appendSlice(exps.items[assign_index + 1..]);
        if (values.items.len != 1) {
            if (lhs_splat_index) {
                if (targets.items.len - 1 > values.items.len) {
                    try lexer.raiseLoc("Multiple assignment count mismatch", location);
                }
            } else {
                if (targets.items.len != values.items.len) {
                    try lexer.raiseLoc("Multiple assignment count mismatch", location);
                }
            }
        }

        var multi = MultiAssign.new(targets, values).startsAt(location);
        _ = multi;
        return error.Unimplemented;
    } else {
        try parser.unexpectedToken();
        unreachable;
    }
}

fn isMultiAssignTarget(exp: Node) bool {
    switch (exp) {
        .underscore, .@"var", .instance_var, .class_var, .global, .assign => {
            return true;
        },
        .call => |call| {
            if (call.has_parentheses) {
                return false;
            }
            return (call.args.items.len == 0 and call.named_args == null) or
                Lexer.isSetter(call.name) or
                std.mem.eql(u8, "[]", call.name) or
                std.mem.eql(u8, "[]=", call.name);
        },
        else => {
            return false;
        }
    }
}

fn isMultiAssignMiddle(exp: Node) bool {
    return switch (exp) {
        .assign => true,
        .call => |call| call.name[call.name.len - 1] == '=',
        else => false,
    };
}

fn multiassignLeftHand(parser: *Parser, exp: Node) Node {
    const lexer = &parser.lexer;
    var lhs = exp;
    switch (exp) {
        .path => |path| {
            try lexer.raiseLoc("can't assign to constant in multiple assignment", path.location.?);
        },
        .call => |call| {
            if (call.obj == null and call.args.items.len == 0) {
                exp = Var.new(parser.allocator, call.anem).at(exp);
            }
        }
    }
    switch (lhs) {
        .@"var" => |v| {
            if (std.mem.eql(u8, v.name, "self")) {
                try lexer.raiseLoc("can't change the value of self", v.location.?);
            }
            parser.pushVar(v);
        }
    }
    return lhs;
}

fn parseExpression(parser: *Parser) !Node {
    _ = parser;
    return error.Unimplemented;
}

// parseExpressionSuffix
// parseExpressionSuffix
// parseOpAssignNoControl
// parseOpAssign
// parseQuestionColon
// parseRange
// newRange
// inline fn parseOperator
// parseOr
// parseAnd
// parseEquality
// parseCmp
// parseLogicalOr
// parseLogicalAnd
// parseShift
// parseAddOrSub
// parseMulOrDiv
// parsePow
// parsePrefix
// AtomicWithMethodCheck
// parseAtomicWithMethod
// parseAtomicMethodSuffix
// parseAtomicMethodSuffixSpecial
// parseSingleArg
// parseIsA
// parseAs
// parseAsQuestion
// parseRespondsTo
// parseRespondsToName
// parseNilQuestion
// parseNegationSuffix
// parseAtomic
// parseAtomicWithoutLocation
// checkTypeDeclaration
// parseTypeDeclaration

fn nextComesColonSpace(parser: *Parser) bool {
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
// parseStringArray
// parseSymbolArray
// parseStringOrSymbolArray
// parseEmptyArrayLiteral
// parseHashOrTupleLiteral
// parseHashLiteral
// atNamedTupleStart
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
// preserveStopOnDo
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
// parseBareProcType
// parseUnionType
// parseAtomicTypeWithSuffix
// parseAtomicType
// parseUnionTypes
// parseGeneric
// parseGeneric
// parsePath
// parsePath
// parseTypeArgs
// parseNamedTypeArgs
// parseTypeSplat
// parseTypeArg
// parseTypeSuffix
// parseProcTypeOutput
// makeNilableType
// makeNilableExpression
// makePointerType
// makeStaticArrayType
// makeTupleType
// makeNamedTupleType
// isTypeStart
// isTypeStart
// isTypePathStart
// isDelimiterOrTypeSuffix
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
// parseLibBodyExp
// parseLibBodyExpWithoutLocation
// parseFunDef
// parseAlias
// parsePointerof
// parseSizeof
// parseInstanceSizeof
// parseSizeof
// parseOffsetof
// parseTypeDef
// parseCStructOrUnion
// parseCStructOrUnionBody
// parseCStructOrUnionFields
// parseEnumDef
// parseEnumBody

fn nodeAndNextToken(parser: *Parser, node: Node) !Node {
    const lexer = &parser.lexer;
    const end_location = lexer.tokenEndLocation();
    _ = try lexer.nextToken();
    return node.endsAt(end_location);
}

fn isEndToken(parser: *Parser) bool {
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
                        }
                    }
                },
                else => {
                    return false;
                }
            }
        }
    }
}

fn canBeAssigned(node: Node) bool {
    switch (node) {
        .@"var", .instance_var, .class_var, .path, .global, .underscore => {
            return true;
        },
        .call => |call| {
            // TODO: check if [] has parentheses?
            return (call.obj == null and call.args.items.len == 0 and call.block == null) or std.mem.eql(u8, call.name, "[]");
        },
        else => {
            return false;
        }
    }
}

const DefOrMacroCheck1 = &[_]Token.Kind{
    .ident, .@"const", .op_grave,
    .op_lt_lt, .op_lt, .op_lt_eq, .op_eq_eq, .op_eq_eq_eq, .op_bang_eq, .op_eq_tilde,
    .op_bang_tilde, .op_gt_gt, .op_gt, .op_gt_eq, .op_plus, .op_minus, .op_star, .op_slash,
    .op_slash_slash, .op_bang, .op_tilde, .op_percent, .op_amp, .op_bar, .op_caret, .op_star_star,
    .op_lsquare_rsquare, .op_lsquare_rsquare_eq, .op_lsquare_rsquare_question, .op_lt_eq_gt,
    .op_amp_plus, .op_amp_minus, .op_amp_star, .op_amp_star_star,
};

fn consumeDefOrMacroName(parser: *Parser) ![]const u8 {
    const lexer = &parser.lexer;
    lexer.wants_def_or_macro_name = true;
    try lexer.skipTokenAndSpaceOrNewline();
    try parser.checkAny(DefOrMacroCheck1);
    lexer.wants_def_or_macro_name = false;
    switch (lexer.token.type) {
        .ident, .@"const" => {
            switch (lexer.token.value) {
                .keyword => |keyword| {
                    return keyword.toString();
                },
                .string => |string| {
                    return string;
                },
                else => {
                    try parser.unexpectedToken();
                    unreachable;
                },
            }
        },
        else => |token_type| {
            return token_type.toString();
        }
    }
}

fn consumeDefEqualsSignSkipSpace(parser: *Parser) !bool {
    const lexer = &parser.lexer;
    _ = try lexer.nextToken();
    if (lexer.token.type == .op_eq) {
        try lexer.skipTokenAndSpace();
        return true;
    } else {
        try lexer.skipSpace();
        return false;
    }
}

// withIsolatedVarScope
// withLexicalVarScope

fn pushVars(parser: *Parser, vars: ArrayList(Node)) !void {
    for (vars.items) |v| {
        try parser.pushVar(v);
    }
}

fn pushVar(parser: *Parser, node: Node) !void {
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
                    try lexer.raise("can't happen");
                }
            }
        },
        else => {
            // Nothing
        }
    }
}

fn pushVarName(parser: *Parser, name: []const u8) !void {
    const var_scopes = parser.var_scopes.items;
    try var_scopes[var_scopes.len - 1].put(name, {});
}

fn isVarInScope(parser: Parser, name: []const u8) bool {
    const var_scopes = parser.var_scopes.items;
    return var_scopes[var_scopes.len - 1].contains(name);
}

// open
// checkVoidValue
// checkVoidExpressionKeyword

fn checkAny(parser: *Parser, token_types: []const Token.Kind) !void {
    const lexer = &parser.lexer;
    for (token_types) |token_type| {
        if (lexer.token.type == token_type) {
            return;
        }
    }
    var buffer = ArrayList(u8).init(parser.allocator);
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
    try lexer.raiseFor(buffer.items, lexer.token);
}

fn check(parser: *Parser, token_type: Token.Kind) !void {
    const lexer = &parser.lexer;
    if (token_type != lexer.token.type) {
        const message = try std.fmt.allocPrint(
            parser.allocator,
            "expecting token '{}', not '{}'",
            .{ token_type, lexer.token },
        );
        try lexer.raiseFor(message, lexer.token);
    }
}

// checkIdent
// checkIdent
// checkConst

fn unexpectedToken(parser: *Parser) !void {
    const lexer = &parser.lexer;
    if (lexer.token.type == .eof) {
        try lexer.raiseFor("unexpected token: EOF", lexer.token);
    } else {
        const message = try std.fmt.allocPrint(
            parser.allocator,
            "unexpected token: {}",
            .{ lexer.token },
        );
        try lexer.raiseFor(message, lexer.token);
    }
}

// unexpectedTokenInAtomic
// isVar
// pushVisibility
// nextToken

fn tempArgName(parser: *Parser) ![]const u8 {
    const arg_name = try std.fmt.allocPrint(
        parser.allocator,
        "__arg{}",
        .{parser.temp_arg_count}
    );
    parser.temp_arg_count += 1;
    return arg_name;
}

pub fn main() !void {
    const p = @import("std").debug.print;
    const assert = @import("std").debug.assert;
    var parser = try Parser.new("foo");
    p("{}\n", .{@TypeOf(parser)});
    p("{} -> {s} -> {}\n", .{parser.temp_arg_count, try parser.tempArgName(), parser.temp_arg_count});
    assert(parser.isVarInScope("bar") == false);
    try parser.pushVarName("bar");
    assert(parser.isVarInScope("bar"));
    assert(parser.isVarInScope("fizz") == false);
    assert(parser.isVarInScope("buzz") == false);
    try parser.pushVar(try Var.new(parser.allocator, "fizz"));
    var vars = ArrayList(Node).init(parser.allocator);
    try vars.append(try Var.new(parser.allocator, "buzz"));
    try parser.pushVars(vars);
    assert(parser.isVarInScope("fizz"));
    assert(parser.isVarInScope("buzz"));
    parser = try Parser.new("foo=");
    assert(
        std.mem.eql(u8, try parser.consumeDefOrMacroName(), "foo"),
    );
    assert(try parser.consumeDefEqualsSignSkipSpace());
    parser = try Parser.new("=");
    if (parser.consumeDefOrMacroName()) |_| unreachable else |err| p("{} {?s}\n", .{err, parser.lexer.error_message});
    assert(canBeAssigned(try Var.new(parser.allocator, "foo")));
    assert(
        canBeAssigned(try Call.new(
            parser.allocator,
            null,
            "foo",
            ArrayList(Node).init(parser.allocator),
        )),
    );
    var args = ArrayList(Node).init(parser.allocator);
    try args.append(try Var.new(parser.allocator, "bar"));
    // try args.append(try Var.new(parser.allocator, "fizz"));
    assert(
        canBeAssigned(try Call.new(
            parser.allocator,
            try Var.new(parser.allocator, "foo"),
            "[]",
            args,
        )),
    );
    parser = try Parser.new("foo.bar");
    var lexer = &parser.lexer;
    _ = try lexer.nextToken();
    var node0 = try Var.new(parser.allocator, "foo");
    var node = try parser.nodeAndNextToken(node0);
    assert(node == .@"var");
    assert(node.@"var" == node0.@"var");
    // node.endLocation()

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