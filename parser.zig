const Parser = @This();

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

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

allocator: Allocator,

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
        .allocator = allocator,
        .lexer = Lexer.init(allocator, string),
        .var_scopes = ArrayList(StringHashMap(void)).init(allocator),
        .unclosed_stack = ArrayList(Unclosed).init(allocator),
        .call_args_start_locations = ArrayList(Location).init(allocator),
        .assigned_vars = ArrayList([]const u8).init(allocator),
    };
    try parser.createIsolatedVarScope();
    return parser;
}

// pub fn parse(parser: *Parser) !Node {
//     const lexer = &parser.lexer;
//     try lexer.skipTokenAndStatementEnd();
//
//     const value = try parser.parseExpressions();
//     try parser.check(.eof);
//     return value;
// }

// parse(mode: ParseMode)

// pub fn parseExpressions(parser: *Parser) !Node {
//     const old_stop_on_do = parser.pushStopOnDo(false);
//     defer parser.resetStopOnDo(old_stop_on_do);
//     return parser.parseExpressionsInternal();
// }

// pub fn parseExpressionsInternal(parser: *Parser) !Node {
//     if (parser.isEndToken()) {
//         return Nop.new(parser.allocator);
//     }
//
//     const exp = try parser.parseMultiAssign();
//
//     const lexer = &parser.lexer;
//     lexer.slash_is_regex = true;
//     try lexer.skipStatementEnd();
//
//     if (parser.isEndToken()) {
//         return exp;
//     }
//
//     var exps = ArrayList(Node).init(parser.allocator);
//     try exps.append(exp);
//
//     while (true) {
//         try exps.append(try parser.parseMultiAssign());
//         try lexer.skipStatementEnd();
//         if (parser.isEndToken()) break;
//     }
//
//     return Expressions.from(parser.allocator, exps);
// }

// pub fn parseMultiAssign(parser: *Parser) !Node {
//     const lexer = &parser.lexer;
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
//     var exps = ArrayList(Node).init(parser.allocator);
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
//         var targets = ArrayList(Node).initCapacity(parser.allocator, assign_index);
//         var target_index: usize = 0;
//         while (target_index < assign_index) : (target_index += 1) {
//             const exp = exps.items[target_index];
//             targets.append(parser.multiassignLeftHand(exp));
//         }
//
//         var assign_exp = exps.items[assign_index];
//         var values = ArrayList(Node).init(parser.allocator);
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
//         const multi = MultiAssign.new(targets, values);
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
//     var lhs = exp;
//     switch (exp) {
//         .path => |path| {
//             return lexer.raiseLoc("can't assign to constant in multiple assignment", path.location.?);
//         },
//         .call => |call| {
//             if (call.obj == null and call.args.items.len == 0) {
//                 var v = Var.new(parser.allocator, call.anem);
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

// pub fn parseExpression(parser: *Parser) !Node {
//     _ = parser;
//     return error.Unimplemented;
// }

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
    const allocator = parser.allocator;
    const lexer = &parser.lexer;
    switch (lexer.token.type) {
        // TODO
        .char => {
            const value = lexer.token.value.char;
            const node = try CharLiteral.new(allocator, value);
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
            const node = try SymbolLiteral.new(allocator, value);
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
                const v = try Var.new(allocator, name);
                v.setLocation(location);
                try parser.pushVar(v);
                try parser.skipNodeToken(v);
                return v;
            } else {
                const node = try Global.new(allocator, name);
                node.setLocation(location);
                return node;
            }
        },
        // TODO
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
    const allocator = parser.allocator;
    const lexer = &parser.lexer;

    var strings = ArrayList(Node).init(allocator);

    while (true) {
        _ = try lexer.nextStringArrayToken();
        switch (lexer.token.type) {
            .string => {
                const value = lexer.token.value.string;
                const string = try StringOrSymbolLiteral.new(allocator, value);
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

    var of = ArrayList([]const u8).init(allocator);
    try of.append(elements_type);
    return ArrayLiteral.new(allocator, strings, .{
        .of = try Path.global(allocator, of),
    });
}

// pub fn parseEmptyArrayLiteral(parser: *Parser) !Node {
//     const lexer = &parser.lexer;
//     const line = lexer.line_number;
//     const column = lexer.token.column_number;
//
//     try lexer.skipTokenAndSpace();
//     if (lexer.token.isKeyword(.of)) {
//         try lexer.skipTokenAndSpaceOrNewline();
//         const of =
//     } else {
//         return lexer.raiseAt("for empty arrays use '[] of ElementType'", line, column);
//     }
// }

// parseArrayLiteral
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

pub fn pushStopOnDo(parser: *Parser, new_value: bool) bool {
    const old_stop_on_do = parser.stop_on_do;
    parser.stop_on_do = new_value;
    return old_stop_on_do;
}

pub fn resetStopOnDo(parser: *Parser, old_value: bool) void {
    parser.stop_on_do = old_value;
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
// parseBareProcType
// parseUnionType
// parseAtomicTypeWithSuffix
// parseAtomicType
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
    const allocator = parser.allocator;
    const lexer = &parser.lexer;

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

    const node = try Path.new(allocator, names, is_global);
    node.setLocation(location);
    node.setEndLocation(end_location);
    return node;
}

// parseTypeArgs
// parseNamedTypeArgs

// pub fn consumeTypeSplatStar(parser: *Parser) Node {
//     const lexer = &parser.lexer;
//     if (lexer.token.type == .op_star) {
//         try lexer.skipTokenAndSpaceOrNewline();
//         return true;
//     } else {
//         return false;
//     }
// }

// pub fn parseUnionTypeSplat(
//     type_: Node,
//     location: Location,
//     splat: bool,
// ) !Node {
//     const lexer = &parser.lexer;
//     const location = lexer.token.location();
//     // if (splat) {
//     //     const node = try Splat.new(parser.allocator, type_);
//     //     node.setLocation(location);
//     //     return node;
//     // }
//     parseUnionType
// }

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
//     const allocator = parser.allocator;
//     const lexer = &parser.lexer;
//
//     var vars = ArrayList(Node).init(allocator);
//
//     const first = try Var.new(allocator, identToString(lexer.token));
//     first.setLocation(lexer.token.location());
//     try vars.append(first);
//
//     try lexer.skipTokenAndSpaceOrNewline();
//
//     while (lexer.token.type == .op_comma) {
//         try lexer.skipTokenAndSpaceOrNewline(); // TODO: redundant?
//
//         const v = try Var.new(allocator, try parser.checkIdent());
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
//         var node = try TypeDeclaration.new(allocator, v, t);
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
            return (call.obj == null and call.args.items.len == 0 and call.block == null) or std.mem.eql(u8, call.name, "[]");
        },
        else => {
            return false;
        },
    }
}

const DefOrMacroCheck1 = &[_]Token.Kind{
    .ident,                 .@"const",                    .op_grave,
    .op_lt_lt,              .op_lt,                       .op_lt_eq,
    .op_eq_eq,              .op_eq_eq_eq,                 .op_bang_eq,
    .op_eq_tilde,           .op_bang_tilde,               .op_gt_gt,
    .op_gt,                 .op_gt_eq,                    .op_plus,
    .op_minus,              .op_star,                     .op_slash,
    .op_slash_slash,        .op_bang,                     .op_tilde,
    .op_percent,            .op_amp,                      .op_bar,
    .op_caret,              .op_star_star,                .op_lsquare_rsquare,
    .op_lsquare_rsquare_eq, .op_lsquare_rsquare_question, .op_lt_eq_gt,
    .op_amp_plus,           .op_amp_minus,                .op_amp_star,
    .op_amp_star_star,
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
    const scope = StringHashMap(void).init(parser.allocator);
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

pub fn isVarInScope(parser: Parser, name: []const u8) bool {
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
    return lexer.raiseFor(buffer.items, lexer.token);
}

pub fn check(parser: *Parser, token_type: Token.Kind) !void {
    const lexer = &parser.lexer;
    if (token_type != lexer.token.type) {
        const message = try std.fmt.allocPrint(
            parser.allocator,
            "expecting token '{}', not '{}'",
            .{ token_type, lexer.token },
        );
        return lexer.raiseFor(message, lexer.token);
    }
}

pub fn checkIdentKeyword(parser: *Parser, value: Keyword) !void {
    const lexer = &parser.lexer;
    if (!lexer.token.isKeyword(value)) {
        const message = try std.fmt.allocPrint(parser.allocator, "expecting identifier '{}', not '{}'", .{ value, lexer.token });
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
    switch (lexer.token.value) {
        .string => |string| return string,
        else => {
            return parser.unexpectedToken();
        },
    }
}

pub fn unexpectedToken(parser: *Parser) error{ SyntaxError, OutOfMemory } {
    return parser.unexpectedTokenMsg(null);
}

pub fn unexpectedTokenMsg(parser: *Parser, msg: ?[]const u8) error{ SyntaxError, OutOfMemory } {
    const lexer = &parser.lexer;
    var buffer = ArrayList(u8).init(parser.allocator);
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
    const unclosed_stack = parser.unclosed_stack.items;
    if (unclosed_stack.len > 0) {
        const unclosed = unclosed_stack[unclosed_stack.len - 1];
        const message = try std.fmt.allocPrint(
            parser.allocator,
            "unterminated {s}",
            .{unclosed.name},
        );
        return lexer.raiseLoc(message, unclosed.location);
    }

    return parser.unexpectedToken();
}

pub fn isVar(parser: Parser, name: []const u8) bool {
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
    const arg_name = try std.fmt.allocPrint(parser.allocator, "__arg{}", .{parser.temp_arg_count});
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
    try parser.pushVar(try Var.new(parser.allocator, "fizz"));
    try parser.pushVars(blk: {
        var vars = ArrayList(Node).init(parser.allocator);
        try vars.append(try Var.new(parser.allocator, "buzz"));
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

    assert(canBeAssigned(try Var.new(parser.allocator, "foo")));
    assert(
        canBeAssigned(try Call.new(
            parser.allocator,
            null,
            "foo",
            ArrayList(Node).init(parser.allocator),
        )),
    );
    assert(
        canBeAssigned(try Call.new(
            parser.allocator,
            try Var.new(parser.allocator, "foo"),
            "[]",
            blk: {
                var args = ArrayList(Node).init(parser.allocator);
                try args.append(try Var.new(parser.allocator, "bar"));
                // try args.append(try Var.new(parser.allocator, "fizz"));
                break :blk args;
            },
        )),
    );

    parser = try Parser.new("foo.bar");
    lexer = &parser.lexer;
    try lexer.skipToken();
    var token_end_location = lexer.tokenEndLocation();
    var node = try Var.new(parser.allocator, "foo");
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
        try Return.new(parser.allocator),
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
    try lexer.skipToken();
    node = try parser.parseAtomic();
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
    try lexer.skipToken();
    node = try parser.parseAtomic();
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
    try lexer.skipToken();
    node = try parser.parseAtomic();
    assert(node == .char_literal);
    assert(node.char_literal.value == 'F');

    // .symbol
    parser = try Parser.new(":foo");
    lexer = &parser.lexer;
    try lexer.skipToken();
    node = try parser.parseAtomic();
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
