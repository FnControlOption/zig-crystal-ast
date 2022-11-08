const Parser = @This();

const std = @import("std");
const Lexer = @import("lexer.zig");
const Location = @import("location.zig");
const Token = @import("token.zig");

const ast = @import("ast.zig");
const Node = ast.Node;
const Expressions = ast.Expressions;

const Unclosed = struct {
    name: []const u8,
    location: Location,
};

allocator: std.mem.Allocator,

lexer: Lexer,
unclosed_stack: std.ArrayList(Unclosed),
calls_super: bool = false,
calls_initialize: bool = false,
calls_previous_def: bool = false,
uses_block_arg: bool = false,
is_macro_def: bool = false,
assigns_special_var: bool = false,
def_nest: usize = 0,
fun_nest: usize = 0,
type_nest: usize = 0,

call_args_start_locations: std.ArrayList(Location),
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
assigned_vars: std.ArrayList([]const u8),

pub fn new(string: []const u8) Parser {
    const allocator = std.heap.page_allocator; // TODO
    return .{
        .allocator = allocator,
        .lexer = .{ .allocator = allocator, .string = string },
        .unclosed_stack = std.ArrayList(Unclosed).init(allocator),
        .call_args_start_locations = std.ArrayList(Location).init(allocator),
        .assigned_vars = std.ArrayList([]const u8).init(allocator),
    };
}

pub fn parse(parser: *Parser) !Node {
    try parser.lexer.nextTokenSkipStatementEnd();

    const value = try parser.parseExpressions();
    try parser.check(.eof);
    return value;
}

// parse

fn parseExpressions(parser: *Parser) !Node {
    const old_stop_on_do = parser.stop_on_do;
    defer parser.stop_on_do = old_stop_on_do;
    parser.stop_on_do = false;
    return parser.parseExpressionsInternal();
}

fn parseExpressionsInternal(parser: *Parser) !Node {
    var exps = std.ArrayList(Node).init(parser.allocator);
    return Expressions.from(parser.allocator, exps);
}

// parseMultiAssign
// isMultiAssignTarget
// isMultiAssignMiddle
// multiassignLeftHand
// parseExpression
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
// nextComesColonSpace
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
// nodeAndNextToken
// isEndToken
// canBeAssigned
// consumeDefOrMacroName
// consumeDefEqualsSignSkipSpace
// withIsolatedVarScope
// withLexicalVarScope
// pushVars
// pushVar
// pushVar
// pushVarName
// pushVar
// isVarInScope
// open
// checkVoidValue
// checkVoidExpressionKeyword
// check
// check
// checkIdent
// checkIdent
// checkConst
// unexpectedToken
// unexpectedTokenInAtomic
// isVar
// pushVisibility
// nextToken
// tempArgName

fn check(parser: *Parser, token_type: Token.Kind) !void {
    if (token_type != parser.lexer.token.type) {
        const message = try std.fmt.allocPrint(
            parser.allocator,
            "expecting token '{}', not '{}'",
            .{ token_type, parser.lexer.token },
        );
        try parser.lexer.raise(message);
    }
}

pub fn main() !void {
    const p = @import("std").debug.print;
    // const assert = @import("std").debug.assert;
    p("{}\n", .{@TypeOf(Parser.new("foo"))});
    var parser = Parser.new("\n; foo");
    if (parser.parse()) |_| unreachable else |err| p("{} {?s}\n", .{err, parser.lexer.error_message});
    parser = Parser.new("+=");
    parser.stop_on_do = true;
    p("{}\n", .{parser.stop_on_do});
    if (parser.parse()) |_| unreachable else |err| p("{} {?s}\n", .{err, parser.lexer.error_message});
    p("{}\n", .{parser.stop_on_do});
    // try parser.parse();
    // p("{}\n", .{parser.lexer.token.type});
    // assert(parser.lexer.token.type == .ident);
}
