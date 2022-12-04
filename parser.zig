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

const Error = error{SyntaxError} ||
    error{ InvalidCharacter, Overflow } ||
    Allocator.Error ||
    Utf8EncodeError;

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
is_constant_assignment: bool = false,

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
assigned_vars: StringHashMap(void),

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
        .assigned_vars = StringHashMap(void).init(allocator),
    };
    try parser.pushIsolatedVarScope();
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
    defer parser.stop_on_do = old_stop_on_do;
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
//         return parser.unexpectedToken(.{});
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
    return parser.parseOpAssign(.{});
}

// parseExpressionSuffix
// parseExpressionSuffix

pub fn parseOpAssignNoControl(parser: *Parser, options: anytype) !Node {
    try parser.checkVoidExpressionKeyword();
    return parser.parseOpAssign(options);
}

pub fn parseOpAssign(
    parser: *Parser,
    options: struct {
        allow_ops: bool = true,
        allow_suffix: bool = true,
    },
) Error!Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const doc = lexer.token.doc();
    const location = lexer.token.location();
    const start_token = lexer.token;

    var atomic = try parser.parseQuestionColon();

    var allow_ops = options.allow_ops;
    while (true) {
        const name_location = lexer.token.location();

        switch (lexer.token.type) {
            .space => {
                try lexer.skipToken();
                continue;
            },
            .ident => {
                if (!options.allow_suffix)
                    return parser.unexpectedToken(.{});
                break;
            },
            .op_eq => {
                lexer.slash_is_regex = true;
                if (atomic == .call and
                    std.mem.eql(u8, atomic.call.name, "[]"))
                {
                    // TODO: implement
                    return parser.unexpectedToken(.{ .msg = "unimplemented" });
                } else {
                    if (!canBeAssigned(atomic)) break;

                    if (atomic == .path and
                        (parser.insideDef() or
                        parser.insideFun() or
                        parser.is_constant_assignment))
                    {
                        return lexer.raise("dynamic constant assignment. Constants can only be declared at the top level or inside other types.");
                    }

                    if (atomic == .path)
                        parser.is_constant_assignment = true;

                    if (atomic == .@"var" and
                        std.mem.eql(u8, atomic.@"var".name, "self"))
                    {
                        return lexer.raiseLoc(
                            "can't change the value of self",
                            location,
                        );
                    }

                    if (atomic == .call and
                        (std.mem.endsWith(u8, atomic.call.name, "?") or
                        std.mem.endsWith(u8, atomic.call.name, "!")))
                    {
                        return parser.unexpectedToken(.{ .token = start_token });
                    }

                    if (atomic == .call) {
                        const v = try Var.node(
                            allocator,
                            atomic.call.name,
                        );
                        v.copyLocation(atomic);
                        atomic = v;
                    }

                    try lexer.skipTokenAndSpaceOrNewline();

                    var needs_new_scope = false;
                    switch (atomic) {
                        .path => {
                            needs_new_scope = true;
                        },
                        .instance_var => {
                            needs_new_scope = parser.def_nest == 0;
                        },
                        .class_var => {
                            needs_new_scope = parser.def_nest == 0;
                        },
                        .@"var" => |v| {
                            if (v.isSpecialVar())
                                parser.assigns_special_var = true;
                        },
                        else => {
                            needs_new_scope = false;
                        }
                    }

                    const atomic_value = blk: {
                        if (needs_new_scope) try parser.pushIsolatedVarScope();
                        defer if (needs_new_scope) parser.popVarScope();

                        if (lexer.token.isKeyword(.uninitialized) and
                            (atomic == .@"var" or
                            atomic == .instance_var or
                            atomic == .class_var or
                            atomic == .global))
                        {
                            // TODO: implement
                            return parser.unexpectedToken(.{ .msg = "unimplemented" });
                        } else {
                            if (atomic == .@"var" and
                                !parser.isVar(atomic.@"var".name))
                            {
                                const name = atomic.@"var".name;
                                const already_assigned = parser.assigned_vars.contains(name);
                                if (!already_assigned)
                                    try parser.assigned_vars.put(name, {});
                                defer {
                                    if (!already_assigned)
                                        _ = parser.assigned_vars.remove(name);
                                }
                                break :blk try parser.parseOpAssignNoControl(.{});
                            } else {
                                break :blk try parser.parseOpAssignNoControl(.{});
                            }
                        }
                    };

                    if (atomic == .path)
                        parser.is_constant_assignment = false;

                    try parser.pushVar(atomic);

                    atomic = try Assign.node(
                        allocator,
                        atomic,
                        atomic_value,
                        .{ .doc = doc },
                    );
                    atomic.setLocation(location);
                    return atomic;
                }
            },
            else => |token_type| {
                if (token_type.isAssignmentOperator()) {
                    // TODO: implement
                    _ = name_location;
                    return parser.unexpectedToken(.{ .msg = "unimplemented" });
                } else {
                    break;
                }
            },
        }
        allow_ops = true;
    }

    return atomic;
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
                    left = try Call.node(
                        allocator,
                        left,
                        method,
                        args,
                        .{ .name_location = name_location },
                    );
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
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const location = lexer.token.location();

    var left = try parser.parseMulOrDiv();
    while (true) {
        switch (lexer.token.type) {
            .space => {
                try lexer.skipToken();
            },
            .op_plus, .op_minus, .op_amp_plus, .op_amp_minus => |op| {
                try parser.checkVoidValue(left, location);

                const method = op.toString();
                const name_location = lexer.token.location();
                // TODO: slash is never regex?
                try lexer.skipTokenAndSpaceOrNewline();
                const right = try parser.parseMulOrDiv();
                var args = ArrayList(Node).init(allocator);
                try args.append(right);
                left = try Call.node(
                    allocator,
                    left,
                    method,
                    args,
                    .{ .name_location = name_location },
                );
                left.setLocation(location);
                left.copyEndLocation(right);
            },
            .number => {
                switch (lexer.token.value.string[0]) {
                    '+', '-' => {
                        const method = lexer.token.value.string[0..1];
                        const name_location = lexer.token.location();

                        // Go back to the +/-, advance one char and continue from there
                        lexer.current_pos = lexer.token.start + 1;
                        try lexer.skipToken();

                        const right = try parser.parseMulOrDiv();
                        var args = ArrayList(Node).init(allocator);
                        try args.append(right);
                        left = try Call.node(
                            allocator,
                            left,
                            method,
                            args,
                            .{ .name_location = name_location },
                        );
                        left.setLocation(location);
                        left.copyEndLocation(right);
                    },
                    else => {
                        return left;
                    },
                }
            },
            else => {
                return left;
            },
        }
    }
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
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    // TODO: redundant variable?
    const name_location = lexer.token.location();
    switch (lexer.token.type) {
        .op_bang, .op_plus, .op_minus, .op_tilde, .op_amp_plus, .op_amp_minus => |token_type| {
            const location = lexer.token.location();
            try lexer.skipTokenAndSpaceOrNewline();
            try parser.checkVoidExpressionKeyword();
            const obj = try parser.parsePrefix();
            if (token_type == .op_bang) {
                const node = try Not.node(allocator, obj);
                node.setLocation(location);
                node.copyEndLocation(obj);
                return node;
            } else {
                const node = try Call.node(
                    allocator,
                    obj,
                    token_type.toString(),
                    ArrayList(Node).init(allocator),
                    .{ .name_location = name_location },
                );
                node.setLocation(location);
                node.copyEndLocation(obj);
                return node;
            }
        },
        else => {
            return parser.parseAtomicWithMethod();
        },
    }
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
        return parser.unexpectedToken(.{ .msg = "expected symbol" });
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
        .op_lparen => {
            return parser.parseParenthesizedExpression();
        },
        .op_lsquare_rsquare => {
            return parser.parseEmptyArrayLiteral();
        },
        .op_lsquare => {
            return parser.parseArrayLiteral();
        },
        .op_lcurly => {
            return parser.parseHashOrTupleLiteral(.{});
        },
        // TODO
        .number => {
            lexer.wants_regex = false;
            const value = lexer.token.value.string;
            const kind = lexer.token.number_kind;
            const node = try NumberLiteral.node(allocator, value, kind);
            try parser.skipNodeToken(node);
            return node;
        },
        .char => {
            const value = lexer.token.value.char;
            const node = try CharLiteral.node(allocator, value);
            try parser.skipNodeToken(node);
            return node;
        },
        .string => {
            // TODO: is it ever possible to have .string here?
            unreachable;
        },
        .delimiter_start => {
            return parser.parseDelimiter(.{});
        },
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
        .ident => {
            switch (lexer.token.value) {
                .keyword => |keyword| {
                    switch (keyword) {
                        // TODO
                        .true => {
                            return try parser.checkTypeDeclaration() orelse {
                                const node = try BoolLiteral.node(allocator, true);
                                try parser.skipNodeToken(node);
                                return node;
                            };
                        },
                        .false => {
                            return try parser.checkTypeDeclaration() orelse {
                                const node = try BoolLiteral.node(allocator, false);
                                try parser.skipNodeToken(node);
                                return node;
                            };
                        },
                        // TODO
                        .typeof => {
                            return try parser.checkTypeDeclaration() orelse {
                                return try parser.parseTypeof();
                            };
                        },
                        // TODO
                        else => {
                            const node = try parser.parseVarOrCall(.{});
                            parser.setVisibility(node);
                            return node;
                        },
                    }
                },
                else => {
                    const node = try parser.parseVarOrCall(.{});
                    parser.setVisibility(node);
                    return node;
                },
            }
        },
        // TODO
        .class_var => {
            return parser.newNodeCheckTypeDeclaration(ClassVar);
        },
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

pub fn checkTypeDeclaration(parser: *Parser) !?Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    if (parser.nextComesColonSpace()) {
        const name = lexer.token.nameToString();
        const v = try Var.node(allocator, name);
        v.setLocation(lexer.token.location());
        try lexer.skipTokenAndSpace();
        try parser.check(.op_colon);
        const node = try parser.parseTypeDeclaration(v);
        parser.setVisibility(node);
        return node;
    } else {
        return null;
    }
}

pub fn parseTypeDeclaration(parser: *Parser, v: Node) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    try lexer.skipTokenAndSpaceOrNewline();
    const var_type = try parser.parseBareProcType();
    try lexer.skipSpace();
    var value: ?Node = null;
    if (lexer.token.type == .op_eq) {
        try lexer.skipTokenAndSpaceOrNewline();
        value = try parser.parseOpAssignNoControl(.{});
    }
    const node = try TypeDeclaration.node(allocator, v, var_type, value);
    node.copyLocation(v);
    return node;
}

pub fn nextComesColonSpace(parser: *Parser) bool {
    const lexer = &parser.lexer;

    if (parser.no_type_declaration == 0) return false;

    const pos = lexer.current_pos;
    defer lexer.current_pos = pos;

    while (std.ascii.isWhitespace(lexer.currentChar())) {
        lexer.skipChar(.{ .column_increment = false });
    }
    if (lexer.currentChar() == ':') {
        lexer.skipChar(.{ .column_increment = false });
        return std.ascii.isWhitespace(lexer.currentChar());
    }
    return false;
}

pub fn newNodeCheckTypeDeclaration(
    parser: *Parser,
    comptime Exp: type,
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const name = lexer.token.value.string;
    const v = try Exp.node(allocator, name);
    v.setLocation(lexer.token.location());
    v.setEndLocation(lexer.tokenEndLocation());
    lexer.wants_regex = false;
    try lexer.skipTokenAndSpace();

    if (parser.no_type_declaration == 0 and
        lexer.token.type == .op_colon)
    {
        return parser.parseTypeDeclaration(v);
    } else {
        return v;
    }
}

// parseGenericOrCustomLiteral
// parseCustomLiteral
// checkNotInsideDef

pub fn insideDef(parser: *const Parser) bool {
    return parser.def_nest > 0;
}

pub fn insideFun(parser: *const Parser) bool {
    return parser.fun_nest > 0;
}

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

pub fn parseParenthesizedExpression(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const location = lexer.token.location();
    lexer.slash_is_regex = true;
    try lexer.skipTokenAndSpaceOrNewline();

    if (lexer.token.type == .op_rparen) {
        const end_location = lexer.tokenEndLocation();
        var expressions = ArrayList(Node).init(allocator);
        try expressions.append(try Nop.node(allocator));
        const node = try Expressions.node(
            allocator,
            expressions,
            .{ .keyword = .paren },
        );
        node.setLocation(location);
        node.setEndLocation(end_location);
        try parser.skipNodeToken(node);
        return node;
    }

    // TODO: implement
    return parser.unexpectedToken(.{ .msg = "unimplemented" });
}

// parseFunLiteral
// checkNotPipeBeforeProcLiteralBody
// parseFunLiteralParam
// parseFunPointer

pub const Piece = struct {
    value: union(enum) {
        string: []const u8,
        node: Node,
    },
    line_number: usize,

    pub fn string(value: []const u8, line_number: usize) Piece {
        return .{
            .value = .{ .string = value },
            .line_number = line_number,
        };
    }

    pub fn node(value: Node, line_number: usize) Piece {
        return .{
            .value = .{ .node = value },
            .line_number = line_number,
        };
    }
};

pub fn parseDelimiter(
    parser: *Parser,
    options: struct { want_skip_space: bool = true },
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    if (lexer.token.type == .string) {
        // TODO: is it ever possible to have .string here?
        unreachable;
    }

    const location = lexer.token.location();
    var delimiter_state = lexer.token.delimiter_state;

    try parser.check(.delimiter_start);

    if (delimiter_state.delimiters == .heredoc) {
        // TODO: implement
        return parser.unexpectedToken(.{ .msg = "unimplemented" });
    }

    _ = try lexer.nextStringToken(delimiter_state);
    delimiter_state = lexer.token.delimiter_state;

    var pieces = ArrayList(Piece).init(allocator);
    var has_interpolation = false;

    var token_end_location: Location = undefined;
    try parser.consumeDelimiter(
        &pieces,
        &delimiter_state,
        &has_interpolation,
        &token_end_location,
    );

    if (options.want_skip_space and delimiter_state.delimiters == .string) {
        while (true) {
            const passed_backslash_newline = lexer.token.passed_backslash_newline;
            try lexer.skipSpace();

            if (passed_backslash_newline and
                lexer.token.type == .delimiter_start and
                lexer.token.delimiter_state.delimiters == .string)
            {
                _ = try lexer.nextStringToken(delimiter_state);
                delimiter_state = lexer.token.delimiter_state;
                try parser.consumeDelimiter(
                    &pieces,
                    &delimiter_state,
                    &has_interpolation,
                    &token_end_location,
                );
            } else {
                break;
            }
        }
    }

    var result: Node = undefined;
    if (has_interpolation) {
        // TODO: implement
        return parser.unexpectedToken(.{ .msg = "unimplemented" });
    } else {
        const string = try parser.combinePieces(pieces, delimiter_state);
        result = try StringLiteral.node(allocator, string);
    }

    switch (delimiter_state.delimiters) {
        .command => {
            var args = ArrayList(Node).init(allocator);
            try args.append(result);
            result = try Call.node(allocator, null, "`", args, .{});
            result.setLocation(location);
        },
        .regex => {
            // TODO: implement
            return parser.unexpectedToken(.{ .msg = "unimplemented" });
        },
        else => {
            // no special treatment
        },
    }

    result.setEndLocation(token_end_location);

    return result;
}

// fn combineInterpolationPieces

fn combinePieces(
    parser: *Parser,
    pieces: ArrayList(Piece),
    delimiter_state: Token.DelimiterState,
) ![]const u8 {
    const allocator = parser.lexer.allocator;
    if (needsHeredocIndentRemoved(delimiter_state)) {
        // TODO: implement
        return parser.unexpectedToken(.{ .msg = "unimplemented" });
    } else {
        var buffer = ArrayList(u8).init(allocator);
        for (pieces.items) |piece| {
            try buffer.appendSlice(piece.value.string);
        }
        return buffer.items;
    }
}

pub fn consumeDelimiter(
    parser: *Parser,
    pieces: *ArrayList(Piece),
    delimiter_state: *Token.DelimiterState,
    has_interpolation: *bool,
    token_end_location: *Location,
) !void {
    const lexer = &parser.lexer;

    while (true) {
        switch (lexer.token.type) {
            .string => {
                try pieces.append(Piece.string(
                    lexer.token.value.string,
                    lexer.token.line_number,
                ));

                _ = try lexer.nextStringToken(delimiter_state.*);
                delimiter_state.* = lexer.token.delimiter_state;
            },
            .delimiter_end => {
                if (delimiter_state.delimiters == .regex) {
                    // TODO: implement
                    return parser.unexpectedToken(.{ .msg = "unimplemented" });
                }
                token_end_location.* = lexer.tokenEndLocation();
                try lexer.skipToken();
                break;
            },
            .eof => {
                switch (delimiter_state.delimiters) {
                    .command => {
                        return lexer.raise("Unterminated command");
                    },
                    .regex => {
                        return lexer.raise("Unterminated regular expression");
                    },
                    .heredoc => {
                        return lexer.raise("Unterminated heredoc");
                    },
                    else => {
                        return lexer.raise("Unterminated string literal");
                    },
                }
            },
            else => {
                const line_number = lexer.token.line_number;
                delimiter_state.* = lexer.token.delimiter_state;
                try lexer.skipTokenAndSpaceOrNewline();
                const old_inside_interpolation = parser.inside_interpolation;
                defer parser.inside_interpolation = old_inside_interpolation;
                parser.inside_interpolation = true;
                const exp = blk: {
                    const old_stop_on_do = parser.replaceStopOnDo(false);
                    defer parser.stop_on_do = old_stop_on_do;
                    break :blk try parser.parseExpression();
                };

                // We cannot reduce `StringLiteral` of interpolation inside heredoc into `String`
                // because heredoc try to remove its indentation.
                if (exp == .string_literal and delimiter_state.delimiters != .heredoc) {
                    try pieces.append(Piece.string(exp.string_literal.value, line_number));
                } else {
                    try pieces.append(Piece.node(exp, line_number));
                    has_interpolation.* = true;
                }

                try lexer.skipSpaceOrNewline();
                if (lexer.token.type != .op_rcurly) {
                    return lexer.raise("Unterminated string interpolation");
                }

                lexer.token.delimiter_state = delimiter_state.*;
                _ = try lexer.nextStringToken(delimiter_state.*);
                delimiter_state.* = lexer.token.delimiter_state;
            },
        }
    }
}

// consumeRegexOptions
// consumeHeredocs
// consumeHeredoc

pub fn needsHeredocIndentRemoved(delimiter_state: Token.DelimiterState) bool {
    return delimiter_state.delimiters == .heredoc and
        delimiter_state.heredoc_indent >= 0;
}

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

    return ArrayLiteral.node(
        allocator,
        strings,
        .{ .of = try Path.global(allocator, elements_type) },
    );
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
        const node = try ArrayLiteral.node(
            allocator,
            ArrayList(Node).init(allocator),
            .{ .of = of },
        );
        node.copyEndLocation(of);
        return node;
    } else {
        return lexer.raiseAt("for empty arrays use '[] of ElementType'", line, column);
    }
}

pub fn parseArrayLiteral(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const line = lexer.line_number;
    const column = lexer.token.column_number;

    lexer.slash_is_regex = true;

    var exps = ArrayList(Node).init(allocator);
    var end_location: ?Location = null;

    try parser.open("array literal");
    {
        defer parser.close();

        try lexer.skipTokenAndSpaceOrNewline();
        while (lexer.token.type != .op_rsquare) {
            const exp_location = lexer.token.location();

            if (lexer.token.type == .op_star) {
                try lexer.skipTokenAndSpaceOrNewline();
                const exp = try Splat.node(
                    allocator,
                    try parser.parseOpAssignNoControl(.{}),
                );
                exp.setLocation(exp_location);
                try exps.append(exp);
            } else {
                try exps.append(
                    try parser.parseOpAssignNoControl(.{}),
                );
            }

            end_location = lexer.tokenEndLocation();
            try lexer.skipSpace();

            if (lexer.token.type == .op_comma) {
                lexer.slash_is_regex = true;
                try lexer.skipTokenAndSpaceOrNewline();
            } else {
                try lexer.skipSpaceOrNewline();
                try parser.check(.op_rsquare);
                break;
            }
        }
        lexer.wants_regex = false;
        try lexer.skipTokenAndSpace();
    }

    var of: ?Node = null;
    if (lexer.token.isKeyword(.of)) {
        try lexer.skipTokenAndSpaceOrNewline();
        const t = try parser.parseBareProcType();
        of = t;
        end_location = t.endLocation();
    } else if (exps.items.len == 0) {
        return lexer.raiseAt(
            "for empty arrays use '[] of ElementType'",
            line,
            column,
        );
    }

    const node = try ArrayLiteral.node(
        allocator,
        exps,
        .{ .of = of },
    );
    node.setEndLocation(end_location);
    return node;
}

pub fn parseHashOrTupleLiteral(
    parser: *Parser,
    options: struct { allow_of: bool = true },
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const location = lexer.token.location();
    const line = lexer.line_number;
    const column = lexer.token.column_number;

    lexer.slash_is_regex = true;
    try lexer.skipTokenAndSpaceOrNewline();

    if (lexer.token.type == .op_rcurly) {
        var end_location: ?Location = lexer.tokenEndLocation();
        try lexer.skipTokenAndSpace();
        return parser.newHashLiteral(
            ArrayList(HashLiteral.Entry).init(allocator),
            line,
            column,
            &end_location,
            .{},
        );
    }

    if (parser.nextComesNamedTupleStart()) {
        // TODO: implement
        return parser.unexpectedToken(.{ .msg = "unimplemented" });
    }

    var first_is_splat = false;
    if (lexer.token.type == .op_star) {
        first_is_splat = true;
        try lexer.skipTokenAndSpaceOrNewline();
    }

    const key_location = lexer.token.location();
    var first_key = try parser.parseOpAssignNoControl(.{});
    if (first_is_splat) {
        first_key = try Splat.node(allocator, first_key);
        first_key.setLocation(location);
    }
    switch (lexer.token.type) {
        .op_colon => {
            // TODO: implement
            _ = key_location;
            return parser.unexpectedToken(.{ .msg = "unimplemented" });
        },
        .op_comma => {
            // TODO: implement
            return parser.unexpectedToken(.{ .msg = "unimplemented" });
        },
        .op_rcurly => {
            // TODO: implement
            return parser.unexpectedToken(.{ .msg = "unimplemented" });
        },
        .newline => {
            // TODO: implement
            return parser.unexpectedToken(.{ .msg = "unimplemented" });
        },
        else => {
            if (first_is_splat)
                return parser.unexpectedToken(.{});
            try parser.check(.op_eq_gt);
        },
    }
    lexer.slash_is_regex = true;
    // TODO: should skip newline too
    try lexer.skipTokenAndSpace();
    return parser.parseHashLiteral(
        first_key,
        location,
        options.allow_of,
    );
}

pub fn parseHashLiteral(
    parser: *Parser,
    first_key: Node,
    location: Location,
    allow_of: bool,
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const line = lexer.line_number;
    const column = lexer.token.column_number;
    var end_location: ?Location = null;

    var entries = ArrayList(HashLiteral.Entry).init(allocator);
    try entries.append(HashLiteral.Entry.init(
        first_key,
        try parser.parseOpAssign(.{}),
    ));

    if (lexer.token.type == .newline) {
        try lexer.skipTokenAndSpaceOrNewline();
        try parser.check(.op_rcurly);
        try lexer.skipTokenAndSpace();
    } else {
        try parser.openAt("hash literal", location);
        {
            defer parser.close();

            try lexer.skipSpaceOrNewline();
            if (lexer.token.type == .op_comma) {
                lexer.slash_is_regex = true;
                try lexer.skipTokenAndSpaceOrNewline();
            } else {
                try lexer.skipSpaceOrNewline();
                try parser.check(.op_rcurly);
            }

            while (lexer.token.type != .op_rcurly) {
                // TODO: implement
                return parser.unexpectedToken(.{ .msg = "unimplemented" });
            }
            end_location = lexer.tokenEndLocation();
            try lexer.skipTokenAndSpace();
        }
    }

    return parser.newHashLiteral(
        entries,
        line,
        column,
        &end_location,
        .{ .allow_of = allow_of },
    );
}

pub fn nextComesNamedTupleStart(parser: *const Parser) bool {
    const lexer = &parser.lexer;
    return switch (lexer.token.type) {
        .ident, .@"const" => lexer.currentChar() == ':' and lexer.peekNextChar() != ':',
        else => false,
    };
}

// atStringLiteralStart
// parseTuple

pub fn newHashLiteral(
    parser: *Parser,
    entries: ArrayList(HashLiteral.Entry),
    line: usize,
    column: usize,
    end_location: *?Location,
    options: struct { allow_of: bool = true },
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    var of: ?HashLiteral.Entry = null;

    if (options.allow_of) {
        if (lexer.token.isKeyword(.of)) {
            try lexer.skipTokenAndSpaceOrNewline();
            const of_key = try parser.parseBareProcType();
            try parser.check(.op_eq_gt);
            try lexer.skipTokenAndSpaceOrNewline();
            const of_value = try parser.parseBareProcType();
            of = HashLiteral.Entry.init(of_key, of_value);
            end_location.* = of_value.endLocation();
        }

        if (entries.items.len == 0 and of == null) {
            return lexer.raiseAt(
                "for empty hashes use '{} of KeyType => ValueType'",
                line,
                column,
            );
        }
    }

    const node = try HashLiteral.node(allocator, entries, of);
    node.setEndLocation(end_location.*);
    return node;
}

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

pub fn setVisibility(parser: *const Parser, node: Node) void {
    // TODO: implement
    _ = parser;
    _ = node;
}

pub fn parseVarOrCall(
    parser: *Parser,
    options: struct {
        is_global: bool = false,
        force_call: bool = false,
    },
) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const location = lexer.token.location();
    var end_location = lexer.tokenEndLocation();
    const doc = lexer.token.doc();

    if (lexer.token.type == .op_bang) {
        const obj = try Var.node(allocator, "self");
        obj.setLocation(location);
        // TODO: implement
        return parser.unexpectedToken(.{ .msg = "unimplemented" });
        // return parser.parseNegationSuffix(obj);
    }

    if (lexer.token.value == .keyword) {
        switch (lexer.token.value.keyword) {
            .is_a_question => {
                // TODO: implement
                return parser.unexpectedToken(.{ .msg = "unimplemented" });
            },
            .as => {
                // TODO: implement
                return parser.unexpectedToken(.{ .msg = "unimplemented" });
            },
            .as_question => {
                // TODO: implement
                return parser.unexpectedToken(.{ .msg = "unimplemented" });
            },
            .responds_to_question => {
                // TODO: implement
                return parser.unexpectedToken(.{ .msg = "unimplemented" });
            },
            .nil_question => {
                // TODO: implement
                return parser.unexpectedToken(.{ .msg = "unimplemented" });
            },
            else => {
                // Not a special call, go on
            },
        }
    }

    const is_global = options.is_global;
    var force_call = options.force_call;

    const name_location = lexer.token.location();
    const name = if (force_call and lexer.token.value == .none)
        lexer.token.type.toString()
    else
        lexer.token.nameToString();

    const is_var = parser.isVar(name);

    // If the name is a var and '+' or '-' follow, never treat the name as a call
    if (is_var and parser.nextComesPlusOrMinus()) {
        const v = try Var.node(allocator, name);
        // TODO: add Var#doc property
        _ = doc;
        v.setLocation(name_location);
        v.setEndLocation(end_location);
        try lexer.skipToken();
        return v;
    }

    lexer.wants_regex = false;
    try lexer.skipToken();

    if (lexer.token.type == .space) {
        lexer.wants_regex = !is_var;
    }

    if (std.mem.eql(u8, "super", name)) {
        parser.calls_super = true;
    } else if (std.mem.eql(u8, "initialize", name)) {
        parser.calls_initialize = true;
    } else if (std.mem.eql(u8, "previous_def", name)) {
        parser.calls_previous_def = true;
    } else {
        // Not a special call
    }

    const call_args = blk: {
        const old_stop_on_do = parser.stop_on_do;
        defer parser.stop_on_do = old_stop_on_do;
        break :blk parser.parseCallArgs(.{
            .stop_on_do_after_space = parser.stop_on_do,
        });
    };

    var args: ?ArrayList(Node) = null;
    var block: ?*Block = null;
    var block_arg: ?Node = null;
    var named_args: ?ArrayList(*NamedArgument) = null;
    var stopped_on_do_after_space = false;
    var has_parentheses = false;

    if (call_args) |c| {
        args = c.args;
        block = c.block;
        block_arg = c.block_arg;
        named_args = c.named_args;
        stopped_on_do_after_space = c.stopped_on_do_after_space;
        has_parentheses = c.has_parentheses;
        if (!force_call) {
            force_call = has_parentheses;
        }
        if (!force_call) {
            if (args) |a|
                force_call = a.items.len > 0;
        }
        if (!force_call) {
            if (named_args) |n|
                force_call = n.items.len > 0;
        }
    }

    if (stopped_on_do_after_space) {
        block = try parser.parseCurlyBlock(block);
    } else if (parser.stop_on_do and has_parentheses) {
        block = try parser.parseCurlyBlock(block);
    } else {
        block = try parser.parseBlock(block, .{});
    }

    if (block != null and block_arg != null) {
        return lexer.raiseLoc(
            "can't use captured and non-captured blocks together",
            location,
        );
    }

    const node = blk: {
        if (block != null or block_arg != null or is_global) {
            // TODO: implement
            return parser.unexpectedToken(.{ .msg = "unimplemented" });
        } else {
            if (args) |a| {
                // TODO: implement
                _ = a;
                return parser.unexpectedToken(.{ .msg = "unimplemented" });
            } else {
                if (parser.no_type_declaration == 0 and
                    lexer.token.type == .op_colon)
                {
                    // TODO: implement
                    return parser.unexpectedToken(.{ .msg = "unimplemented" });
                } else if (!force_call and is_var) {
                    // TODO: implement
                    return parser.unexpectedToken(.{ .msg = "unimplemented" });
                } else {
                    if (!force_call and
                        named_args == null and
                        !is_global and
                        parser.assigned_vars.contains(name))
                    {
                        return lexer.raiseLoc(
                            try std.fmt.allocPrint(
                                allocator,
                                "can't use variable name '{0s}' inside assignment to variable '{0s}'",
                                .{name},
                            ),
                            location,
                        );
                    }

                    break :blk try Call.node(
                        allocator,
                        null,
                        name,
                        ArrayList(Node).init(allocator),
                        .{
                            .named_args = named_args,
                            .is_global = is_global,
                            .name_location = name_location,
                            .has_parentheses = has_parentheses,
                        },
                    );
                }
            }
        }
    };

    // TODO: node.setDoc(doc);
    node.setLocation(location);
    if (block != null and block.?.end_location != null) {
        end_location = block.?.end_location.?;
    } else if (call_args != null and call_args.?.end_location != null) {
        end_location = call_args.?.end_location.?;
    }
    node.setEndLocation(end_location);
    return node;
}

pub fn nextComesPlusOrMinus(parser: *Parser) bool {
    const lexer = &parser.lexer;

    const pos = lexer.current_pos;
    defer lexer.current_pos = pos;

    while (std.ascii.isWhitespace(lexer.currentChar())) {
        lexer.skipChar(.{ .column_increment = false });
    }
    return switch (lexer.currentChar()) {
        '+', '-' => true,
        else => false,
    };
}

pub fn replaceStopOnDo(parser: *Parser, new_value: bool) bool {
    const old_stop_on_do = parser.stop_on_do;
    parser.stop_on_do = new_value;
    return old_stop_on_do;
}

pub fn parseBlock(
    parser: *Parser,
    block: ?*Block,
    options: struct { stop_on_do: bool = false },
) !?*Block {
    // TODO: implement
    _ = options;
    return parser.parseCurlyBlock(block);
}

pub fn parseCurlyBlock(parser: *Parser, block: ?*Block) !?*Block {
    // TODO: implement
    _ = parser;
    return block;
}

// parseBlock2

const CallArgs = struct {
    args: ?ArrayList(Node),
    block: ?*Block,
    block_arg: ?Node,
    named_args: ?ArrayList(*NamedArgument),
    stopped_on_do_after_space: bool,
    end_location: ?Location,
    has_parentheses: bool,

    fn init(
        args: ?ArrayList(Node),
        block: ?*Block,
        block_arg: ?Node,
        named_args: ?ArrayList(*NamedArgument),
        stopped_on_do_after_space: bool,
        end_location: ?Location,
        has_parentheses: bool,
    ) CallArgs {
        return .{
            .args = args,
            .block = block,
            .block_arg = block_arg,
            .named_args = named_args,
            .stopped_on_do_after_space = stopped_on_do_after_space,
            .end_location = end_location,
            .has_parentheses = has_parentheses,
        };
    }
};

pub fn parseCallArgs(
    parser: *Parser,
    options: struct {
        stop_on_do_after_space: bool = false,
        allow_curly: bool = false,
        control: bool = false,
    },
) ?CallArgs {
    // TODO: implement
    _ = parser;
    _ = options;
    return null;
}

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
            try parser.nextComesTypeStart(.{ .consume_newlines = true });
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
                try parser.nextComesTypeStart(.{ .consume_newlines = true });
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
                return parser.parseTypeof();
            }
            return parser.unexpectedToken(.{});
        },
        .underscore => {
            try lexer.skipTokenAndSpace();
            const node = try Underscore.node(allocator);
            node.setLocation(location);
            return node;
        },
        // TODO
        else => {
            return parser.unexpectedToken(.{});
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

pub fn parseTypeArg(parser: *Parser) Error!Node {
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

    const has_output_type = try parser.nextComesTypeStart(.{ .consume_newlines = false });

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

pub fn nextComesTypeStart(
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
            if (parser.nextComesNamedTupleStart()) {
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
            if (parser.nextComesNamedTupleStart()) {
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

pub fn parseTypeof(parser: *Parser) !Node {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const location = lexer.token.location();

    try lexer.skipTokenAndSpace();
    try parser.check(.op_lparen);
    try lexer.skipTokenAndSpaceOrNewline();
    if (lexer.token.type == .op_rparen) {
        return lexer.raise("missing typeof argument");
    }

    try parser.pushLexicalVarScope();
    defer parser.popVarScope();

    var exps = ArrayList(Node).init(allocator);
    while (lexer.token.type != .op_rparen) {
        try exps.append(try parser.parseOpAssign(.{}));
        if (lexer.token.type == .op_comma) {
            try lexer.skipTokenAndSpaceOrNewline();
        } else {
            try lexer.skipSpaceOrNewline();
            try parser.check(.op_rparen);
        }
    }

    const end_location = lexer.tokenEndLocation();
    try lexer.skipTokenAndSpace();

    const node = try TypeOf.node(allocator, exps);
    node.setLocation(location);
    node.setEndLocation(end_location);
    return node;
}
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
//     const first = try Var.node(allocator, lexer.token.nameToString());
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
            // TODO: check if [] has parentheses
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
        .ident, .@"const" => lexer.token.nameToString(),
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

pub fn pushIsolatedVarScope(parser: *Parser) !void {
    const allocator = parser.lexer.allocator;
    const scope = StringHashMap(void).init(allocator);
    try parser.var_scopes.append(scope);
}

pub fn pushLexicalVarScope(parser: *Parser) !void {
    const var_scopes = parser.var_scopes.items;
    const current_scope = try var_scopes[var_scopes.len - 1].clone();
    try parser.var_scopes.append(current_scope);
}

pub fn popVarScope(parser: *Parser) void {
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
    return lexer.token.nameToString();
}

pub fn checkConst(parser: *Parser) ![]const u8 {
    const lexer = &parser.lexer;
    try parser.check(.@"const");
    return lexer.token.value.string;
}

pub fn unexpectedToken(
    parser: *Parser,
    options: struct {
        msg: ?[]const u8 = null,
        token: ?Token = null,
    },
) error{ SyntaxError, OutOfMemory } {
    const lexer = &parser.lexer;
    const allocator = lexer.allocator;

    const token = options.token orelse lexer.token;
    var buffer = ArrayList(u8).init(allocator);
    var writer = buffer.writer();
    try writer.writeAll("unexpected token: ");
    if (token.type == .eof) {
        try writer.writeAll("EOF");
    } else {
        try writer.writeByte('"');
        try token.toString(writer);
        try writer.writeByte('"');
    }
    if (options.msg) |msg| {
        try writer.writeAll(" (");
        try writer.writeAll(msg);
        try writer.writeByte(')');
    }
    return lexer.raiseFor(buffer.items, token);
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

    return parser.unexpectedToken(.{});
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
const Main = struct {
var parser: Parser = undefined;
fn _main() !void {
    const p = @import("std").debug.print;
    const assert = @import("std").debug.assert;
    p("", .{});
    parser = try Parser.new("foo");
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

    try parser.pushIsolatedVarScope();
    var_scopes = parser.var_scopes.items;
    assert(var_scopes.len == 2);
    assert(var_scopes[var_scopes.len - 1].count() == 0);

    parser.popVarScope();
    var_scopes = parser.var_scopes.items;
    assert(var_scopes.len == 1);
    assert(var_scopes[var_scopes.len - 1].count() == 3);

    try parser.pushLexicalVarScope();
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
    parser = try Parser.new("true || false");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .@"or");
    assert(node.@"or".left == .bool_literal);
    assert(node.@"or".left.bool_literal.value == true);
    assert(node.@"or".right == .bool_literal);
    assert(node.@"or".right.bool_literal.value == false);

    // parseAddOrSub
    parser = try Parser.new("12 + 34");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .call);
    assert(node.call.obj.? == .number_literal);
    assert(std.mem.eql(u8, node.call.obj.?.number_literal.value, "12"));
    assert(std.mem.eql(u8, node.call.name, "+"));
    assert(node.call.args.items.len == 1);
    assert(node.call.args.items[0] == .number_literal);
    assert(std.mem.eql(u8, node.call.args.items[0].number_literal.value, "34"));

    parser = try Parser.new("12-34");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .call);

    // parsePrefix
    parser = try Parser.new("!false");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .not);
    assert(node.not.exp == .bool_literal);
    assert(node.not.exp.bool_literal.value == false);

    // checkTypeDeclaration
    parser = try Parser.new("false : self");
    parser.no_type_declaration = 1;
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .type_declaration);

    // parseDelimiter
    parser = try Parser.new("\"foo\"");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .string_literal);
    assert(std.mem.eql(u8, node.string_literal.value, "foo"));

    parser = try Parser.new("%q(\"foo\" bar)");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .string_literal);
    assert(std.mem.eql(u8, node.string_literal.value, "\"foo\" bar"));

    // parseParenthesizedExpression
    parser = try Parser.new("()");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .expressions);
    assert(node.expressions.expressions.items.len == 1);
    assert(node.expressions.expressions.items[0] == .nop);

    // parseArrayLiteral
    parser = try Parser.new("[1, 2]");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .array_literal);
    assert(node.array_literal.elements.items.len == 2);
    assert(node.array_literal.elements.items[0] == .number_literal);
    assert(node.array_literal.elements.items[1] == .number_literal);

    // parseHashOrTupleLiteral
    parser = try Parser.new("{} of self => _");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .hash_literal);
    assert(node.hash_literal.of.?.key == .self);
    assert(node.hash_literal.of.?.value == .underscore);

    // parseHashLiteral
    parser = try Parser.new("{1 => 2\n}");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .hash_literal);

    parser = try Parser.new("{1 => 2,}");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .hash_literal);

    parser = try Parser.new("{1 => 2}");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .hash_literal);

    // parseTypeof
    parser = try Parser.new("[] of typeof(123)");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .array_literal);
    node = node.array_literal.of.?;
    assert(node == .type_of);
    assert(node.type_of.expressions.items.len == 1);
    node = node.type_of.expressions.items[0];
    assert(node == .number_literal);

    parser = try Parser.new("typeof(123)");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .type_of);
    assert(node.type_of.expressions.items.len == 1);
    node = node.type_of.expressions.items[0];
    assert(node == .number_literal);

    // parseVarOrCall
    parser = try Parser.new("foo");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .call);

    // parseOpAssign + Assign.node
    parser = try Parser.new("foo = bar");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .assign);

    parser = try Parser.new("@@foo = bar");
    lexer = &parser.lexer;
    node = try parser.parse();
    assert(node == .assign);

    // p("{}\n", .{});

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Success\n", .{});
}
};
pub fn main() !void {
    Main._main() catch |err| {
        switch (err) {
            error.SyntaxError => {
                std.debug.print("Error: {s}\n", .{Main.parser.lexer.error_message.?});
                std.debug.print("Press Enter to see stack trace...", .{});
                const stdin = std.io.getStdIn().reader();
                try stdin.skipUntilDelimiterOrEof('\n');
                return err;
            },
            else => return err,
        }
    };
}
