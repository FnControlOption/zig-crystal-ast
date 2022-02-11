const std = @import("std");

pub const ASTNode = union(enum) {
    alias: *Alias,
    and_: *And,
    annotation: *Annotation,
    annotation_def: *AnnotationDef,
    arg: *Arg,
    array_literal: *ArrayLiteral,
    asm_: *Asm,
    asm_operand: *AsmOperand,
    assign: *Assign,
    block: *Block,
    bool_literal: *BoolLiteral,
    break_: *Break,
    c_struct_or_union_def: *CStructOrUnionDef,
    call: *Call,
    case: *Case,
    cast: *Cast,
    char_literal: *CharLiteral,
    class_def: *ClassDef,
    class_var: *ClassVar,
    def: *Def,
    double_splat: *DoubleSplat,
    enum_def: *EnumDef,
    exception_handler: *ExceptionHandler,
    expressions: *Expressions,
    extend: *Extend,
    external_var: *ExternalVar,
    fun_def: *FunDef,
    generic: *Generic,
    global: *Global,
    hash_literal: *HashLiteral,
    if_: *If,
    implicit_obj: *ImplicitObj,
    include: *Include,
    instance_size_of: *InstanceSizeOf,
    instance_var: *InstanceVar,
    is_a: *IsA,
    lib_def: *LibDef,
    macro: *Macro,
    macro_expression: *MacroExpression,
    macro_for: *MacroFor,
    macro_if: *MacroIf,
    macro_literal: *MacroLiteral,
    macro_var: *MacroVar,
    macro_verbatim: *MacroVerbatim,
    magic_constant: *MagicConstant,
    metaclass: *Metaclass,
    module_def: *ModuleDef,
    multi_assign: *MultiAssign,
    named_argument: *NamedArgument,
    named_tuple_literal: *NamedTupleLiteral,
    next: *Next,
    nil: *Nil,
    nilable_cast: *NilableCast,
    nop: *Nop,
    not: *Not,
    number_literal: *NumberLiteral,
    offset_of: *OffsetOf,
    op_assign: *OpAssign,
    or_: *Or,
    out: *Out,
    path: *Path,
    pointer_of: *PointerOf,
    proc_literal: *ProcLiteral,
    proc_notation: *ProcNotation,
    proc_pointer: *ProcPointer,
    range_literal: *RangeLiteral,
    read_instance_var: *ReadInstanceVar,
    regex_literal: *RegexLiteral,
    require: *Require,
    rescue: *Rescue,
    responds_to: *RespondsTo,
    return_: *Return,
    select: *Select,
    self: *Self,
    size_of: *SizeOf,
    splat: *Splat,
    string_interpolation: *StringInterpolation,
    string_literal: *StringLiteral,
    symbol_literal: *SymbolLiteral,
    tuple_literal: *TupleLiteral,
    type_declaration: *TypeDeclaration,
    type_def: *TypeDef,
    type_of: *TypeOf,
    underscore: *Underscore,
    uninitialized_var: *UninitializedVar,
    union_: *Union,
    unless: *Unless,
    until: *Until,
    var_: *Var,
    visibility_modifier: *VisibilityModifier,
    when: *When,
    while_: *While,
    yield: *Yield,

    pub fn isNop(self: @This()) bool {
        return self == .nop;
    }

    pub fn isTrueLiteral(self: @This()) bool {
        return self == .bool_literal and self.bool_literal.value;
    }

    pub fn isFalseLiteral(self: @This()) bool {
        return self == .bool_literal and !self.bool_literal.value;
    }

    pub fn singleExpression(self: @This()) ASTNode {
        if (self == .expressions) {
            if (self.expressions.singleExpression()) |single_expression| {
                return single_expression;
            }
        }
        return self;
    }
};

pub const Nop = struct {
    pub fn create(allocator: std.mem.Allocator) !*@This() {
        return try allocator.create(@This());
    }

    pub fn new(allocator: std.mem.Allocator) !ASTNode {
        return ASTNode { .nop = try create(allocator) };
    }
};

pub const Nil = struct {
    pub fn create(allocator: std.mem.Allocator) !*@This() {
        return try allocator.create(@This());
    }

    pub fn new(allocator: std.mem.Allocator) !ASTNode {
        return ASTNode { .nil = try create(allocator) };
    }
};

pub const Expressions = struct {
    pub const Keyword = enum {
        None,
        Paren,
        Begin,
    };

    expressions: []ASTNode,
    keyword: Keyword = .None,

    pub fn create(allocator: std.mem.Allocator, expressions: []ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .expressions = expressions };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, expressions: []ASTNode) !ASTNode {
        return ASTNode { .expressions = try create(allocator, expressions) };
    }

    pub fn from(allocator: std.mem.Allocator, obj: anytype) !ASTNode {
        switch (@TypeOf(obj)) {
            @TypeOf(null) => return Nop.new(allocator),
            []ASTNode => {
                switch (obj.len) {
                    0 => return Nop.new(allocator),
                    1 => return obj[0],
                    else => return new(allocator, obj),
                }
            },
            ASTNode => return obj,
            ?ASTNode => return if (obj) |node| node else Nop.new(allocator),
            else => @compileError("Expected pointer or array, found " ++ @typeName(@TypeOf(obj))),
        }
    }

    pub fn singleExpression(self: @This()) ?ASTNode {
        if (self.expressions.len == 1) {
            return self.expressions[0].singleExpression();
        } else {
            return null;
        }
    }
};

pub const BoolLiteral = struct {
    value: bool,

    pub fn create(allocator: std.mem.Allocator, value: bool) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, value: bool) !ASTNode {
        return ASTNode { .bool_literal = try create(allocator, value) };
    }
};

pub const NumberKind = enum {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,

    pub fn bytesize(self: @This()) u8 {
        return switch (self) {
            .I8   => 8,
            .I16  => 16,
            .I32  => 32,
            .I64  => 64,
            .I128 => 128,
            .U8   => 8,
            .U16  => 16,
            .U32  => 32,
            .U64  => 64,
            .U128 => 128,
            .F32  => 32,
            .F64  => 64,
        };
    }

    pub fn isSignedInt(self: @This()) bool {
        return switch (self) {
            .I8, .I16, .I32, .I64, .I128 => true,
            else => false,
        };
    }

    pub fn isUnignedInt(self: @This()) bool {
        return switch (self) {
            .U8, .U16, .U32, .U64, .U128 => true,
            else => false,
        };
    }

    pub fn isFloat(self: @This()) bool {
        return switch (self) {
            .F32, .F64 => true,
            else => false,
        };
    }

    pub fn fromNumber(number: anytype) @This() {
        switch (@TypeOf(number)) {
            i8   => return .I8,
            i16  => return .I16,
            i32  => return .I32,
            i64  => return .I64,
            i128 => return .I128,
            u8   => return .U8,
            u16  => return .U16,
            u32  => return .U32,
            u64  => return .U64,
            u128 => return .U128,
            f32  => return .F32,
            f64  => return .F64,
            comptime_int => comptime {
                if (number >= std.math.minInt(i32) and number <= std.math.maxInt(i32)) return .I32;
                if (number >= std.math.minInt(i64) and number <= std.math.maxInt(i64)) return .I64;
                if (number >= std.math.minInt(i128) and number <= std.math.maxInt(i128)) return .I128;
                if (number >= std.math.minInt(u128) and number <= std.math.maxInt(u128)) return .U128;
                @compileError("Unsupported int for NumberLiteral: " ++ std.fmt.comptimePrint("{}", .{number}));
            },
            comptime_float => comptime {
                if (number == 0.0) return .F32;
                if (@fabs(number) >= std.math.f32_min and @fabs(number) <= std.math.f32_max) return .F32;
                if (@fabs(number) >= std.math.f64_min and @fabs(number) <= std.math.f64_max) return .F64;
                @compileError("Unsupported float for NumberLiteral: " ++ std.fmt.comptimePrint("{}", .{number}));
            },
            else => @compileError("Unsupported number type for NumberLiteral: " ++ @typeName(@TypeOf(number))),
        }
    }

    pub fn numberType(self: @This()) type {
        return switch (self) {
            .I8   => i8,
            .I16  => i16,
            .I32  => i32,
            .I64  => i64,
            .I128 => i128,
            .U8   => u8,
            .U16  => u16,
            .U32  => u32,
            .U64  => u64,
            .U128 => u128,
            .F32  => f32,
            .F64  => f64,
        };
    }
};

pub const NumberLiteral = struct {
    value: []const u8,
    kind: NumberKind = .I32,

    pub fn create(allocator: std.mem.Allocator, value: anytype) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .value = try std.fmt.allocPrint(allocator, "{d}", .{value}),
            .kind = NumberKind.fromNumber(value),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, value: anytype) !ASTNode {
        return ASTNode { .number_literal = try create(allocator, value) };
    }

    pub fn hasSign(self: @This()) bool {
        return self.value[0] == '+' or self.value[0] == '-';
    }
};

pub const CharLiteral = struct {
    value: u8,

    pub fn create(allocator: std.mem.Allocator, value: u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, value: u8) !ASTNode {
        return ASTNode { .char_literal = try create(allocator, value) };
    }
};

pub const StringLiteral = struct {
    value: []const u8,

    pub fn create(allocator: std.mem.Allocator, value: []const u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, value: []const u8) !ASTNode {
        return ASTNode { .string_literal = try create(allocator, value) };
    }
};

pub const StringInterpolation = struct {
    expressions: []ASTNode,
    heredoc_indent: i32 = 0,

    pub fn create(allocator: std.mem.Allocator, expressions: []ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .expressions = expressions };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, expressions: []ASTNode) !ASTNode {
        return ASTNode { .string_interpolation = try create(allocator, expressions) };
    }
};

pub const SymbolLiteral = struct {
    value: []const u8,

    pub fn create(allocator: std.mem.Allocator, value: []const u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, value: []const u8) !ASTNode {
        return ASTNode { .symbol_literal = try create(allocator, value) };
    }
};

pub const ArrayLiteral = struct {
    elements: []ASTNode,
    of: ?ASTNode = null,
    name: ?ASTNode = null,

    pub fn create(allocator: std.mem.Allocator, elements: []ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .elements = elements };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, elements: []ASTNode) !ASTNode {
        return ASTNode { .array_literal = try create(allocator, elements) };
    }

    pub fn map(allocator: std.mem.Allocator, values: anytype, block: anytype) !ASTNode {
        // TODO: validate block
        var new_values = try allocator.alloc(ASTNode, values.len);
        for (values) |value, index| {
            new_values[index] = try block.call(allocator, value);
        }
        return new(allocator, new_values);
    }

    pub fn mapWithIndex(allocator: std.mem.Allocator, values: anytype, block: anytype) !ASTNode {
        // TODO: validate block
        var new_values = try allocator.alloc(ASTNode, values.len);
        for (values) |value, index| {
            new_values[index] = try block.call(allocator, value, index);
        }
        return new(allocator, new_values);
    }
};

pub const HashLiteral = struct {
    entries: []Entry,
    of: ?ASTNode = null,
    name: ?ASTNode = null,

    pub fn create(allocator: std.mem.Allocator, entries: []Entry) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .entries = entries };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, entries: []Entry) !ASTNode {
        return ASTNode { .hash_literal = try create(allocator, entries) };
    }

    pub const Entry = struct { key: ASTNode, value: ASTNode };
};

pub const NamedTupleLiteral = struct {
    entries: []Entry,

    pub fn create(allocator: std.mem.Allocator, entries: []Entry) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .entries = entries };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, entries: []Entry) !ASTNode {
        return ASTNode { .named_tuple_literal = try create(allocator, entries) };
    }

    pub const Entry = struct { key: []const u8, value: ASTNode };
};

pub const RangeLiteral = struct {
    from: ASTNode,
    to: ASTNode,
    is_exclusive: bool,

    pub fn create(allocator: std.mem.Allocator, from: ASTNode, to: ASTNode, is_exclusive: bool) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .from = from,
            .to = to,
            .is_exclusive = is_exclusive,
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, from: ASTNode, to: ASTNode, is_exclusive: bool) !ASTNode {
        return ASTNode { .range_literal = try create(allocator, from, to, is_exclusive) };
    }
};

pub const RegexOptions = struct {
    ignore_case: bool = false,
    multiline: bool = false,
    extended: bool = false,
    anchored: bool = false,

    utf_8: bool = false,
    no_utf8_check: bool = false,
    dupnames: bool = false,
    ucp: bool = false,
};

pub const RegexLiteral = struct {
    value: ASTNode,
    options: RegexOptions = .{},

    pub fn create(allocator: std.mem.Allocator, value: ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, value: ASTNode) !ASTNode {
        return ASTNode { .regex_literal = try create(allocator, value) };
    }
};

pub const TupleLiteral = struct {
    elements: []ASTNode,

    pub fn create(allocator: std.mem.Allocator, elements: []ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .elements = elements };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, elements: []ASTNode) !ASTNode {
        return ASTNode { .tuple_literal = try create(allocator, elements) };
    }
};

pub const Var = struct {
    name: []const u8,

    pub fn create(allocator: std.mem.Allocator, name: []const u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .name = name };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, name: []const u8) !ASTNode {
        return ASTNode { .var_ = try create(allocator, name) };
    }
};

pub const Block = struct {
    args: []*Var,
    body: ASTNode,
    call: ?*Call = null,
    splat_index: ?i32 = null,

    pub fn create(allocator: std.mem.Allocator, args: []*Var, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .args = args,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, args: []*Var, body: ?ASTNode) !ASTNode {
        return ASTNode { .block = try create(allocator, args, body) };
    }
};

pub const Call = struct {
    obj: ?ASTNode,
    name: []const u8,
    args: []ASTNode,
    block: ?*Block = null,
    block_arg: ?ASTNode = null,
    named_args: ?[]*NamedArgument = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,
    is_global: bool = false,
    is_expansion: bool = false,
    has_parentheses: bool = false,
};

pub const NamedArgument = struct {
    name: []const u8,
    value: ASTNode,
};

pub const If = struct {
    cond: ASTNode,
    then: ASTNode,
    else_: ASTNode,
    is_ternary: bool = false,

    pub fn create(allocator: std.mem.Allocator, cond: ASTNode, then: ?ASTNode, else_: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .then = try Expressions.from(allocator, then),
            .else_ = try Expressions.from(allocator, else_),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, cond: ASTNode, then: ?ASTNode, else_: ?ASTNode) !ASTNode {
        return ASTNode { .if_ = try create(allocator, cond, then, else_) };
    }
};

pub const Unless = struct {
    cond: ASTNode,
    then: ASTNode,
    else_: ASTNode,

    pub fn create(allocator: std.mem.Allocator, cond: ASTNode, then: ?ASTNode, else_: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .then = try Expressions.from(allocator, then),
            .else_ = try Expressions.from(allocator, else_),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, cond: ASTNode, then: ?ASTNode, else_: ?ASTNode) !ASTNode {
        return ASTNode { .unless = try create(allocator, cond, then, else_) };
    }
};

pub const Assign = struct {
    target: ASTNode,
    value: ASTNode,
    doc: ?[]const u8 = null,
};

pub const OpAssign = struct {
    target: ASTNode,
    op: []const u8,
    value: ASTNode,
};

pub const MultiAssign = struct {
    targets: []ASTNode,
    values: []ASTNode,
};

pub const InstanceVar = struct {
    name: []const u8,
};

pub const ReadInstanceVar = struct {
    obj: ASTNode,
    name: []const u8,
};

pub const ClassVar = struct {
    name: []const u8,
};

pub const Global = struct {
    name: []const u8,
};

pub fn BinaryOp() type {
    return struct {
        left: ASTNode,
        right: ASTNode,
    };
}

pub const And = BinaryOp();

pub const Or = BinaryOp();

pub const Arg = struct {
    name: []const u8,
    external_name: []const u8,
    default_value: ?ASTNode = null,
    restruction: ?ASTNode = null,
    doc: ?[]const u8 = null,
};

pub const ProcNotation = struct {
    inputs: ?[]ASTNode = null,
    output: ?ASTNode = null,
};

pub const Def = struct {
    free_vars: ?[][]const u8 = null,
    receiver: ?ASTNode = null,
    name: []const u8,
    args: []*Arg,
    double_splat: ?*Arg = null,
    body: ASTNode,
    block_arg: ?*Arg = null,
    return_type: ?ASTNode = null,
    yields: ?i32 = null,
    splat_index: ?i32 = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,

    is_macro_def: bool = false,
    calls_super: bool = false,
    calls_initialize: bool = false,
    calls_previous_def: bool = false,
    uses_block_arg: bool = false,
    assigns_special_var: bool = false,
    is_abstract: bool = false,

    pub fn create(allocator: std.mem.Allocator, name: []const u8, args: []*Arg, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .args = args,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, name: []const u8, args: []*Arg, body: ?ASTNode) !ASTNode {
        return ASTNode { .def = try create(allocator, name, args, body) };
    }
};

pub const Macro = struct {
    name: []const u8,
    args: []*Arg,
    body: ASTNode,
    double_splat: ?*Arg = null,
    block_arg: ?*Arg = null,
    splat_index: ?i32 = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,

    pub fn create(allocator: std.mem.Allocator, name: []const u8, args: []*Arg, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .args = args,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, name: []const u8, args: []*Arg, body: ?ASTNode) !ASTNode {
        return ASTNode { .macro = try create(allocator, name, args, body) };
    }
};

pub fn UnaryExpression() type {
    return struct {
        exp: ASTNode,
    };
}

pub const Not = UnaryExpression();

pub const PointerOf = UnaryExpression();

pub const SizeOf = UnaryExpression();

pub const InstanceSizeOf = UnaryExpression();

pub const Out = UnaryExpression();

pub const OffsetOf = struct {
    offsetof_type: ASTNode,
    offset: ASTNode,
};

pub const VisibilityModifier = struct {
    modifier: Visibility,
    exp: ASTNode,
    doc: ?[]const u8 = null,
};

pub const IsA = struct {
    obj: ASTNode,
    const_: ASTNode,
    is_nil_check: bool = false,
};

pub const RespondsTo = struct {
    obj: ASTNode,
    name: []const u8,
};

pub const Require = struct {
    string: []const u8,
};

pub const When = struct {
    conds: []ASTNode,
    body: ASTNode,
    is_exhaustive: bool = false,

    pub fn create(allocator: std.mem.Allocator, conds: []ASTNode, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .conds = conds,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, conds: []ASTNode, body: ?ASTNode) !ASTNode {
        return ASTNode { .when = try create(allocator, conds, body) };
    }
};

pub const Case = struct {
    cond: ?ASTNode,
    whens: []*When,
    else_: ?ASTNode,
    is_exhaustive: bool,
};

pub const Select = struct {
    pub const When = struct { condition: ASTNode, body: ASTNode };

    whens: []@This().When,
    else_: ?ASTNode = null,
};

pub const ImplicitObj = struct {};

pub const Path = struct {
    names: [][]const u8,
    is_global: bool = false,
    visibility: Visibility = .Public,

    pub fn create(allocator: std.mem.Allocator, names: [][]const u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .names = names,
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, names: [][]const u8) !ASTNode {
        return ASTNode { .path = try create(allocator, names) };
    }
};

pub const ClassDef = struct {
    name: *Path,
    body: ASTNode,
    superclass: ?ASTNode = null,
    type_vars: ?[][]const u8 = null,
    doc: ?[]const u8 = null,
    splat_index: ?i32 = null,
    is_abstract: bool = false,
    is_struct: bool = false,
    visibility: Visibility = .Public,

    pub fn create(allocator: std.mem.Allocator, name: *Path, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, name: *Path, body: ?ASTNode) !ASTNode {
        return ASTNode { .class_def = try create(allocator, name, body) };
    }
};

pub const ModuleDef = struct {
    name: *Path,
    body: ASTNode,
    type_vars: ?[][]const u8 = null,
    splat_index: ?i32 = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,

    pub fn create(allocator: std.mem.Allocator, name: *Path, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, name: *Path, body: ?ASTNode) !ASTNode {
        return ASTNode { .module_def = try create(allocator, name, body) };
    }
};

pub const AnnotationDef = struct {
    name: *Path,
    doc: ?[]const u8 = null,
};

pub const While = struct {
    cond: ASTNode,
    body: ASTNode,

    pub fn create(allocator: std.mem.Allocator, cond: ASTNode, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, cond: ASTNode, body: ?ASTNode) !ASTNode {
        return ASTNode { .while_ = try create(allocator, cond, body) };
    }
};

pub const Until = struct {
    cond: ASTNode,
    body: ASTNode,

    pub fn create(allocator: std.mem.Allocator, cond: ASTNode, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, cond: ASTNode, body: ?ASTNode) !ASTNode {
        return ASTNode { .until = try create(allocator, cond, body) };
    }
};

pub const Generic = struct {
    name: ASTNode,
    type_vars: []ASTNode,
    names_args: ?[]*NamedArgument = null,
    suffix: Suffix = .None,

    pub const Suffix = enum {
        None,
        Question,
        Asterisk,
        Bracket,
    };
};

pub const TypeDeclaration = struct {
    var_: ASTNode,
    declared_type: ASTNode,
    value: ?ASTNode = null,
};

pub const UninitializedVar = struct {
    var_: ASTNode,
    declared_type: ASTNode,
};

pub const Rescue = struct {
    body: ASTNode,
    types: ?[]ASTNode = null,
    name: ?[]const u8 = null,

    pub fn create(allocator: std.mem.Allocator, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, body: ?ASTNode) !ASTNode {
        return ASTNode { .rescue = try create(allocator, body) };
    }
};

pub const ExceptionHandler = struct {
    body: ASTNode,
    rescues: ?[]*Rescue = null,
    else_: ?ASTNode = null,
    ensure: ?ASTNode = null,
    is_implicit: bool = false,
    is_suffix: bool = false,

    pub fn create(allocator: std.mem.Allocator, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, body: ?ASTNode) !ASTNode {
        return ASTNode { .exception_handler = try create(allocator, body) };
    }
};

pub const ProcLiteral = struct {
    def: *Def,
};

pub const ProcPointer = struct {
    obj: ?ASTNode,
    name: []const u8,
    args: []ASTNode,
};

pub const Union = struct {
    types: []ASTNode,
};

pub const Self = struct {
    pub fn create(allocator: std.mem.Allocator) !*@This() {
        return try allocator.create(@This());
    }

    pub fn new(allocator: std.mem.Allocator) !ASTNode {
        return ASTNode { .self = try create(allocator) };
    }
};

pub fn ControlExpression() type {
    return struct {
        exp: ?ASTNode = null,
    };
}

pub const Return = ControlExpression();

pub const Break = ControlExpression();

pub const Next = ControlExpression();

pub const Yield = struct {
    exps: []ASTNode,
    scope: ?ASTNode = null,
    has_parentheses: bool = false,
};

pub const Include = struct {
    name: ASTNode,
};

pub const Extend = struct {
    name: ASTNode,
};

pub const LibDef = struct {
    name: []const u8,
    body: ASTNode,
    visibility: Visibility = .Public,

    pub fn create(allocator: std.mem.Allocator, name: []const u8, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, name: []const u8, body: ?ASTNode) !ASTNode {
        return ASTNode { .lib_def = try create(allocator, name, body) };
    }
};

pub const FunDef = struct {
    name: []const u8,
    args: []*Arg,
    return_type: ?ASTNode = null,
    body: ?ASTNode = null,
    real_name: []const u8,
    doc: ?[]const u8 = null,
    varargs: bool = false,
};

pub const TypeDef = struct {
    name: []const u8,
    type_spec: ASTNode,
};

pub const CStructOrUnionDef = struct {
    name: []const u8,
    body: ASTNode,
    is_union: bool = false,

    pub fn create(allocator: std.mem.Allocator, name: []const u8, body: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, name: []const u8, body: ?ASTNode) !ASTNode {
        return ASTNode { .c_struct_or_union_def = try create(allocator, name, body) };
    }
};

pub const EnumDef = struct {
    name: *Path,
    members: []ASTNode,
    base_type: ?ASTNode = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,
};

pub const ExternalVar = struct {
    name: []const u8,
    type_spec: ASTNode,
    real_name: ?[]const u8 = null,
};

pub const Alias = struct {
    name: *Path,
    value: ASTNode,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,
};

pub const Metaclass = struct {
    name: ASTNode,
};

pub const Cast = struct {
    obj: ASTNode,
    to: ASTNode,
};

pub const NilableCast = struct {
    obj: ASTNode,
    to: ASTNode,
};

pub const TypeOf = struct {
    expressions: []ASTNode,
};

pub const Annotation = struct {
    path: Path,
    args: []ASTNode,
    named_args: ?[]*NamedArgument = null,
    doc: ?[]const u8 = null,
};

pub const MacroExpression = struct {
    exp: ASTNode,
    output: bool = true,
};

pub const MacroLiteral = struct {
    value: []const u8,
};

pub const MacroVerbatim = UnaryExpression();

pub const MacroIf = struct {
    cond: ASTNode,
    then: ASTNode,
    else_: ASTNode,

    pub fn create(allocator: std.mem.Allocator, cond: ASTNode, then: ?ASTNode, else_: ?ASTNode) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .then = try Expressions.from(allocator, then),
            .else_ = try Expressions.from(allocator, else_),
        };
        return instance;
    }

    pub fn new(allocator: std.mem.Allocator, cond: ASTNode, then: ?ASTNode, else_: ?ASTNode) !ASTNode {
        return ASTNode { .macro_if = try create(allocator, cond, then, else_) };
    }
};

pub const MacroFor = struct {
    vars: []*Var,
    exp: ASTNode,
    body: ASTNode,
};

pub const MacroVar = struct {
    name: []const u8,
    exps: ?[]ASTNode = null,
};

pub const Underscore = struct {
    pub fn create(allocator: std.mem.Allocator) !*@This() {
        return try allocator.create(@This());
    }

    pub fn new(allocator: std.mem.Allocator) !ASTNode {
        return ASTNode { .underscore = try create(allocator) };
    }
};

pub const Splat = UnaryExpression();

pub const DoubleSplat = UnaryExpression();

pub const MagicConstant = struct {
    name: []const u8, // Symbol
};

pub const Asm = struct {
    text: []const u8,
    outputs: ?[]*AsmOperand = null,
    inputs: ?[]*AsmOperand = null,
    clobbers: ?[][]const u8 = null,
    volatile_: bool = false,
    alignstack: bool = false,
    intel: bool = false,
    can_throw: bool = false,
};

pub const AsmOperand = struct {
    constraint: []const u8,
    exp: ASTNode,
};

pub const Visibility = enum(i8) {
    Public,
    Protected,
    Private,
};

const print = std.debug.print;
const c_allocator = std.heap.c_allocator;

const utils = @import("utils.zig");
const inspect = utils.inspect;
const pp = utils.pp;
const p = utils.p;
const xprint = utils.xprint;

pub fn main() !void {
    var n: ASTNode = undefined;
    p(.{ @TypeOf(std.debug) });
    p(.{ @as(ASTNode, .nop) });
    // p(.{ ASTNode { .expressions = .{ .expressions = &.{} } } });
    // p(.{ ASTNode { .expressions = .{ .expressions = &.{}, .keyword = .Paren } } });
    p(NumberKind.fromNumber(0) != .I8);
    p(NumberKind.fromNumber(128) != .I16);
    p(NumberKind.fromNumber(32768) == .I32);
    p(NumberKind.fromNumber(2147483648) == .I64);
    p(NumberKind.fromNumber(9223372036854775808) == .I128);
    p(NumberKind.fromNumber(170141183460469231731687303715884105728) == .U128);
    p(NumberKind.fromNumber(0.0) == .F32);
    p(NumberKind.fromNumber(-3.402823466385288598121e+38) == .F64);
    // pp(NumberKind.fromNumber(-1.797693134862315708151e+308));
    // pp(NumberKind.fromNumber(340282366920938463463374607431768211456));
    p(NumberKind.fromNumber(@as(f64, -3.402823466385288598121e+38)));
    p(NumberKind.fromNumber(@as(f64, -1.79769313486231570815e+308)));
    p(NumberKind.fromNumber(@as(i8, 1)));
    p(NumberKind.fromNumber(@as(i16, 1)));
    p(NumberKind.fromNumber(@as(i32, 1)));
    p(NumberKind.fromNumber(@as(i64, 1)));
    p(NumberKind.fromNumber(@as(i128, 1)));
    p(NumberKind.fromNumber(@as(u8, 1)));
    p(NumberKind.fromNumber(@as(u16, 1)));
    p(NumberKind.fromNumber(@as(u32, 1)));
    p(NumberKind.fromNumber(@as(u64, 1)));
    p(NumberKind.fromNumber(@as(u128, 1)));
    p(NumberKind.fromNumber(@as(f32, 1)));
    p(NumberKind.fromNumber(@as(f64, 1)));
    p(NumberKind.fromNumber(@as(i8, 1)).bytesize());
    p(NumberKind.fromNumber(@as(i16, 1)).bytesize());
    p(NumberKind.fromNumber(@as(i32, 1)).bytesize());
    p(NumberKind.fromNumber(@as(i64, 1)).bytesize());
    p(NumberKind.fromNumber(@as(i128, 1)).bytesize());
    p(NumberKind.fromNumber(@as(u8, 1)).bytesize());
    p(NumberKind.fromNumber(@as(u16, 1)).bytesize());
    p(NumberKind.fromNumber(@as(u32, 1)).bytesize());
    p(NumberKind.fromNumber(@as(u64, 1)).bytesize());
    p(NumberKind.fromNumber(@as(u128, 1)).bytesize());
    p(NumberKind.fromNumber(@as(f32, 1)).bytesize());
    p(NumberKind.fromNumber(@as(f64, 1)).bytesize());
    p(NumberKind.fromNumber(@as(i32, 1)).isSignedInt());
    p(NumberKind.fromNumber(@as(i32, 1)).isUnignedInt());
    p(NumberKind.fromNumber(@as(i32, 1)).isFloat());
    p(NumberKind.fromNumber(@as(u32, 1)).isSignedInt());
    p(NumberKind.fromNumber(@as(u32, 1)).isUnignedInt());
    p(NumberKind.fromNumber(@as(u32, 1)).isFloat());
    p(NumberKind.fromNumber(@as(f32, 1)).isSignedInt());
    p(NumberKind.fromNumber(@as(f32, 1)).isUnignedInt());
    p(NumberKind.fromNumber(@as(f32, 1)).isFloat());
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .I8 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .I16 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .I32 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .I64 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .I128 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .U8 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .U16 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .U32 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .U64 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .U128 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .F32 } } });
    // p(.{ ASTNode { .number_literal = .{ .value = "1", .kind = .F64 } } });
    // n = .{ .number_literal = .{ .value = "1" } }; p(.{ n.number_literal.hasSign() });
    // n = .{ .number_literal = .{ .value = "+1" } }; p(.{ n.number_literal.hasSign() });
    // n = .{ .number_literal = .{ .value = "-1" } }; p(.{ n.number_literal.hasSign() });
    n = try NumberLiteral.new(c_allocator, 1); xprint("{s}\n", .{ n.number_literal.value });
    n = try NumberLiteral.new(c_allocator, 1); p(.{ n.number_literal.hasSign() });
    // xprint("{}\n", .{@TypeOf(null)});
    // xprint("{}\n", .{@Type(.Null)});
    p(try Expressions.from(c_allocator, null));
    // const x: []*ASTNode = &.{};
    // p([]*ASTNode);
    // p(@TypeOf(x));
    p(try Expressions.from(c_allocator, try NumberLiteral.new(c_allocator, 1)));
    p(try Expressions.from(c_allocator, try c_allocator.alloc(ASTNode, 0)));
    {
    var array = try c_allocator.alloc(ASTNode, 0);
    array = try c_allocator.alloc(ASTNode, 1); array[0] = try NumberLiteral.new(c_allocator, 2); p(try Expressions.from(c_allocator, array));
    array = try c_allocator.alloc(ASTNode, 2); array[0] = try NumberLiteral.new(c_allocator, 3);
                                                array[1] = try NumberLiteral.new(c_allocator, 4); xprint("{any}\n", .{(try Expressions.from(c_allocator, array)).expressions.*.expressions});
    }
    p(try Block.new(c_allocator, try c_allocator.alloc(*Var, 0), null));
    p(try If.new(c_allocator, try BoolLiteral.new(c_allocator, true), null, null));
    p(try Unless.new(c_allocator, try BoolLiteral.new(c_allocator, true), null, null));
    p(try Def.new(c_allocator, "foo", try c_allocator.alloc(*Arg, 0), null));
    p(try When.new(c_allocator, try c_allocator.alloc(ASTNode, 0), null));
    p(try Path.create(c_allocator, try c_allocator.alloc([]const u8, 0)));
    p(try Path.new(c_allocator, try c_allocator.alloc([]const u8, 0)));
    p(try ClassDef.new(c_allocator, try Path.create(c_allocator, try c_allocator.alloc([]const u8, 0)), null));
    p(try ModuleDef.new(c_allocator, try Path.create(c_allocator, try c_allocator.alloc([]const u8, 0)), null));
    p(try While.new(c_allocator, try BoolLiteral.new(c_allocator, true), null));
    p(try Until.new(c_allocator, try BoolLiteral.new(c_allocator, true), null));
    p(try Rescue.new(c_allocator, null));
    p(try ExceptionHandler.new(c_allocator, null));
    p(try LibDef.new(c_allocator, "Foo", null));
    p(try CStructOrUnionDef.new(c_allocator, "Foo", null));
    p(try MacroIf.new(c_allocator, try BoolLiteral.new(c_allocator, true), null, null));
    p((try Nop.new(c_allocator)).isNop());
    p((try BoolLiteral.new(c_allocator, true)).isTrueLiteral());
    p((try BoolLiteral.new(c_allocator, false)).isFalseLiteral());
    p((try NumberLiteral.new(c_allocator, 1)).singleExpression());
    p((try Expressions.new(c_allocator, try c_allocator.alloc(ASTNode, 0))).singleExpression());
    {
        var expressions = try c_allocator.alloc(ASTNode, 1);
        expressions[0] = try NumberLiteral.new(c_allocator, 1);
        p((try Expressions.new(c_allocator, expressions)).singleExpression());
    }
    {
        var expressions = try c_allocator.alloc(ASTNode, 2);
        expressions[0] = try NumberLiteral.new(c_allocator, 1);
        expressions[1] = try NumberLiteral.new(c_allocator, 2);
        p((try Expressions.new(c_allocator, expressions)).singleExpression());
    }
    {
        var values = try c_allocator.alloc(ASTNode, 2);
        values[0] = try BoolLiteral.new(c_allocator, true);
        values[1] = try BoolLiteral.new(c_allocator, false);
        const array = try ArrayLiteral.new(c_allocator, values);
        const array2 = try ArrayLiteral.map(c_allocator, values, struct {
            fn call(allocator: std.mem.Allocator, node: ASTNode) !ASTNode {
                return try BoolLiteral.new(allocator, !node.bool_literal.*.value);
            }
        });
        const array3 = try ArrayLiteral.mapWithIndex(c_allocator, values, struct {
            fn call(allocator: std.mem.Allocator, node: ASTNode, index: usize) !ASTNode {
                return if (index == 0) try BoolLiteral.new(allocator, !node.bool_literal.*.value) else node;
            }
        });
        p(array.array_literal.*.elements[0]);
        p(array2.array_literal.*.elements[0]);
        p(array3.array_literal.*.elements[0]);
        p(array3.array_literal.*.elements[1]);
    }
}
