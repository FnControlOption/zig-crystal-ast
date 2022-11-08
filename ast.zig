const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Location = @import("location.zig");

pub const Node = union(enum) {
    alias: *Alias,
    @"and": *And,
    annotation: *Annotation,
    annotation_def: *AnnotationDef,
    arg: *Arg,
    array_literal: *ArrayLiteral,
    @"asm": *Asm,
    asm_operand: *AsmOperand,
    assign: *Assign,
    block: *Block,
    bool_literal: *BoolLiteral,
    @"break": *Break,
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
    @"if": *If,
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
    @"or": *Or,
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
    @"return": *Return,
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
    @"union": *Union,
    unless: *Unless,
    until: *Until,
    @"var": *Var,
    visibility_modifier: *VisibilityModifier,
    when: *When,
    @"while": *While,
    yield: *Yield,

    pub fn startsAt(self: Node, location: ?Location) Node {
        _ = location; // TODO
        return self;
    }

    pub fn at(self: Node, node: Node) Node {
        _ = node; // TODO
        return self;
    }

    pub fn endsAt(self: Node, end_location: ?Location) Node {
        _ = end_location; // TODO
        return self;
    }

    // pub fn atEnd(self: Node, node: Node) Node {
    //     _ = node; // TODO
    //     return self;
    // }

    pub fn isNop(self: Node) bool {
        return self == .nop;
    }

    pub fn isTrueLiteral(self: Node) bool {
        return self == .bool_literal and self.bool_literal.value;
    }

    pub fn isFalseLiteral(self: Node) bool {
        return self == .bool_literal and !self.bool_literal.value;
    }

    pub fn singleExpression(self: Node) Node {
        if (self == .expressions) {
            if (self.expressions.singleExpression()) |single_expression| {
                return single_expression;
            }
        }
        return self;
    }
};

fn Singleton(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        pub fn allocate(allocator: Allocator) !*@This() {
            var instance = try allocator.create(@This());
            instance.* = .{}; // initialize fields to default values
            return instance;
        }

        pub fn new(allocator: Allocator) !Node {
            return @unionInit(Node, name, try allocate(allocator));
        }
    };
}

pub const Nop = Singleton("nop");

pub const Nil = Singleton("nil");

pub const Expressions = struct {
    pub const Keyword = enum {
        None,
        Paren,
        Begin,
    };

    location: ?Location = null,
    end_location: ?Location = null,

    expressions: ArrayList(Node),
    keyword: Keyword = .None,

    pub fn allocate(allocator: Allocator, expressions: ArrayList(Node)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .expressions = expressions };
        return instance;
    }

    pub fn new(allocator: Allocator, expressions: ArrayList(Node)) !Node {
        return Node { .expressions = try allocate(allocator, expressions) };
    }

    pub fn from(allocator: Allocator, obj: anytype) !Node {
        switch (@TypeOf(obj)) {
            @TypeOf(null) => return Nop.new(allocator),
            ArrayList(Node) => {
                switch (obj.items.len) {
                    0 => return Nop.new(allocator),
                    1 => return obj.items[0],
                    else => return new(allocator, obj),
                }
            },
            Node => return obj,
            ?Node => return if (obj) |node| node else Nop.new(allocator),
            else => @compileError("Expected Node or ArrayList(Node), found " ++ @typeName(@TypeOf(obj))),
        }
    }

    pub fn singleExpression(self: @This()) ?Node {
        if (self.expressions.items.len == 1) {
            return self.expressions.items[0].singleExpression();
        } else {
            return null;
        }
    }
};

pub const BoolLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: bool,

    pub fn allocate(allocator: Allocator, value: bool) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: Allocator, value: bool) !Node {
        return Node { .bool_literal = try allocate(allocator, value) };
    }
};

pub const NumberKind = enum {
    @"i8",
    @"i16",
    @"i32",
    @"i64",
    @"i128",
    @"u8",
    @"u16",
    @"u32",
    @"u64",
    @"u128",
    @"f32",
    @"f64",

    pub fn bytesize(self: @This()) u8 {
        return switch (self) {
            .@"i8"   => 8,
            .@"i16"  => 16,
            .@"i32"  => 32,
            .@"i64"  => 64,
            .@"i128" => 128,
            .@"u8"   => 8,
            .@"u16"  => 16,
            .@"u32"  => 32,
            .@"u64"  => 64,
            .@"u128" => 128,
            .@"f32"  => 32,
            .@"f64"  => 64,
        };
    }

    pub fn isSignedInt(self: @This()) bool {
        return switch (self) {
            .@"i8", .@"i16", .@"i32", .@"i64", .@"i128" => true,
            else => false,
        };
    }

    pub fn isUnignedInt(self: @This()) bool {
        return switch (self) {
            .@"u8", .@"u16", .@"u32", .@"u64", .@"u128" => true,
            else => false,
        };
    }

    pub fn isFloat(self: @This()) bool {
        return switch (self) {
            .@"f32", .@"f64" => true,
            else => false,
        };
    }

    pub fn fromNumber(number: anytype) @This() {
        switch (@TypeOf(number)) {
            i8   => return .@"i8",
            i16  => return .@"i16",
            i32  => return .@"i32",
            i64  => return .@"i64",
            i128 => return .@"i128",
            u8   => return .@"u8",
            u16  => return .@"u16",
            u32  => return .@"u32",
            u64  => return .@"u64",
            u128 => return .@"u128",
            f32  => return .@"f32",
            f64  => return .@"f64",
            comptime_int => comptime {
                if (number >= std.math.minInt(i32) and number <= std.math.maxInt(i32)) return .@"i32";
                if (number >= std.math.minInt(i64) and number <= std.math.maxInt(i64)) return .@"i64";
                if (number >= std.math.minInt(i128) and number <= std.math.maxInt(i128)) return .@"i128";
                if (number >= std.math.minInt(u128) and number <= std.math.maxInt(u128)) return .@"u128";
                @compileError("Unsupported int for NumberLiteral: " ++ std.fmt.comptimePrint("{}", .{number}));
            },
            comptime_float => comptime {
                if (number == 0.0) return .@"f32";
                if (@fabs(number) >= std.math.f32_min and @fabs(number) <= std.math.f32_max) return .@"f32";
                if (@fabs(number) >= std.math.f64_min and @fabs(number) <= std.math.f64_max) return .@"f64";
                @compileError("Unsupported float for NumberLiteral: " ++ std.fmt.comptimePrint("{}", .{number}));
            },
            else => @compileError("Unsupported number type for NumberLiteral: " ++ @typeName(@TypeOf(number))),
        }
    }

    pub fn numberType(self: @This()) type {
        return switch (self) {
            .@"i8"   => i8,
            .@"i16"  => i16,
            .@"i32"  => i32,
            .@"i64"  => i64,
            .@"i128" => i128,
            .@"u8"   => u8,
            .@"u16"  => u16,
            .@"u32"  => u32,
            .@"u64"  => u64,
            .@"u128" => u128,
            .@"f32"  => f32,
            .@"f64"  => f64,
        };
    }
};

pub const NumberLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,
    kind: NumberKind = .@"i32",

    pub fn allocate(allocator: Allocator, value: anytype) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .value = try std.fmt.allocPrint(allocator, "{d}", .{value}),
            .kind = NumberKind.fromNumber(value),
        };
        return instance;
    }

    pub fn new(allocator: Allocator, value: anytype) !Node {
        return Node { .number_literal = try allocate(allocator, value) };
    }

    pub fn hasSign(self: @This()) bool {
        return self.value[0] == '+' or self.value[0] == '-';
    }
};

pub const CharLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: u8,

    pub fn allocate(allocator: Allocator, value: u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: Allocator, value: u8) !Node {
        return Node { .char_literal = try allocate(allocator, value) };
    }
};

pub const StringLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,

    pub fn allocate(allocator: Allocator, value: []const u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: Allocator, value: []const u8) !Node {
        return Node { .string_literal = try allocate(allocator, value) };
    }
};

pub const StringInterpolation = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    expressions: []Node,
    heredoc_indent: i32 = 0,

    pub fn allocate(allocator: Allocator, expressions: []Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .expressions = expressions };
        return instance;
    }

    pub fn new(allocator: Allocator, expressions: []Node) !Node {
        return Node { .string_interpolation = try allocate(allocator, expressions) };
    }
};

pub const SymbolLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,

    pub fn allocate(allocator: Allocator, value: []const u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: Allocator, value: []const u8) !Node {
        return Node { .symbol_literal = try allocate(allocator, value) };
    }
};

pub const ArrayLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    elements: []Node,
    of: ?Node = null,
    name: ?Node = null,

    pub fn allocate(allocator: Allocator, elements: []Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .elements = elements };
        return instance;
    }

    pub fn new(allocator: Allocator, elements: []Node) !Node {
        return Node { .array_literal = try allocate(allocator, elements) };
    }

    pub fn map(allocator: Allocator, values: anytype, block: anytype) !Node {
        // TODO: validate block
        var new_values = try allocator.alloc(Node, values.len);
        for (values) |value, index| {
            new_values[index] = try block.call(allocator, value);
        }
        return new(allocator, new_values);
    }

    pub fn mapWithIndex(allocator: Allocator, values: anytype, block: anytype) !Node {
        // TODO: validate block
        var new_values = try allocator.alloc(Node, values.len);
        for (values) |value, index| {
            new_values[index] = try block.call(allocator, value, index);
        }
        return new(allocator, new_values);
    }
};

pub const HashLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    entries: []Entry,
    of: ?Node = null,
    name: ?Node = null,

    pub fn allocate(allocator: Allocator, entries: []Entry) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .entries = entries };
        return instance;
    }

    pub fn new(allocator: Allocator, entries: []Entry) !Node {
        return Node { .hash_literal = try allocate(allocator, entries) };
    }

    pub const Entry = struct { key: Node, value: Node };
};

pub const NamedTupleLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    entries: []Entry,

    pub fn allocate(allocator: Allocator, entries: []Entry) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .entries = entries };
        return instance;
    }

    pub fn new(allocator: Allocator, entries: []Entry) !Node {
        return Node { .named_tuple_literal = try allocate(allocator, entries) };
    }

    pub const Entry = struct { key: []const u8, value: Node };
};

pub const RangeLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    from: Node,
    to: Node,
    is_exclusive: bool,

    pub fn allocate(
        allocator: Allocator,
        from: Node,
        to: Node,
        is_exclusive: bool,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .from = from,
            .to = to,
            .is_exclusive = is_exclusive,
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        from: Node,
        to: Node,
        is_exclusive: bool,
    ) !Node {
        return Node { .range_literal = try allocate(allocator, from, to, is_exclusive) };
    }
};

pub const RegexOptions = struct {
    location: ?Location = null,
    end_location: ?Location = null,

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
    location: ?Location = null,
    end_location: ?Location = null,

    value: Node,
    options: RegexOptions = .{},

    pub fn allocate(allocator: Allocator, value: Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .value = value };
        return instance;
    }

    pub fn new(allocator: Allocator, value: Node) !Node {
        return Node { .regex_literal = try allocate(allocator, value) };
    }
};

pub const TupleLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    elements: []Node,

    pub fn allocate(allocator: Allocator, elements: []Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .elements = elements };
        return instance;
    }

    pub fn new(allocator: Allocator, elements: []Node) !Node {
        return Node { .tuple_literal = try allocate(allocator, elements) };
    }
};

pub const Var = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,

    pub fn allocate(allocator: Allocator, name: []const u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .name = name };
        return instance;
    }

    pub fn new(allocator: Allocator, name: []const u8) !Node {
        return Node { .@"var" = try allocate(allocator, name) };
    }
};

pub const Block = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    args: []*Var,
    body: Node,
    call: ?*Call = null,
    splat_index: ?i32 = null,

    pub fn allocate(
        allocator: Allocator,
        args: []*Var,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .args = args,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        args: []*Var,
        body: ?Node,
    ) !Node {
        return Node { .block = try allocate(allocator, args, body) };
    }
};

pub const Call = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: ?Node,
    name: []const u8,
    args: ArrayList(Node),
    block: ?*Block = null,
    block_arg: ?Node = null,
    named_args: ?ArrayList(*NamedArgument) = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,
    is_global: bool = false,
    is_expansion: bool = false,
    has_parentheses: bool = false,

    pub fn allocate(
        allocator: Allocator,
        obj: ?Node,
        name: []const u8,
        args: ArrayList(Node),
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .obj = obj,
            .name = name,
            .args = args,
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        obj: ?Node,
        name: []const u8,
        args: ArrayList(Node),
    ) !Node {
        return Node { .call = try allocate(allocator, obj, name, args) };
    }
};

pub const NamedArgument = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    value: Node,
};

pub const If = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    then: Node,
    @"else": Node,
    is_ternary: bool = false,

    pub fn allocate(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .then = try Expressions.from(allocator, then),
            .@"else" = try Expressions.from(allocator, @"else"),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !Node {
        return Node { .@"if" = try allocate(allocator, cond, then, @"else") };
    }
};

pub const Unless = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    then: Node,
    @"else": Node,

    pub fn allocate(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .then = try Expressions.from(allocator, then),
            .@"else" = try Expressions.from(allocator, @"else"),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !Node {
        return Node { .unless = try allocate(allocator, cond, then, @"else") };
    }
};

pub const Assign = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    target: Node,
    value: Node,
    doc: ?[]const u8 = null,
};

pub const OpAssign = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    target: Node,
    op: []const u8,
    value: Node,
};

pub const MultiAssign = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    targets: ArrayList(Node),
    values: ArrayList(Node),

    pub fn allocate(
        allocator: Allocator,
        targets: ArrayList(Node),
        values: ArrayList(Node),
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .targets = targets,
            .values = values,
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        targets: ArrayList(Node),
        values: ArrayList(Node),
    ) !Node {
        return Node { .multi_assign = try allocate(allocator, targets, values) };
    }
};

pub const InstanceVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
};

pub const ReadInstanceVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    name: []const u8,
};

pub const ClassVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
};

pub const Global = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
};

fn BinaryOp(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        left: Node,
        right: Node,

        pub fn allocate(
            allocator: Allocator,
            left: Node,
            right: Node,
        ) !*@This() {
            var instance = try allocator.create(@This());
            instance.* = .{
                .left = left,
                .right = right,
            };
            return instance;
        }

        pub fn new(
            allocator: Allocator,
            left: Node,
            right: Node,
        ) !Node {
            return @unionInit(Node, name, try allocate(allocator, left, right));
        }
    };
}

pub const And = BinaryOp("and");

pub const Or = BinaryOp("or");

pub const Arg = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    external_name: []const u8,
    default_value: ?Node = null,
    restruction: ?Node = null,
    doc: ?[]const u8 = null,
};

pub const ProcNotation = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    inputs: ?[]Node = null,
    output: ?Node = null,
};

pub const Def = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    free_vars: ?[][]const u8 = null,
    receiver: ?Node = null,
    name: []const u8,
    args: []*Arg,
    double_splat: ?*Arg = null,
    body: Node,
    block_arg: ?*Arg = null,
    return_type: ?Node = null,
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

    pub fn allocate(
        allocator: Allocator,
        name: []const u8,
        args: []*Arg,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .args = args,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        name: []const u8,
        args: []*Arg,
        body: ?Node,
    ) !Node {
        return Node { .def = try allocate(allocator, name, args, body) };
    }
};

pub const Macro = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    args: []*Arg,
    body: Node,
    double_splat: ?*Arg = null,
    block_arg: ?*Arg = null,
    splat_index: ?i32 = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,

    pub fn allocate(
        allocator: Allocator,
        name: []const u8,
        args: []*Arg,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .args = args,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        name: []const u8,
        args: []*Arg,
        body: ?Node,
    ) !Node {
        return Node { .macro = try allocate(allocator, name, args, body) };
    }
};

fn UnaryExpression(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        exp: Node,

        pub fn allocate(allocator: Allocator, exp: Node) !*@This() {
            var instance = try allocator.create(@This());
            instance.* = .{ .exp = exp };
            return instance;
        }

        pub fn new(allocator: Allocator, exp: Node) !Node {
            return @unionInit(Node, name, try allocate(allocator, exp));
        }
    };
}

pub const Not = UnaryExpression("not");

pub const PointerOf = UnaryExpression("pointer_of");

pub const SizeOf = UnaryExpression("size_of");

pub const InstanceSizeOf = UnaryExpression("instance_size_of");

pub const Out = UnaryExpression("out");

pub const OffsetOf = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    offsetof_type: Node,
    offset: Node,
};

pub const VisibilityModifier = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    modifier: Visibility,
    exp: Node,
    doc: ?[]const u8 = null,
};

pub const IsA = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    @"const": Node,
    is_nil_check: bool = false,
};

pub const RespondsTo = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    name: []const u8,
};

pub const Require = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    string: []const u8,
};

pub const When = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    conds: []Node,
    body: Node,
    is_exhaustive: bool = false,

    pub fn allocate(
        allocator: Allocator,
        conds: []Node,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .conds = conds,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        conds: []Node,
        body: ?Node,
    ) !Node {
        return Node { .when = try allocate(allocator, conds, body) };
    }
};

pub const Case = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: ?Node,
    whens: []*When,
    @"else": ?Node,
    is_exhaustive: bool,
};

pub const Select = struct {
    pub const When = struct { condition: Node, body: Node };

    location: ?Location = null,
    end_location: ?Location = null,

    whens: []@This().When,
    @"else": ?Node = null,
};

pub const ImplicitObj = struct {
    location: ?Location = null,
    end_location: ?Location = null,
};

pub const Path = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    names: [][]const u8,
    is_global: bool = false,
    visibility: Visibility = .Public,

    pub fn allocate(allocator: Allocator, names: [][]const u8) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .names = names,
        };
        return instance;
    }

    pub fn new(allocator: Allocator, names: [][]const u8) !Node {
        return Node { .path = try allocate(allocator, names) };
    }
};

pub const ClassDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    body: Node,
    superclass: ?Node = null,
    type_vars: ?[][]const u8 = null,
    doc: ?[]const u8 = null,
    splat_index: ?i32 = null,
    is_abstract: bool = false,
    is_struct: bool = false,
    visibility: Visibility = .Public,

    pub fn allocate(
        allocator: Allocator,
        name: *Path,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        name: *Path,
        body: ?Node,
    ) !Node {
        return Node { .class_def = try allocate(allocator, name, body) };
    }
};

pub const ModuleDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    body: Node,
    type_vars: ?[][]const u8 = null,
    splat_index: ?i32 = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,

    pub fn allocate(
        allocator: Allocator,
        name: *Path,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        name: *Path,
        body: ?Node,
    ) !Node {
        return Node { .module_def = try allocate(allocator, name, body) };
    }
};

pub const AnnotationDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    doc: ?[]const u8 = null,
};

pub const While = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    body: Node,

    pub fn allocate(
        allocator: Allocator,
        cond: Node,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        cond: Node,
        body: ?Node,
    ) !Node {
        return Node { .@"while" = try allocate(allocator, cond, body) };
    }
};

pub const Until = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    body: Node,

    pub fn allocate(
        allocator: Allocator,
        cond: Node,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        cond: Node,
        body: ?Node,
    ) !Node {
        return Node { .until = try allocate(allocator, cond, body) };
    }
};

pub const Generic = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,
    type_vars: []Node,
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
    location: ?Location = null,
    end_location: ?Location = null,

    @"var": Node,
    declared_type: Node,
    value: ?Node = null,
};

pub const UninitializedVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    @"var": Node,
    declared_type: Node,
};

pub const Rescue = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    body: Node,
    types: ?[]Node = null,
    name: ?[]const u8 = null,

    pub fn allocate(allocator: Allocator, body: ?Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: Allocator, body: ?Node) !Node {
        return Node { .rescue = try allocate(allocator, body) };
    }
};

pub const ExceptionHandler = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    body: Node,
    rescues: ?[]*Rescue = null,
    @"else": ?Node = null,
    ensure: ?Node = null,
    is_implicit: bool = false,
    is_suffix: bool = false,

    pub fn allocate(allocator: Allocator, body: ?Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(allocator: Allocator, body: ?Node) !Node {
        return Node { .exception_handler = try allocate(allocator, body) };
    }
};

pub const ProcLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    def: *Def,
};

pub const ProcPointer = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: ?Node,
    name: []const u8,
    args: []Node,
};

pub const Union = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    types: []Node,
};

pub const Self = Singleton("self");

fn ControlExpression(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        exp: ?Node = null,

        pub fn allocate(allocator: Allocator) !*@This() {
            var instance = try allocator.create(@This());
            instance.* = .{}; // initialize fields to default values
            return instance;
        }

        pub fn new(allocator: Allocator) !Node {
            return @unionInit(Node, name, try allocate(allocator));
        }
    };
}

pub const Return = ControlExpression("return");

pub const Break = ControlExpression("break");

pub const Next = ControlExpression("next");

pub const Yield = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    exps: []Node,
    scope: ?Node = null,
    has_parentheses: bool = false,
};

pub const Include = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,
};

pub const Extend = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,
};

pub const LibDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    body: Node,
    visibility: Visibility = .Public,

    pub fn allocate(
        allocator: Allocator,
        name: []const u8,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        name: []const u8,
        body: ?Node,
    ) !Node {
        return Node { .lib_def = try allocate(allocator, name, body) };
    }
};

pub const FunDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    args: []*Arg,
    return_type: ?Node = null,
    body: ?Node = null,
    real_name: []const u8,
    doc: ?[]const u8 = null,
    varargs: bool = false,
};

pub const TypeDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    type_spec: Node,
};

pub const CStructOrUnionDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    body: Node,
    is_union: bool = false,

    pub fn allocate(
        allocator: Allocator,
        name: []const u8,
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        name: []const u8,
        body: ?Node,
    ) !Node {
        return Node { .c_struct_or_union_def = try allocate(allocator, name, body) };
    }
};

pub const EnumDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    members: []Node,
    base_type: ?Node = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,
};

pub const ExternalVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    type_spec: Node,
    real_name: ?[]const u8 = null,
};

pub const Alias = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    value: Node,
    doc: ?[]const u8 = null,
    visibility: Visibility = .Public,
};

pub const Metaclass = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,
};

pub const Cast = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    to: Node,
};

pub const NilableCast = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    to: Node,
};

pub const TypeOf = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    expressions: []Node,
};

pub const Annotation = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    path: Path,
    args: []Node,
    named_args: ?[]*NamedArgument = null,
    doc: ?[]const u8 = null,
};

pub const MacroExpression = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    exp: Node,
    output: bool = true,
};

pub const MacroLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,
};

pub const MacroVerbatim = UnaryExpression("macro_verbatim");

pub const MacroIf = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    then: Node,
    @"else": Node,

    pub fn allocate(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .then = try Expressions.from(allocator, then),
            .@"else" = try Expressions.from(allocator, @"else"),
        };
        return instance;
    }

    pub fn new(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !Node {
        return Node { .macro_if = try allocate(allocator, cond, then, @"else") };
    }
};

pub const MacroFor = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    vars: []*Var,
    exp: Node,
    body: Node,
};

pub const MacroVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    exps: ?[]Node = null,
};

pub const Underscore = Singleton("underscore");

pub const Splat = UnaryExpression("splat");

pub const DoubleSplat = UnaryExpression("double_splat");

pub const MagicConstant = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8, // Symbol
};

pub const Asm = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    text: []const u8,
    outputs: ?[]*AsmOperand = null,
    inputs: ?[]*AsmOperand = null,
    clobbers: ?[][]const u8 = null,
    @"volatile": bool = false,
    alignstack: bool = false,
    intel: bool = false,
    can_throw: bool = false,
};

pub const AsmOperand = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    constraint: []const u8,
    exp: Node,
};

pub const Visibility = enum(i8) {
    Public,
    Protected,
    Private,
};

pub fn main() !void {
    const p = @import("std").debug.print;
    const assert = @import("std").debug.assert;
    const allocator = std.heap.page_allocator;

    var n: Node = undefined;
    p("{}\n", .{ @TypeOf(std.debug) });
    p("{}\n", .{ @as(std.meta.Tag(Node), .nop) });
    // p("{}\n", .{ Node { .expressions = .{ .expressions = &.{} } } });
    // p("{}\n", .{ Node { .expressions = .{ .expressions = &.{}, .keyword = .Paren } } });
    assert(NumberKind.fromNumber(0) != .@"i8");
    assert(NumberKind.fromNumber(128) != .@"i16");
    assert(NumberKind.fromNumber(32768) == .@"i32");
    assert(NumberKind.fromNumber(2147483648) == .@"i64");
    assert(NumberKind.fromNumber(9223372036854775808) == .@"i128");
    assert(NumberKind.fromNumber(170141183460469231731687303715884105728) == .@"u128");
    assert(NumberKind.fromNumber(0.0) == .@"f32");
    assert(NumberKind.fromNumber(-3.402823466385288598121e+38) == .@"f64");
    // p("{}\n", .{NumberKind.fromNumber(-1.797693134862315708151e+308)});
    // p("{}\n", .{NumberKind.fromNumber(340282366920938463463374607431768211456)});
    p("{}\n", .{NumberKind.fromNumber(@as(f64, -3.402823466385288598121e+38))});
    p("{}\n", .{NumberKind.fromNumber(@as(f64, -1.79769313486231570815e+308))});
    p("{}\n", .{NumberKind.fromNumber(@as(i8, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(i16, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(i32, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(i64, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(i128, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(u8, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(u16, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(u32, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(u64, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(u128, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(f32, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(f64, 1))});
    p("{}\n", .{NumberKind.fromNumber(@as(i8, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(i16, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(i32, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(i64, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(i128, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(u8, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(u16, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(u32, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(u64, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(u128, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(f32, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(f64, 1)).bytesize()});
    p("{}\n", .{NumberKind.fromNumber(@as(i32, 1)).isSignedInt()});
    p("{}\n", .{NumberKind.fromNumber(@as(i32, 1)).isUnignedInt()});
    p("{}\n", .{NumberKind.fromNumber(@as(i32, 1)).isFloat()});
    p("{}\n", .{NumberKind.fromNumber(@as(u32, 1)).isSignedInt()});
    p("{}\n", .{NumberKind.fromNumber(@as(u32, 1)).isUnignedInt()});
    p("{}\n", .{NumberKind.fromNumber(@as(u32, 1)).isFloat()});
    p("{}\n", .{NumberKind.fromNumber(@as(f32, 1)).isSignedInt()});
    p("{}\n", .{NumberKind.fromNumber(@as(f32, 1)).isUnignedInt()});
    p("{}\n", .{NumberKind.fromNumber(@as(f32, 1)).isFloat()});
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"i8" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"i16" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"i32" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"i64" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"i128" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"u8" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"u16" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"u32" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"u64" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"u128" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"f32" } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .@"f64" } } });
    // n = .{ .number_literal = .{ .value = "1" } }; p("{}\n", .{ n.number_literal.hasSign() });
    // n = .{ .number_literal = .{ .value = "+1" } }; p("{}\n", .{ n.number_literal.hasSign() });
    // n = .{ .number_literal = .{ .value = "-1" } }; p("{}\n", .{ n.number_literal.hasSign() });
    n = try NumberLiteral.new(allocator, 1); p("{s}\n", .{ n.number_literal.value });
    n = try NumberLiteral.new(allocator, 1); p("{}\n", .{ n.number_literal.hasSign() });
    // p("{}\n", .{@TypeOf(null)});
    // p("{}\n", .{@Type(.Null)});
    p("{}\n", .{try Expressions.from(allocator, null)});
    // const x: []*Node = &.{};
    // p("{}\n", .{[]*Node});
    // p("{}\n", .{@TypeOf(x)});
    p("{}\n", .{try Expressions.from(allocator, try NumberLiteral.new(allocator, 1))});
    p("{}\n", .{try Expressions.from(allocator, ArrayList(Node).init(allocator))});
    // {
    // var array = try allocator.alloc(Node, 0);
    // array = try allocator.alloc(Node, 1); array[0] = try NumberLiteral.new(allocator, 2); p("{}\n", .{try Expressions.from(allocator, array)});
    // array = try allocator.alloc(Node, 2); array[0] = try NumberLiteral.new(allocator, 3);
    //                                             array[1] = try NumberLiteral.new(allocator, 4); p("{any}\n", .{(try Expressions.from(allocator, array)).expressions.*.expressions});
    // }
    p("{}\n", .{try Block.new(allocator, try allocator.alloc(*Var, 0), null)});
    p("{}\n", .{try If.new(allocator, try BoolLiteral.new(allocator, true), null, null)});
    p("{}\n", .{try Unless.new(allocator, try BoolLiteral.new(allocator, true), null, null)});
    p("{}\n", .{try Def.new(allocator, "foo", try allocator.alloc(*Arg, 0), null)});
    p("{}\n", .{try When.new(allocator, try allocator.alloc(Node, 0), null)});
    p("{}\n", .{try Path.allocate(allocator, try allocator.alloc([]const u8, 0))});
    p("{}\n", .{try Path.new(allocator, try allocator.alloc([]const u8, 0))});
    p("{}\n", .{try ClassDef.new(allocator, try Path.allocate(allocator, try allocator.alloc([]const u8, 0)), null)});
    p("{}\n", .{try ModuleDef.new(allocator, try Path.allocate(allocator, try allocator.alloc([]const u8, 0)), null)});
    p("{}\n", .{try While.new(allocator, try BoolLiteral.new(allocator, true), null)});
    p("{}\n", .{try Until.new(allocator, try BoolLiteral.new(allocator, true), null)});
    p("{}\n", .{try Rescue.new(allocator, null)});
    p("{}\n", .{try ExceptionHandler.new(allocator, null)});
    p("{}\n", .{try LibDef.new(allocator, "Foo", null)});
    p("{}\n", .{try CStructOrUnionDef.new(allocator, "Foo", null)});
    p("{}\n", .{try MacroIf.new(allocator, try BoolLiteral.new(allocator, true), null, null)});
    p("{}\n", .{(try Nop.new(allocator)).isNop()});
    p("{}\n", .{(try BoolLiteral.new(allocator, true)).isTrueLiteral()});
    p("{}\n", .{(try BoolLiteral.new(allocator, false)).isFalseLiteral()});
    p("{}\n", .{(try NumberLiteral.new(allocator, 1)).singleExpression()});
    p("{}\n", .{(try Expressions.new(allocator, ArrayList(Node).init(allocator))).singleExpression()});
    {
        var expressions = ArrayList(Node).init(allocator);
        try expressions.append(try NumberLiteral.new(allocator, 1));
        p("{}\n", .{(try Expressions.new(allocator, expressions)).singleExpression()});
    }
    {
        var expressions = ArrayList(Node).init(allocator);
        try expressions.append(try NumberLiteral.new(allocator, 1));
        try expressions.append(try NumberLiteral.new(allocator, 2));
        p("{}\n", .{(try Expressions.new(allocator, expressions)).singleExpression()});
    }
    {
        var values = try allocator.alloc(Node, 2);
        values[0] = try BoolLiteral.new(allocator, true);
        values[1] = try BoolLiteral.new(allocator, false);
        const array = try ArrayLiteral.new(allocator, values);
        const array2 = try ArrayLiteral.map(allocator, values, struct {
            fn call(_: Allocator, node: Node) !Node {
                return try BoolLiteral.new(allocator, !node.bool_literal.*.value);
            }
        });
        const array3 = try ArrayLiteral.mapWithIndex(allocator, values, struct {
            fn call(_: Allocator, node: Node, index: usize) !Node {
                return if (index == 0) try BoolLiteral.new(allocator, !node.bool_literal.*.value) else node;
            }
        });
        p("{}\n", .{array.array_literal.*.elements[0]});
        p("{}\n", .{array2.array_literal.*.elements[0]});
        p("{}\n", .{array3.array_literal.*.elements[0]});
        p("{}\n", .{array3.array_literal.*.elements[1]});
    }
}
