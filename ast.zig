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

    pub fn location(node: Node) ?Location {
        switch (node) {
            inline else => |n| return n.location,
        }
    }

    pub fn endLocation(node: Node) ?Location {
        switch (node) {
            inline else => |n| return n.end_location,
        }
    }

    pub fn setLocation(node: Node, loc: ?Location) void {
        switch (node) {
            inline else => |n| n.location = loc,
        }
    }

    pub fn setEndLocation(node: Node, end_location: ?Location) void {
        switch (node) {
            inline else => |n| n.end_location = end_location,
        }
    }

    pub fn copyLocation(self: Node, node: Node) void {
        self.setLocation(node.location());
        self.setEndLocation(node.endLocation());
    }

    pub fn copyEndLocation(self: Node, node: Node) void {
        self.setEndLocation(node.endLocation());
    }

    pub fn isNop(node: Node) bool {
        return node == .nop;
    }

    pub fn isTrueLiteral(node: Node) bool {
        return node == .bool_literal and node.bool_literal.value;
    }

    pub fn isFalseLiteral(node: Node) bool {
        return node == .bool_literal and !node.bool_literal.value;
    }

    pub fn singleExpression(node: Node) Node {
        switch (node) {
            .expressions => |expressions| {
                if (expressions.singleExpression()) |single_expression| {
                    return single_expression;
                } else {
                    return node;
                }
            },
            else => return node,
        }
    }

    pub fn isSingleExpression(node: Node) bool {
        return switch (node) {
            .expressions => |expressions| expressions.isSingleExpression(),
            else => false,
        };
    }
};

inline fn Singleton(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        pub fn allocate(allocator: Allocator) !*@This() {
            var instance = try allocator.create(@This());
            instance.* = .{}; // initialize fields to default values
            return instance;
        }

        pub fn node(allocator: Allocator) !Node {
            return @unionInit(Node, name, try allocate(allocator));
        }
    };
}

pub const Nop = Singleton("nop");

pub const Nil = Singleton("nil");

pub const Expressions = struct {
    pub const Keyword = enum {
        none,
        paren,
        begin,
    };

    location: ?Location = null,
    end_location: ?Location = null,

    expressions: ArrayList(Node),
    keyword: Keyword,

    pub fn allocate(
        allocator: Allocator,
        expressions: ArrayList(Node),
        options: struct {
            keyword: Keyword = .none,
        },
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .expressions = expressions,
            .keyword = options.keyword,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        expressions: ArrayList(Node),
        options: anytype,
    ) !Node {
        return Node{ .expressions = try allocate(allocator, expressions, options) };
    }

    pub fn from(allocator: Allocator, obj: anytype) !Node {
        switch (@TypeOf(obj)) {
            @TypeOf(null) => return Nop.node(allocator),
            ArrayList(Node) => {
                switch (obj.items.len) {
                    0 => return Nop.node(allocator),
                    1 => return obj.items[0],
                    else => return node(allocator, obj, .{}),
                }
            },
            Node => return obj,
            ?Node => return if (obj) |n| n else Nop.node(allocator),
            else => @compileError("Expected Node or ArrayList(Node), found " ++ @typeName(@TypeOf(obj))),
        }
    }

    pub fn isEmpty(self: @This()) bool {
        return self.expressions.items.len == 0;
    }

    pub fn at(self: @This(), i: usize) Node {
        return self.expressions.items[i];
    }

    pub fn last(self: @This()) Node {
        const expressions = self.expressions.items;
        return expressions[expressions.len - 1];
    }

    pub fn isSingleExpression(self: @This()) bool {
        return self.expressions.items.len == 1;
    }

    pub fn singleExpression(self: @This()) ?Node {
        if (self.isSingleExpression()) {
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

    pub fn node(allocator: Allocator, value: bool) !Node {
        return Node{ .bool_literal = try allocate(allocator, value) };
    }
};

pub const NumberKind = enum {
    i8,
    i16,
    i32,
    i64,
    i128,
    u8,
    u16,
    u32,
    u64,
    u128,
    f32,
    f64,

    pub fn bytesize(self: @This()) u8 {
        return switch (self) {
            .i8 => 8,
            .i16 => 16,
            .i32 => 32,
            .i64 => 64,
            .i128 => 128,
            .u8 => 8,
            .u16 => 16,
            .u32 => 32,
            .u64 => 64,
            .u128 => 128,
            .f32 => 32,
            .f64 => 64,
        };
    }

    pub fn isSignedInt(self: @This()) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128 => true,
            else => false,
        };
    }

    pub fn isUnignedInt(self: @This()) bool {
        return switch (self) {
            .u8, .u16, .u32, .u64, .u128 => true,
            else => false,
        };
    }

    pub fn isFloat(self: @This()) bool {
        return switch (self) {
            .f32, .f64 => true,
            else => false,
        };
    }

    pub fn fromNumber(number: anytype) @This() {
        switch (@TypeOf(number)) {
            i8 => return .i8,
            i16 => return .i16,
            i32 => return .i32,
            i64 => return .i64,
            i128 => return .i128,
            u8 => return .u8,
            u16 => return .u16,
            u32 => return .u32,
            u64 => return .u64,
            u128 => return .u128,
            f32 => return .f32,
            f64 => return .f64,
            comptime_int => comptime {
                if (number >= std.math.minInt(i32) and number <= std.math.maxInt(i32)) return .i32;
                if (number >= std.math.minInt(i64) and number <= std.math.maxInt(i64)) return .i64;
                if (number >= std.math.minInt(i128) and number <= std.math.maxInt(i128)) return .i128;
                if (number >= std.math.minInt(u128) and number <= std.math.maxInt(u128)) return .u128;
                @compileError("Unsupported int for NumberLiteral: " ++ std.fmt.comptimePrint("{}", .{number}));
            },
            comptime_float => comptime {
                if (number == 0.0) return .f32;
                if (@fabs(number) >= std.math.f32_min and @fabs(number) <= std.math.f32_max) return .f32;
                if (@fabs(number) >= std.math.f64_min and @fabs(number) <= std.math.f64_max) return .f64;
                @compileError("Unsupported float for NumberLiteral: " ++ std.fmt.comptimePrint("{}", .{number}));
            },
            else => @compileError("Unsupported number type for NumberLiteral: " ++ @typeName(@TypeOf(number))),
        }
    }

    pub fn numberType(self: @This()) type {
        return switch (self) {
            .i8 => i8,
            .i16 => i16,
            .i32 => i32,
            .i64 => i64,
            .i128 => i128,
            .u8 => u8,
            .u16 => u16,
            .u32 => u32,
            .u64 => u64,
            .u128 => u128,
            .f32 => f32,
            .f64 => f64,
        };
    }
};

pub const NumberLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,
    kind: NumberKind = .i32,

    pub fn allocate(
        allocator: Allocator,
        value: []const u8,
        kind: NumberKind,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .value = value,
            .kind = kind,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        value: []const u8,
        kind: NumberKind,
    ) !Node {
        return Node{ .number_literal = try allocate(allocator, value, kind) };
    }

    pub fn fromNumber(allocator: Allocator, number: anytype) !Node {
        const value = try std.fmt.allocPrint(allocator, "{d}", .{number});
        const kind = NumberKind.fromNumber(number);
        return node(allocator, value, kind);
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

    pub fn node(allocator: Allocator, value: u8) !Node {
        return Node{ .char_literal = try allocate(allocator, value) };
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

    pub fn node(allocator: Allocator, value: []const u8) !Node {
        return Node{ .string_literal = try allocate(allocator, value) };
    }
};

pub const StringInterpolation = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    expressions: ArrayList(Node),
    heredoc_indent: i32 = 0,

    pub fn allocate(allocator: Allocator, expressions: ArrayList(Node)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .expressions = expressions };
        return instance;
    }

    pub fn node(allocator: Allocator, expressions: ArrayList(Node)) !Node {
        return Node{ .string_interpolation = try allocate(allocator, expressions) };
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

    pub fn node(allocator: Allocator, value: []const u8) !Node {
        return Node{ .symbol_literal = try allocate(allocator, value) };
    }
};

pub const ArrayLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    elements: ArrayList(Node),
    of: ?Node,
    name: ?Node,

    pub fn allocate(
        allocator: Allocator,
        elements: ArrayList(Node),
        options: struct {
            of: ?Node = null,
            name: ?Node = null,
        },
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .elements = elements,
            .of = options.of,
            .name = options.name,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        elements: ArrayList(Node),
        options: anytype,
    ) !Node {
        return Node{
            .array_literal = try allocate(allocator, elements, options),
        };
    }

    pub fn map(
        allocator: Allocator,
        values: anytype,
        options: struct {
            of: ?Node = null,
        },
        block: anytype,
    ) !Node {
        // TODO: validate values and block
        var new_values = try ArrayList(Node).initCapacity(allocator, values.items.len);
        for (values.items) |value| {
            new_values.appendAssumeCapacity(try block(allocator, value));
        }
        return node(allocator, new_values, .{ .of = options.of });
    }

    pub fn mapWithIndex(
        allocator: Allocator,
        values: anytype,
        block: anytype,
    ) !Node {
        // TODO: validate values and block
        var new_values = try ArrayList(Node).initCapacity(allocator, values.items.len);
        for (values.items) |value, index| {
            new_values.appendAssumeCapacity(try block(allocator, value, index));
        }
        return node(allocator, new_values, .{ .of = null });
    }
};

pub const HashLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    entries: ArrayList(Entry),
    of: ?Entry = null,
    name: ?Node = null,

    pub fn allocate(
        allocator: Allocator,
        entries: ArrayList(Entry),
        of: ?Entry,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .entries = entries,
            .of = of,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        entries: ArrayList(Entry),
        of: ?Entry,
    ) !Node {
        return Node{ .hash_literal = try allocate(allocator, entries, of) };
    }

    pub const Entry = struct {
        key: Node,
        value: Node,

        pub fn init(key: Node, value: Node) Entry {
            return .{ .key = key, .value = value };
        }
    };
};

pub const NamedTupleLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    entries: ArrayList(Entry),

    pub fn allocate(allocator: Allocator, entries: ArrayList(Entry)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .entries = entries };
        return instance;
    }

    pub fn node(allocator: Allocator, entries: ArrayList(Entry)) !Node {
        return Node{ .named_tuple_literal = try allocate(allocator, entries) };
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

    pub fn node(
        allocator: Allocator,
        from: Node,
        to: Node,
        is_exclusive: bool,
    ) !Node {
        return Node{ .range_literal = try allocate(allocator, from, to, is_exclusive) };
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

    pub fn node(allocator: Allocator, value: Node) !Node {
        return Node{ .regex_literal = try allocate(allocator, value) };
    }
};

pub const TupleLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    elements: ArrayList(Node),

    pub fn allocate(allocator: Allocator, elements: ArrayList(Node)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .elements = elements };
        return instance;
    }

    pub fn node(allocator: Allocator, elements: ArrayList(Node)) !Node {
        return Node{ .tuple_literal = try allocate(allocator, elements) };
    }
};

fn SimpleNamedNode(comptime tag_name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        name: []const u8,

        pub fn allocate(allocator: Allocator, name: []const u8) !*@This() {
            var instance = try allocator.create(@This());
            instance.* = .{ .name = name };
            return instance;
        }

        pub fn node(allocator: Allocator, name: []const u8) !Node {
            return @unionInit(Node, tag_name, try allocate(allocator, name));
        }
    };
}

pub const Var = SimpleNamedNode("var");

pub const Block = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    args: ArrayList(*Var),
    body: Node,
    call: ?*Call = null,
    splat_index: ?i32 = null,

    pub fn allocate(
        allocator: Allocator,
        args: ArrayList(*Var),
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .args = args,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        args: ArrayList(*Var),
        body: ?Node,
    ) !Node {
        return Node{ .block = try allocate(allocator, args, body) };
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
    name_location: ?Location = null,
    // name_size: ?usize = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .public,
    is_global: bool = false,
    is_expansion: bool = false,
    has_parentheses: bool = false,

    pub fn allocate(
        allocator: Allocator,
        obj: ?Node,
        name: []const u8,
        args: ArrayList(Node),
        options: struct {
            name_location: ?Location = null,
        },
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .obj = obj,
            .name = name,
            .args = args,
            .name_location = options.name_location,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        obj: ?Node,
        name: []const u8,
        args: ArrayList(Node),
        options: anytype,
    ) !Node {
        return Node{ .call = try allocate(allocator, obj, name, args, options) };
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

    pub fn node(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !Node {
        return Node{ .@"if" = try allocate(allocator, cond, then, @"else") };
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

    pub fn node(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !Node {
        return Node{ .unless = try allocate(allocator, cond, then, @"else") };
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

    pub fn node(
        allocator: Allocator,
        targets: ArrayList(Node),
        values: ArrayList(Node),
    ) !Node {
        return Node{ .multi_assign = try allocate(allocator, targets, values) };
    }
};

pub const InstanceVar = SimpleNamedNode("instance_var");

pub const ReadInstanceVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    name: []const u8,
};

pub const ClassVar = SimpleNamedNode("class_var");

pub const Global = SimpleNamedNode("global");

inline fn BinaryOp(comptime name: []const u8) type {
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

        pub fn node(
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

    inputs: ?ArrayList(Node) = null,
    output: ?Node = null,

    pub fn allocate(
        allocator: Allocator,
        inputs: ?ArrayList(Node),
        output: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .inputs = inputs,
            .output = output,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        inputs: ?ArrayList(Node),
        output: ?Node,
    ) !Node {
        return Node{ .proc_notation = try allocate(allocator, inputs, output) };
    }
};

pub const Def = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    free_vars: ?ArrayList([]const u8) = null,
    receiver: ?Node = null,
    name: []const u8,
    args: ArrayList(*Arg),
    double_splat: ?*Arg = null,
    body: Node,
    block_arg: ?*Arg = null,
    return_type: ?Node = null,
    yields: ?i32 = null,
    splat_index: ?i32 = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .public,

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
        args: ArrayList(*Arg),
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

    pub fn node(
        allocator: Allocator,
        name: []const u8,
        args: ArrayList(*Arg),
        body: ?Node,
    ) !Node {
        return Node{ .def = try allocate(allocator, name, args, body) };
    }
};

pub const Macro = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    args: ArrayList(*Arg),
    body: Node,
    double_splat: ?*Arg = null,
    block_arg: ?*Arg = null,
    splat_index: ?i32 = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .public,

    pub fn allocate(
        allocator: Allocator,
        name: []const u8,
        args: ArrayList(*Arg),
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

    pub fn node(
        allocator: Allocator,
        name: []const u8,
        args: ArrayList(*Arg),
        body: ?Node,
    ) !Node {
        return Node{ .macro = try allocate(allocator, name, args, body) };
    }
};

inline fn UnaryExpression(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        exp: Node,

        pub fn allocate(allocator: Allocator, exp: Node) !*@This() {
            var instance = try allocator.create(@This());
            instance.* = .{ .exp = exp };
            return instance;
        }

        pub fn node(allocator: Allocator, exp: Node) !Node {
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

    conds: ArrayList(Node),
    body: Node,
    is_exhaustive: bool = false,

    pub fn allocate(
        allocator: Allocator,
        conds: ArrayList(Node),
        body: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .conds = conds,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        conds: ArrayList(Node),
        body: ?Node,
    ) !Node {
        return Node{ .when = try allocate(allocator, conds, body) };
    }
};

pub const Case = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: ?Node,
    whens: ArrayList(*When),
    @"else": ?Node,
    is_exhaustive: bool,
};

pub const Select = struct {
    pub const When = struct { condition: Node, body: Node };

    location: ?Location = null,
    end_location: ?Location = null,

    whens: ArrayList(@This().When),
    @"else": ?Node = null,
};

pub const ImplicitObj = struct {
    location: ?Location = null,
    end_location: ?Location = null,
};

pub const Path = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    names: ArrayList([]const u8),
    is_global: bool = false,
    visibility: Visibility = .public,

    pub fn allocate(
        allocator: Allocator,
        names: ArrayList([]const u8),
        is_global: bool,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .names = names,
            .is_global = is_global,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        names: ArrayList([]const u8),
        is_global: bool,
    ) !Node {
        return Node{ .path = try allocate(allocator, names, is_global) };
    }

    pub fn global(
        allocator: Allocator,
        name: []const u8,
    ) !Node {
        var names = ArrayList([]const u8).init(allocator);
        try names.append(name);
        return node(allocator, names, true);
    }
};

pub const ClassDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    body: Node,
    superclass: ?Node = null,
    type_vars: ?ArrayList([]const u8) = null,
    doc: ?[]const u8 = null,
    splat_index: ?i32 = null,
    is_abstract: bool = false,
    is_struct: bool = false,
    visibility: Visibility = .public,

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

    pub fn node(
        allocator: Allocator,
        name: *Path,
        body: ?Node,
    ) !Node {
        return Node{ .class_def = try allocate(allocator, name, body) };
    }
};

pub const ModuleDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    body: Node,
    type_vars: ?ArrayList([]const u8) = null,
    splat_index: ?i32 = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .public,

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

    pub fn node(
        allocator: Allocator,
        name: *Path,
        body: ?Node,
    ) !Node {
        return Node{ .module_def = try allocate(allocator, name, body) };
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

    pub fn node(
        allocator: Allocator,
        cond: Node,
        body: ?Node,
    ) !Node {
        return Node{ .@"while" = try allocate(allocator, cond, body) };
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

    pub fn node(
        allocator: Allocator,
        cond: Node,
        body: ?Node,
    ) !Node {
        return Node{ .until = try allocate(allocator, cond, body) };
    }
};

pub const Generic = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,
    type_vars: ArrayList(Node),
    names_args: ?ArrayList(*NamedArgument) = null,
    suffix: Suffix,

    pub const Suffix = enum {
        none,
        question,
        asterisk,
        bracket,
    };

    pub fn allocate(
        allocator: Allocator,
        name: Node,
        type_vars: ArrayList(Node),
        options: struct {
            suffix: Suffix = .none,
        },
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .name = name,
            .type_vars = type_vars,
            .suffix = options.suffix,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        name: Node,
        type_vars: ArrayList(Node),
        options: anytype,
    ) !Node {
        return Node{ .generic = try allocate(allocator, name, type_vars, options) };
    }
};

pub const TypeDeclaration = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    @"var": Node,
    declared_type: Node,
    value: ?Node = null,

    pub fn allocate(
        allocator: Allocator,
        @"var": Node,
        declared_type: Node,
        value: ?Node,
    ) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .@"var" = @"var",
            .declared_type = declared_type,
            .value = value,
        };
        return instance;
    }

    pub fn node(
        allocator: Allocator,
        @"var": Node,
        declared_type: Node,
        value: ?Node,
    ) !Node {
        return Node{ .type_declaration = try allocate(allocator, @"var", declared_type, value) };
    }
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
    types: ?ArrayList(Node) = null,
    name: ?[]const u8 = null,

    pub fn allocate(allocator: Allocator, body: ?Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn node(allocator: Allocator, body: ?Node) !Node {
        return Node{ .rescue = try allocate(allocator, body) };
    }
};

pub const ExceptionHandler = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    body: Node,
    rescues: ?ArrayList(*Rescue) = null,
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

    pub fn node(allocator: Allocator, body: ?Node) !Node {
        return Node{ .exception_handler = try allocate(allocator, body) };
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
    args: ArrayList(Node),
};

pub const Union = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    types: ArrayList(Node),

    pub fn allocate(allocator: Allocator, types: ArrayList(Node)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .types = types };
        return instance;
    }

    pub fn node(allocator: Allocator, types: ArrayList(Node)) !Node {
        return Node{ .@"union" = try allocate(allocator, types) };
    }
};

pub const Self = Singleton("self");

inline fn ControlExpression(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        exp: ?Node = null,

        pub fn allocate(allocator: Allocator) !*@This() {
            var instance = try allocator.create(@This());
            instance.* = .{}; // initialize fields to default values
            return instance;
        }

        pub fn node(allocator: Allocator) !Node {
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

    exps: ArrayList(Node),
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
    visibility: Visibility = .public,

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

    pub fn node(
        allocator: Allocator,
        name: []const u8,
        body: ?Node,
    ) !Node {
        return Node{ .lib_def = try allocate(allocator, name, body) };
    }
};

pub const FunDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    args: ArrayList(*Arg),
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

    pub fn node(
        allocator: Allocator,
        name: []const u8,
        body: ?Node,
    ) !Node {
        return Node{ .c_struct_or_union_def = try allocate(allocator, name, body) };
    }
};

pub const EnumDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    members: ArrayList(Node),
    base_type: ?Node = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .public,
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
    visibility: Visibility = .public,
};

pub const Metaclass = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,

    pub fn allocate(allocator: Allocator, name: Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .name = name };
        return instance;
    }

    pub fn node(allocator: Allocator, name: Node) !Node {
        return Node{ .metaclass = try allocate(allocator, name) };
    }
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

    expressions: ArrayList(Node),
};

pub const Annotation = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    path: Path,
    args: ArrayList(Node),
    named_args: ?ArrayList(*NamedArgument) = null,
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

    pub fn node(
        allocator: Allocator,
        cond: Node,
        then: ?Node,
        @"else": ?Node,
    ) !Node {
        return Node{ .macro_if = try allocate(allocator, cond, then, @"else") };
    }
};

pub const MacroFor = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    vars: ArrayList(*Var),
    exp: Node,
    body: Node,
};

pub const MacroVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    exps: ?ArrayList(Node) = null,
};

pub const Underscore = Singleton("underscore");

pub const Splat = UnaryExpression("splat");

pub const DoubleSplat = UnaryExpression("double_splat");

pub const MagicConstant = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8, // Symbol

    pub fn expandLineNode(
        allocator: Allocator,
        location: ?Location,
    ) !Node {
        return NumberLiteral.fromNumber(allocator, try expandLine(location));
    }

    pub fn expandLine(location: ?Location) !i32 {
        if (location) |loc| {
            return std.math.cast(i32, loc.line_number) orelse error.Overflow;
        }
        return 0;
    }
};

pub const Asm = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    text: []const u8,
    outputs: ?ArrayList(*AsmOperand) = null,
    inputs: ?ArrayList(*AsmOperand) = null,
    clobbers: ?ArrayList([]const u8) = null,
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
    public,
    protected,
    private,
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
    const allocator = std.heap.page_allocator;

    var n: Node = undefined;
    p("{}\n", .{ @TypeOf(std.debug) });
    p("{}\n", .{ @as(std.meta.Tag(Node), .nop) });
    // p("{}\n", .{ Node { .expressions = .{ .expressions = &.{} } } });
    // p("{}\n", .{ Node { .expressions = .{ .expressions = &.{}, .keyword = .paren } } });
    assert(NumberKind.fromNumber(0) != .i8);
    assert(NumberKind.fromNumber(128) != .i16);
    assert(NumberKind.fromNumber(32768) == .i32);
    assert(NumberKind.fromNumber(2147483648) == .i64);
    assert(NumberKind.fromNumber(9223372036854775808) == .i128);
    assert(NumberKind.fromNumber(170141183460469231731687303715884105728) == .u128);
    assert(NumberKind.fromNumber(0.0) == .f32);
    assert(NumberKind.fromNumber(-3.402823466385288598121e+38) == .f64);
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
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .i8 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .i16 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .i32 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .i64 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .i128 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .u8 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .u16 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .u32 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .u64 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .u128 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .f32 } } });
    // p("{}\n", .{ Node { .number_literal = .{ .value = "1", .kind = .f64 } } });
    // n = .{ .number_literal = .{ .value = "1" } }; p("{}\n", .{ n.number_literal.hasSign() });
    // n = .{ .number_literal = .{ .value = "+1" } }; p("{}\n", .{ n.number_literal.hasSign() });
    // n = .{ .number_literal = .{ .value = "-1" } }; p("{}\n", .{ n.number_literal.hasSign() });
    n = try NumberLiteral.fromNumber(allocator, 1); p("{s}\n", .{ n.number_literal.value });
    n = try NumberLiteral.fromNumber(allocator, 1); p("{}\n", .{ n.number_literal.hasSign() });
    // p("{}\n", .{@TypeOf(null)});
    // p("{}\n", .{@Type(.Null)});
    p("{}\n", .{try Expressions.from(allocator, null)});
    // const x: []*Node = &.{};
    // p("{}\n", .{[]*Node});
    // p("{}\n", .{@TypeOf(x)});
    p("{}\n", .{try Expressions.from(allocator, try NumberLiteral.fromNumber(allocator, 1))});
    p("{}\n", .{try Expressions.from(allocator, ArrayList(Node).init(allocator))});
    // {
    // var list = ArrayList(Node).init(allocator);
    // list = ArrayList(Node).init(allocator); try list.append(try NumberLiteral.fromNumber(allocator, 2)); p("{}\n", .{try Expressions.from(allocator, list)});
    // list = ArrayList(Node).init(allocator); try list.append(try NumberLiteral.fromNumber(allocator, 3)); try list.append(try NumberLiteral.fromNumber(allocator, 4)); p("{any}\n", .{(try Expressions.from(allocator, list)).expressions.expressions});
    // }
    p("{}\n", .{try Block.node(allocator, ArrayList(*Var).init(allocator), null)});
    p("{}\n", .{try If.node(allocator, try BoolLiteral.node(allocator, true), null, null)});
    p("{}\n", .{try Unless.node(allocator, try BoolLiteral.node(allocator, true), null, null)});
    p("{}\n", .{try Def.node(allocator, "foo", ArrayList(*Arg).init(allocator), null)});
    p("{}\n", .{try When.node(allocator, ArrayList(Node).init(allocator), null)});
    p("{}\n", .{try Path.allocate(allocator, ArrayList([]const u8).init(allocator), false)});
    p("{}\n", .{try Path.node(allocator, ArrayList([]const u8).init(allocator), false)});
    p("{}\n", .{try ClassDef.node(allocator, try Path.allocate(allocator, ArrayList([]const u8).init(allocator), false), null)});
    p("{}\n", .{try ModuleDef.node(allocator, try Path.allocate(allocator, ArrayList([]const u8).init(allocator), false), null)});
    p("{}\n", .{try While.node(allocator, try BoolLiteral.node(allocator, true), null)});
    p("{}\n", .{try Until.node(allocator, try BoolLiteral.node(allocator, true), null)});
    p("{}\n", .{try Rescue.node(allocator, null)});
    p("{}\n", .{try ExceptionHandler.node(allocator, null)});
    p("{}\n", .{try LibDef.node(allocator, "Foo", null)});
    p("{}\n", .{try CStructOrUnionDef.node(allocator, "Foo", null)});
    p("{}\n", .{try MacroIf.node(allocator, try BoolLiteral.node(allocator, true), null, null)});
    p("{}\n", .{(try Nop.node(allocator)).isNop()});
    p("{}\n", .{(try BoolLiteral.node(allocator, true)).isTrueLiteral()});
    p("{}\n", .{(try BoolLiteral.node(allocator, false)).isFalseLiteral()});
    p("{}\n", .{(try NumberLiteral.fromNumber(allocator, 1)).singleExpression()});
    p("{}\n", .{(try Expressions.node(allocator, ArrayList(Node).init(allocator), .{})).singleExpression()});
    {
        var expressions = ArrayList(Node).init(allocator);
        try expressions.append(try NumberLiteral.fromNumber(allocator, 1));
        p("{}\n", .{(try Expressions.node(allocator, expressions, .{})).singleExpression()});
    }
    {
        var expressions = ArrayList(Node).init(allocator);
        try expressions.append(try NumberLiteral.fromNumber(allocator, 1));
        try expressions.append(try NumberLiteral.fromNumber(allocator, 2));
        p("{}\n", .{(try Expressions.node(allocator, expressions, .{})).singleExpression()});
    }
    {
        var values = ArrayList(Node).init(allocator);
        try values.append(try BoolLiteral.node(allocator, true));
        try values.append(try BoolLiteral.node(allocator, false));
        const array = try ArrayLiteral.node(allocator, values, .{});
        const array2 = try ArrayLiteral.map(allocator, values, .{}, struct {
            fn f(_: Allocator, node: Node) !Node {
                return try BoolLiteral.node(allocator, !node.bool_literal.value);
            }
        }.f);
        const array3 = try ArrayLiteral.mapWithIndex(allocator, values, struct {
            fn f(_: Allocator, node: Node, index: usize) !Node {
                return if (index == 0) try BoolLiteral.node(allocator, !node.bool_literal.value) else node;
            }
        }.f);
        p("{}\n", .{array.array_literal.elements.items[0]});
        p("{}\n", .{array2.array_literal.elements.items[0]});
        p("{}\n", .{array3.array_literal.elements.items[0]});
        p("{}\n", .{array3.array_literal.elements.items[1]});
        var node = try Nop.node(allocator);
        node.setLocation(Location.new("foo", 1, 2));
        node.setEndLocation(Location.new("bar", 3, 4));
        p("{?} {?}\n", .{node.location(), node.endLocation()});
    }
}
