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
    when: *CaseWhen,
    @"while": *While,
    yield: *Yield,

    pub fn init(exp: anytype) Node {
        return exp.toNode();
    }

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

fn NodeAllocator(
    comptime name: []const u8,
    comptime NodeType: type,
    comptime expressions: []const []const u8,
) type {
    const parameters = @typeInfo(NodeType).Struct.fields;
    invalid: inline for (expressions) |exp| {
        inline for (parameters) |param| {
            if (comptime std.mem.eql(u8, param.name, exp))
                continue :invalid;
        }
        @compileError("invalid parameter '" ++ exp ++ "' for " ++ @typeName(NodeType));
    }
    return struct {
        pub fn allocate(allocator: Allocator, options: anytype) !*NodeType {
            const OptionsType = @TypeOf(options);
            const options_type_info = @typeInfo(OptionsType);
            if (options_type_info != .Struct) {
                @compileError("expected tuple or struct argument, found " ++ @typeName(OptionsType));
            }
            const arguments = options_type_info.Struct.fields;
            unknown: inline for (arguments) |arg| {
                inline for (parameters) |param| {
                    if (comptime std.mem.eql(u8, param.name, arg.name))
                        continue :unknown;
                }
                @compileError("unknown argument '" ++ arg.name ++ "' for " ++ @typeName(NodeType));
            }
            const instance = try allocator.create(NodeType);
            inline for (parameters) |param| {
                @field(instance, param.name) = blk: {
                    inline for (arguments) |arg| {
                        if (comptime std.mem.eql(u8, param.name, arg.name)) {
                            const value = @field(options, arg.name);
                            inline for (expressions) |exp| {
                                if (comptime std.mem.eql(u8, exp, arg.name))
                                    break :blk try Expressions.from(allocator, value);
                            }
                            break :blk value;
                        }
                    }
                    if (param.default_value) |default_value|
                        break :blk @ptrCast(*align(1) const param.field_type, default_value).*;
                    @compileError("missing argument '" ++ param.name ++ "' for " ++ @typeName(NodeType));
                };
            }
            return instance;
        }

        pub fn node(allocator: Allocator, options: anytype) !Node {
            return @unionInit(Node, name, try allocate(allocator, options));
        }

        pub fn toNode(self: *NodeType) Node {
            return @unionInit(Node, name, self);
        }
    };
}

fn Singleton(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        pub usingnamespace NodeAllocator(name, @This(), &.{});
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
    keyword: Keyword = .none,

    pub usingnamespace NodeAllocator("expressions", Expressions, &.{});

    pub fn from(allocator: Allocator, obj: anytype) !Node {
        switch (@TypeOf(obj)) {
            @TypeOf(null) => return Nop.node(allocator, .{}),
            ArrayList(Node) => {
                switch (obj.items.len) {
                    0 => return Nop.node(allocator, .{}),
                    1 => return obj.items[0],
                    else => return Expressions.node(allocator, .{
                        .expressions = obj,
                    }),
                }
            },
            Node => return obj,
            ?Node => return if (obj) |n| n else Nop.node(allocator, .{}),
            else => @compileError("Expected Node or ArrayList(Node), found " ++ @typeName(@TypeOf(obj))),
        }
    }

    pub fn isEmpty(self: *const @This()) bool {
        return self.expressions.items.len == 0;
    }

    pub fn at(self: *const @This(), i: usize) Node {
        return self.expressions.items[i];
    }

    pub fn last(self: *const @This()) Node {
        const expressions = self.expressions.items;
        return expressions[expressions.len - 1];
    }

    pub fn isSingleExpression(self: *const @This()) bool {
        return self.expressions.items.len == 1;
    }

    pub fn singleExpression(self: *const @This()) ?Node {
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

    pub usingnamespace NodeAllocator("bool_literal", @This(), &.{});
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

    pub usingnamespace NodeAllocator("number_literal", @This(), &.{});

    pub fn fromNumber(allocator: Allocator, number: anytype) !Node {
        return NumberLiteral.node(allocator, .{
            .value = try std.fmt.allocPrint(
                allocator,
                "{d}",
                .{number},
            ),
            .kind = NumberKind.fromNumber(number),
        });
    }

    pub fn hasSign(self: *const @This()) bool {
        return self.value[0] == '+' or self.value[0] == '-';
    }
};

pub const CharLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: u8,

    pub usingnamespace NodeAllocator("char_literal", @This(), &.{});
};

pub const StringLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,

    pub usingnamespace NodeAllocator("string_literal", @This(), &.{});
};

pub const StringInterpolation = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    expressions: ArrayList(Node),
    heredoc_indent: i32 = 0,

    pub usingnamespace NodeAllocator("string_interpolation", @This(), &.{});
};

pub const SymbolLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,

    pub usingnamespace NodeAllocator("symbol_literal", @This(), &.{});
};

pub const ArrayLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    elements: ArrayList(Node),
    of: ?Node = null,
    name: ?Node = null,

    pub usingnamespace NodeAllocator("array_literal", @This(), &.{});

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
        return ArrayLiteral.node(allocator, .{
            .elements = new_values,
            .of = options.of,
        });
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
        return ArrayLiteral.node(allocator, .{
            .elements = new_values,
            .of = null,
        });
    }
};

pub const HashLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    entries: ArrayList(Entry),
    of: ?Entry = null,
    name: ?Node = null,

    pub usingnamespace NodeAllocator("hash_literal", @This(), &.{});

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

    pub usingnamespace NodeAllocator("named_tuple_literal", @This(), &.{});

    pub const Entry = struct { key: []const u8, value: Node };
};

pub const RangeLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    from: Node,
    to: Node,
    is_exclusive: bool,

    pub usingnamespace NodeAllocator("range_literal", @This(), &.{});
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

    pub usingnamespace NodeAllocator("regex_literal", @This(), &.{});
};

pub const TupleLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    elements: ArrayList(Node),

    pub usingnamespace NodeAllocator("tuple_literal", @This(), &.{});
};

fn SpecialVar(comptime NodeType: type) type {
    return struct {
        pub fn isSpecialVar(self: *const NodeType) bool {
            return std.mem.startsWith(u8, self.name, "$");
        }
    };
}

pub const Var = struct {
    pub usingnamespace SpecialVar(@This());

    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,

    pub usingnamespace NodeAllocator("var", @This(), &.{});
};

pub const Block = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    args: ArrayList(*Var),
    body: Node,
    call: ?*Call = null,
    splat_index: ?i32 = null,

    pub usingnamespace NodeAllocator("block", @This(), &.{"body"});
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

    pub usingnamespace NodeAllocator("call", @This(), &.{});
};

pub const NamedArgument = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    value: Node,

    pub usingnamespace NodeAllocator("named_argument", @This(), &.{});
};

pub const If = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    then: Node,
    @"else": Node,
    is_ternary: bool = false,

    pub usingnamespace NodeAllocator(
        "if",
        @This(),
        &.{ "then", "else" },
    );
};

pub const Unless = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    then: Node,
    @"else": Node,

    pub usingnamespace NodeAllocator(
        "unless",
        @This(),
        &.{ "then", "else" },
    );
};

pub const Assign = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    target: Node,
    value: Node,
    doc: ?[]const u8 = null,

    pub usingnamespace NodeAllocator("assign", @This(), &.{});
};

pub const OpAssign = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    target: Node,
    op: []const u8,
    value: Node,

    pub usingnamespace NodeAllocator("op_assign", @This(), &.{});
};

pub const MultiAssign = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    targets: ArrayList(Node),
    values: ArrayList(Node),

    pub usingnamespace NodeAllocator("multi_assign", @This(), &.{});
};

fn SimpleNamedNode(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        name: []const u8,

        pub usingnamespace NodeAllocator(name, @This(), &.{});
    };
}

pub const InstanceVar = SimpleNamedNode("instance_var");

pub const ReadInstanceVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    name: []const u8,

    pub usingnamespace NodeAllocator("read_instance_var", @This(), &.{});
};

pub const ClassVar = SimpleNamedNode("class_var");

pub const Global = SimpleNamedNode("global");

fn BinaryOp(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        left: Node,
        right: Node,

        pub usingnamespace NodeAllocator(name, @This(), &.{});
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

    pub usingnamespace NodeAllocator("arg", @This(), &.{});
};

pub const ProcNotation = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    inputs: ?ArrayList(Node) = null,
    output: ?Node = null,

    pub usingnamespace NodeAllocator("proc_notation", @This(), &.{});
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

    pub usingnamespace NodeAllocator("def", @This(), &.{"body"});
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

    pub usingnamespace NodeAllocator("macro", @This(), &.{"body"});
};

fn UnaryExpression(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        exp: Node,

        pub usingnamespace NodeAllocator(name, @This(), &.{});
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

    pub usingnamespace NodeAllocator("offset_of", @This(), &.{});
};

pub const VisibilityModifier = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    modifier: Visibility,
    exp: Node,
    doc: ?[]const u8 = null,

    pub usingnamespace NodeAllocator("visibility_modifier", @This(), &.{});
};

pub const IsA = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    @"const": Node,
    is_nil_check: bool = false,

    pub usingnamespace NodeAllocator("is_a", @This(), &.{});
};

pub const RespondsTo = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    name: []const u8,

    pub usingnamespace NodeAllocator("responds_to", @This(), &.{});
};

pub const Require = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    string: []const u8,

    pub usingnamespace NodeAllocator("require", @This(), &.{});
};

pub usingnamespace struct {
    // Conflicts with Select.When
    pub const When = CaseWhen;
};

const CaseWhen = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    conds: ArrayList(Node),
    body: Node,
    is_exhaustive: bool = false,

    pub usingnamespace NodeAllocator("when", @This(), &.{"body"});
};

pub const Case = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: ?Node,
    whens: ArrayList(*CaseWhen),
    @"else": ?Node,
    is_exhaustive: bool,

    pub usingnamespace NodeAllocator("case", @This(), &.{});
};

pub const Select = struct {
    pub const When = struct {
        condition: Node,
        body: Node,

        pub fn init(condition: Node, body: Node) When {
            return .{ .condition = condition, .body = body };
        }
    };

    location: ?Location = null,
    end_location: ?Location = null,

    whens: ArrayList(When),
    @"else": ?Node = null,

    pub usingnamespace NodeAllocator("select", @This(), &.{});
};

pub const ImplicitObj = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    pub usingnamespace NodeAllocator("implicit_obj", @This(), &.{});
};

pub const Path = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    names: ArrayList([]const u8),
    is_global: bool = false,
    visibility: Visibility = .public,

    pub usingnamespace NodeAllocator("path", @This(), &.{});

    pub fn global(allocator: Allocator, name: []const u8) !Node {
        var names = ArrayList([]const u8).init(allocator);
        try names.append(name);
        return Node.init(try Path.allocate(allocator, .{
            .names = names,
            .is_global = true,
        }));
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

    pub usingnamespace NodeAllocator("class_def", @This(), &.{"body"});
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

    pub usingnamespace NodeAllocator("module_def", @This(), &.{"body"});
};

pub const AnnotationDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    doc: ?[]const u8 = null,

    pub usingnamespace NodeAllocator("annotation_def", @This(), &.{});
};

pub const While = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    body: Node,

    pub usingnamespace NodeAllocator("while", @This(), &.{"body"});
};

pub const Until = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    body: Node,

    pub usingnamespace NodeAllocator("until", @This(), &.{"body"});
};

pub const Generic = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,
    type_vars: ArrayList(Node),
    names_args: ?ArrayList(*NamedArgument) = null,
    suffix: Suffix = .none,

    pub const Suffix = enum {
        none,
        question,
        asterisk,
        bracket,
    };

    pub usingnamespace NodeAllocator("generic", @This(), &.{});
};

pub const TypeDeclaration = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    @"var": Node,
    declared_type: Node,
    value: ?Node = null,

    pub usingnamespace NodeAllocator("type_declaration", @This(), &.{});
};

pub const UninitializedVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    @"var": Node,
    declared_type: Node,

    pub usingnamespace NodeAllocator("uninitialized_var", @This(), &.{});
};

pub const Rescue = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    body: Node,
    types: ?ArrayList(Node) = null,
    name: ?[]const u8 = null,

    pub usingnamespace NodeAllocator("rescue", @This(), &.{"body"});
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

    pub usingnamespace NodeAllocator(
        "exception_handler",
        @This(),
        &.{"body"},
    );
};

pub const ProcLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    def: *Def,

    pub usingnamespace NodeAllocator("proc_literal", @This(), &.{});
};

pub const ProcPointer = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: ?Node,
    name: []const u8,
    args: ArrayList(Node),

    pub usingnamespace NodeAllocator("proc_pointer", @This(), &.{});
};

pub const Union = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    types: ArrayList(Node),

    pub usingnamespace NodeAllocator("union", @This(), &.{});
};

pub const Self = Singleton("self");

fn ControlExpression(comptime name: []const u8) type {
    return struct {
        location: ?Location = null,
        end_location: ?Location = null,

        exp: ?Node = null,

        pub usingnamespace NodeAllocator(name, @This(), &.{});
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

    pub usingnamespace NodeAllocator("yield", @This(), &.{});
};

pub const Include = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,

    pub usingnamespace NodeAllocator("include", @This(), &.{});
};

pub const Extend = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,

    pub usingnamespace NodeAllocator("extend", @This(), &.{});
};

pub const LibDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    body: Node,
    name_location: ?Location = null,
    visibility: Visibility = .public,

    pub usingnamespace NodeAllocator("lib_def", @This(), &.{"body"});
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

    pub usingnamespace NodeAllocator("fun_def", @This(), &.{});
};

pub const TypeDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    type_spec: Node,

    pub usingnamespace NodeAllocator("type_def", @This(), &.{});
};

pub const CStructOrUnionDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    body: Node,
    is_union: bool = false,

    pub usingnamespace NodeAllocator(
        "c_struct_or_union_def",
        @This(),
        &.{"body"},
    );
};

pub const EnumDef = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    members: ArrayList(Node),
    base_type: ?Node = null,
    doc: ?[]const u8 = null,
    visibility: Visibility = .public,

    pub usingnamespace NodeAllocator("enum_def", @This(), &.{});
};

pub const ExternalVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    type_spec: Node,
    real_name: ?[]const u8 = null,

    pub usingnamespace NodeAllocator("external_var", @This(), &.{});
};

pub const Alias = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: *Path,
    value: Node,
    doc: ?[]const u8 = null,
    visibility: Visibility = .public,

    pub usingnamespace NodeAllocator("alias", @This(), &.{});
};

pub const Metaclass = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: Node,

    pub usingnamespace NodeAllocator("metaclass", @This(), &.{});
};

pub const Cast = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    to: Node,

    pub usingnamespace NodeAllocator("cast", @This(), &.{});
};

pub const NilableCast = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    obj: Node,
    to: Node,

    pub usingnamespace NodeAllocator("nilable_cast", @This(), &.{});
};

pub const TypeOf = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    expressions: ArrayList(Node),

    pub usingnamespace NodeAllocator("type_of", @This(), &.{});
};

pub const Annotation = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    path: Path,
    args: ArrayList(Node),
    named_args: ?ArrayList(*NamedArgument) = null,
    doc: ?[]const u8 = null,

    pub usingnamespace NodeAllocator("annotation", @This(), &.{});
};

pub const MacroExpression = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    exp: Node,
    output: bool = true,

    pub usingnamespace NodeAllocator("macro_expression", @This(), &.{});
};

pub const MacroLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,

    pub usingnamespace NodeAllocator("macro_literal", @This(), &.{});
};

pub const MacroVerbatim = UnaryExpression("macro_verbatim");

pub const MacroIf = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    cond: Node,
    then: Node,
    @"else": Node,

    pub usingnamespace NodeAllocator(
        "macro_if",
        @This(),
        &.{ "then", "else" },
    );
};

pub const MacroFor = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    vars: ArrayList(*Var),
    exp: Node,
    body: Node,

    pub usingnamespace NodeAllocator("macro_for", @This(), &.{});
};

pub const MacroVar = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    name: []const u8,
    exps: ?ArrayList(Node) = null,

    pub usingnamespace NodeAllocator("macro_var", @This(), &.{});
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

    pub usingnamespace NodeAllocator("asm", @This(), &.{});
};

pub const AsmOperand = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    constraint: []const u8,
    exp: Node,

    pub usingnamespace NodeAllocator("asm_operand", @This(), &.{});
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
    p("{}\n", .{try Block.node(allocator, .{ .args = ArrayList(*Var).init(allocator), .body = null })});
    p("{}\n", .{try If.node(allocator, .{ .cond = try BoolLiteral.node(allocator, .{ .value = true }), .then = null, .@"else" = null })});
    p("{}\n", .{try Unless.node(allocator, .{ .cond = try BoolLiteral.node(allocator, .{ .value = true }), .then = null, .@"else" = null })});
    p("{}\n", .{try Def.node(allocator, .{ .name = "foo", .args = ArrayList(*Arg).init(allocator), .body = null })});
    p("{}\n", .{try CaseWhen.node(allocator, .{ .conds = ArrayList(Node).init(allocator), .body = null })});
    p("{}\n", .{try Path.allocate(allocator, .{ .names = ArrayList([]const u8).init(allocator), .is_global = false })});
    p("{}\n", .{Node.init(try Path.allocate(allocator, .{ .names = ArrayList([]const u8).init(allocator), .is_global = false }))});
    p("{}\n", .{try ClassDef.node(allocator, .{ .name = try Path.allocate(allocator, .{ .names = ArrayList([]const u8).init(allocator), .is_global = false }), .body = null })});
    p("{}\n", .{try ModuleDef.node(allocator, .{ .name = try Path.allocate(allocator, .{ .names = ArrayList([]const u8).init(allocator), .is_global = false }), .body = null })});
    p("{}\n", .{try While.node(allocator, .{ .cond = try BoolLiteral.node(allocator, .{ .value = true }), .body = null })});
    p("{}\n", .{try Until.node(allocator, .{ .cond = try BoolLiteral.node(allocator, .{ .value = true }), .body = null })});
    p("{}\n", .{try Rescue.node(allocator, .{ .body = null })});
    p("{}\n", .{try ExceptionHandler.node(allocator, .{ .body = null })});
    p("{}\n", .{try LibDef.node(allocator, .{ .name = "Foo", .body = null })});
    p("{}\n", .{try CStructOrUnionDef.node(allocator, .{ .name = "Foo", .body = null })});
    p("{}\n", .{try MacroIf.node(allocator, .{ .cond = try BoolLiteral.node(allocator, .{ .value = true }), .then = null, .@"else" = null })});
    p("{}\n", .{(try Nop.node(allocator, .{})).isNop()});
    p("{}\n", .{(try BoolLiteral.node(allocator, .{ .value = true })).isTrueLiteral()});
    p("{}\n", .{(try BoolLiteral.node(allocator, .{ .value = false })).isFalseLiteral()});
    p("{}\n", .{(try NumberLiteral.fromNumber(allocator, 1)).singleExpression()});
    p("{}\n", .{(try Expressions.node(allocator, .{ .expressions = ArrayList(Node).init(allocator) })).singleExpression()});
    {
        var expressions = ArrayList(Node).init(allocator);
        try expressions.append(try NumberLiteral.fromNumber(allocator, 1));
        p("{}\n", .{(try Expressions.node(allocator, .{ .expressions = expressions })).singleExpression()});
    }
    {
        var expressions = ArrayList(Node).init(allocator);
        try expressions.append(try NumberLiteral.fromNumber(allocator, 1));
        try expressions.append(try NumberLiteral.fromNumber(allocator, 2));
        p("{}\n", .{(try Expressions.node(allocator, .{ .expressions = expressions })).singleExpression()});
    }
    {
        var values = ArrayList(Node).init(allocator);
        try values.append(try BoolLiteral.node(allocator, .{ .value = true }));
        try values.append(try BoolLiteral.node(allocator, .{ .value = false }));
        const array = try ArrayLiteral.node(allocator, .{ .elements = values });
        const array2 = try ArrayLiteral.map(allocator, values, .{}, struct {
            fn f(_: Allocator, node: Node) !Node {
                return try BoolLiteral.node(allocator, .{ .value = !node.bool_literal.value });
            }
        }.f);
        const array3 = try ArrayLiteral.mapWithIndex(allocator, values, struct {
            fn f(_: Allocator, node: Node, index: usize) !Node {
                return if (index == 0) try BoolLiteral.node(allocator, .{ .value = !node.bool_literal.value }) else node;
            }
        }.f);
        p("{}\n", .{array.array_literal.elements.items[0]});
        p("{}\n", .{array2.array_literal.elements.items[0]});
        p("{}\n", .{array3.array_literal.elements.items[0]});
        p("{}\n", .{array3.array_literal.elements.items[1]});
        var node = try Nop.node(allocator, .{});
        node.setLocation(Location.new("foo", 1, 2));
        node.setEndLocation(Location.new("bar", 3, 4));
        p("{?} {?}\n", .{node.location(), node.endLocation()});
    }
}
