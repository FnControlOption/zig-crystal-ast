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
        return switch (node) {
            .alias => |alias| alias.location,
            .@"and" => |@"and"| @"and".location,
            .annotation => |annotation| annotation.location,
            .annotation_def => |annotation_def| annotation_def.location,
            .arg => |arg| arg.location,
            .array_literal => |array_literal| array_literal.location,
            .@"asm" => |@"asm"| @"asm".location,
            .asm_operand => |asm_operand| asm_operand.location,
            .assign => |assign| assign.location,
            .block => |block| block.location,
            .bool_literal => |bool_literal| bool_literal.location,
            .@"break" => |@"break"| @"break".location,
            .c_struct_or_union_def => |c_struct_or_union_def| c_struct_or_union_def.location,
            .call => |call| call.location,
            .case => |case| case.location,
            .cast => |cast| cast.location,
            .char_literal => |char_literal| char_literal.location,
            .class_def => |class_def| class_def.location,
            .class_var => |class_var| class_var.location,
            .def => |def| def.location,
            .double_splat => |double_splat| double_splat.location,
            .enum_def => |enum_def| enum_def.location,
            .exception_handler => |exception_handler| exception_handler.location,
            .expressions => |expressions| expressions.location,
            .extend => |extend| extend.location,
            .external_var => |external_var| external_var.location,
            .fun_def => |fun_def| fun_def.location,
            .generic => |generic| generic.location,
            .global => |global| global.location,
            .hash_literal => |hash_literal| hash_literal.location,
            .@"if" => |@"if"| @"if".location,
            .implicit_obj => |implicit_obj| implicit_obj.location,
            .include => |include| include.location,
            .instance_size_of => |instance_size_of| instance_size_of.location,
            .instance_var => |instance_var| instance_var.location,
            .is_a => |is_a| is_a.location,
            .lib_def => |lib_def| lib_def.location,
            .macro => |macro| macro.location,
            .macro_expression => |macro_expression| macro_expression.location,
            .macro_for => |macro_for| macro_for.location,
            .macro_if => |macro_if| macro_if.location,
            .macro_literal => |macro_literal| macro_literal.location,
            .macro_var => |macro_var| macro_var.location,
            .macro_verbatim => |macro_verbatim| macro_verbatim.location,
            .magic_constant => |magic_constant| magic_constant.location,
            .metaclass => |metaclass| metaclass.location,
            .module_def => |module_def| module_def.location,
            .multi_assign => |multi_assign| multi_assign.location,
            .named_argument => |named_argument| named_argument.location,
            .named_tuple_literal => |named_tuple_literal| named_tuple_literal.location,
            .next => |next| next.location,
            .nil => |nil| nil.location,
            .nilable_cast => |nilable_cast| nilable_cast.location,
            .nop => |nop| nop.location,
            .not => |not| not.location,
            .number_literal => |number_literal| number_literal.location,
            .offset_of => |offset_of| offset_of.location,
            .op_assign => |op_assign| op_assign.location,
            .@"or" => |@"or"| @"or".location,
            .out => |out| out.location,
            .path => |path| path.location,
            .pointer_of => |pointer_of| pointer_of.location,
            .proc_literal => |proc_literal| proc_literal.location,
            .proc_notation => |proc_notation| proc_notation.location,
            .proc_pointer => |proc_pointer| proc_pointer.location,
            .range_literal => |range_literal| range_literal.location,
            .read_instance_var => |read_instance_var| read_instance_var.location,
            .regex_literal => |regex_literal| regex_literal.location,
            .require => |require| require.location,
            .rescue => |rescue| rescue.location,
            .responds_to => |responds_to| responds_to.location,
            .@"return" => |@"return"| @"return".location,
            .select => |select| select.location,
            .self => |self| self.location,
            .size_of => |size_of| size_of.location,
            .splat => |splat| splat.location,
            .string_interpolation => |string_interpolation| string_interpolation.location,
            .string_literal => |string_literal| string_literal.location,
            .symbol_literal => |symbol_literal| symbol_literal.location,
            .tuple_literal => |tuple_literal| tuple_literal.location,
            .type_declaration => |type_declaration| type_declaration.location,
            .type_def => |type_def| type_def.location,
            .type_of => |type_of| type_of.location,
            .underscore => |underscore| underscore.location,
            .uninitialized_var => |uninitialized_var| uninitialized_var.location,
            .@"union" => |@"union"| @"union".location,
            .unless => |unless| unless.location,
            .until => |until| until.location,
            .@"var" => |@"var"| @"var".location,
            .visibility_modifier => |visibility_modifier| visibility_modifier.location,
            .when => |when| when.location,
            .@"while" => |@"while"| @"while".location,
            .yield => |yield| yield.location,
        };
    }

    pub fn endLocation(node: Node) ?Location {
        return switch (node) {
            .alias => |alias| alias.end_location,
            .@"and" => |@"and"| @"and".end_location,
            .annotation => |annotation| annotation.end_location,
            .annotation_def => |annotation_def| annotation_def.end_location,
            .arg => |arg| arg.end_location,
            .array_literal => |array_literal| array_literal.end_location,
            .@"asm" => |@"asm"| @"asm".end_location,
            .asm_operand => |asm_operand| asm_operand.end_location,
            .assign => |assign| assign.end_location,
            .block => |block| block.end_location,
            .bool_literal => |bool_literal| bool_literal.end_location,
            .@"break" => |@"break"| @"break".end_location,
            .c_struct_or_union_def => |c_struct_or_union_def| c_struct_or_union_def.end_location,
            .call => |call| call.end_location,
            .case => |case| case.end_location,
            .cast => |cast| cast.end_location,
            .char_literal => |char_literal| char_literal.end_location,
            .class_def => |class_def| class_def.end_location,
            .class_var => |class_var| class_var.end_location,
            .def => |def| def.end_location,
            .double_splat => |double_splat| double_splat.end_location,
            .enum_def => |enum_def| enum_def.end_location,
            .exception_handler => |exception_handler| exception_handler.end_location,
            .expressions => |expressions| expressions.end_location,
            .extend => |extend| extend.end_location,
            .external_var => |external_var| external_var.end_location,
            .fun_def => |fun_def| fun_def.end_location,
            .generic => |generic| generic.end_location,
            .global => |global| global.end_location,
            .hash_literal => |hash_literal| hash_literal.end_location,
            .@"if" => |@"if"| @"if".end_location,
            .implicit_obj => |implicit_obj| implicit_obj.end_location,
            .include => |include| include.end_location,
            .instance_size_of => |instance_size_of| instance_size_of.end_location,
            .instance_var => |instance_var| instance_var.end_location,
            .is_a => |is_a| is_a.end_location,
            .lib_def => |lib_def| lib_def.end_location,
            .macro => |macro| macro.end_location,
            .macro_expression => |macro_expression| macro_expression.end_location,
            .macro_for => |macro_for| macro_for.end_location,
            .macro_if => |macro_if| macro_if.end_location,
            .macro_literal => |macro_literal| macro_literal.end_location,
            .macro_var => |macro_var| macro_var.end_location,
            .macro_verbatim => |macro_verbatim| macro_verbatim.end_location,
            .magic_constant => |magic_constant| magic_constant.end_location,
            .metaclass => |metaclass| metaclass.end_location,
            .module_def => |module_def| module_def.end_location,
            .multi_assign => |multi_assign| multi_assign.end_location,
            .named_argument => |named_argument| named_argument.end_location,
            .named_tuple_literal => |named_tuple_literal| named_tuple_literal.end_location,
            .next => |next| next.end_location,
            .nil => |nil| nil.end_location,
            .nilable_cast => |nilable_cast| nilable_cast.end_location,
            .nop => |nop| nop.end_location,
            .not => |not| not.end_location,
            .number_literal => |number_literal| number_literal.end_location,
            .offset_of => |offset_of| offset_of.end_location,
            .op_assign => |op_assign| op_assign.end_location,
            .@"or" => |@"or"| @"or".end_location,
            .out => |out| out.end_location,
            .path => |path| path.end_location,
            .pointer_of => |pointer_of| pointer_of.end_location,
            .proc_literal => |proc_literal| proc_literal.end_location,
            .proc_notation => |proc_notation| proc_notation.end_location,
            .proc_pointer => |proc_pointer| proc_pointer.end_location,
            .range_literal => |range_literal| range_literal.end_location,
            .read_instance_var => |read_instance_var| read_instance_var.end_location,
            .regex_literal => |regex_literal| regex_literal.end_location,
            .require => |require| require.end_location,
            .rescue => |rescue| rescue.end_location,
            .responds_to => |responds_to| responds_to.end_location,
            .@"return" => |@"return"| @"return".end_location,
            .select => |select| select.end_location,
            .self => |self| self.end_location,
            .size_of => |size_of| size_of.end_location,
            .splat => |splat| splat.end_location,
            .string_interpolation => |string_interpolation| string_interpolation.end_location,
            .string_literal => |string_literal| string_literal.end_location,
            .symbol_literal => |symbol_literal| symbol_literal.end_location,
            .tuple_literal => |tuple_literal| tuple_literal.end_location,
            .type_declaration => |type_declaration| type_declaration.end_location,
            .type_def => |type_def| type_def.end_location,
            .type_of => |type_of| type_of.end_location,
            .underscore => |underscore| underscore.end_location,
            .uninitialized_var => |uninitialized_var| uninitialized_var.end_location,
            .@"union" => |@"union"| @"union".end_location,
            .unless => |unless| unless.end_location,
            .until => |until| until.end_location,
            .@"var" => |@"var"| @"var".end_location,
            .visibility_modifier => |visibility_modifier| visibility_modifier.end_location,
            .when => |when| when.end_location,
            .@"while" => |@"while"| @"while".end_location,
            .yield => |yield| yield.end_location,
        };
    }

    pub fn setLocation(node: Node, loc: ?Location) void {
        switch (node) {
            .alias => |alias| alias.location = loc,
            .@"and" => |@"and"| @"and".location = loc,
            .annotation => |annotation| annotation.location = loc,
            .annotation_def => |annotation_def| annotation_def.location = loc,
            .arg => |arg| arg.location = loc,
            .array_literal => |array_literal| array_literal.location = loc,
            .@"asm" => |@"asm"| @"asm".location = loc,
            .asm_operand => |asm_operand| asm_operand.location = loc,
            .assign => |assign| assign.location = loc,
            .block => |block| block.location = loc,
            .bool_literal => |bool_literal| bool_literal.location = loc,
            .@"break" => |@"break"| @"break".location = loc,
            .c_struct_or_union_def => |c_struct_or_union_def| c_struct_or_union_def.location = loc,
            .call => |call| call.location = loc,
            .case => |case| case.location = loc,
            .cast => |cast| cast.location = loc,
            .char_literal => |char_literal| char_literal.location = loc,
            .class_def => |class_def| class_def.location = loc,
            .class_var => |class_var| class_var.location = loc,
            .def => |def| def.location = loc,
            .double_splat => |double_splat| double_splat.location = loc,
            .enum_def => |enum_def| enum_def.location = loc,
            .exception_handler => |exception_handler| exception_handler.location = loc,
            .expressions => |expressions| expressions.location = loc,
            .extend => |extend| extend.location = loc,
            .external_var => |external_var| external_var.location = loc,
            .fun_def => |fun_def| fun_def.location = loc,
            .generic => |generic| generic.location = loc,
            .global => |global| global.location = loc,
            .hash_literal => |hash_literal| hash_literal.location = loc,
            .@"if" => |@"if"| @"if".location = loc,
            .implicit_obj => |implicit_obj| implicit_obj.location = loc,
            .include => |include| include.location = loc,
            .instance_size_of => |instance_size_of| instance_size_of.location = loc,
            .instance_var => |instance_var| instance_var.location = loc,
            .is_a => |is_a| is_a.location = loc,
            .lib_def => |lib_def| lib_def.location = loc,
            .macro => |macro| macro.location = loc,
            .macro_expression => |macro_expression| macro_expression.location = loc,
            .macro_for => |macro_for| macro_for.location = loc,
            .macro_if => |macro_if| macro_if.location = loc,
            .macro_literal => |macro_literal| macro_literal.location = loc,
            .macro_var => |macro_var| macro_var.location = loc,
            .macro_verbatim => |macro_verbatim| macro_verbatim.location = loc,
            .magic_constant => |magic_constant| magic_constant.location = loc,
            .metaclass => |metaclass| metaclass.location = loc,
            .module_def => |module_def| module_def.location = loc,
            .multi_assign => |multi_assign| multi_assign.location = loc,
            .named_argument => |named_argument| named_argument.location = loc,
            .named_tuple_literal => |named_tuple_literal| named_tuple_literal.location = loc,
            .next => |next| next.location = loc,
            .nil => |nil| nil.location = loc,
            .nilable_cast => |nilable_cast| nilable_cast.location = loc,
            .nop => |nop| nop.location = loc,
            .not => |not| not.location = loc,
            .number_literal => |number_literal| number_literal.location = loc,
            .offset_of => |offset_of| offset_of.location = loc,
            .op_assign => |op_assign| op_assign.location = loc,
            .@"or" => |@"or"| @"or".location = loc,
            .out => |out| out.location = loc,
            .path => |path| path.location = loc,
            .pointer_of => |pointer_of| pointer_of.location = loc,
            .proc_literal => |proc_literal| proc_literal.location = loc,
            .proc_notation => |proc_notation| proc_notation.location = loc,
            .proc_pointer => |proc_pointer| proc_pointer.location = loc,
            .range_literal => |range_literal| range_literal.location = loc,
            .read_instance_var => |read_instance_var| read_instance_var.location = loc,
            .regex_literal => |regex_literal| regex_literal.location = loc,
            .require => |require| require.location = loc,
            .rescue => |rescue| rescue.location = loc,
            .responds_to => |responds_to| responds_to.location = loc,
            .@"return" => |@"return"| @"return".location = loc,
            .select => |select| select.location = loc,
            .self => |self| self.location = loc,
            .size_of => |size_of| size_of.location = loc,
            .splat => |splat| splat.location = loc,
            .string_interpolation => |string_interpolation| string_interpolation.location = loc,
            .string_literal => |string_literal| string_literal.location = loc,
            .symbol_literal => |symbol_literal| symbol_literal.location = loc,
            .tuple_literal => |tuple_literal| tuple_literal.location = loc,
            .type_declaration => |type_declaration| type_declaration.location = loc,
            .type_def => |type_def| type_def.location = loc,
            .type_of => |type_of| type_of.location = loc,
            .underscore => |underscore| underscore.location = loc,
            .uninitialized_var => |uninitialized_var| uninitialized_var.location = loc,
            .@"union" => |@"union"| @"union".location = loc,
            .unless => |unless| unless.location = loc,
            .until => |until| until.location = loc,
            .@"var" => |@"var"| @"var".location = loc,
            .visibility_modifier => |visibility_modifier| visibility_modifier.location = loc,
            .when => |when| when.location = loc,
            .@"while" => |@"while"| @"while".location = loc,
            .yield => |yield| yield.location = loc,
        }
    }

    pub fn setEndLocation(node: Node, end_location: ?Location) void {
        switch (node) {
            .alias => |alias| alias.end_location = end_location,
            .@"and" => |@"and"| @"and".end_location = end_location,
            .annotation => |annotation| annotation.end_location = end_location,
            .annotation_def => |annotation_def| annotation_def.end_location = end_location,
            .arg => |arg| arg.end_location = end_location,
            .array_literal => |array_literal| array_literal.end_location = end_location,
            .@"asm" => |@"asm"| @"asm".end_location = end_location,
            .asm_operand => |asm_operand| asm_operand.end_location = end_location,
            .assign => |assign| assign.end_location = end_location,
            .block => |block| block.end_location = end_location,
            .bool_literal => |bool_literal| bool_literal.end_location = end_location,
            .@"break" => |@"break"| @"break".end_location = end_location,
            .c_struct_or_union_def => |c_struct_or_union_def| c_struct_or_union_def.end_location = end_location,
            .call => |call| call.end_location = end_location,
            .case => |case| case.end_location = end_location,
            .cast => |cast| cast.end_location = end_location,
            .char_literal => |char_literal| char_literal.end_location = end_location,
            .class_def => |class_def| class_def.end_location = end_location,
            .class_var => |class_var| class_var.end_location = end_location,
            .def => |def| def.end_location = end_location,
            .double_splat => |double_splat| double_splat.end_location = end_location,
            .enum_def => |enum_def| enum_def.end_location = end_location,
            .exception_handler => |exception_handler| exception_handler.end_location = end_location,
            .expressions => |expressions| expressions.end_location = end_location,
            .extend => |extend| extend.end_location = end_location,
            .external_var => |external_var| external_var.end_location = end_location,
            .fun_def => |fun_def| fun_def.end_location = end_location,
            .generic => |generic| generic.end_location = end_location,
            .global => |global| global.end_location = end_location,
            .hash_literal => |hash_literal| hash_literal.end_location = end_location,
            .@"if" => |@"if"| @"if".end_location = end_location,
            .implicit_obj => |implicit_obj| implicit_obj.end_location = end_location,
            .include => |include| include.end_location = end_location,
            .instance_size_of => |instance_size_of| instance_size_of.end_location = end_location,
            .instance_var => |instance_var| instance_var.end_location = end_location,
            .is_a => |is_a| is_a.end_location = end_location,
            .lib_def => |lib_def| lib_def.end_location = end_location,
            .macro => |macro| macro.end_location = end_location,
            .macro_expression => |macro_expression| macro_expression.end_location = end_location,
            .macro_for => |macro_for| macro_for.end_location = end_location,
            .macro_if => |macro_if| macro_if.end_location = end_location,
            .macro_literal => |macro_literal| macro_literal.end_location = end_location,
            .macro_var => |macro_var| macro_var.end_location = end_location,
            .macro_verbatim => |macro_verbatim| macro_verbatim.end_location = end_location,
            .magic_constant => |magic_constant| magic_constant.end_location = end_location,
            .metaclass => |metaclass| metaclass.end_location = end_location,
            .module_def => |module_def| module_def.end_location = end_location,
            .multi_assign => |multi_assign| multi_assign.end_location = end_location,
            .named_argument => |named_argument| named_argument.end_location = end_location,
            .named_tuple_literal => |named_tuple_literal| named_tuple_literal.end_location = end_location,
            .next => |next| next.end_location = end_location,
            .nil => |nil| nil.end_location = end_location,
            .nilable_cast => |nilable_cast| nilable_cast.end_location = end_location,
            .nop => |nop| nop.end_location = end_location,
            .not => |not| not.end_location = end_location,
            .number_literal => |number_literal| number_literal.end_location = end_location,
            .offset_of => |offset_of| offset_of.end_location = end_location,
            .op_assign => |op_assign| op_assign.end_location = end_location,
            .@"or" => |@"or"| @"or".end_location = end_location,
            .out => |out| out.end_location = end_location,
            .path => |path| path.end_location = end_location,
            .pointer_of => |pointer_of| pointer_of.end_location = end_location,
            .proc_literal => |proc_literal| proc_literal.end_location = end_location,
            .proc_notation => |proc_notation| proc_notation.end_location = end_location,
            .proc_pointer => |proc_pointer| proc_pointer.end_location = end_location,
            .range_literal => |range_literal| range_literal.end_location = end_location,
            .read_instance_var => |read_instance_var| read_instance_var.end_location = end_location,
            .regex_literal => |regex_literal| regex_literal.end_location = end_location,
            .require => |require| require.end_location = end_location,
            .rescue => |rescue| rescue.end_location = end_location,
            .responds_to => |responds_to| responds_to.end_location = end_location,
            .@"return" => |@"return"| @"return".end_location = end_location,
            .select => |select| select.end_location = end_location,
            .self => |self| self.end_location = end_location,
            .size_of => |size_of| size_of.end_location = end_location,
            .splat => |splat| splat.end_location = end_location,
            .string_interpolation => |string_interpolation| string_interpolation.end_location = end_location,
            .string_literal => |string_literal| string_literal.end_location = end_location,
            .symbol_literal => |symbol_literal| symbol_literal.end_location = end_location,
            .tuple_literal => |tuple_literal| tuple_literal.end_location = end_location,
            .type_declaration => |type_declaration| type_declaration.end_location = end_location,
            .type_def => |type_def| type_def.end_location = end_location,
            .type_of => |type_of| type_of.end_location = end_location,
            .underscore => |underscore| underscore.end_location = end_location,
            .uninitialized_var => |uninitialized_var| uninitialized_var.end_location = end_location,
            .@"union" => |@"union"| @"union".end_location = end_location,
            .unless => |unless| unless.end_location = end_location,
            .until => |until| until.end_location = end_location,
            .@"var" => |@"var"| @"var".end_location = end_location,
            .visibility_modifier => |visibility_modifier| visibility_modifier.end_location = end_location,
            .when => |when| when.end_location = end_location,
            .@"while" => |@"while"| @"while".end_location = end_location,
            .yield => |yield| yield.end_location = end_location,
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
            else => return node
        }
    }

    pub fn isSingleExpression(node: Node) bool {
        return switch (node) {
            .expressions => |expressions| expressions.isSingleExpression(),
            else => false
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

        pub fn new(allocator: Allocator) !Node {
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
    keyword: Keyword = .none,

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

    pub fn new(allocator: Allocator, value: bool) !Node {
        return Node { .bool_literal = try allocate(allocator, value) };
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
            .i8   => 8,
            .i16  => 16,
            .i32  => 32,
            .i64  => 64,
            .i128 => 128,
            .u8   => 8,
            .u16  => 16,
            .u32  => 32,
            .u64  => 64,
            .u128 => 128,
            .f32  => 32,
            .f64  => 64,
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
            i8   => return .i8,
            i16  => return .i16,
            i32  => return .i32,
            i64  => return .i64,
            i128 => return .i128,
            u8   => return .u8,
            u16  => return .u16,
            u32  => return .u32,
            u64  => return .u64,
            u128 => return .u128,
            f32  => return .f32,
            f64  => return .f64,
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
            .i8   => i8,
            .i16  => i16,
            .i32  => i32,
            .i64  => i64,
            .i128 => i128,
            .u8   => u8,
            .u16  => u16,
            .u32  => u32,
            .u64  => u64,
            .u128 => u128,
            .f32  => f32,
            .f64  => f64,
        };
    }
};

pub const NumberLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    value: []const u8,
    kind: NumberKind = .i32,

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

    expressions: ArrayList(Node),
    heredoc_indent: i32 = 0,

    pub fn allocate(allocator: Allocator, expressions: ArrayList(Node)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .expressions = expressions };
        return instance;
    }

    pub fn new(allocator: Allocator, expressions: ArrayList(Node)) !Node {
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

    elements: ArrayList(Node),
    of: ?Node = null,
    name: ?Node = null,

    pub fn allocate(allocator: Allocator, elements: ArrayList(Node)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .elements = elements };
        return instance;
    }

    pub fn new(allocator: Allocator, elements: ArrayList(Node)) !Node {
        return Node { .array_literal = try allocate(allocator, elements) };
    }

    pub fn map(allocator: Allocator, values: anytype, block: anytype) !Node {
        // TODO: validate values and block
        var new_values = try ArrayList(Node).initCapacity(allocator, values.items.len);
        for (values.items) |value| {
            new_values.appendAssumeCapacity(try block(allocator, value));
        }
        return new(allocator, new_values);
    }

    pub fn mapWithIndex(allocator: Allocator, values: anytype, block: anytype) !Node {
        // TODO: validate values and block
        var new_values = try ArrayList(Node).initCapacity(allocator, values.items.len);
        for (values.items) |value, index| {
            new_values.appendAssumeCapacity(try block(allocator, value, index));
        }
        return new(allocator, new_values);
    }
};

pub const HashLiteral = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    entries: ArrayList(Entry),
    of: ?Node = null,
    name: ?Node = null,

    pub fn allocate(allocator: Allocator, entries: ArrayList(Entry)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .entries = entries };
        return instance;
    }

    pub fn new(allocator: Allocator, entries: ArrayList(Entry)) !Node {
        return Node { .hash_literal = try allocate(allocator, entries) };
    }

    pub const Entry = struct { key: Node, value: Node };
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

    pub fn new(allocator: Allocator, entries: ArrayList(Entry)) !Node {
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

    elements: ArrayList(Node),

    pub fn allocate(allocator: Allocator, elements: ArrayList(Node)) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{ .elements = elements };
        return instance;
    }

    pub fn new(allocator: Allocator, elements: ArrayList(Node)) !Node {
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

    pub fn new(
        allocator: Allocator,
        args: ArrayList(*Var),
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
    visibility: Visibility = .public,
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

    inputs: ?ArrayList(Node) = null,
    output: ?Node = null,
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

    pub fn new(
        allocator: Allocator,
        name: []const u8,
        args: ArrayList(*Arg),
        body: ?Node,
    ) !Node {
        return Node { .def = try allocate(allocator, name, args, body) };
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

    pub fn new(
        allocator: Allocator,
        name: []const u8,
        args: ArrayList(*Arg),
        body: ?Node,
    ) !Node {
        return Node { .macro = try allocate(allocator, name, args, body) };
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

    pub fn new(
        allocator: Allocator,
        conds: ArrayList(Node),
        body: ?Node,
    ) !Node {
        return Node { .when = try allocate(allocator, conds, body) };
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

    pub fn new(
        allocator: Allocator,
        names: ArrayList([]const u8),
        is_global: bool,
    ) !Node {
        return Node { .path = try allocate(allocator, names, is_global) };
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
    type_vars: ArrayList(Node),
    names_args: ?ArrayList(*NamedArgument) = null,
    suffix: Suffix = .none,

    pub const Suffix = enum {
        none,
        question,
        asterisk,
        bracket,
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
    types: ?ArrayList(Node) = null,
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
    args: ArrayList(Node),
};

pub const Union = struct {
    location: ?Location = null,
    end_location: ?Location = null,

    types: ArrayList(Node),
};

pub const Self = Singleton("self");

comptime {
    // const allocator = std.heap.page_allocator;
    // var control_expression_types = ArrayList(type).init(allocator);
    // std.debug.print("{}\n", .{control_expression_types});
    // @compileLog("foobar");
    // @typeInfo(Fields).Struct.fields
}
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

pub fn main() !void {
    const p = @import("std").debug.print;
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
    // var list = ArrayList(Node).init(allocator);
    // list = ArrayList(Node).init(allocator); try list.append(try NumberLiteral.new(allocator, 2)); p("{}\n", .{try Expressions.from(allocator, list)});
    // list = ArrayList(Node).init(allocator); try list.append(try NumberLiteral.new(allocator, 3)); try list.append(try NumberLiteral.new(allocator, 4)); p("{any}\n", .{(try Expressions.from(allocator, list)).expressions.expressions});
    // }
    p("{}\n", .{try Block.new(allocator, ArrayList(*Var).init(allocator), null)});
    p("{}\n", .{try If.new(allocator, try BoolLiteral.new(allocator, true), null, null)});
    p("{}\n", .{try Unless.new(allocator, try BoolLiteral.new(allocator, true), null, null)});
    p("{}\n", .{try Def.new(allocator, "foo", ArrayList(*Arg).init(allocator), null)});
    p("{}\n", .{try When.new(allocator, ArrayList(Node).init(allocator), null)});
    p("{}\n", .{try Path.allocate(allocator, ArrayList([]const u8).init(allocator), false)});
    p("{}\n", .{try Path.new(allocator, ArrayList([]const u8).init(allocator), false)});
    p("{}\n", .{try ClassDef.new(allocator, try Path.allocate(allocator, ArrayList([]const u8).init(allocator), false), null)});
    p("{}\n", .{try ModuleDef.new(allocator, try Path.allocate(allocator, ArrayList([]const u8).init(allocator), false), null)});
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
        var values = ArrayList(Node).init(allocator);
        try values.append(try BoolLiteral.new(allocator, true));
        try values.append(try BoolLiteral.new(allocator, false));
        const array = try ArrayLiteral.new(allocator, values);
        const array2 = try ArrayLiteral.map(allocator, values, struct {
            fn f(_: Allocator, node: Node) !Node {
                return try BoolLiteral.new(allocator, !node.bool_literal.value);
            }
        }.f);
        const array3 = try ArrayLiteral.mapWithIndex(allocator, values, struct {
            fn f(_: Allocator, node: Node, index: usize) !Node {
                return if (index == 0) try BoolLiteral.new(allocator, !node.bool_literal.value) else node;
            }
        }.f);
        p("{}\n", .{array.array_literal.elements.items[0]});
        p("{}\n", .{array2.array_literal.elements.items[0]});
        p("{}\n", .{array3.array_literal.elements.items[0]});
        p("{}\n", .{array3.array_literal.elements.items[1]});
    }
}
