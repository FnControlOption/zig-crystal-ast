# zig-crystal-ast

Crystal language parser and AST implemented in Zig (WIP)

Crystal:

```cr
abstract class ASTNode
  # ...
end

class While < ASTNode
  property cond : ASTNode
  property body : ASTNode

  def initialize(@cond, body = nil)
    @body = Expressions.from body
  end
end
```

Zig:

```zig
pub const Node = union(enum) {
    // ...
    @"while": *While,
    yield: *Yield,
};

pub const While = struct {
    cond: Node,
    body: Node,

    pub fn allocate(allocator: std.mem.Allocator, cond: Node, body: ?Node) !*@This() {
        var instance = try allocator.create(@This());
        instance.* = .{
            .cond = cond,
            .body = try Expressions.from(allocator, body),
        };
        return instance;
    }

    pub fn node(allocator: std.mem.Allocator, cond: Node, body: ?Node) !Node {
        return Node { .@"while" = try allocate(allocator, cond, body) };
    }
};
```
