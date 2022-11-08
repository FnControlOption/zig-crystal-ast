# zig-crystal-ast

Crystal AST implemented in Zig (WIP)

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

Zig (outdated, but same idea):

```zig
pub const ASTNode = union(enum) {
    // ...
    while_: *While,
    yield: *Yield,
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
```
