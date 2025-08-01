const std = @import("std");

pub const ModuleDefinition = struct {
    exports: std.ArrayListUnmanaged(Export) = .empty,
    name: ?[]const u8 = null,
    base_address: usize = 0,
    arena: std.heap.ArenaAllocator,

    pub const Export = struct {
        name: []const u8,
        external_name: ?[]const u8,
        import_name: ?[]const u8,
        ordinal: u16,
        type: std.coff.ImportType,
        private: bool,
    };

    pub fn deinit(self: *const ModuleDefinition) void {
        self.arena.deinit();
    }
};

pub const Diagnostics = struct {
    err: Error,
    token: Token,
    extra: Extra = .{ .none = {} },

    pub const Extra = union {
        none: void,
        expected: Token.Tag,
    };

    pub const Error = enum {
        invalid_byte,
        unfinished_quoted_identifier,
        /// `expected` is populated
        expected_token,
        expected_integer,
        unknown_statement,
        unimplemented,
    };
};

pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8, diagnostics: *Diagnostics) !ModuleDefinition {
    var tokenizer = Tokenizer.init(source);
    var parser = Parser.init(&tokenizer, diagnostics);

    return parser.parse(allocator);
}

const Token = struct {
    tag: Tag,
    start: usize,
    end: usize,

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "BASE", .keyword_base },
        .{ "CONSTANT", .keyword_constant },
        .{ "DATA", .keyword_data },
        .{ "EXPORTS", .keyword_exports },
        .{ "EXPORTAS", .keyword_exportas },
        .{ "HEAPSIZE", .keyword_heapsize },
        .{ "LIBRARY", .keyword_library },
        .{ "NAME", .keyword_name },
        .{ "NONAME", .keyword_noname },
        .{ "PRIVATE", .keyword_private },
        .{ "STACKSIZE", .keyword_stacksize },
        .{ "VERSION", .keyword_version },
    });

    pub const Tag = enum {
        invalid,
        eof,
        identifier,
        comma,
        equal,
        equal_equal,
        keyword_base,
        keyword_constant,
        keyword_data,
        keyword_exports,
        keyword_exportas,
        keyword_heapsize,
        keyword_library,
        keyword_name,
        keyword_noname,
        keyword_private,
        keyword_stacksize,
        keyword_version,
    };

    /// Returns a useful slice of the token, e.g. for quoted identifiers, this
    /// will return a slice without the quotes included.
    pub fn slice(self: Token, source: [:0]const u8) []const u8 {
        return source[self.start..self.end];
    }
};

const Tokenizer = struct {
    source: [:0]const u8,
    index: usize,
    error_context_token: ?Token = null,

    pub fn init(source: [:0]const u8) Tokenizer {
        return .{
            .source = source,
            .index = 0,
        };
    }

    const State = enum {
        start,
        identifier_or_keyword,
        quoted_identifier,
        comment,
        equal,
        eof_or_invalid,
    };

    pub const Error = error{
        InvalidByte,
        UnfinishedQuotedIdentifier,
    };

    pub fn next(self: *Tokenizer) Error!Token {
        var result: Token = .{
            .tag = undefined,
            .start = self.index,
            .end = undefined,
        };
        state: switch (State.start) {
            .start => switch (self.source[self.index]) {
                0 => continue :state .eof_or_invalid,
                '\r', '\n', ' ', '\t', '\x0B' => {
                    self.index += 1;
                    result.start = self.index;
                    continue :state .start;
                },
                ';' => continue :state .comment,
                '=' => continue :state .equal,
                ',' => {
                    result.tag = .comma;
                    self.index += 1;
                },
                '"' => continue :state .quoted_identifier,
                else => continue :state .identifier_or_keyword,
            },
            .comment => {
                self.index += 1;
                switch (self.source[self.index]) {
                    0 => continue :state .eof_or_invalid,
                    '\n' => {
                        self.index += 1;
                        result.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .comment,
                }
            },
            .equal => {
                self.index += 1;
                switch (self.source[self.index]) {
                    '=' => {
                        result.tag = .equal_equal;
                        self.index += 1;
                    },
                    else => result.tag = .equal,
                }
            },
            .quoted_identifier => {
                self.index += 1;
                switch (self.source[self.index]) {
                    0 => {
                        self.error_context_token = .{
                            .tag = .eof,
                            .start = self.index,
                            .end = self.index,
                        };
                        return error.UnfinishedQuotedIdentifier;
                    },
                    '"' => {
                        result.tag = .identifier;
                        self.index += 1;

                        // Return the token unquoted
                        return .{
                            .tag = result.tag,
                            .start = result.start + 1,
                            .end = self.index - 1,
                        };
                    },
                    else => continue :state .quoted_identifier,
                }
            },
            .identifier_or_keyword => {
                self.index += 1;
                switch (self.source[self.index]) {
                    0, '=', ',', ';', '\r', '\n', ' ', '\t', '\x0B' => {
                        const keyword = Token.keywords.get(self.source[result.start..self.index]);
                        result.tag = keyword orelse .identifier;
                    },
                    else => continue :state .identifier_or_keyword,
                }
            },
            .eof_or_invalid => {
                if (self.index == self.source.len) {
                    return .{
                        .tag = .eof,
                        .start = self.index,
                        .end = self.index,
                    };
                }
                self.error_context_token = .{
                    .tag = .invalid,
                    .start = self.index,
                    .end = self.index + 1,
                };
                return error.InvalidByte;
            },
        }

        result.end = self.index;
        return result;
    }
};

test Tokenizer {
    try testTokenizer(
        \\foo
        \\; hello
        \\BASE
        \\"bar"
        \\
    , &.{
        .identifier,
        .keyword_base,
        .identifier,
    });
}

fn testTokenizer(source: [:0]const u8, expected: []const Token.Tag) !void {
    var tokenizer = Tokenizer.init(source);
    for (expected) |expected_tag| {
        const token = try tokenizer.next();
        try std.testing.expectEqual(expected_tag, token.tag);
    }
    const last_token = try tokenizer.next();
    try std.testing.expectEqual(.eof, last_token.tag);
}

pub const Parser = struct {
    tokenizer: *Tokenizer,
    diagnostics: *Diagnostics,
    lookahead_tokenizer: Tokenizer,

    pub fn init(tokenizer: *Tokenizer, diagnostics: *Diagnostics) Parser {
        return .{
            .tokenizer = tokenizer,
            .diagnostics = diagnostics,
            .lookahead_tokenizer = undefined,
        };
    }

    pub const Error = error{ParseError} || std.mem.Allocator.Error;

    pub fn parse(self: *Parser, allocator: std.mem.Allocator) Error!ModuleDefinition {
        var module: ModuleDefinition = .{
            .arena = .init(allocator),
        };
        const arena = module.arena.allocator();
        errdefer module.deinit();
        while (true) {
            const tok = try self.nextToken();
            switch (tok.tag) {
                .eof => break,
                .keyword_library => {
                    const name = try self.lookaheadToken();
                    if (name.tag != .identifier) continue;
                    self.commitLookahead();

                    const base_tok = try self.lookaheadToken();
                    if (base_tok.tag == .keyword_base) {
                        self.commitLookahead();

                        _ = try self.expectToken(.equal);

                        module.base_address = try self.expectInteger(usize);
                    }

                    module.name = try arena.dupe(u8, name.slice(self.tokenizer.source));
                },
                .keyword_exports => {
                    while (true) {
                        const name_tok = try self.lookaheadToken();
                        if (name_tok.tag != .identifier) break;
                        self.commitLookahead();

                        const external_name_tok = ext_name: {
                            const equal = try self.lookaheadToken();
                            if (equal.tag != .equal) break :ext_name null;
                            self.commitLookahead();

                            break :ext_name try self.expectToken(.identifier);
                        };

                        // TODO: AddUnderscores

                        var import_name_tok: ?Token = null;
                        var ordinal: ?u16 = null;
                        var import_type: std.coff.ImportType = .CODE;
                        var private: bool = false;
                        while (true) {
                            const arg_tok = try self.lookaheadToken();
                            switch (arg_tok.tag) {
                                .identifier => {
                                    const slice = arg_tok.slice(self.tokenizer.source);
                                    if (slice[0] != '@') break;

                                    // foo @ 10
                                    if (slice.len == 1) {
                                        self.commitLookahead();
                                        ordinal = try self.expectInteger(u16);
                                        continue;
                                    }
                                    // foo @10
                                    ordinal = std.fmt.parseUnsigned(u16, slice[1..], 0) catch {
                                        // e.g. foo @bar, the @bar is presumed to be the start of a separate
                                        // export (and there could be a newline between them)
                                        break;
                                    };
                                    // finally safe to commit to consuming the token
                                    self.commitLookahead();

                                    const noname_tok = try self.lookaheadToken();
                                    if (noname_tok.tag == .keyword_noname) {
                                        self.commitLookahead();
                                        return self.unimplemented(noname_tok);
                                    }
                                },
                                .equal_equal => {
                                    self.commitLookahead();
                                    import_name_tok = try self.expectToken(.identifier);
                                },
                                .keyword_data => {
                                    self.commitLookahead();
                                    import_type = .DATA;
                                },
                                .keyword_constant => {
                                    self.commitLookahead();
                                    import_type = .CONST;
                                },
                                .keyword_private => {
                                    self.commitLookahead();
                                    private = true;
                                },
                                .keyword_exportas => return self.unimplemented(arg_tok),
                                else => break,
                            }
                        }

                        try module.exports.append(arena, .{
                            .name = try arena.dupe(u8, name_tok.slice(self.tokenizer.source)),
                            .external_name = if (external_name_tok) |ext_name| try arena.dupe(u8, ext_name.slice(self.tokenizer.source)) else null,
                            .import_name = if (import_name_tok) |imp_name| try arena.dupe(u8, imp_name.slice(self.tokenizer.source)) else null,
                            .ordinal = ordinal orelse 0,
                            .type = import_type,
                            .private = private,
                        });
                    }
                },
                .keyword_heapsize,
                .keyword_stacksize,
                .keyword_name,
                .keyword_version,
                => return self.unimplemented(tok),
                else => {
                    self.diagnostics.* = .{
                        .err = .unknown_statement,
                        .token = tok,
                    };
                    return error.ParseError;
                },
            }
        }
        return module;
    }

    fn expectInteger(self: *Parser, T: type) Error!T {
        const tok = try self.nextToken();
        blk: {
            if (tok.tag != .identifier) break :blk;
            return std.fmt.parseUnsigned(T, tok.slice(self.tokenizer.source), 0) catch break :blk;
        }
        self.diagnostics.* = .{
            .err = .expected_integer,
            .token = tok,
        };
        return error.ParseError;
    }

    fn unimplemented(self: *Parser, tok: Token) Error {
        self.diagnostics.* = .{
            .err = .unimplemented,
            .token = tok,
        };
        return error.ParseError;
    }

    fn expectToken(self: *Parser, tag: Token.Tag) Error!Token {
        const tok = try self.nextToken();
        if (tok.tag != tag) {
            self.diagnostics.* = .{
                .err = .expected_token,
                .token = tok,
                .extra = .{ .expected = tag },
            };
            return error.ParseError;
        }
        return tok;
    }

    fn nextToken(self: *Parser) Error!Token {
        return self.nextFromTokenizer(self.tokenizer);
    }

    fn lookaheadToken(self: *Parser) Error!Token {
        self.lookahead_tokenizer = self.tokenizer.*;
        return self.nextFromTokenizer(&self.lookahead_tokenizer);
    }

    fn commitLookahead(self: *Parser) void {
        self.tokenizer.* = self.lookahead_tokenizer;
    }

    fn nextFromTokenizer(
        self: *Parser,
        tokenizer: *Tokenizer,
    ) Error!Token {
        return tokenizer.next() catch |err| {
            self.diagnostics.* = .{
                .err = switch (err) {
                    error.InvalidByte => .invalid_byte,
                    error.UnfinishedQuotedIdentifier => .unfinished_quoted_identifier,
                },
                .token = tokenizer.error_context_token.?,
            };
            return error.ParseError;
        };
    }
};

test parse {
    var diagnostics: Diagnostics = undefined;
    const source =
        \\LIBRARY "foo"
        \\; hello
        \\EXPORTS
        \\foo @ 10
        \\bar @104
        \\foo == bar
        \\
    ;
    const module = parse(std.testing.allocator, source, &diagnostics) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.ParseError => {
            std.debug.print("{}: {} {s}\n", .{ diagnostics.err, diagnostics.token, diagnostics.token.slice(source) });
            return err;
        },
    };
    defer module.deinit();

    try std.testing.expectEqualStrings("foo", module.name.?);

    try std.testing.expectEqual(3, module.exports.items.len);

    {
        const ex = module.exports.items[0];
        try std.testing.expectEqualStrings("foo", ex.name);
        try std.testing.expectEqual(null, ex.import_name);
        try std.testing.expectEqual(null, ex.external_name);
        try std.testing.expectEqual(10, ex.ordinal);
    }
    {
        const ex = module.exports.items[1];
        try std.testing.expectEqualStrings("bar", ex.name);
        try std.testing.expectEqual(null, ex.import_name);
        try std.testing.expectEqual(null, ex.external_name);
        try std.testing.expectEqual(104, ex.ordinal);
    }
    {
        const ex = module.exports.items[2];
        try std.testing.expectEqualStrings("foo", ex.name);
        try std.testing.expectEqualStrings("bar", ex.import_name.?);
        try std.testing.expectEqual(null, ex.external_name);
        try std.testing.expectEqual(0, ex.ordinal);
    }
}

test "ntdll" {
    var diagnostics: Diagnostics = undefined;
    const source =
        \\;
        \\; Definition file of ntdll.dll
        \\; Automatic generated by gendef
        \\; written by Kai Tietz 2008
        \\;
        \\LIBRARY "ntdll.dll"
        \\EXPORTS
        \\RtlDispatchAPC@12
        \\RtlActivateActivationContextUnsafeFast@0
    ;
    const module = parse(std.testing.allocator, source, &diagnostics) catch |err| switch (err) {
        error.OutOfMemory => |e| return e,
        error.ParseError => {
            std.debug.print("{}: {} {s}\n", .{ diagnostics.err, diagnostics.token, diagnostics.token.slice(source) });
            return err;
        },
    };
    defer module.deinit();

    try std.testing.expectEqualStrings("ntdll.dll", module.name.?);

    try std.testing.expectEqual(2, module.exports.items.len);

    {
        const ex = module.exports.items[0];
        try std.testing.expectEqualStrings("RtlDispatchAPC@12", ex.name);
        try std.testing.expectEqual(null, ex.import_name);
        try std.testing.expectEqual(null, ex.external_name);
        try std.testing.expectEqual(0, ex.ordinal);
    }
    {
        const ex = module.exports.items[1];
        try std.testing.expectEqualStrings("RtlActivateActivationContextUnsafeFast@0", ex.name);
        try std.testing.expectEqual(null, ex.import_name);
        try std.testing.expectEqual(null, ex.external_name);
        try std.testing.expectEqual(0, ex.ordinal);
    }
}
