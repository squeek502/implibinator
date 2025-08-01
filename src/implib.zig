const std = @import("std");
const def = @import("def.zig");

pub fn writeCoffArchive(writer: *std.io.Writer, members: Members) !void {
    // TODO: Write a different archive format in this case?
    if (members.list.items.len > 0xfffe) return error.TooManyMembers;

    try writer.writeAll(archive_start);

    // TODO: Write 'first linker member'
    // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#first-linker-member

    // TODO: Write 'second linker member'
    // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#second-linker-member

    // TODO: Write longnames member (only if necessary)
    // https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#longnames-member

    for (members.list.items) |member| {
        try writeArchiveMemberHeader(writer, member.name, member.bytes.len);
        try writer.writeAll(member.bytes);
        if (member.bytes.len % 2 != 0) try writer.writeByte(archive_pad_byte);
    }

    try writer.flush();
}

const archive_start = "!<arch>\n";
const archive_header_end = "`\n";
const archive_pad_byte = '\n';

fn writeArchiveMemberHeader(writer: *std.io.Writer, name: []const u8, size: usize) !void {
    try writer.writeAll(name);
    try writer.writeByte('/');
    try writer.splatByteAll(' ', 16 - (name.len + 1));

    try writer.writeAll("0           "); // date
    try writer.writeAll("0     "); // user id
    try writer.writeAll("0     "); // group id
    try writer.writeAll("644     "); // mode
    try writer.print("{d: <10}", .{size});
    try writer.writeAll(archive_header_end);
}

pub const Members = struct {
    list: std.ArrayListUnmanaged(Member) = .empty,
    arena: std.heap.ArenaAllocator,

    pub const Member = struct {
        bytes: []const u8,
        name: []const u8,
    };

    pub fn deinit(self: *const Members) void {
        self.arena.deinit();
    }
};

pub fn getMembers(
    allocator: std.mem.Allocator,
    module_def: def.ModuleDefinition,
    machine_type: std.coff.MachineType,
) !Members {
    var members: Members = .{
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    const arena = members.arena.allocator();
    errdefer members.deinit();

    try members.list.ensureTotalCapacity(arena, 3 + module_def.exports.items.len);
    const import_name = try arena.dupe(u8, module_def.name orelse "");
    const library = std.fs.path.stem(import_name);

    const import_descriptor_symbol_name = try std.mem.concat(arena, u8, &.{
        import_descriptor_prefix,
        library,
    });
    const null_thunk_symbol_name = try std.mem.concat(arena, u8, &.{
        null_thunk_data_prefix,
        library,
        null_thunk_data_suffix,
    });

    members.list.appendAssumeCapacity(.{
        .bytes = try getImportDescriptor(arena, machine_type, import_name, import_descriptor_symbol_name, null_thunk_symbol_name),
        .name = import_name,
    });
    members.list.appendAssumeCapacity(.{
        .bytes = try getNullImportDescriptor(arena, machine_type),
        .name = import_name,
    });
    members.list.appendAssumeCapacity(.{
        .bytes = try getNullThunk(arena, machine_type, null_thunk_symbol_name),
        .name = import_name,
    });

    for (module_def.exports.items) |e| {
        if (e.private) continue;

        const bytes = try getShortImport(arena, import_name, e.name, e.external_name, machine_type, e.ordinal, e.type, .NAME);
        try members.list.append(arena, .{
            .bytes = bytes,
            .name = import_name,
        });
    }

    return members;
}

fn is64Bit(machine_type: std.coff.MachineType) bool {
    return switch (machine_type) {
        .X64, .ARM64, .ARM64EC, .ARM64X => true,
        else => false,
    };
}

const null_import_descriptor_symbol_name = "__NULL_IMPORT_DESCRIPTOR";
const import_descriptor_prefix = "__IMPORT_DESCRIPTOR_";
const null_thunk_data_prefix = "\x7F";
const null_thunk_data_suffix = "_NULL_THUNK_DATA";

// past the string table length field
const first_string_table_entry_offset = @sizeOf(u32);
const first_string_table_entry = getNameBytesForStringTableOffset(first_string_table_entry_offset);

const byte_size_of_relocation = 10;

fn getNameBytesForStringTableOffset(offset: u32) [8]u8 {
    var bytes = [_]u8{0} ** 8;
    std.mem.writeInt(u32, bytes[4..8], offset, .little);
    return bytes;
}

fn getImportDescriptor(
    allocator: std.mem.Allocator,
    machine_type: std.coff.MachineType,
    import_name: []const u8,
    import_descriptor_symbol_name: []const u8,
    null_thunk_symbol_name: []const u8,
) ![]const u8 {
    const number_of_sections = 2;
    const number_of_symbols = 7;
    const number_of_relocations = 3;

    const pointer_to_idata2_data = @sizeOf(std.coff.CoffHeader) +
        (@sizeOf(std.coff.SectionHeader) * number_of_sections);
    const pointer_to_idata6_data = pointer_to_idata2_data +
        @sizeOf(std.coff.ImportDirectoryEntry) +
        (byte_size_of_relocation * number_of_relocations);
    const pointer_to_symbol_table = pointer_to_idata6_data +
        import_name.len + 1;

    const string_table_byte_len = 4 +
        (import_descriptor_symbol_name.len + 1) +
        (null_import_descriptor_symbol_name.len + 1) +
        (null_thunk_symbol_name.len + 1);
    const total_byte_len = pointer_to_symbol_table +
        (std.coff.Symbol.sizeOf() * number_of_symbols) +
        string_table_byte_len;

    var alloc_writer: std.io.Writer.Allocating = try .initCapacity(allocator, total_byte_len);
    errdefer alloc_writer.deinit();
    var writer = &alloc_writer.writer;

    try writer.writeStruct(std.coff.CoffHeader{
        .machine = machine_type,
        .number_of_sections = number_of_sections,
        .time_date_stamp = 0,
        .pointer_to_symbol_table = @intCast(pointer_to_symbol_table),
        .number_of_symbols = number_of_symbols,
        .size_of_optional_header = 0,
        .flags = .{ .@"32BIT_MACHINE" = @intFromBool(!is64Bit(machine_type)) },
    }, .little);

    try writer.writeStruct(std.coff.SectionHeader{
        .name = ".idata$2".*,
        .virtual_size = 0,
        .virtual_address = 0,
        .size_of_raw_data = @sizeOf(std.coff.ImportDirectoryEntry),
        .pointer_to_raw_data = pointer_to_idata2_data,
        .pointer_to_relocations = pointer_to_idata2_data + @sizeOf(std.coff.ImportDirectoryEntry),
        .pointer_to_linenumbers = 0,
        .number_of_relocations = number_of_relocations,
        .number_of_linenumbers = 0,
        .flags = .{
            .ALIGN = @bitCast(@intFromEnum(Alignment.@"4BYTES")),
            .CNT_INITIALIZED_DATA = 1,
            .MEM_WRITE = 1,
            .MEM_READ = 1,
        },
    }, .little);

    try writer.writeStruct(std.coff.SectionHeader{
        .name = ".idata$6".*,
        .virtual_size = 0,
        .virtual_address = 0,
        .size_of_raw_data = @intCast(import_name.len + 1),
        .pointer_to_raw_data = pointer_to_idata6_data,
        .pointer_to_relocations = 0,
        .pointer_to_linenumbers = 0,
        .number_of_relocations = 0,
        .number_of_linenumbers = 0,
        .flags = .{
            .ALIGN = @bitCast(@intFromEnum(Alignment.@"2BYTES")),
            .CNT_INITIALIZED_DATA = 1,
            .MEM_WRITE = 1,
            .MEM_READ = 1,
        },
    }, .little);

    // .idata$2
    try writer.writeStruct(std.coff.ImportDirectoryEntry{
        .forwarder_chain = 0,
        .import_address_table_rva = 0,
        .import_lookup_table_rva = 0,
        .name_rva = 0,
        .time_date_stamp = 0,
    }, .little);

    const relocation_rva_type = rvaRelocationTypeIndicator(machine_type) orelse return error.UnsupportedMachineType;
    try writeRelocation(writer, .{
        .virtual_address = @offsetOf(std.coff.ImportDirectoryEntry, "name_rva"),
        .symbol_table_index = 2,
        .type = relocation_rva_type,
    });
    try writeRelocation(writer, .{
        .virtual_address = @offsetOf(std.coff.ImportDirectoryEntry, "import_lookup_table_rva"),
        .symbol_table_index = 3,
        .type = relocation_rva_type,
    });
    try writeRelocation(writer, .{
        .virtual_address = @offsetOf(std.coff.ImportDirectoryEntry, "import_address_table_rva"),
        .symbol_table_index = 4,
        .type = relocation_rva_type,
    });

    // .idata$6
    try writer.writeAll(import_name);
    try writer.writeByte(0);

    var string_table_offset: usize = first_string_table_entry_offset;
    try writeSymbol(writer, .{
        .name = first_string_table_entry,
        .value = 0,
        .section_number = @enumFromInt(1),
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .EXTERNAL,
        .number_of_aux_symbols = 0,
    });
    string_table_offset += import_descriptor_symbol_name.len + 1;
    try writeSymbol(writer, .{
        .name = ".idata$2".*,
        .value = 0,
        .section_number = @enumFromInt(1),
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .SECTION,
        .number_of_aux_symbols = 0,
    });
    try writeSymbol(writer, .{
        .name = ".idata$6".*,
        .value = 0,
        .section_number = @enumFromInt(2),
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .STATIC,
        .number_of_aux_symbols = 0,
    });
    try writeSymbol(writer, .{
        .name = ".idata$4".*,
        .value = 0,
        .section_number = .UNDEFINED,
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .SECTION,
        .number_of_aux_symbols = 0,
    });
    try writeSymbol(writer, .{
        .name = ".idata$5".*,
        .value = 0,
        .section_number = .UNDEFINED,
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .SECTION,
        .number_of_aux_symbols = 0,
    });
    try writeSymbol(writer, .{
        .name = getNameBytesForStringTableOffset(@intCast(string_table_offset)),
        .value = 0,
        .section_number = .UNDEFINED,
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .EXTERNAL,
        .number_of_aux_symbols = 0,
    });
    string_table_offset += null_import_descriptor_symbol_name.len + 1;
    try writeSymbol(writer, .{
        .name = getNameBytesForStringTableOffset(@intCast(string_table_offset)),
        .value = 0,
        .section_number = .UNDEFINED,
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .EXTERNAL,
        .number_of_aux_symbols = 0,
    });
    string_table_offset += null_thunk_symbol_name.len + 1;

    // string table
    try writer.writeInt(u32, @intCast(string_table_byte_len), .little);
    try writer.writeAll(import_descriptor_symbol_name);
    try writer.writeByte(0);
    try writer.writeAll(null_import_descriptor_symbol_name);
    try writer.writeByte(0);
    try writer.writeAll(null_thunk_symbol_name);
    try writer.writeByte(0);

    return alloc_writer.toOwnedSlice();
}

fn getNullImportDescriptor(allocator: std.mem.Allocator, machine_type: std.coff.MachineType) ![]const u8 {
    const number_of_sections = 1;
    const number_of_symbols = 1;
    const pointer_to_idata3_data = @sizeOf(std.coff.CoffHeader) +
        (@sizeOf(std.coff.SectionHeader) * number_of_sections);
    const pointer_to_symbol_table = pointer_to_idata3_data +
        @sizeOf(std.coff.ImportDirectoryEntry);

    const string_table_byte_len = 4 + null_import_descriptor_symbol_name.len + 1;
    const total_byte_len = pointer_to_symbol_table +
        (std.coff.Symbol.sizeOf() * number_of_symbols) +
        string_table_byte_len;

    var alloc_writer: std.io.Writer.Allocating = try .initCapacity(allocator, total_byte_len);
    errdefer alloc_writer.deinit();
    var writer = &alloc_writer.writer;

    try writer.writeStruct(std.coff.CoffHeader{
        .machine = machine_type,
        .number_of_sections = number_of_sections,
        .time_date_stamp = 0,
        .pointer_to_symbol_table = @intCast(pointer_to_symbol_table),
        .number_of_symbols = number_of_symbols,
        .size_of_optional_header = 0,
        .flags = .{ .@"32BIT_MACHINE" = @intFromBool(!is64Bit(machine_type)) },
    }, .little);

    try writer.writeStruct(std.coff.SectionHeader{
        .name = ".idata$3".*,
        .virtual_size = 0,
        .virtual_address = 0,
        .size_of_raw_data = @sizeOf(std.coff.ImportDirectoryEntry),
        .pointer_to_raw_data = pointer_to_idata3_data,
        .pointer_to_relocations = 0,
        .pointer_to_linenumbers = 0,
        .number_of_relocations = 0,
        .number_of_linenumbers = 0,
        .flags = .{
            .ALIGN = @bitCast(@intFromEnum(Alignment.@"4BYTES")),
            .CNT_INITIALIZED_DATA = 1,
            .MEM_WRITE = 1,
            .MEM_READ = 1,
        },
    }, .little);

    try writer.writeStruct(std.coff.ImportDirectoryEntry{
        .forwarder_chain = 0,
        .import_address_table_rva = 0,
        .import_lookup_table_rva = 0,
        .name_rva = 0,
        .time_date_stamp = 0,
    }, .little);

    try writeSymbol(writer, .{
        .name = first_string_table_entry,
        .value = 0,
        .section_number = @enumFromInt(1),
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .EXTERNAL,
        .number_of_aux_symbols = 0,
    });

    // string table
    try writer.writeInt(u32, string_table_byte_len, .little);
    try writer.writeAll(null_import_descriptor_symbol_name);
    try writer.writeByte(0);

    return alloc_writer.toOwnedSlice();
}

fn getNullThunk(allocator: std.mem.Allocator, machine_type: std.coff.MachineType, null_thunk_symbol_name: []const u8) ![]const u8 {
    const number_of_sections = 2;
    const number_of_symbols = 1;
    const va_size: u32 = if (is64Bit(machine_type)) 8 else 4;
    const pointer_to_idata5_data = @sizeOf(std.coff.CoffHeader) +
        (@sizeOf(std.coff.SectionHeader) * number_of_sections);
    const pointer_to_idata4_data = pointer_to_idata5_data + va_size;
    const pointer_to_symbol_table = pointer_to_idata4_data + va_size;

    const string_table_byte_len = 4 + null_thunk_symbol_name.len + 1;
    const total_byte_len = pointer_to_symbol_table +
        (std.coff.Symbol.sizeOf() * number_of_symbols) +
        string_table_byte_len;

    var alloc_writer: std.io.Writer.Allocating = try .initCapacity(allocator, total_byte_len);
    errdefer alloc_writer.deinit();
    var writer = &alloc_writer.writer;

    try writer.writeStruct(std.coff.CoffHeader{
        .machine = machine_type,
        .number_of_sections = number_of_sections,
        .time_date_stamp = 0,
        .pointer_to_symbol_table = @intCast(pointer_to_symbol_table),
        .number_of_symbols = number_of_symbols,
        .size_of_optional_header = 0,
        .flags = .{ .@"32BIT_MACHINE" = @intFromBool(!is64Bit(machine_type)) },
    }, .little);

    try writer.writeStruct(std.coff.SectionHeader{
        .name = ".idata$5".*,
        .virtual_size = 0,
        .virtual_address = 0,
        .size_of_raw_data = va_size,
        .pointer_to_raw_data = pointer_to_idata5_data,
        .pointer_to_relocations = 0,
        .pointer_to_linenumbers = 0,
        .number_of_relocations = 0,
        .number_of_linenumbers = 0,
        .flags = .{
            .ALIGN = if (is64Bit(machine_type))
                @bitCast(@intFromEnum(Alignment.@"8BYTES"))
            else
                @bitCast(@intFromEnum(Alignment.@"4BYTES")),
            .CNT_INITIALIZED_DATA = 1,
            .MEM_WRITE = 1,
            .MEM_READ = 1,
        },
    }, .little);

    try writer.writeStruct(std.coff.SectionHeader{
        .name = ".idata$4".*,
        .virtual_size = 0,
        .virtual_address = 0,
        .size_of_raw_data = va_size,
        .pointer_to_raw_data = pointer_to_idata4_data,
        .pointer_to_relocations = 0,
        .pointer_to_linenumbers = 0,
        .number_of_relocations = 0,
        .number_of_linenumbers = 0,
        .flags = .{
            .ALIGN = if (is64Bit(machine_type))
                @bitCast(@intFromEnum(Alignment.@"8BYTES"))
            else
                @bitCast(@intFromEnum(Alignment.@"4BYTES")),
            .CNT_INITIALIZED_DATA = 1,
            .MEM_WRITE = 1,
            .MEM_READ = 1,
        },
    }, .little);

    // .idata$5
    try writer.splatByteAll(0, va_size);
    // .idata$4
    try writer.splatByteAll(0, va_size);

    try writeSymbol(writer, .{
        .name = first_string_table_entry,
        .value = 0,
        .section_number = @enumFromInt(1),
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .EXTERNAL,
        .number_of_aux_symbols = 0,
    });

    // string table
    try writer.writeInt(u32, @intCast(string_table_byte_len), .little);
    try writer.writeAll(null_thunk_symbol_name);
    try writer.writeByte(0);

    return alloc_writer.toOwnedSlice();
}

fn getShortImport(
    allocator: std.mem.Allocator,
    import_name: []const u8,
    sym: []const u8,
    export_name: ?[]const u8,
    machine_type: std.coff.MachineType,
    ordinal_hint: u16,
    import_type: std.coff.ImportType,
    name_type: std.coff.ImportNameType,
) ![]const u8 {
    var size_of_data = import_name.len + 1 + sym.len + 1;
    if (export_name) |name| size_of_data += name.len + 1;
    const total_byte_len = @sizeOf(std.coff.ImportHeader) + size_of_data;

    const buf = try allocator.alloc(u8, total_byte_len);
    errdefer allocator.free(buf);

    var writer = std.io.Writer.fixed(buf);

    writer.writeStruct(std.coff.ImportHeader{
        .sig1 = .UNKNOWN,
        .sig2 = 0xFFFF,
        .version = 0,
        .machine = machine_type,
        .time_date_stamp = 0,
        .size_of_data = @intCast(size_of_data),
        .hint = ordinal_hint,
        .types = .{
            .type = import_type,
            .name_type = name_type,
            .reserved = 0,
        },
    }, .little) catch unreachable;

    writer.writeAll(sym) catch unreachable;
    writer.writeByte(0) catch unreachable;
    writer.writeAll(import_name) catch unreachable;
    writer.writeByte(0) catch unreachable;
    if (export_name) |name| {
        writer.writeAll(name) catch unreachable;
        writer.writeByte(0) catch unreachable;
    }

    return buf;
}

fn writeSymbol(writer: *std.io.Writer, symbol: std.coff.Symbol) !void {
    try writer.writeAll(&symbol.name);
    try writer.writeInt(u32, symbol.value, .little);
    try writer.writeInt(u16, @intFromEnum(symbol.section_number), .little);
    try writer.writeInt(u8, @intFromEnum(symbol.type.base_type), .little);
    try writer.writeInt(u8, @intFromEnum(symbol.type.complex_type), .little);
    try writer.writeInt(u8, @intFromEnum(symbol.storage_class), .little);
    try writer.writeInt(u8, symbol.number_of_aux_symbols, .little);
}

fn writeRelocation(writer: *std.io.Writer, relocation: std.coff.Relocation) !void {
    try writer.writeInt(u32, relocation.virtual_address, .little);
    try writer.writeInt(u32, relocation.symbol_table_index, .little);
    try writer.writeInt(u16, relocation.type, .little);
}

// https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#type-indicators
pub fn rvaRelocationTypeIndicator(target: std.coff.MachineType) ?u16 {
    return switch (target) {
        .X64 => 0x3, // IMAGE_REL_AMD64_ADDR32NB
        .I386 => 0x7, // IMAGE_REL_I386_DIR32NB
        .ARMNT => 0x2, // IMAGE_REL_ARM_ADDR32NB
        .ARM64, .ARM64EC, .ARM64X => 0x2, // IMAGE_REL_ARM64_ADDR32NB
        .IA64 => 0x10, // IMAGE_REL_IA64_DIR32NB
        else => null,
    };
}

// TODO: upstream
const Alignment = enum(u4) {
    unspecified = 0,
    @"1BYTES",
    @"2BYTES",
    @"4BYTES",
    @"8BYTES",
    @"16BYTES",
    @"32BYTES",
    @"64BYTES",
    @"128BYTES",
    @"256BYTES",
    @"512BYTES",
    @"1024BYTES",
    @"2048BYTES",
    @"4096BYTES",
    @"8192BYTES",

    pub fn toByteUnits(a: Alignment) ?u16 {
        if (@intFromEnum(a) == 0) return null;
        return @as(u16, 1) << (@intFromEnum(a) - 1);
    }

    pub fn fromByteUnits(n: u16) Alignment {
        std.debug.assert(std.math.isPowerOfTwo(n));
        return @enumFromInt(@ctz(n) + 1);
    }
};
