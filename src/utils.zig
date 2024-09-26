const std = @import("std");

fn toLower(comptime data: []const u8) [data.len]u8 {
    var result: [data.len]u8 = undefined;

    for (data, 0..) |c, i| {
        result[i] = std.ascii.toLower(c);
    }
    return result;
}

pub fn stringToLowerEnum(comptime T: type, str: []const u8) ?T {
    //based on std stringToEnum
    const kvs = comptime build_kvs: {
        const EnumKV = struct { []const u8, T };
        var kvs_array: [@typeInfo(T).Enum.fields.len]EnumKV = undefined;
        for (@typeInfo(T).Enum.fields, 0..) |enumField, i| {
            const lower_string = toLower(enumField.name);
            kvs_array[i] = .{ &lower_string, @field(T, enumField.name) };
        }
        break :build_kvs kvs_array[0..];
    };
    const map = std.StaticStringMap(T).initComptime(kvs);
    return map.get(str);
}

test "stringToLowerEnum" {
    const Test = enum {
        LET,
    };
    try std.testing.expectEqual(.LET, stringToLowerEnum(Test, "let"));
}
