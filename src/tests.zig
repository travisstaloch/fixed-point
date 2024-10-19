const std = @import("std");
const testing = std.testing;
const math = std.math;
const fixed_point = @import("fixed-point");
const FixedPoint = fixed_point.FixedPoint;
const FixedPointAdvanced = fixed_point.FixedPointAdvanced;

fn testBasic(comptime T: type) !void {
    const F = FixedPoint(T, 1);
    const a = F.fromParts(1, 2);
    try testing.expectEqual(1, a.wholePart());
    try testing.expectEqual(2, a.fractionPart());
    const b = F.parseComptime("1.2");
    try testing.expectEqual(a, b);
    try testing.expectEqual(27, F.parseComptime("2.7").asInt());
}

test "basic" {
    const F = FixedPoint(i8, 1);
    const fraction_bits = @typeInfo(F.Fraction).int.bits;
    try testing.expectEqual(4, fraction_bits);
    try testing.expectEqual(10, F.fraction_max);
    try testing.expectEqual(-12, F.whole_min);
    try testing.expectEqual(12, F.whole_max);

    try testBasic(i8);
    try testBasic(u8);
    try testBasic(u7);
    try testBasic(i16);
    try testBasic(u16);

    try testing.expectEqual(42, fixed_point.fixedPoint(u8, 1, 42).asInt());
}

fn testAddition(comptime T: type) !void {
    const F = FixedPoint(T, 1);
    const a = F.parseComptime("4.3");
    const b = try a.addChecked(try .parse("2.5"));
    try testing.expectEqual(F.parseComptime("6.8"), b);
}

test "addition" {
    const F = FixedPoint(i8, 1);
    const a = F.fromParts(-4, 3);
    const b = a.addChecked(.fromParts(2, 5));
    try testing.expectEqual(F.fromParts(-1, 8), b);

    try testAddition(i8);
    try testAddition(u8);
    try testAddition(u7);
    try testAddition(i16);
    try testAddition(u16);
}

fn testMultiplication(comptime T: type) !void {
    const F = FixedPoint(T, 1);
    const a = F.parseComptime("2.7");
    const b = try F.parse("2.5");
    const c = F.parseComptime("6.7");
    // std.log.debug("{} + {} = {} expected {}", .{ a, b, a.mul(b), c });
    try testing.expectEqual(c, a.mulChecked(b));
}

test "multiplcation" {
    const F = FixedPoint(i8, 1);
    try testing.expectEqual(F.fromInt(46), F.fromInt(23).mulChecked(F.fromInt(20)));
    try testing.expectEqual(F.fromInt(69), F.fromInt(23).mulChecked(F.fromInt(30)));
    try testing.expectEqual(F.fromInt(-20), F.fromInt(-10).mulChecked(F.fromInt(20)));
    try testing.expectEqual(F.fromInt(-23), F.fromInt(23).mulChecked(F.fromInt(-10)));

    try testMultiplication(i8);
    try testMultiplication(u8);
    try testMultiplication(u7);
    try testMultiplication(i16);
    try testMultiplication(u16);
}

fn testDivision(comptime T: type) !void {
    const F = FixedPoint(T, 1);
    try testing.expectEqual(F.fromInt(23), F.fromInt(46).div(.fromInt(20)));
    try testing.expectEqual(F.fromInt(23), F.fromInt(69).div(.fromInt(30)));
    if (@typeInfo(T).int.signedness == .signed) {
        try testing.expectEqual(F.fromInt(-10), F.fromInt(-20).div(.fromInt(20)));
        try testing.expectEqual(F.fromInt(-23), F.fromInt(23).div(.fromInt(-10)));
    }
}

test "division" {
    try testDivision(i8);
    try testDivision(u8);
    try testDivision(u7);
    try testDivision(i16);
    try testDivision(u16);
}

test "sqrt" {
    const F = FixedPoint(i21, 1);
    const Float = f32;
    try testing.expectEqual(F.fromInt(20), F.fromInt(40).sqrt(Float));
    try testing.expectEqual(F.fromInt(30), F.fromInt(90).sqrt(Float));
    try testing.expectEqual(F.fromInt(40), F.fromInt(160).sqrt(Float));
    try testing.expectEqual(F.fromInt(50), F.fromInt(250).sqrt(Float));
    try testing.expectEqual(F.fromInt(60), F.fromInt(360).sqrt(Float));
    try testing.expectEqual(F.fromInt(70), F.fromInt(490).sqrt(Float));
    try testing.expectEqual(F.fromInt(2550), F.fromInt(650250).sqrt(Float));
    try testing.expectEqual(F.fromInt(141), F.fromInt(2000).sqrt(Float));

    const F2 = FixedPoint(i64, 1);
    try testing.expectApproxEqAbs(
        F2.fromInt(750_000_000_0).as(f32),
        F2.fromInt(750_000_000 * 750_000_000 * 10).sqrt(f32).as(f32),
        0.0001,
    );
}

test "toScale() basic" {
    const F = FixedPoint(u8, 2);
    const a = F.fromInt(202);
    try testing.expectFmt("2.02", "{}", .{a});
    try testing.expectFmt("2.0", "{}", .{a.toScale(1)});
}

test "add/sub/mul/divAny() basic" {
    const F2 = FixedPoint(i16, 2);
    const F3 = FixedPoint(i16, 3);
    const a = F2.fromInt(202);
    try testing.expectEqual(F2.parseComptime("2.02"), a);
    const b = F3.fromInt(2000);
    try testing.expectEqual(F3.parseComptime("2.0"), b);

    try testing.expectEqual(F2.fromInt(402), a.addAny(b));
    try testing.expectEqual(F2.fromInt(2), a.subAny(b));
    try testing.expectEqual(F2.fromInt(404), a.mulAny(b));
    try testing.expectEqual(F2.fromInt(101), a.divAny(b));

    try testing.expectEqual(F3.fromInt(4020), b.addAny(a));
    try testing.expectEqual(F3.fromInt(-20), b.subAny(a));
    try testing.expectEqual(F3.fromInt(4040), b.mulAny(a));
    try testing.expectEqual(F3.fromInt(990), b.divAny(a));

    try testing.expectEqual(try F3.parse("4.02"), b.addAny(a));
    try testing.expectEqual(try F3.parse("-0.02"), b.subAny(a));
    try testing.expectEqual(try F3.parse("4.04"), b.mulAny(a));
    try testing.expectEqual(try F3.parse("0.99"), b.divAny(a));
}

test "toScale() fuzz" {
    var prng = std.Random.DefaultPrng.init(0);
    const rand = prng.random();
    inline for (.{ u8, i8 }) |T| {
        const F = FixedPoint(T, 1);
        const F2 = FixedPoint(T, 2);
        for (0..1000) |_| {
            const i = rand.int(T);
            try testing.expectEqual(
                F.fromInt(@divTrunc(i, 10)),
                F2.fromInt(i).toScale(1),
            );
        }
    }
}

fn fuzzArithmetic(comptime T: type, comptime digits: u16, rand: std.Random) !void {
    const F = FixedPoint(T, digits);

    for (0..200) |_| {
        const a = rand.int(T);
        const b = rand.int(T);

        if (math.add(T, a, b)) |x| {
            try testing.expectEqual(x, F.fromInt(a).add(F.fromInt(b)).asInt());
            var aa = F.fromInt(a);
            aa.addBy(F.fromInt(b));
            try testing.expectEqual(x, aa.asInt());
        } else |_| {
            try testing.expectError(error.Overflow, F.fromInt(a).addChecked(F.fromInt(b)));
        }
        if (math.sub(T, a, b)) |x| {
            try testing.expectEqual(x, F.fromInt(a).sub(F.fromInt(b)).asInt());
            var aa = F.fromInt(a);
            aa.subBy(F.fromInt(b));
            try testing.expectEqual(x, aa.asInt());
        } else |_| {
            try testing.expectError(error.Overflow, F.fromInt(a).subChecked(F.fromInt(b)));
        }
        const ab = @as(F.MulInt, a) * @as(F.MulInt, b);
        const ab2 = @divTrunc(ab, F.fraction_max);
        if (math.cast(F.Int, ab2)) |x| {
            try testing.expectEqual(x, F.fromInt(a).mul(F.fromInt(b)).asInt());
            var aa = F.fromInt(a);
            aa.mulBy(F.fromInt(b));
            try testing.expectEqual(x, aa.asInt());
        } else {
            try testing.expectError(error.Overflow, F.fromInt(a).mulChecked(F.fromInt(b)));
        }

        if (math.divTrunc(F.MulInt, @as(F.MulInt, a) * F.fraction_max, b)) |x| {
            if (math.cast(F.Int, x)) |xx| {
                try testing.expectEqual(xx, F.fromInt(a).div(F.fromInt(b)).asInt());
                var aa = F.fromInt(a);
                aa.divBy(F.fromInt(b));
                try testing.expectEqual(xx, aa.asInt());
            } else try testing.expectError(error.Overflow, F.fromInt(a).divChecked(F.fromInt(b)));
        } else |_| {
            const expected = if (b == 0) error.DivisionByZero else error.Overflow;
            try testing.expectError(expected, F.fromInt(a).divChecked(F.fromInt(b)));
        }
    }
}

test "fuzz arithmetic" {
    var prng = std.Random.DefaultPrng.init(0);
    const rand = prng.random();

    try fuzzArithmetic(i64, 2, rand);
    try fuzzArithmetic(i64, 3, rand);
    try fuzzArithmetic(i64, 4, rand);
    try fuzzArithmetic(u64, 2, rand);
    try fuzzArithmetic(u64, 3, rand);
    try fuzzArithmetic(u64, 4, rand);

    try fuzzArithmetic(i32, 2, rand);
    try fuzzArithmetic(i32, 3, rand);
    try fuzzArithmetic(i32, 4, rand);
    try fuzzArithmetic(u32, 2, rand);
    try fuzzArithmetic(u32, 3, rand);
    try fuzzArithmetic(u32, 4, rand);

    try fuzzArithmetic(i16, 2, rand);
    try fuzzArithmetic(i16, 3, rand);
    try fuzzArithmetic(i16, 4, rand);
    try fuzzArithmetic(u16, 2, rand);
    try fuzzArithmetic(u16, 3, rand);
    try fuzzArithmetic(u16, 4, rand);

    try fuzzArithmetic(i8, 1, rand);
    try fuzzArithmetic(i8, 2, rand);
    try fuzzArithmetic(u8, 1, rand);
    try fuzzArithmetic(u8, 2, rand);

    // large int
    try fuzzArithmetic(u512, 10, rand);
}

fn fuzzArithmeticAny(comptime T: type, rand: std.Random) !void {
    const F2 = FixedPoint(T, 2);
    const F3 = FixedPoint(T, 3);

    for (0..400) |_| {
        const a = rand.int(T);
        const b = rand.int(T);
        const f2a = F2.fromInt(a);
        const f3b = F3.fromInt(b);
        const b2 = f3b.toScale(2).asInt();
        const a2 = f2a.toScale(3).asInt();

        if (math.add(T, a, b2)) |x| {
            try testing.expectEqual(x, f2a.addAny(f3b).asInt());
        } else |_| {}
        if (math.add(T, b, a2)) |x| {
            try testing.expectEqual(x, f3b.addAny(f2a).asInt());
        } else |_| {}
        if (math.sub(T, a, b2)) |x| {
            try testing.expectEqual(x, f2a.subAny(f3b).asInt());
        } else |_| {}
        if (math.mul(T, a, b2)) |x| {
            try testing.expectEqual(x, f2a.mulAny(f3b).asInt());
        } else |_| {}
        if (math.divTrunc(F2.MulInt, @as(F2.MulInt, a) * F2.fraction_max, b2)) |x| {
            try testing.expectEqual(x, f2a.divAny(f3b).asInt());
        } else |_| {}
    }
}

test "fuzz arithmetic - any" {
    var prng = std.Random.DefaultPrng.init(0);
    const rand = prng.random();
    try fuzzArithmeticAny(i64, rand);
    try fuzzArithmeticAny(u64, rand);
    try fuzzArithmeticAny(i32, rand);
    try fuzzArithmeticAny(u32, rand);
    try fuzzArithmeticAny(u20, rand);
    try fuzzArithmeticAny(i21, rand);
    // smaller ints fail
}

test "currency" {
    const Currency = FixedPoint(i32, 2);
    try testing.expectEqual(7, @typeInfo(Currency.Fraction).int.bits);

    const a = Currency.fromParts(2222, 22);
    const b = Currency.fromParts(8888, 88);
    try testing.expectEqual(Currency.fromParts(11111, 10), a.addChecked(b));

    const one_cent = Currency.parseComptime("0.01");
    try testing.expectEqual(0, one_cent.wholePart());
    try testing.expectEqual(1, one_cent.fractionPart());
    var expected: i16 = -100;
    var actual = Currency.fromInt(expected);
    while (expected < 1001) : (expected += 1) {
        const dollars = @divTrunc(expected, 100);
        const cents = @abs(expected - dollars * 100);
        // std.debug.print("sum {} whole/fraction part {} {} dc {} {} {}\n", .{ sum, sum.wholePart(), sum.fractionPart(), dc, dollars, cents });

        try testing.expectEqual(dollars, actual.wholePart());
        try testing.expectEqual(cents, actual.fractionPart());
        try testing.expectApproxEqAbs(actual.as(f32), @as(f32, @floatFromInt(expected)) / 100.0, 0.0001);
        actual.addBy(one_cent);
    }
}

test "parse alone" {
    const F2 = FixedPoint(i16, 2);
    try testing.expectEqual(F2.fromInt(-2), F2.parse("-0.02"));
    try testing.expectEqual(F2.fromInt(202), F2.parse("2.02"));

    const F3 = FixedPoint(i16, 3);
    try testing.expectEqual(F3.fromInt(4200), F3.parse("4.2"));
    try testing.expectEqual(F3.fromInt(4020), F3.parse("4.02"));

    const F = FixedPointAdvanced(.signed, 50, 14);
    try testing.expectEqual(F.fromParts(1, 0), F.parse("1"));
    try testing.expectEqual(F.fromParts(-1, 0), F.parse("-1"));
    try testing.expectEqual(F.fromParts(-1, 0), F.parse("-1."));
    try testing.expectEqual(F.fromParts(1, 2500), F.parse("1.25"));
    try testing.expectEqual(F.fromParts(-1, 2500), F.parse("-1.25"));
    try testing.expectEqual(F.fromParts(3, 1415), F.parse("3.1415"));
    try testing.expectEqual(F.fromParts(1, 2500), F.parse("1.25"));
    // rounding
    try testing.expectEqual(F.fromParts(2, 7183), F.parse("2.7182818284"));

    try testing.expectError(error.InvalidCharacter, F.parse(""));
    try testing.expectError(error.InvalidCharacter, F.parse("-0x10.25"));
    try testing.expectError(error.InvalidCharacter, F.parse("0x10.25"));
    try testing.expectError(error.InvalidCharacter, F.parse("0x-10.25"));
    try testing.expectError(error.InvalidCharacter, F.parse("10.-25"));
    try testing.expectError(error.InvalidCharacter, F.parse("00"));
    try testing.expectError(error.InvalidCharacter, F.parse("00.0"));
    try testing.expectError(error.InvalidCharacter, F.parse("-00.0"));
}

test "digitsLenBase10" {
    const digitsLenBase10 = fixed_point.digitsLenBase10;
    try testing.expectEqual(0, digitsLenBase10(u8, 0));
    try testing.expectEqual(1, digitsLenBase10(u8, 1));
    try testing.expectEqual(1, digitsLenBase10(u8, 9));
    try testing.expectEqual(2, digitsLenBase10(u8, 10));
    try testing.expectEqual(2, digitsLenBase10(u8, 99));
    try testing.expectEqual(3, digitsLenBase10(u8, 100));
    try testing.expectEqual(3, digitsLenBase10(u16, 999));
}

test "format signed" {
    const F = FixedPoint(i8, 1);
    try testing.expectFmt("-1.0", "{d:.1}", .{F.fromInt(-10)});
    try testing.expectFmt("-1.0000", "{d:.4}", .{F.fromInt(-10)});
    try testing.expectFmt("-1.0", "{d:.1}", .{F.fromInt(-10)});
    try testing.expectFmt("-1.5", "{d:.1}", .{F.fromInt(-15)});
    try testing.expectFmt("-1.2", "{d:.1}", .{F.fromInt(-12)});
    try testing.expectFmt("-4.2", "{d:.1}", .{F.fromInt(-42)});

    try testing.expectError(error.UnsupportedSpecifier, F.fromInt(0).format("{x}", .{}, std.io.null_writer));
}

test "format unsigned" {
    const F = FixedPoint(u8, 1);
    try testing.expectFmt("1.0", "{!d:.1}", .{F.parse("1.")});
    try testing.expectFmt("1.0000", "{d:.4}", .{F.fromInt(10)});
    try testing.expectFmt("1.0", "{d:.1}", .{F.fromInt(10)});
    try testing.expectFmt("1.5", "{d:.1}", .{F.fromInt(15)});
    try testing.expectFmt("1.2", "{d:.1}", .{F.fromInt(12)});
    try testing.expectFmt("4.2", "{d:.1}", .{F.fromInt(42)});
    try testing.expectFmt("1.01", "{d}", .{FixedPoint(u8, 2).fromInt(101)});
    try testing.expectFmt("0.01", "{d}", .{FixedPoint(u8, 2).fromInt(1)});
}

test "format width and rounding" {
    const F = FixedPointAdvanced(.signed, 50, 14);
    try testing.expectFmt("4.0006", "{d}", .{F.fromParts(4, 6)});
    try testing.expectFmt("4.1", "{d:.1}", .{try F.parse("4.1111")});
    try testing.expectFmt("4.11", "{d:.2}", .{try F.parse("4.1111")});
    try testing.expectFmt("4.18", "{d:.2}", .{try F.parse("4.18")});
    try testing.expectFmt("4.2", "{d:.1}", .{try F.parse("4.18")});
}

test "parse/format" {
    try testing.expectFmt("1.04", "{d}", .{FixedPoint(u8, 2).fromInt(104)});
    const F = FixedPoint(u64, 5);
    try testing.expectFmt("1.00000", "{d}", .{try F.parse("1.0")});
    try testing.expectFmt("1.0", "{d:.1}", .{try F.parse("1.0")});
    try testing.expectFmt("1.00", "{d:.2}", .{try FixedPoint(u64, 1).parse("1.0")});
}

fn testParseFormat(
    comptime T: type,
    comptime digits: u16,
    rand: std.Random,
    fbs: *std.io.FixedBufferStream([]u8),
) !void {
    const F = FixedPoint(T, digits);
    for (0..200) |_| {
        const max_int = math.maxInt(T);
        const int = rand.intRangeAtMost(F.Int, math.minInt(T), max_int);
        const f = F.fromInt(int);
        const digits_s = std.fmt.comptimePrint("{}", .{digits});
        fbs.pos = 0;
        const fmt_d = "{d:." ++ digits_s ++ "}";
        try fbs.writer().print(fmt_d, .{f.as(F.Float(1))});
        const s = fbs.getWritten();
        try testing.expectFmt(s, "{}", .{try F.parse(s)});
    }
}

test "parse/format fuzz" {
    var prng = std.Random.DefaultPrng.init(0);
    const rand = prng.random();
    var buf: [128]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);

    try testParseFormat(u8, 0, rand, &fbs);
    try testParseFormat(i8, 0, rand, &fbs);
    try testParseFormat(u8, 1, rand, &fbs);
    try testParseFormat(i8, 1, rand, &fbs);
    try testParseFormat(u8, 2, rand, &fbs);
    try testParseFormat(i8, 2, rand, &fbs);
    try testParseFormat(u32, 4, rand, &fbs);
    try testParseFormat(i32, 4, rand, &fbs);
    try testParseFormat(u128, 7, rand, &fbs);
    // larger ints fail due to @floatFromInt()
}

test "Float()" {
    try testing.expectEqual(f16, FixedPoint(u8, 1).Float(0));
    try testing.expectEqual(f16, FixedPoint(i8, 1).Float(0));
    try testing.expectEqual(f16, FixedPoint(u16, 1).Float(0));
    try testing.expectEqual(f16, FixedPoint(i16, 1).Float(0));
    try testing.expectEqual(f32, FixedPoint(u32, 2).Float(0));
    try testing.expectEqual(f32, FixedPoint(i32, 2).Float(0));
    try testing.expectEqual(f64, FixedPoint(u32, 7).Float(0));
    try testing.expectEqual(f64, FixedPoint(i32, 7).Float(0));
    try testing.expectEqual(f32, FixedPoint(u64, 2).Float(0));
    try testing.expectEqual(f32, FixedPoint(i64, 2).Float(0));
    try testing.expectEqual(f64, FixedPoint(u64, 7).Float(0));
    try testing.expectEqual(f64, FixedPoint(i64, 7).Float(0));
}

test "FixedPointFittingRange" {
    {
        const F = fixed_point.FixedPointFittingRange(-20.99, 20.99);
        try testing.expectEqual(-40, F.whole_min);
        try testing.expectEqual(40, F.whole_max);
        try testing.expectEqual(100, F.fraction_max);
        try testing.expectEqual(i13, F.Int);
    }

    comptime {
        var i = 1;
        while (i < 10000) : (i *= 2) {
            const x: f32 = @floatFromInt(i);
            const min = -@round(x) - 0.99;
            const max = @round(x) + 0.99;
            const F = fixed_point.FixedPointFittingRange(min, max);
            try testing.expect(F.whole_min < max);
            try testing.expect(F.whole_max > min);
            try testing.expectEqual(100, F.fraction_max);
        }
    }
}

test "large int format" {
    const F = FixedPoint(u512, 10);
    try testing.expectFmt("1.0", "{d:.1}", .{F.fromParts(1, 0)});
}
