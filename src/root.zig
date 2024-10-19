const std = @import("std");
const math = std.math;
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

pub fn FixedPointAdvanced(
    comptime signedness: std.builtin.Signedness,
    comptime whole_bits: comptime_int,
    comptime fraction_bits: comptime_int,
) type {
    return FixedPoint(
        std.meta.Int(signedness, whole_bits + fraction_bits),
        math.log(usize, 10, (1 << fraction_bits)),
    );
}

/// FixedPoint
pub fn FixedPointFittingRange(
    comptime min: comptime_float,
    comptime max: comptime_float,
) type {
    assert(max > min);
    const frac_min = @abs(min - @as(comptime_int, @intFromFloat(min)));
    const frac_max = @abs(max - @as(comptime_int, @intFromFloat(max)));
    const frac_min_digs = @abs(math.log(f64, 10, 1.0 - frac_min));
    const frac_max_digs = @abs(math.log(f64, 10, 1.0 - frac_max));
    const minca = @ceil(@abs(min)) * math.sign(min);
    const maxca = @ceil(@abs(max)) * math.sign(max);
    const Whole = std.math.IntFittingRange(minca, maxca);
    const precision: u16 = @intFromFloat(@round(@max(frac_min_digs, frac_max_digs)));
    // @compileLog(std.fmt.comptimePrint(
    //     "min {d}/{d}/{d:.2}/{d:.2} max {d}/{d}/{d:.2}/{d:.2} precision {}/{d:.10}",
    //     .{ min, minca, frac_min, frac_min_digs, max, maxca, frac_max, frac_max_digs, precision, @max(frac_min_digs, frac_max_digs) },
    // ));
    const fraction_max: comptime_int = math.pow(u128, 10, precision);
    const fraction_bits: comptime_int = math.log2_int_ceil(u128, fraction_max);
    const I = std.meta.Int(
        if (min < 0) .signed else .unsigned,
        @typeInfo(Whole).int.bits + fraction_bits,
    );
    return FixedPoint(I, precision);
}

pub fn fixedPoint(
    comptime I: type,
    comptime precision: comptime_int,
    int: I,
) FixedPoint(I, precision) {
    return FixedPoint(I, precision).fromInt(int);
}

pub fn FixedPoint(
    comptime I: type,
    comptime precision: comptime_int,
) type {
    return enum(I) {
        _,

        const Self = @This();
        const info = @typeInfo(I);
        /// max value for the fractional part of an integer + 1
        pub const fraction_max: comptime_int = math.pow(u128, 10, precision);
        const max_int = math.maxInt(Int);
        const min_int = math.minInt(Int);
        /// max value for the whole part of an integer
        pub const whole_max = max_int / fraction_max;
        pub const whole_min = min_int / fraction_max;
        pub const Int = I;
        const UInt = std.meta.Int(.unsigned, info.int.bits);
        /// same sign as Int but with an additional bit. used for safe addition and subtraction.
        pub const AddInt = std.meta.Int(info.int.signedness, info.int.bits + 1);
        const UAddInt = std.meta.Int(.unsigned, info.int.bits + 1);
        /// same sign as Int but with twice the bits. used for safe multiplication and division.
        pub const MulInt = std.meta.Int(info.int.signedness, info.int.bits * 2);
        const fraction_bits: comptime_int = math.log2_int_ceil(u128, fraction_max);
        pub const Whole = std.meta.Int(info.int.signedness, info.int.bits - fraction_bits);
        pub const Fraction = std.meta.Int(.unsigned, fraction_bits);

        pub fn asInt(x: Self) Int {
            return @intFromEnum(x);
        }

        /// T may be an integer or floating point type
        pub fn as(x: Self, comptime T: type) T {
            return switch (@typeInfo(T)) {
                .int => @as(T, @intFromEnum(x)),
                .float => if (info.int.bits > max_float_bits)
                    @compileError("integer type '" ++ @typeName(Int) ++ "' is larger than 128 bits and unsupported by @floatFromInt()")
                else
                    @as(T, @floatFromInt(x.asInt())) / (0.0 + fraction_max),
                else => |i| @compileError("unexpected type '" ++ @tagName(i) ++ "'"),
            };
        }

        pub fn fromInt(i: Int) Self {
            return @enumFromInt(i);
        }

        /// convert float to an int with rounding
        pub fn fromFloat(float: anytype) Self {
            const scaled = float * fraction_max;
            // log("fromFloat({d}) scaled and rounded {d}", .{ float, @round(scaled) });
            return fromInt(@intFromFloat(@round(scaled)));
        }

        pub fn fromParts(whole: Int, fraction: Int) Self {
            assert(fraction >= 0);
            assert(fraction < fraction_max);
            if (info.int.signedness == .signed) {
                const sum = @as(MulInt, @abs(whole)) * fraction_max + fraction;
                // map [<0, >=0] to [-1, 1]
                const negation = 1 - 2 * @as(i3, @intFromBool(whole < 0));
                return fromInt(@intCast(sum * negation));
            } else return fromInt(@intCast(@as(MulInt, whole) * fraction_max + fraction));
        }

        pub fn wholePart(x: Self) Int {
            return @truncate(@divTrunc(x.as(MulInt), fraction_max));
        }

        pub fn fractionPart(x: Self) Int {
            return @truncate(@as(MulInt, @bitCast(@mod(@abs(x.as(MulInt)), fraction_max))));
        }

        /// convert x to a FixedPoint() with new_precision
        pub fn toScale(x: Self, comptime new_precision: comptime_int) FixedPoint(I, new_precision) {
            const FNew = FixedPoint(I, new_precision);
            const new_fmax = FNew.fraction_max;
            const scaled_int = @divTrunc(x.as(MulInt) * new_fmax, fraction_max);
            return FNew.fromInt(@truncate(scaled_int));
        }

        pub fn add(a: Self, b: Self) Self {
            return fromInt(@intCast(a.as(AddInt) + b.as(AddInt)));
        }

        pub fn addChecked(a: Self, b: Self) !Self {
            return fromInt(math.cast(Int, a.as(AddInt) + b.as(AddInt)) orelse
                return error.Overflow);
        }

        pub fn addAny(a: Self, b: anytype) Self {
            return a.add(b.toScale(precision));
        }

        pub fn sub(a: Self, b: Self) Self {
            return fromInt(@intCast(a.as(AddInt) - b.as(AddInt)));
        }

        pub fn subChecked(a: Self, b: Self) !Self {
            const c = try math.sub(AddInt, a.as(AddInt), b.as(AddInt));
            return fromInt(math.cast(Int, c) orelse return error.Overflow);
        }

        pub fn subAny(a: Self, b: anytype) Self {
            return a.sub(b.toScale(precision));
        }

        pub fn mul(a: Self, b: Self) Self {
            const c = a.as(MulInt) * b.as(MulInt);
            // log("mul() a {} b {} c {} r {}", .{ a, b, c, @divTrunc(c, fraction_max) });
            return fromInt(@intCast(@divTrunc(c, fraction_max)));
        }

        pub fn mulChecked(a: Self, b: Self) !Self {
            const c = @divTrunc(a.as(MulInt) * b.as(MulInt), fraction_max);
            // log("a {}/{} b {}/{} c {}", .{ a, ma, b, mb, c });
            return fromInt(math.cast(Int, c) orelse return error.Overflow);
        }

        pub fn mulAny(a: Self, b: anytype) Self {
            return a.mul(b.toScale(precision));
        }

        pub fn div(a: Self, b: Self) Self {
            const aa = a.as(MulInt) * fraction_max;
            const c = @divTrunc(aa, b.as(MulInt));
            return fromInt(@intCast(c));
        }

        pub fn divChecked(a: Self, b: Self) !Self {
            const aa = a.as(MulInt) * fraction_max;
            const c = try math.divTrunc(MulInt, aa, b.as(MulInt));
            return fromInt(math.cast(Int, c) orelse return error.Overflow);
        }

        pub fn divAny(a: Self, b: anytype) Self {
            return a.div(b.toScale(precision));
        }

        pub fn inc(a: *Self) void {
            a.* = fromInt(a.asInt() + 1);
        }

        pub fn addBy(a: *Self, b: Self) void {
            a.* = a.add(b);
        }

        pub fn subBy(a: *Self, b: Self) void {
            a.* = a.sub(b);
        }

        pub fn mulBy(a: *Self, b: Self) void {
            a.* = a.mul(b);
        }

        pub fn divBy(a: *Self, b: Self) void {
            a.* = a.div(b);
        }

        pub fn sqrt(a: Self, comptime F: type) Self {
            return fromInt(@intFromFloat(@sqrt(a.as(F)) * fraction_max));
        }

        pub fn neg(a: Self) Self {
            return if (info.int.signedness == .signed)
                fromInt(-a.asInt())
            else
                @compileError("can't negate unsigned integer");
        }

        pub fn order(a: Self, b: Self) math.Order {
            return std.math.order(a.asInt(), b.asInt());
        }

        pub fn gt(a: Self, b: Self) bool {
            return a.asInt() > b.asInt();
        }

        pub fn lt(a: Self, b: Self) bool {
            return a.asInt() < b.asInt();
        }

        pub fn eq(a: Self, b: Self) bool {
            return a.asInt() == b.asInt();
        }

        /// parse s with rounding.
        pub fn parse(s: []const u8) !Self {
            // log("parse({s}) {s}", .{ s, @typeName(I) });
            if (s.len == 0) return error.InvalidCharacter;

            var pos: usize = 0;
            var isneg = false;
            if (s[pos] == '-') {
                isneg = true;
                pos += 1;
            }

            var dotpos: usize = math.maxInt(usize);
            if (s.len - pos > 1) switch (mem.readInt(u16, s[pos..][0..2], .big)) {
                mem.readInt(u16, "0.", .big) => {
                    dotpos = pos + 1;
                    pos += 2;
                },
                mem.readInt(u16, "00", .big) => {
                    return error.InvalidCharacter;
                },
                else => {},
            };

            var int: MulInt = 0;
            while (pos < s.len) : (pos += 1) {
                // log("i {} c {c} int {}", .{ pos, s[pos], int });
                switch (s[pos]) {
                    '0' => {
                        int *= 10;
                    },
                    '1'...'9' => {
                        int *= 10;
                        int += s[pos] - '0';
                    },
                    '.' => {
                        if (dotpos != math.maxInt(usize))
                            return error.InvalidCharacter;
                        dotpos = pos;
                    },
                    else => return error.InvalidCharacter,
                }

                if (dotpos != math.maxInt(usize)) {
                    // rounding
                    if (pos - dotpos == precision + 1) {
                        int = @divTrunc(int + 5, 10);
                        break;
                    }
                }
            }

            int = if (dotpos != math.maxInt(usize))
                int * @as(MulInt, @intCast(base_10_powers[@intCast(precision -| (s.len - dotpos - 1))]))
            else
                int * fraction_max;

            // log("int {}/{}/{} dotpos {}/{}", .{ int, parsed, max_int, dotpos, precision });

            int = if (info.int.signedness == .signed)
                if (isneg) -int else int
            else
                int;

            return fromInt(math.cast(Int, int) orelse return error.Overflow);
        }

        pub inline fn parseComptime(comptime s: []const u8) Self {
            return comptime parse(s) catch |e|
                @compileError("parseComptime(): '" ++ @errorName(e) ++ "'");
        }

        pub fn format(self: Self, comptime fmt: []const u8, fopts: std.fmt.FormatOptions, writer: anytype) !void {
            if (!(fmt.len == 0 or mem.eql(u8, fmt, "d"))) return error.UnsupportedSpecifier;

            const int = self.asInt();
            const Len = u16;
            const prec: Len = @truncate(if (fopts.precision) |p| p else precision);
            var uint: UInt = @abs(int);
            const l = digitsLenBase10(UInt, uint);
            const larger_than_len = info.int.bits > @bitSizeOf(Len);
            const smaller_than_max = info.int.bits < @bitSizeOf(MaxUint);
            const len: Len = if (larger_than_len) @truncate(l) else l;
            const whole = uint / fraction_max;
            const wl = digitsLenBase10(UInt, whole);
            const whole_len: Len = if (larger_than_len) @truncate(wl) else wl;
            const fraction = uint % fraction_max;
            _ = fraction; // autofix
            var frac_len: Len = len - whole_len;
            // log(
            //     "format({}) uint/len {}/{} whole/len {}/{} frac/len {}/{} prec {}",
            //     .{ int, uint, len, whole, whole_len, fraction, frac_len, prec },
            // );

            // optionally round the last digit
            if (frac_len > prec) {
                const powi: Len = @truncate(frac_len - prec);
                const round_pow: Len = @truncate(base_10_powers[powi - 1]);
                const round_digit = uint / (round_pow + @intFromBool(round_pow == 0)) % 10;
                // log("round_digit {} powi {} round_pow {}", .{ round_digit, powi, round_pow });
                uint /= if (smaller_than_max) @truncate(base_10_powers[powi]) else base_10_powers[powi];
                uint += @intFromBool(round_digit >= 5);
                frac_len = prec;
            }
            // log("uint {} frac_len {}", .{ uint, frac_len });

            // format into buf backwards
            var buf: [digitsLenBase10(MaxUint, math.maxInt(MaxUint)) + 1]u8 = undefined;
            var i = buf.len;
            while (true) {
                if (uint == 0) break;
                // log("i {} uint {} buf {s} buf.len - i {}", .{ i, uint, buf[i..], buf.len - i });
                i -= 1;
                buf[i] = if (info.int.bits < 8)
                    uint % 10
                else
                    @as(u8, @truncate(uint % 10)) + '0';

                uint = uint / 10;
                if (uint != 0 and buf.len - i == frac_len) {
                    i -= 1;
                    buf[i] = '.';
                }
            }

            // write optional leading '-' and '0.0...'
            _ = try writer.write("-"[0..@intFromBool(int < 0)]);
            const write_zeroes = @intFromBool(whole == 0);
            _ = try writer.write("0."[0 .. (1 + @as(u8, @intFromBool(precision > 0 and precision >= frac_len))) * write_zeroes]);
            _ = try writer.writeByteNTimes('0', (precision - frac_len) * write_zeroes);
            // write digits from buf
            const written = buf[i..];
            // log("frac_len {} precision {} prec {} len {} written '{s}'", .{ frac_len, precision, prec, len, written });
            _ = try writer.write(written);
            // write optional trailing zeroes
            try writer.writeByteNTimes('0', (prec - frac_len) * @intFromBool(len >= precision));
        }

        /// guess the smallest float type needed to represent Self.
        /// used internally by format() and parse().
        /// returns either f16, f32, f64 or f128.
        /// adjust: return type will be 'adjust' types larger. i.e.
        ///   I=u16, Float(0) => f16
        ///   I=u16, Float(1) => f32
        pub fn Float(comptime adjust: u8) type {
            comptime {
                const fraction_max_f = 0.0 + fraction_max;
                const min = (0.0 + min_int) / fraction_max_f;
                const max = (0.0 + max_int) / fraction_max_f;
                const resolution = 1.0 / fraction_max_f;

                const canRepresent = struct {
                    fn canRepresent(comptime T: type) bool {
                        return -math.floatMax(T) <= min and max <= math.floatMax(T) and
                            resolution >= math.floatEps(T);
                    }
                }.canRepresent;

                const Ts = [_]type{ f16, f32, f64, f128 };
                for (0..Ts.len) |i| {
                    if (canRepresent(Ts[i])) return Ts[@min(Ts.len - 1, adjust + i)];
                }
                return f128;
            }
        }

        fn log(comptime fmt: []const u8, args: anytype) void {
            if (true) return;
            if (!@inComptime()) {
                std.log.debug(fmt, args);
            } else {
                // @compileLog(std.fmt.comptimePrint(fmt, args));
            }
        }
    };
}

pub const max_bits = 256;
pub const max_float_bits = 128;
pub const MaxUint = std.meta.Int(.unsigned, max_bits);
/// pre calculated powers of 10 to eliminate some expensive math.pow() and
/// log() calls.
const base_10_powers = blk: {
    const uint_digits = math.log(MaxUint, 10, math.maxInt(MaxUint)) + 1;
    var powers: [uint_digits]MaxUint = undefined;
    powers[0] = 1;
    for (1..uint_digits) |i| {
        powers[i] = powers[i - 1] * 10;
    }
    break :blk powers;
};

/// similar to log10() but returns 0 when int == 0.
pub fn digitsLenBase10(comptime T: type, int: T) T {
    inline for (0..base_10_powers.len - 1) |i| {
        if (int < base_10_powers[i]) return i;
    }
    assert(int / 10 <= base_10_powers[base_10_powers.len - 1]);
    return base_10_powers.len - 1;
}
