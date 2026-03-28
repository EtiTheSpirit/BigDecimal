using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Numerics;
using System.Diagnostics;
using System.Globalization;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Diagnostics.Contracts;

using NoDiscard = System.Diagnostics.Contracts.PureAttribute;

namespace Star3D.Maths.Numbers {

	/// <summary>
	/// <admonition type="danger">
	/// <strong>Extremely slow type!</strong>
	/// This type uses excessive and often unnecessary precision in its computations, which can make it several hundreds of times slower
	/// than native system numeric types like <see cref="double"/>.<br/><br/>
	/// <see cref="BigDecimal"/>'s primary use case is in computing values from the physical constants of the universe, which are regularly
	/// extremely large or small, well beyond the precision limits of <see cref="double"/>.
	/// </admonition>
	/// <para/>
	/// Represents a decimal value with arbitrary position. This can be used for near-perfect accuracy computations.
	/// <para/>
	/// Originally written by <em>Jan Christoph Bernack</em> for the public domain, and then heavily modified for use in The Conservatory 
	/// including the implementation of algebraic operations.
	/// </summary>
	// [SecurityDeny(Capability.Patching)]
	public struct BigDecimal :
		INumber<BigDecimal>,
		IPowerFunctions<BigDecimal>,
		IEquatable<BigDecimal>,
		IComparable,
		IComparable<BigDecimal> {

		/// <summary>
		/// The maximum precision of division operations, measured as 10^<see cref="DIVISION_PRECISION"/>. 
		/// (that is, <see cref="DIVISION_PRECISION"/> decimal places).
		/// This protects against irrational numbers being infinitely long.
		/// </summary>
		public static readonly int DIVISION_PRECISION = 100;

		/// <summary>
		/// The same as <see cref="DIVISION_PRECISION"/>, but with higher accuracy, used in some intermediary calculations.
		/// </summary>
		public static readonly int HIGH_DIVISION_PRECISION = 200;

		/// <summary>
		/// The typical value used for parameters which limit iterative functions.
		/// </summary>
		public const uint STANDARD_MAX_ITERATIONS = 2000;

		/// <inheritdoc/>
		public static BigDecimal One { get; } = new BigDecimal(1, 0);

		/// <summary>
		/// Gets the value <c>0.5</c> for the type.
		/// </summary>
		public static BigDecimal OneHalf { get; } = new BigDecimal(5, -1);

		/// <inheritdoc/>
		public static int Radix { get; } = 10;

		/// <inheritdoc/>
		public static BigDecimal Zero { get; } = default;

		/// <summary>
		/// The mathematical constant <see langword="e"/>, computed up to 100 iterations.
		/// </summary>
		public static BigDecimal E { get; }

		/// <summary>
		/// The mathematical constant <see langword="pi"/>, computed up to 100 decimal places.
		/// </summary>
		public static BigDecimal Pi { get; }

		/// <summary>
		/// The inverse of the mathematical constant <see langword="pi"/>: <see langword="1/pi"/>.
		/// </summary>
		public static BigDecimal InversePi { get; }

		/// <summary>
		/// The mathematical constant <see langword="tau"/>, equal to <see langword="pi"/> times two.
		/// </summary>
		public static BigDecimal Tau { get; }

		/// <summary>
		/// <see cref="Half.MinValue"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal HalfMinValue { get; }

		/// <summary>
		/// <see cref="Half.MaxValue"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal HalfMaxValue { get; }

		/// <summary>
		/// <see cref="Half.Epsilon"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal HalfEpsilonValue { get; }

		/// <summary>
		/// <see cref="float.MinValue"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal FloatMinValue { get; }

		/// <summary>
		/// <see cref="float.MaxValue"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal FloatMaxValue { get; }

		/// <summary>
		/// <see cref="float.Epsilon"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal FloatEpsilonValue { get; }

		/// <summary>
		/// <see cref="double.MinValue"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal DoubleMinValue { get; }

		/// <summary>
		/// <see cref="double.MaxValue"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal DoubleMaxValue { get; }

		/// <summary>
		/// <see cref="double.Epsilon"/> expressed as <see cref="BigDecimal"/>.
		/// </summary>
		public static BigDecimal DoubleEpsilonValue { get; }

		/// <summary>
		/// The epsilon from division, or the smallest achievable value via division, as determined by <see cref="DIVISION_PRECISION"/>.
		/// </summary>
		public static BigDecimal DivisionEpsilon { get; } = new BigDecimal(1, -DIVISION_PRECISION);

		/// <inheritdoc/>
		public static BigDecimal AdditiveIdentity => Zero;

		/// <inheritdoc/>
		public static BigDecimal MultiplicativeIdentity => One;

		/// <summary>
		/// The value representing -1.
		/// </summary>
		public static BigDecimal NegativeOne { get; } = new BigDecimal(-1, 0);

		/// <summary>
		/// The cached result of ln(2).
		/// </summary>
		public static BigDecimal Ln2 { get; }

		/// <summary>
		/// The cached result of ln(10).
		/// </summary>
		public static BigDecimal Ln10 { get; }

		/// <summary>
		/// The cached result of ln(100).
		/// </summary>
		public static BigDecimal Ln100 { get; }

		/// <summary>
		/// The mantissa of the scientific notation of this value. This is the whole number component.
		/// </summary>
		public BigInteger Mantissa { get; private set; }

		/// <summary>
		/// The exponent of the scientific notation of this value. This controls where the decimal point goes
		/// in the mantissa, relative to the least significant digit.
		/// </summary>
		public int Exponent { get; private set; }

		/// <summary>
		/// Create a new <see cref="BigDecimal"/> as <paramref name="mantissa"/> times 10 to the power of <paramref name="exponent"/>.
		/// </summary>
		/// <param name="mantissa"></param>
		/// <param name="exponent"></param>
		[DebuggerStepThrough]
		public BigDecimal(BigInteger mantissa, int exponent = 0) {
			Unsafe.SkipInit(out this);
			Mantissa = mantissa;
			Exponent = exponent;
			NormalizeSelf();
		}

		/// <summary>
		/// Create a new <see cref="BigDecimal"/> from the system <see cref="decimal"/> type.
		/// </summary>
		/// <param name="decVal"></param>
		public BigDecimal(decimal decVal) {
			Unsafe.SkipInit(out this);

			// Returns a binary representation of a Decimal. The return value is an
			// integer array with four elements. Elements 0, 1, and 2 contain the low,
			// middle, and high 32 bits of the 96-bit integer part of the Decimal.
			// Element 3 contains the scale factor and sign of the Decimal: bits 0-15
			// (the lower word) are unused; bits 16-23 contain a value between 0 and
			// 28, indicating the power of 10 to divide the 96-bit integer part by to
			// produce the Decimal value; bits 24-30 are unused; and finally bit 31
			// indicates the sign of the Decimal value, 0 meaning positive and 1
			// meaning negative.
			int[] bits = decimal.GetBits(decVal);
			uint mostSig = (uint)bits[2];
			uint midSig = (uint)bits[1];
			uint leastSig = (uint)bits[0];
			uint scale = (uint)bits[3];

			bool invert = unchecked(scale & 0x80000000) != 0;
			long expo = -((scale >> 16) & 0x7F);

			BigInteger bigInt = new BigInteger(mostSig) << 64;
			bigInt |= new BigInteger(midSig) << 32;
			bigInt |= new BigInteger(leastSig);

			Mantissa = invert ? -bigInt : bigInt;
			Exponent = (int)expo;
			NormalizeSelf();
		}

		/// <summary>
		/// Separates the whole number from its decimal part. XPointY(7, 125) would be 7.125.
		/// </summary>
		/// <param name="x"></param>
		/// <param name="y"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal XPointY(BigInteger x, BigInteger y) {
			int nDigits = NumberOfDigits(y);
			BigDecimal frac = new BigDecimal(y, -nDigits);
			return x + frac;
		}

		/// <summary>
		/// Separates the whole number from its decimal part. XPointY(7, 125) would be 7.125.
		/// The exponent is a factor of 10, allowing this to be used for scientific notation.
		/// <c><paramref name="x"/>.<paramref name="y"/>*(10^<paramref name="exponent"/>)</c>
		/// </summary>
		/// <param name="x"></param>
		/// <param name="y"></param>
		/// <param name="exponent"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal XPointY(BigInteger x, BigInteger y, int exponent = 0) {
			int nDigits = NumberOfDigits(y);
			BigDecimal frac = new BigDecimal(y, -nDigits);
			BigDecimal result = x + frac;
			result.Exponent += exponent;
			return result;
		}

		/// <summary>
		/// Separates the whole number from its decimal part. NegativeXPointY(7, 125) would be -7.125.
		/// This is typically useful for -0.y
		/// </summary>
		/// <param name="x"></param>
		/// <param name="y"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal NegativeXPointY(BigInteger x, BigInteger y) {
			int nDigits = NumberOfDigits(y);
			BigDecimal frac = new BigDecimal(y, -nDigits);
			return -(x + frac);
		}

		/// <summary>
		/// Separates the whole number from its decimal part. XPointY(7, 125) would be -7.125.
		/// The exponent is a factor of 10, allowing this to be used for scientific notation.
		/// <c><paramref name="x"/>.<paramref name="y"/>*(10^<paramref name="exponent"/>)</c>
		/// This is typically useful for -0.y
		/// </summary>
		/// <param name="x"></param>
		/// <param name="y"></param>
		/// <param name="exponent"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal NegativeXPointY(BigInteger x, BigInteger y, int exponent = 0) {
			int nDigits = NumberOfDigits(y);
			BigDecimal frac = new BigDecimal(y, -nDigits);
			BigDecimal result = x + frac;
			result.Exponent += exponent;
			return -result;
		}

		/// <summary>
		/// Removes trailing zeros on the mantissa of this instance.
		/// </summary>
		[DebuggerStepThrough]
		[NoDiscard]
		public void NormalizeSelf() {
			if (Mantissa.IsZero) {
				Exponent = 0;
			} else {
				BigInteger remainder = 0;
				while (remainder == 0) {
					BigInteger shortened = BigInteger.DivRem(Mantissa, 10, out remainder);
					if (remainder == 0) {
						Mantissa = shortened;
						Exponent++;
					}
				}
			}
		}

		/// <summary>
		/// Truncate the number to the given precision by removing the least significant digits.
		/// </summary>
		/// <returns>The truncated number</returns>
		[DebuggerStepThrough]
		[NoDiscard]
		public static BigDecimal Truncate(BigDecimal value, int precision) {
			// Self-reminder that structs are copied by value, so <value> here is a new instance because it is not a ref parameter
			value.TruncateSelf(precision);
			return value;
		}

		/// <summary>
		/// Truncate this value. This mutates the struct!
		/// </summary>
		/// <param name="precision"></param>
		/// <returns></returns>
		[NoDiscard]
		private void TruncateSelf(int precision) {
			// save some time because the number of digits is not needed to remove trailing zeros
			NormalizeSelf();
			// remove the least significant digits, as long as the number of digits is higher than the given Precision
			int digitCount = NumberOfDigits(Mantissa);
			int difference = digitCount - precision;

			bool skipNormalization = true;
			while (digitCount > precision) {
				skipNormalization = false;
				Mantissa /= BigInteger.Pow(10, difference);
				Exponent += difference;
				digitCount = NumberOfDigits(Mantissa);
			}
			// normalize again to make sure there are no trailing zeros left
			if (!skipNormalization) {
				NormalizeSelf();
			}
		}

		/// <summary>
		/// Truncate the number so that up to the provided amount of digits after the decimal are shown.
		/// If the number has less than the provided amount, it will <strong>not</strong> add additional zeros.
		/// </summary>
		/// <returns>The truncated number</returns>
		[DebuggerStepThrough]
		[NoDiscard]
		public static BigDecimal TruncateToPlaces(BigDecimal value, int decimalDigits) {
			return Truncate(value, NumberOfDigits(value.Mantissa) + value.Exponent + decimalDigits);
		}

		/// <summary>
		/// Cuts the length of the value down to <see cref="DIVISION_PRECISION"/>.
		/// </summary>
		/// <returns></returns>
		[DebuggerStepThrough]
		[NoDiscard]
		public static BigDecimal Truncate(BigDecimal value) {
			return Truncate(value, DIVISION_PRECISION);
		}

		/// <summary>
		/// Removes the decimal component of the value.
		/// </summary>
		/// <returns></returns>
		[DebuggerStepThrough]
		[NoDiscard]
		public static BigDecimal Floor(BigDecimal value) {
			return Truncate(value, NumberOfDigits(value.Mantissa) + value.Exponent);
		}

		/// <summary>
		/// Removes the decimal component of the value.
		/// </summary>
		/// <returns></returns>
		[DebuggerStepThrough]
		[NoDiscard]
		public static BigInteger FloorToInt(BigDecimal value) {
			return Truncate(value, NumberOfDigits(value.Mantissa) + value.Exponent).Mantissa;
		}

		/// <summary>
		/// Rounds up, away from zero.
		/// </summary>
		/// <returns></returns>
		[DebuggerStepThrough]
		[NoDiscard]
		public static BigDecimal Ceil(BigDecimal value) {
			BigDecimal ofst = value < 0 ? -1 : 1;
			return Floor(value + ofst);
		}

		/// <summary>
		/// Rounds value away from zero.
		/// </summary>
		/// <param name="value"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal Round(BigDecimal value) {
			bool n = value < 0;
			BigDecimal abs = Abs(value);
			BigDecimal floor = Floor(abs);
			BigDecimal frac = abs - floor;
			if (frac >= new BigDecimal(5, -1)) {
				value = Floor(abs + 1);
				if (n) value.Mantissa *= -1;
				return value;
			}
			if (n) floor.Mantissa *= -1;
			return floor;
		}

		/// <summary>
		/// Returns the number of digits present in this number, not counting the sign.
		/// </summary>
		/// <param name="value"></param>
		/// <returns></returns>
		[DebuggerStepThrough]
		[NoDiscard]
		public static int NumberOfDigits(BigInteger value) {
			// do not count the sign
			//return (value * value.Sign).ToString().Length;
			// faster version
			return (int)Math.Ceiling(BigInteger.Log10(value * value.Sign));
		}

		#region Conversions

		#region Primitive Integers, BigInteger, Decimal => BigDecimal

		/// <inheritdoc/>
		public static implicit operator BigDecimal(byte value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(short value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(int value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(long value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(sbyte value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(ushort value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(uint value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(ulong value) {
			return new BigDecimal(value, 0);
		}


		/// <inheritdoc/>
		public static implicit operator BigDecimal(Int128 value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(UInt128 value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(BigInteger value) {
			return new BigDecimal(value, 0);
		}

		/// <inheritdoc/>
		public static implicit operator BigDecimal(decimal value) {
			return new BigDecimal(value);
		}
		#endregion

		#region Floating Point => BigDecimal
		/// <summary>
		/// Convert the provided <see cref="Half"/> into a <see cref="BigDecimal"/>.
		/// <strong>NOTE: This will inherit floating point error as a real value.</strong>
		/// </summary>
		/// <param name="value"></param>
		public static implicit operator BigDecimal(Half value) {
			// S, E, M
			// 1, 5, 10
			ushort bits = BitConverter.HalfToUInt16Bits(value);
			bool negate = (bits & 0x8000) != 0;
			const int bias = (1 << 4) - 1;
			int realExponent = ((bits & 0x7C00) >> 10) - bias;
			int mantissa = bits & 0x3FF;


			BigDecimal expoValue = BigInteger.Pow(2, int.Abs(realExponent));
			if (realExponent < 0) {
				expoValue = One / expoValue;
			}

			BigDecimal fraction = 0;
			for (int i = 9; i >= 0; i--) {
				byte bit = (byte)((long)(mantissa >> i) & 1);
				if (bit > 0) {
					fraction += One / (1 << (10 - i));
				}
			}
			fraction += One;
			BigDecimal result = fraction * expoValue;
			result.NormalizeSelf();
			if (negate) return -result;
			return result;
		}

		/// <summary>
		/// Convert the provided <see cref="float"/> into a <see cref="BigDecimal"/>.
		/// <strong>NOTE: This will inherit floating point error as a real value.</strong>
		/// </summary>
		/// <param name="value"></param>
		public static implicit operator BigDecimal(float value) {
			// S, E, M
			// 1, 8, 23
			uint bits = BitConverter.SingleToUInt32Bits(value);
			bool negate = (bits & 0x80000000) != 0;
			const int bias = (1 << 7) - 1;
			int realExponent = (int)((bits & 0x7F800000) >> 23) - bias;
			int mantissa = (int)(bits & 0x7FFFFF);

			BigDecimal expoValue = BigInteger.Pow(2, int.Abs(realExponent));
			if (realExponent < 0) {
				expoValue = One / expoValue;
			}

			BigDecimal fraction = 0;
			for (int i = 22; i >= 0; i--) {
				byte bit = (byte)((long)(mantissa >> i) & 1);
				if (bit > 0) {
					fraction += One / (1 << (23 - i));
				}
			}
			fraction += One;
			BigDecimal result = fraction * expoValue;
			result.NormalizeSelf();
			if (negate) return -result;
			return result;
		}

		/// <summary>
		/// Convert the provided <see cref="double"/> into a <see cref="BigDecimal"/>.
		/// <strong>NOTE: This will inherit floating point error as a real value.</strong>
		/// </summary>
		/// <param name="value"></param>
		public static implicit operator BigDecimal(double value) {
			// S, E, M
			// 1, 11, 52
			ulong bits = BitConverter.DoubleToUInt64Bits(value);
			bool negate = (bits & 0x8000000000000000) != 0;
			const int bias = (1 << 10) - 1;
			int realExponent = (int)((bits & 0x7FF0000000000000) >> 52) - bias;
			long mantissa = (long)bits & 0xFFFFFFFFFFFFF;

			BigDecimal expoValue = BigInteger.Pow(2, int.Abs(realExponent));
			if (realExponent < 0) {
				expoValue = One / expoValue;
			}

			BigDecimal fraction = 0;
			for (int i = 51; i >= 0; i--) {
				byte bit = (byte)((mantissa >> i) & 1);
				if (bit > 0) {
					fraction += One / (1UL << (52 - i));
				}
			}
			fraction += One;
			BigDecimal result = fraction * expoValue;
			result.NormalizeSelf();
			if (negate) return -result;
			return result;
		}

		#endregion

		/// <inheritdoc/>
		public static explicit operator decimal(BigDecimal value) {
			// integer array with four elements. Elements 0, 1, and 2 contain the low,
			// middle, and high 32 bits of the 96-bit integer part of the Decimal.
			// Element 3 contains the scale factor and sign of the Decimal: bits 0-15
			// (the lower word) are unused; bits 16-23 contain a value between 0 and
			// 28, indicating the power of 10 to divide the 96-bit integer part by to
			// produce the Decimal value; bits 24-30 are unused; and finally bit 31
			// indicates the sign of the Decimal value, 0 meaning positive and 1
			// meaning negative.
			const decimal DECIMAL_EPSILON = 0.0000000000000000000000000001m;
			if (value.Exponent == 0) return (decimal)value.Mantissa;
			if (value < decimal.MinValue) return decimal.MinValue;
			if (value > decimal.MaxValue) return decimal.MaxValue;
			if (Abs(value) < DECIMAL_EPSILON) return 0;
			decimal v = 1m;
			for (int i = 1; i < value.Exponent; i++) {
				// ^ Yes start at 1, ^0 needs to do nothing.
				v *= 10m;
			}
			return (decimal)value.Mantissa * v;
		}

		/// <inheritdoc/>
		public static explicit operator double(BigDecimal value) {
			if (value > DoubleMaxValue) return double.PositiveInfinity;
			if (value < DoubleMinValue) return double.NegativeInfinity;
			if (Abs(value) < DoubleEpsilonValue) return 0.0D;
			value = Truncate(value, 17);
			return (double)value.Mantissa * Math.Pow(10, value.Exponent);
		}

		/// <inheritdoc/>
		public static explicit operator float(BigDecimal value) {
			return (float)(double)value;
		}

		/// <inheritdoc/>
		public static explicit operator Half(BigDecimal value) {
			return (Half)(double)value;
		}

		/// <inheritdoc/>
		public static explicit operator checked sbyte(BigDecimal value) {
			return checked((sbyte)(long)value);
		}

		/// <inheritdoc/>
		public static explicit operator checked byte(BigDecimal value) {
			return checked((byte)(ulong)value);
		}

		/// <inheritdoc/>
		public static explicit operator checked short(BigDecimal value) {
			return checked((short)(long)value);
		}

		/// <inheritdoc/>
		public static explicit operator checked ushort(BigDecimal value) {
			return checked((ushort)(ulong)value);
		}

		/// <inheritdoc/>
		public static explicit operator checked int(BigDecimal value) {
			return checked((int)(long)value);
		}

		/// <inheritdoc/>
		public static explicit operator checked uint(BigDecimal value) {
			return checked((uint)(ulong)value);
		}

		/// <inheritdoc/>
		public static explicit operator checked long(BigDecimal value) {
			return checked((long)(value.Mantissa * BigInteger.Pow(10, value.Exponent)));
		}

		/// <inheritdoc/>
		public static explicit operator checked ulong(BigDecimal value) {
			return checked((ulong)(value.Mantissa * BigInteger.Pow(10, value.Exponent)));
		}


		/// <inheritdoc/>
		public static explicit operator sbyte(BigDecimal value) {
			return unchecked((sbyte)(long)value);
		}

		/// <inheritdoc/>
		public static explicit operator byte(BigDecimal value) {
			return unchecked((byte)(ulong)value);
		}

		/// <inheritdoc/>
		public static explicit operator short(BigDecimal value) {
			return unchecked((short)(long)value);
		}

		/// <inheritdoc/>
		public static explicit operator ushort(BigDecimal value) {
			return unchecked((ushort)(ulong)value);
		}

		/// <inheritdoc/>
		public static explicit operator int(BigDecimal value) {
			return unchecked((int)(long)value);
		}

		/// <inheritdoc/>
		public static explicit operator uint(BigDecimal value) {
			return unchecked((uint)(ulong)value);
		}

		/// <inheritdoc/>
		public static explicit operator long(BigDecimal value) {
			return unchecked((long)(value.Mantissa * BigInteger.Pow(10, value.Exponent)));
		}

		/// <inheritdoc/>
		public static explicit operator ulong(BigDecimal value) {
			return unchecked((ulong)(value.Mantissa * BigInteger.Pow(10, value.Exponent)));
		}

		/// <inheritdoc/>
		public static explicit operator BigInteger(BigDecimal value) {
			return unchecked(value.Mantissa * BigInteger.Pow(10, value.Exponent));
		}

		#endregion

		#region Operators

		/// <inheritdoc/>
		public static BigDecimal operator +(BigDecimal value) {
			return value;
		}

		/// <inheritdoc/>
		public static BigDecimal operator -(BigDecimal value) {
			value.Mantissa = -value.Mantissa;
			return value;
		}

		/// <inheritdoc/>
		public static BigDecimal operator ++(BigDecimal value) {
			return value + 1;
		}

		/// <inheritdoc/>
		public static BigDecimal operator --(BigDecimal value) {
			return value - 1;
		}

		/// <inheritdoc/>
		public static BigDecimal operator +(BigDecimal left, BigDecimal right) {
			return Add(left, right);
		}

		/// <inheritdoc/>
		public static BigDecimal operator -(BigDecimal left, BigDecimal right) {
			return Add(left, -right);
		}

		private static BigDecimal Add(BigDecimal left, BigDecimal right) {
			return left.Exponent > right.Exponent
				? new BigDecimal(AlignExponent(left, right) + right.Mantissa, right.Exponent)
				: new BigDecimal(AlignExponent(right, left) + left.Mantissa, left.Exponent);
		}

		/// <inheritdoc/>
		public static BigDecimal operator *(BigDecimal left, BigDecimal right) {
			return new BigDecimal(left.Mantissa * right.Mantissa, left.Exponent + right.Exponent);
		}

		/// <inheritdoc/>
		public static BigDecimal operator /(BigDecimal dividend, BigDecimal divisor) {
			int exponentChange = DIVISION_PRECISION - (NumberOfDigits(dividend.Mantissa) - NumberOfDigits(divisor.Mantissa));
			if (exponentChange < 0) {
				exponentChange = 0;
			}
			dividend.Mantissa *= BigInteger.Pow(10, exponentChange);
			return new BigDecimal(dividend.Mantissa / divisor.Mantissa, dividend.Exponent - divisor.Exponent - exponentChange);
		}

		/// <summary>
		/// Divide, but override the value of <see cref="DIVISION_PRECISION"/>.
		/// </summary>
		/// <param name="dividend"></param>
		/// <param name="divisor"></param>
		/// <param name="divisionPrecision"></param>
		/// <returns></returns>
		public static BigDecimal DivideWithCustomPrecision(BigDecimal dividend, BigDecimal divisor, int divisionPrecision) {
			ArgumentOutOfRangeException.ThrowIfNegativeOrZero(divisionPrecision);
			int exponentChange = divisionPrecision - (NumberOfDigits(dividend.Mantissa) - NumberOfDigits(divisor.Mantissa));
			if (exponentChange < 0) {
				exponentChange = 0;
			}
			dividend.Mantissa *= BigInteger.Pow(10, exponentChange);
			return new BigDecimal(dividend.Mantissa / divisor.Mantissa, dividend.Exponent - divisor.Exponent - exponentChange);
		}

		/// <inheritdoc/>
		public static BigDecimal operator %(BigDecimal left, BigDecimal right) {
			return left - right * Floor(left / right);
		}

		/// <inheritdoc/>
		public static bool operator ==(BigDecimal left, BigDecimal right) {
			return left.Exponent == right.Exponent && left.Mantissa == right.Mantissa;
		}

		/// <inheritdoc/>
		public static bool operator !=(BigDecimal left, BigDecimal right) {
			return left.Exponent != right.Exponent || left.Mantissa != right.Mantissa;
		}

		/// <inheritdoc/>
		public static bool operator <(BigDecimal left, BigDecimal right) {
			return left.Exponent > right.Exponent ? AlignExponent(left, right) < right.Mantissa : left.Mantissa < AlignExponent(right, left);
		}

		/// <inheritdoc/>
		public static bool operator >(BigDecimal left, BigDecimal right) {
			return left.Exponent > right.Exponent ? AlignExponent(left, right) > right.Mantissa : left.Mantissa > AlignExponent(right, left);
		}

		/// <inheritdoc/>
		public static bool operator <=(BigDecimal left, BigDecimal right) {
			return left.Exponent > right.Exponent ? AlignExponent(left, right) <= right.Mantissa : left.Mantissa <= AlignExponent(right, left);
		}

		/// <inheritdoc/>
		public static bool operator >=(BigDecimal left, BigDecimal right) {
			return left.Exponent > right.Exponent ? AlignExponent(left, right) >= right.Mantissa : left.Mantissa >= AlignExponent(right, left);
		}

		/// <summary>
		/// Returns the mantissa of value, aligned to the exponent of reference.
		/// Assumes the exponent of value is larger than of reference.
		/// </summary>
		private static BigInteger AlignExponent(BigDecimal value, BigDecimal reference) {
			return value.Mantissa * BigInteger.Pow(10, value.Exponent - reference.Exponent);
		}

		#endregion

		#region Additional mathematical functions

		/// <summary>
		/// Raises <paramref name="base"/> to the power of <paramref name="exponent"/> where the exponent is a positive integer.
		/// </summary>
		/// <param name="base"></param>
		/// <param name="exponent"></param>
		/// <returns></returns>
		public static BigDecimal IntPow(BigDecimal @base, BigInteger exponent) {
			BigDecimal buf0;
			BigDecimal buf1;
			if (exponent == 0) {
				return 1;
			} else if (exponent == 1) {
				return @base;

			} else if (exponent == 2) {
				return @base * @base;

			} else if (exponent == 3) {
				return @base * @base * @base;

			} else if (exponent == 4) {
				buf0 = @base * @base;
				return buf0 * buf0;

			} else if (exponent == 5) {
				buf0 = @base * @base;
				return buf0 * buf0 * @base;

			} else if (exponent == 6) {
				buf0 = @base * @base * @base;
				return buf0 * buf0;

			} else if (exponent == 7) {
				buf0 = @base * @base * @base;
				return buf0 * buf0 * @base;

			} else if (exponent == 8) {
				buf0 = @base * @base;
				buf1 = buf0 * buf0;
				return buf1 * buf1;

			} else {
				BigDecimal result = 1;
				BigInteger eight = 8;
				while (exponent > eight) {
					// Use the largest available unit so that more multiplications can be done in less operations.
					result *= IntPow(@base, eight);
					exponent -= eight;
				}
				return result * IntPow(@base, exponent);
			}
		}

		/// <summary>
		/// Raises <paramref name="base"/> to the power of <paramref name="exponent"/> where the exponent is a positive integer.
		/// </summary>
		/// <param name="base"></param>
		/// <param name="exponent"></param>
		/// <returns></returns>
		public static BigDecimal IntPow(BigDecimal @base, ulong exponent) {
			BigDecimal buf0;
			BigDecimal buf1;
			if (exponent == 0) {
				return 1;
			} else if (exponent == 1) {
				return @base;

			} else if (exponent == 2) {
				return @base * @base;

			} else if (exponent == 3) {
				return @base * @base * @base;

			} else if (exponent == 4) {
				buf0 = @base * @base;
				return buf0 * buf0;

			} else if (exponent == 5) {
				buf0 = @base * @base;
				return buf0 * buf0 * @base;

			} else if (exponent == 6) {
				buf0 = @base * @base * @base;
				return buf0 * buf0;

			} else if (exponent == 7) {
				buf0 = @base * @base * @base;
				return buf0 * buf0 * @base;

			} else if (exponent == 8) {
				buf0 = @base * @base;
				buf1 = buf0 * buf0;
				return buf1 * buf1;

			} else {
				BigDecimal result = 1;
				while (exponent > 8UL) {
					// Use the largest available unit so that more multiplications can be done in less operations.
					result *= IntPow(@base, 8UL);
					exponent -= 8UL;
				}
				return result * IntPow(@base, exponent);
			}
		}

		/// <summary>
		/// Raises <c>e</c> to the power of <paramref name="exponent"/> using a taylor series of the
		/// provided amount of iterations. More iterations yields more accurate results, at the expense of time.
		/// </summary>
		/// <param name="exponent">The exponent that <c>e</c> is being raised by.</param>
		/// <param name="maxIterations">The amount of iterations to perform. More iterations increases accuracy.</param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal ApproximateExp(BigDecimal exponent, uint maxIterations = STANDARD_MAX_ITERATIONS) {
			ArgumentOutOfRangeException.ThrowIfNegativeOrZero(maxIterations);
			// scout_insanity.mp4
			BigDecimal numerator = One;
			BigDecimal denominator = One;
			BigDecimal result = One;
			BigDecimal lastResult = One;
			exponent.TruncateSelf(HIGH_DIVISION_PRECISION);
			for (uint k = 1; k <= maxIterations; k++) {
				numerator *= exponent;
				denominator *= k;
				result += numerator / denominator;

				numerator.TruncateSelf(HIGH_DIVISION_PRECISION);
				denominator.TruncateSelf(HIGH_DIVISION_PRECISION);
				result.TruncateSelf(HIGH_DIVISION_PRECISION);
				if (result == lastResult) {
					break;
				}
				lastResult = result;
			}
			return result;
		}

		/// <summary>
		/// Raises <paramref name="base"/> to the power of <paramref name="exponent"/> as a <see cref="BigDecimal"/>.
		/// <para/>
		/// If the exponent is an integer, this will automatically swap to <see cref="IntPow(BigDecimal, BigInteger)"/>.
		/// </summary>
		/// <param name="base">The base number to exponentiate.</param>
		/// <param name="exponent">The exponent to raise by.</param>
		/// <param name="maxIterations">Only used if the exponent or base is not an integer. See <see cref="ApproximateNaturalLog(BigDecimal, uint)"/></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal ApproximatePow(BigDecimal @base, BigDecimal exponent, uint maxIterations = STANDARD_MAX_ITERATIONS) {
			ArgumentOutOfRangeException.ThrowIfNegativeOrZero(maxIterations);
			@base.TruncateSelf(HIGH_DIVISION_PRECISION);
			exponent.TruncateSelf(HIGH_DIVISION_PRECISION);
			if (IsInteger(exponent)) {
				BigDecimal power = IntPow(@base, (ulong)Abs(exponent));
				if (IsPositive(exponent)) {
					return power;
				} else {
					return One / power;
				}
			}
			// exp(ln(x)) = x
			// exp(ln(base^exponent))=base^exponent, THUS...
			// ln(base^exponent) is exponent*ln(base), SO...
			// result=exp(exponent*ln(base))
			BigDecimal ln = ApproximateNaturalLog(@base, maxIterations);
			exponent *= ln;
			return ApproximateExp(exponent);
		}


		/// <inheritdoc cref="ApproximatePow(BigDecimal, BigDecimal, uint, uint, int)"/>
		static BigDecimal IPowerFunctions<BigDecimal>.Pow(BigDecimal x, BigDecimal y) => ApproximatePow(x, y);

		/// <summary>
		/// Computes ln(x) based on a fast-converging algorithm.
		/// </summary>
		/// <param name="x"></param>
		/// <param name="iterations">The amount of times to expand the square root out.</param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal ApproximateNaturalLog(BigDecimal x, uint iterations = STANDARD_MAX_ITERATIONS) {
			ArgumentOutOfRangeException.ThrowIfNegative(x);
			if (IsZero(x)) return Zero; // ln 1 = 0
			if (x == E) return One;     // ln e = 1

			// This function converges very rapidly, but only when 0.02<=x<=1.5 (or so) (x is the original, not adjusted --)
			int originalExponent = x.Exponent;
			if (x > new BigDecimal(15, -1)) {
				int digitCount = NumberOfDigits(x.Mantissa);
				x.Exponent = -digitCount;
			} else if (x < new BigDecimal(5, -2)) {
				x.Exponent++;
			}

			x--;
			x.TruncateSelf(HIGH_DIVISION_PRECISION);
			BigDecimal xExpo = x;
			BigDecimal result = x;
			BigDecimal lastResult = x;
			BigDecimal den = One;

			xExpo *= x;
			den++;
			result -= xExpo / den;
			for (uint i = 0; i < iterations; i++) {
				xExpo *= x;
				den++;
				result += xExpo / den;
				
				xExpo *= x;
				den++;
				result -= xExpo / den;

				result.TruncateSelf(HIGH_DIVISION_PRECISION);
				xExpo.TruncateSelf(HIGH_DIVISION_PRECISION);
				if (result == lastResult) {
					break;
				}
				lastResult = result;
			}

			while (originalExponent < x.Exponent) {
				originalExponent++;
				result -= Ln10;
			}

			while (originalExponent > x.Exponent) {
				originalExponent--;
				result += Ln10;
			}

			return result;
		}

		/// <summary>
		/// Returns <c>n!</c>
		/// <para/>
		/// <strong>Do not use this in a loop! Instead, keep track of the value yourself otherwise you waste a lot of iterations repeating math you have already done.</strong>
		/// </summary>
		/// <param name="n"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigInteger Factorial(BigInteger n) {
			ArgumentOutOfRangeException.ThrowIfNegative(n);
			if (n == 0) return 1;
			BigInteger c = n - 1;
			while (c > One) {
				n *= c--;
			}
			return n;
		}

		/// <summary>
		/// Compute the falling factorial of the provided value. (x)n-1
		/// </summary>
		/// <param name="x"></param>
		/// <param name="n"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal FallingFactorial(BigDecimal x, BigInteger n) {
			ArgumentOutOfRangeException.ThrowIfNegative(n);
			BigDecimal xOrg = x;
			BigDecimal result = x;
			BigDecimal nIdx = One;
			while (n > One) {
				result *= xOrg - nIdx;
				n--;
				nIdx++;
			}
			return result;
		}

		/// <summary>
		/// Returns the absolute value of the provided decimal.
		/// </summary>
		/// <param name="value"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal Abs(BigDecimal value) {
			return value with {
				Mantissa = BigInteger.Abs(value.Mantissa)
			};
		}

		/// <summary>
		/// Slowly evaluates the square root of the provided value.
		/// </summary>
		/// <param name="value">The value to get the square root of.</param>
		/// <param name="maxIterations">The amount of iterations done to approach the result.</param>
		/// <returns></returns>
		/// <exception cref="NotSupportedException">If the input would result in a complex number (negative).</exception>
		/// <exception cref="ArgumentOutOfRangeException"></exception>
		[NoDiscard]
		public static BigDecimal ApproximateSqrt(BigDecimal value, uint maxIterations = STANDARD_MAX_ITERATIONS) {
			if (value < 0) throw new NotSupportedException("Imaginary numbers are not supported.");
			ArgumentOutOfRangeException.ThrowIfNegativeOrZero(maxIterations);

			BigDecimal leastI;
			BigDecimal mostI;

			bool invert = false;
			if (value < One) {
				invert = true;
				value = One / value;
			}
			value.TruncateSelf(HIGH_DIVISION_PRECISION);

			// Some quick checks:
			BigDecimal startingI = 2;
			BigDecimal lastStartingI = One;
			BigDecimal lastStartingI2 = One;
			do {
				BigDecimal test = startingI * startingI;
				test.TruncateSelf(HIGH_DIVISION_PRECISION);

				mostI = startingI;
				if (test < value) {
					startingI = test;
				} else {
					leastI = lastStartingI2;
					break;
				}
				lastStartingI2 = lastStartingI;
				lastStartingI = test;
			} while (true);

			BigDecimal mag;
			BigDecimal halfMag;
			BigDecimal oneQuarter = new BigDecimal(25, -2);
			BigDecimal threeQuarters = new BigDecimal(75, -2);
			BigDecimal lastSqr = default;
			for (uint i = 0; i < maxIterations; i++) {
				mag = mostI - leastI;
				if (mag == 0) break; // Exact match?

				halfMag = mag * OneHalf;

				BigDecimal ofst = leastI + halfMag;
				ofst.TruncateSelf(HIGH_DIVISION_PRECISION);

				BigDecimal sqr = ofst * ofst;
				sqr.TruncateSelf(HIGH_DIVISION_PRECISION);
				if (sqr == value || sqr == lastSqr) {
					break;
				} else if (sqr < value) {
					// Undershot
					if (sqr < value * oneQuarter) {
						// *really* undershot
						leastI += mag * threeQuarters;
					} else {
						leastI += halfMag;
					}
				} else if (sqr > value) {
					// Overshot
					if (sqr > value * 4) {
						// *really* overshot
						mostI -= mag * threeQuarters;
					} else {
						mostI -= halfMag;
					}
				}
				lastSqr = sqr;
				leastI.TruncateSelf(HIGH_DIVISION_PRECISION);
				mostI.TruncateSelf(HIGH_DIVISION_PRECISION);
			}
			mag = mostI - leastI;
			halfMag = mag * OneHalf;
			BigDecimal result = leastI + halfMag;
			if (invert) result = One / result;
			result.TruncateSelf(HIGH_DIVISION_PRECISION);
			return result;
		}

		#endregion

		/// <summary>
		/// The real number, in full precision. This is abysmally slow.
		/// </summary>
		/// <param name="enforceDecimalPlaces">Enforces that there are this many digits after the final decimal digit, optionally truncating if too long, and appending 0s if too short.</param>
		/// <param name="maxStringLength">Limits the length of the resulting string to the provided amount of characters.</param>
		/// <param name="decimal">The character to use for decimals.</param>
		/// <param name="noTruncate">If true, <paramref name="enforceDecimalPlaces"/> will not truncate.</param>
		/// <returns></returns>
		[NoDiscard]
		public readonly string ToStringDetailed(int enforceDecimalPlaces = 0, int maxStringLength = -1, string @decimal = ".", bool noTruncate = true) {
			ArgumentOutOfRangeException.ThrowIfNegative(enforceDecimalPlaces);
			bool isNegative = false;
			if (Mantissa < 0) {
				isNegative = true;
			}
			string mantissa = BigInteger.Abs(Mantissa).ToString();
			StringBuilder sb = new StringBuilder(mantissa);
			// 10^Exponent
			if (Exponent < 0) {
				int position = sb.Length + Exponent;
				if (position >= 0) {
					sb.Insert(position, @decimal);
					if (position == 0) {
						sb.Insert(0, '0');
					}
					int adjLength = mantissa.Length - position;
					if (adjLength < enforceDecimalPlaces) {
						int c = enforceDecimalPlaces - adjLength;
						sb.Append(new string('0', c));
					} else if (adjLength > enforceDecimalPlaces && !noTruncate) {
						sb.Length -= (adjLength - enforceDecimalPlaces);
					}
				} else {
					int zerosAfterDecimal = int.Abs(position);
					StringBuilder resultBuilder = new StringBuilder("0");
					resultBuilder.Append(@decimal);
					if (zerosAfterDecimal > 0) {
						resultBuilder.Append(new string('0', zerosAfterDecimal));
					}
					resultBuilder.Append(sb);
					int totalLength = sb.Length + zerosAfterDecimal;

					if (totalLength < enforceDecimalPlaces) {
						int c = enforceDecimalPlaces - totalLength;
						resultBuilder.Append(new string('0', c));
					} else if (!noTruncate) {
						int r = totalLength - enforceDecimalPlaces;
						resultBuilder.Length -= r;
					}

					sb = resultBuilder;
				}
			} else if (Exponent > 0) {
				sb.Append(new string('0', Exponent));
				if (enforceDecimalPlaces > 0) {
					sb.Append(@decimal);
					sb.Append(new string('0', enforceDecimalPlaces));
				}
			}
			if (isNegative) {
				sb.Insert(0, '-');
			}
			if (maxStringLength > -1 && sb.Length > maxStringLength) {
				sb.Length = maxStringLength;
			}
			return sb.ToString();
		}

		/// <summary>
		/// Returns the value as scientific notation Mantissa E Exponent
		/// </summary>
		/// <returns></returns>
		[NoDiscard]
		public readonly string ToStringScientific() {
			return string.Concat(Mantissa.ToString(), "E", Exponent);
		}

		/// <summary>
		/// Defaults to <see cref="ToStringScientific"/>
		/// </summary>
		/// <returns></returns>
		public override readonly string ToString() => ToStringScientific();

		/// <inheritdoc/>
		[NoDiscard]
		public readonly bool Equals(BigDecimal other) {
			return other.Mantissa.Equals(Mantissa) && other.Exponent == Exponent;
		}

		/// <inheritdoc/>
		[NoDiscard]
		readonly bool IEquatable<BigDecimal>.Equals(BigDecimal other) {
			return other.Mantissa.Equals(Mantissa) && other.Exponent == Exponent;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public readonly override bool Equals(object? obj) {
			return obj is BigDecimal bd && Equals(bd);
		}

		/// <inheritdoc/>
		[NoDiscard]
		public readonly override int GetHashCode() {
			unchecked {
				return Mantissa.GetHashCode() * 397 ^ Exponent;
			}
		}

		/// <inheritdoc/>
		[NoDiscard]
		public readonly int CompareTo(object? obj) {
			if (obj is not BigDecimal) {
				throw new ArgumentException("Invalid object type.", nameof(obj));
			}
			return CompareTo((BigDecimal)obj);
		}

		/// <inheritdoc/>
		[NoDiscard]
		public readonly int CompareTo(BigDecimal other) {
			return this < other ? -1 : this > other ? 1 : 0;
		}

		/// <summary>
		/// Parses the provided string into a <see cref="BigDecimal"/>.
		/// </summary>
		/// <param name="value"></param>
		/// <returns></returns>
		[NoDiscard]
		public static BigDecimal Parse(string value) {
			string decimalPoint = CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator;
			int deci = value.IndexOf(decimalPoint);
			if (deci < 0) {
				return BigInteger.Parse(value);
			}
			BigDecimal whole;
			if (deci == 0) {
				whole = 0;
			} else {
				string upToDecimal = value[..deci];
				whole = BigInteger.Parse(upToDecimal);
			}

			string decString = value[(deci+decimalPoint.Length)..];
			BigDecimal dec = BigInteger.Parse(decString);
			dec.Exponent -= decString.Length;

			BigDecimal result = whole + dec;
			result.NormalizeSelf();
			return result;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsCanonical(BigDecimal value) {
			_ = BigInteger.DivRem(value.Mantissa, 10, out BigInteger remainder);
			return remainder != 0;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsComplexNumber(BigDecimal value) => false;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsEvenInteger(BigDecimal value) {
			if (value.Exponent != 0) return false;
			return value.Mantissa.IsEven;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsFinite(BigDecimal value) => true;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsImaginaryNumber(BigDecimal value) => false;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsInfinity(BigDecimal value) => false;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsInteger(BigDecimal value) => value.Exponent == 0;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsNaN(BigDecimal value) => false;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsNegative(BigDecimal value) => BigInteger.IsNegative(value.Mantissa);

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsNegativeInfinity(BigDecimal value) => false;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsNormal(BigDecimal value) {
			_ = BigInteger.DivRem(value.Mantissa, 10, out BigInteger remainder);
			return remainder != 0;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsOddInteger(BigDecimal value) {
			if (value.Exponent != 0) return false;
			return !value.Mantissa.IsEven;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsPositive(BigDecimal value) => BigInteger.IsPositive(value.Mantissa);

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsPositiveInfinity(BigDecimal value) => false;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsRealNumber(BigDecimal value) => true;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsSubnormal(BigDecimal value) => value.Mantissa.IsZero;

		/// <inheritdoc/>
		[NoDiscard]
		public static bool IsZero(BigDecimal value) => value.Mantissa.IsZero;

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal MaxMagnitude(BigDecimal x, BigDecimal y) {
			if (BigInteger.IsNegative(x.Mantissa)) x = Abs(x);
			if (BigInteger.IsNegative(y.Mantissa)) y = Abs(y);
			if (x > y) return x;
			return y;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal MaxMagnitudeNumber(BigDecimal x, BigDecimal y) {
			if (BigInteger.IsNegative(x.Mantissa)) x = Abs(x);
			if (BigInteger.IsNegative(y.Mantissa)) y = Abs(y);
			if (x > y) return x;
			return y;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal MinMagnitude(BigDecimal x, BigDecimal y) {
			if (BigInteger.IsNegative(x.Mantissa)) x = Abs(x);
			if (BigInteger.IsNegative(y.Mantissa)) y = Abs(y);
			if (x < y) return x;
			return y;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal MinMagnitudeNumber(BigDecimal x, BigDecimal y) {
			if (BigInteger.IsNegative(x.Mantissa)) x = Abs(x);
			if (BigInteger.IsNegative(y.Mantissa)) y = Abs(y);
			if (x < y) return x;
			return y;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal Max(BigDecimal x, BigDecimal y) {
			return x > y ? x : y;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal Min(BigDecimal x, BigDecimal y) {
			return x > y ? y : x;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal Clamp(BigDecimal value, BigDecimal min, BigDecimal max) {
			return Max(min, Min(max, value));
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal Parse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider) {
			string decimalPoint = style.HasFlag(NumberStyles.AllowCurrencySymbol) ? NumberFormatInfo.GetInstance(provider).CurrencyDecimalSeparator : NumberFormatInfo.GetInstance(provider).NumberDecimalSeparator;
			int deci = s.IndexOf(decimalPoint);
			if (deci < 0) {
				return BigInteger.Parse(s);
			}
			BigDecimal whole;
			if (deci == 0) {
				whole = 0;
			} else {
				ReadOnlySpan<char> upToDecimal = s[..deci];
				whole = BigInteger.Parse(upToDecimal);
			}

			ReadOnlySpan<char> decString = s[(deci+decimalPoint.Length)..];
			BigDecimal dec = BigInteger.Parse(decString);
			dec.Exponent -= decString.Length;

			return whole + dec;
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal Parse(string s, NumberStyles style, IFormatProvider? provider) {
			return Parse(s.AsSpan(), style, provider);
		}


		/// <summary>
		/// Format this string using <c>xRy(T)</c> as a format argument to get the full detail, where 
		/// x is the number of decimal places at a minimum and y is the length of the string.
		/// If x is omitted, it is 0. If y is omitted, it is infinite. Typically, y will be omitted.
		/// If T is present (string literal), the string will be truncated if the decimal places is larger than the value of x.
		/// </summary>
		/// <param name="s"></param>
		/// <param name="style"></param>
		/// <param name="provider"></param>
		/// <param name="result"></param>
		/// <returns></returns>
		public static bool TryParse(ReadOnlySpan<char> s, NumberStyles style, IFormatProvider? provider, [MaybeNullWhen(false)] out BigDecimal result) {
			string decimalPoint = style.HasFlag(NumberStyles.AllowCurrencySymbol) ? NumberFormatInfo.GetInstance(provider).CurrencyDecimalSeparator : NumberFormatInfo.GetInstance(provider).NumberDecimalSeparator;
			int deci = s.IndexOf(decimalPoint);
			if (deci < 0) {
				if (BigInteger.TryParse(s, style, provider, out BigInteger iResult)) {
					result = iResult;
					return true;
				}
				result = default;
				return false;
			}
			BigDecimal whole;
			if (deci == 0) {
				whole = 0;
			} else {
				ReadOnlySpan<char> upToDecimal = s[..deci];
				whole = BigInteger.Parse(upToDecimal, style);
			}

			ReadOnlySpan<char> decString = s[(deci+decimalPoint.Length)..];
			if (BigInteger.TryParse(decString, style, provider, out BigInteger iResult2)) {
				BigDecimal dec = iResult2;
				dec.Exponent -= decString.Length;
				result = whole + dec;
				return true;
			}
			result = default;
			return false;
		}


		/// <summary>
		/// Format this string using <c>xRy(T)</c> as a format argument to get the full detail, where 
		/// x is the number of decimal places at a minimum and y is the length of the string.
		/// If x is omitted, it is 0. If y is omitted, it is infinite. Typically, y will be omitted.
		/// If T is present (string literal), the string will be truncated if the decimal places is larger than the value of x.
		/// </summary>
		/// <param name="s"></param>
		/// <param name="style"></param>
		/// <param name="provider"></param>
		/// <param name="result"></param>
		/// <returns></returns>
		public static bool TryParse([NotNullWhen(true)] string? s, NumberStyles style, IFormatProvider? provider, [MaybeNullWhen(false)] out BigDecimal result) {
			return TryParse(s.AsSpan(), style, provider, out result);
		}


		/// <summary>
		/// Format this string using <c>xRy(T)</c> as a format argument to get the full detail, where 
		/// x is the number of decimal places at a minimum and y is the length of the string.
		/// If x is omitted, it is 0. If y is omitted, it is infinite. Typically, y will be omitted.
		/// If T is present (string literal), the string will be truncated if the decimal places is larger than the value of x.
		/// </summary>
		/// <param name="destination"></param>
		/// <param name="charsWritten"></param>
		/// <param name="format"></param>
		/// <param name="provider"></param>
		/// <returns></returns>
		public readonly bool TryFormat(Span<char> destination, out int charsWritten, ReadOnlySpan<char> format, IFormatProvider? provider) {
			string decimalPoint = NumberFormatInfo.GetInstance(provider).NumberDecimalSeparator ?? ".";

			if (!format.IsEmpty) {
				Span<System.Range> substrings = stackalloc System.Range[2];
				int splitLength = format.Split(substrings, "R");
				if (splitLength == 2) {
					ReadOnlySpan<char> enforceDecsS = format[substrings[0]];
					ReadOnlySpan<char> maxLengthS = format[substrings[1]];
					int minDecs = 0;
					int maxLength = -1;

					bool noTruncate = true;
					if (maxLengthS.Length > 0 && maxLengthS[^1] == 'T') {
						maxLengthS = maxLengthS[..(maxLengthS.Length - 1)];
						noTruncate = false;
					}
					if (enforceDecsS.Length > 0 && !int.TryParse(enforceDecsS, out minDecs)) {
						charsWritten = 0;
						return false;
					}
					if (maxLengthS.Length > 0 && !int.TryParse(maxLengthS, out maxLength)) {
						charsWritten = 0;
						return false;
					}
					string result = ToStringDetailed(minDecs, int.Min(destination.Length, maxLength), decimalPoint, noTruncate);
					charsWritten = result.Length;
					result.CopyTo(destination);
					return true;
				}
			} else {
				string result = ToString();
				if (result.Length > destination.Length) {
					result = result[..destination.Length];
				}
				charsWritten = result.Length;
				result.CopyTo(destination);
			}
			charsWritten = 0;
			return false;
		}

		/// <summary>
		/// Format this string using <c>xRy(T)</c> as a format argument to get the full detail, where 
		/// x is the number of decimal places at a minimum and y is the maximum length of the string.
		/// <para/>
		/// If x is omitted, it is 0. If y is omitted, it is infinite. Typically, y will be omitted.
		/// If T is present (string literal), the string will be truncated if the decimal places is larger than the value of x.
		/// <para/>
		/// Examples:
		/// 4R10 = Display no fewer than four numbers after the decimal point. More is allowed. If the string is longer than 10 characters, however, trim it to 10 characters.<br/>
		/// R5 = Display no more than five characters.<br/>
		/// 2R = Display no fewer than two numbers after the decimal point. More is allowed.<br/>
		/// 2RT = Display exactly two numbers after the decimal point.
		/// </summary>
		/// <param name="format"></param>
		/// <param name="formatProvider"></param>
		/// <returns></returns>
		[NoDiscard]
		public readonly string ToString(string? format, IFormatProvider? formatProvider = null) {
			const string MESSAGE = "For more information on the format, see the documentation of this method. If the documentation is not present, please check the install for [The Conservatory.xml]. If this is missing, please contact the developer.";
			string decimalPoint = NumberFormatInfo.GetInstance(formatProvider).NumberDecimalSeparator ?? ".";

			if (format != null) {
				string[] split = format.Split('R');
				if (split.Length == 2) {
					string enforceDecsS = split[0];
					string maxLengthS = split[1];
					bool noTruncate = true;
					if (maxLengthS.Length > 0 && maxLengthS[^1] == 'T') {
						maxLengthS = maxLengthS[..(maxLengthS.Length - 1)];
						noTruncate = false;
					}
					int minDecs = 0;
					int maxLength = -1;
					if (enforceDecsS.Length > 0 && !int.TryParse(enforceDecsS, out minDecs)) {
						throw new FormatException(MESSAGE);
					}
					if (maxLengthS.Length > 0 && !int.TryParse(maxLengthS, out maxLength)) {
						throw new FormatException(MESSAGE);
					}
					return ToStringDetailed(minDecs, maxLength, decimalPoint, noTruncate);
				} else {
					throw new FormatException(MESSAGE);
				}
			} else {
				return ToString();
			}
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal Parse(ReadOnlySpan<char> s, IFormatProvider? provider) {
			return Parse(s, NumberStyles.None, provider);
		}

		/// <inheritdoc/>
		public static bool TryParse(ReadOnlySpan<char> s, IFormatProvider? provider, [MaybeNullWhen(false)] out BigDecimal result) {
			return TryParse(s, NumberStyles.None, provider, out result);
		}

		/// <inheritdoc/>
		[NoDiscard]
		public static BigDecimal Parse(string s, IFormatProvider? provider) {
			return Parse(s, NumberStyles.None, provider);
		}

		/// <inheritdoc/>
		public static bool TryParse([NotNullWhen(true)] string? s, IFormatProvider? provider, [MaybeNullWhen(false)] out BigDecimal result) {
			return TryParse(s, NumberStyles.None, provider, out result);
		}

		/// <inheritdoc/>
		static bool INumberBase<BigDecimal>.TryConvertFromChecked<TOther>(TOther value, out BigDecimal result) {
			if (value is BigInteger bigInt) {
				result = bigInt;
				return true;
			} else if (value is BigDecimal bigDecimal) {
				result = bigDecimal;
				return true;
			} else if (value is sbyte s8) {
				result = s8;
				return true;
			} else if (value is byte u8) {
				result = u8;
				return true;
			} else if (value is short s16) {
				result = s16;
				return true;
			} else if (value is ushort u16) {
				result = u16;
				return true;
			} else if (value is int s32) {
				result = s32;
				return true;
			} else if (value is uint u32) {
				result = u32;
				return true;
			} else if (value is long s64) {
				result = s64;
				return true;
			} else if (value is ulong u64) {
				result = u64;
				return true;
			} else if (value is Int128 s128) {
				result = s128;
				return true;
			} else if (value is UInt128 u128) {
				result = u128;
				return true;
			} else if (value is Half f16) {
				result = f16;
				return true;
			} else if (value is float f32) {
				result = f32;
				return true;
			} else if (value is double f64) {
				result = f64;
				return true;
			} else if (value is decimal f128) {
				result = f128;
				return true;
			}
			result = default;
			return false;
		}

		/// <inheritdoc/>
		static bool INumberBase<BigDecimal>.TryConvertFromSaturating<TOther>(TOther value, out BigDecimal result) {
			if (value is BigInteger bigInt) {
				result = bigInt;
				return true;
			} else if (value is BigDecimal bigDecimal) {
				result = bigDecimal;
				return true;
			} else if (value is sbyte s8) {
				result = s8;
				return true;
			} else if (value is byte u8) {
				result = u8;
				return true;
			} else if (value is short s16) {
				result = s16;
				return true;
			} else if (value is ushort u16) {
				result = u16;
				return true;
			} else if (value is int s32) {
				result = s32;
				return true;
			} else if (value is uint u32) {
				result = u32;
				return true;
			} else if (value is long s64) {
				result = s64;
				return true;
			} else if (value is ulong u64) {
				result = u64;
				return true;
			} else if (value is Int128 s128) {
				result = s128;
				return true;
			} else if (value is UInt128 u128) {
				result = u128;
				return true;
			} else if (value is Half f16) {
				result = f16;
				return true;
			} else if (value is float f32) {
				result = f32;
				return true;
			} else if (value is double f64) {
				result = f64;
				return true;
			} else if (value is decimal f128) {
				result = f128;
				return true;
			}
			result = default;
			return false;
		}

		/// <inheritdoc/>
		static bool INumberBase<BigDecimal>.TryConvertFromTruncating<TOther>(TOther value, out BigDecimal result) {
			if (value is BigInteger bigInt) {
				result = bigInt;
				return true;
			} else if (value is BigDecimal bigDecimal) {
				result = bigDecimal;
				return true;
			} else if (value is sbyte s8) {
				result = s8;
				return true;
			} else if (value is byte u8) {
				result = u8;
				return true;
			} else if (value is short s16) {
				result = s16;
				return true;
			} else if (value is ushort u16) {
				result = u16;
				return true;
			} else if (value is int s32) {
				result = s32;
				return true;
			} else if (value is uint u32) {
				result = u32;
				return true;
			} else if (value is long s64) {
				result = s64;
				return true;
			} else if (value is ulong u64) {
				result = u64;
				return true;
			} else if (value is Int128 s128) {
				result = s128;
				return true;
			} else if (value is UInt128 u128) {
				result = u128;
				return true;
			} else if (value is Half f16) {
				result = f16;
				return true;
			} else if (value is float f32) {
				result = f32;
				return true;
			} else if (value is double f64) {
				result = f64;
				return true;
			} else if (value is decimal f128) {
				result = f128;
				return true;
			}
			result = default;
			return false;
		}

		/// <inheritdoc/>
		static bool INumberBase<BigDecimal>.TryConvertToChecked<TOther>(BigDecimal value, out TOther result) {
			result = TOther.AdditiveIdentity;
			if (result is BigInteger) {
				result = (TOther)(object)Round(value).Mantissa;
				return true;
			} else if (result is BigDecimal) {
				result = (TOther)(object)value;
				return true;
			} else if (result is sbyte) {
				return IntCast<TOther, sbyte, BigInteger>(in value, 0, out result);
			} else if (result is byte) {
				return IntCast<TOther, byte, BigInteger>(in value, 0, out result);
			} else if (result is short) {
				return IntCast<TOther, short, BigInteger>(in value, 0, out result);
			} else if (result is ushort) {
				return IntCast<TOther, ushort, BigInteger>(in value, 0, out result);
			} else if (result is int) {
				return IntCast<TOther, int, BigInteger>(in value, 0, out result);
			} else if (result is uint) {
				return IntCast<TOther, uint, BigInteger>(in value, 0, out result);
			} else if (result is long) {
				return IntCast<TOther, long, BigInteger>(in value, 0, out result);
			} else if (result is ulong) {
				return IntCast<TOther, ulong, BigInteger>(in value, 0, out result);
			} else if (result is Int128) {
				return IntCast<TOther, Int128, BigInteger>(in value, 0, out result);
			} else if (result is UInt128) {
				return IntCast<TOther, UInt128, BigInteger>(in value, 0, out result);
			} else if (result is Half) {
				if (value > Half.MaxValue || value < Half.MinValue) throw new OverflowException();
				result = (TOther)(object)(Half)value;
				return true;
			} else if (result is float) {
				if (value > float.MaxValue || value < float.MinValue) throw new OverflowException();
				result = (TOther)(object)(float)value;
				return true;
			} else if (result is double) {
				if (value > double.MaxValue || value < double.MinValue) throw new OverflowException();
				result = (TOther)(object)(double)value;
				return true;
			} else if (result is decimal) {
				if (value > decimal.MaxValue || value < decimal.MinValue) throw new OverflowException();
				result = (TOther)(object)(decimal)value;
				return true;
			}
			result = default!;
			return false;
		}

		/// <inheritdoc/>
		static bool INumberBase<BigDecimal>.TryConvertToSaturating<TOther>(BigDecimal value, out TOther result) {
			result = TOther.AdditiveIdentity;
			if (result is BigInteger) {
				result = (TOther)(object)Round(value).Mantissa;
				return true;
			} else if (result is BigDecimal) {
				result = (TOther)(object)value;
				return true;
			} else if (result is sbyte) {
				return IntCast<TOther, sbyte, BigInteger>(in value, 1, out result);
			} else if (result is byte) {
				return IntCast<TOther, byte, BigInteger>(in value, 1, out result);
			} else if (result is short) {
				return IntCast<TOther, short, BigInteger>(in value, 1, out result);
			} else if (result is ushort) {
				return IntCast<TOther, ushort, BigInteger>(in value, 1, out result);
			} else if (result is int) {
				return IntCast<TOther, int, BigInteger>(in value, 1, out result);
			} else if (result is uint) {
				return IntCast<TOther, uint, BigInteger>(in value, 1, out result);
			} else if (result is long) {
				return IntCast<TOther, long, BigInteger>(in value, 1, out result);
			} else if (result is ulong) {
				return IntCast<TOther, ulong, BigInteger>(in value, 1, out result);
			} else if (result is Int128) {
				return IntCast<TOther, Int128, BigInteger>(in value, 1, out result);
			} else if (result is UInt128) {
				return IntCast<TOther, UInt128, BigInteger>(in value, 1, out result);
			} else if (result is Half) {
				if (value > Half.MaxValue) value = Half.PositiveInfinity;
				if (value < Half.MinValue) value = Half.NegativeInfinity;
				result = (TOther)(object)(Half)value;
				return true;
			} else if (result is float) {
				if (value > float.MaxValue) value = float.PositiveInfinity;
				if (value < float.MinValue) value = float.NegativeInfinity;
				result = (TOther)(object)(float)value;
				return true;
			} else if (result is double) {
				if (value > double.MaxValue) value = double.PositiveInfinity;
				if (value < double.MinValue) value = double.NegativeInfinity;
				result = (TOther)(object)(double)value;
				return true;
			} else if (result is decimal) {
				if (value > decimal.MaxValue) value = decimal.MaxValue;
				if (value < decimal.MinValue) value = decimal.MinValue;
				result = (TOther)(object)(decimal)value;
				return true;
			}
			result = default!;
			return false;
		}

		/// <inheritdoc/>
		static bool INumberBase<BigDecimal>.TryConvertToTruncating<TOther>(BigDecimal value, out TOther result) {
			result = TOther.AdditiveIdentity;
			if (result is BigInteger) {
				result = (TOther)(object)Round(value).Mantissa;
				return true;
			} else if (result is BigDecimal) {
				result = (TOther)(object)value;
				return true;
			} else if (result is sbyte) {
				return IntCast<TOther, sbyte, BigInteger>(in value, 2, out result);
			} else if (result is byte) {
				return IntCast<TOther, byte, BigInteger>(in value, 2, out result);
			} else if (result is short) {
				return IntCast<TOther, short, BigInteger>(in value, 2, out result);
			} else if (result is ushort) {
				return IntCast<TOther, ushort, BigInteger>(in value, 2, out result);
			} else if (result is int) {
				return IntCast<TOther, int, BigInteger>(in value, 2, out result);
			} else if (result is uint) {
				return IntCast<TOther, uint, BigInteger>(in value, 2, out result);
			} else if (result is long) {
				return IntCast<TOther, long, BigInteger>(in value, 2, out result);
			} else if (result is ulong) {
				return IntCast<TOther, ulong, BigInteger>(in value, 2, out result);
			} else if (result is Int128) {
				return IntCast<TOther, Int128, BigInteger>(in value, 2, out result);
			} else if (result is UInt128) {
				return IntCast<TOther, UInt128, BigInteger>(in value, 2, out result);
			} else if (result is Half) {
				if (value > Half.MaxValue) value = Half.MaxValue;
				if (value < Half.MinValue) value = Half.MinValue;
				result = (TOther)(object)(Half)value;
				return true;
			} else if (result is float) {
				if (value > float.MaxValue) value = float.MaxValue;
				if (value < float.MinValue) value = float.MinValue;
				result = (TOther)(object)(float)value;
				return true;
			} else if (result is double) {
				if (value > double.MaxValue) value = double.MaxValue;
				if (value < double.MinValue) value = double.MinValue;
				result = (TOther)(object)(double)value;
				return true;
			} else if (result is decimal) {
				if (value > decimal.MaxValue) value = decimal.MaxValue;
				if (value < decimal.MinValue) value = decimal.MinValue;
				result = (TOther)(object)(decimal)value;
				return true;
			}
			result = default!;
			return false;
		}

		private static bool IntCast<TOther, TInteger, TBigInteger>(in BigDecimal value, byte checkType, out TOther result)
			where TOther : INumberBase<TOther>
			where TInteger : INumberBase<TInteger>, IMinMaxValue<TInteger>
			where TBigInteger : INumberBase<TBigInteger> // Must always be BigInteger
		{
			BigInteger truncated = FloorToInt(value);
			if (checkType == 0) {
				BigInteger min = BigInteger.CreateChecked(TInteger.MinValue);
				BigInteger max = BigInteger.CreateChecked(TInteger.MaxValue);
				if (truncated < min || truncated > max) throw new OverflowException();
				return TBigInteger.TryConvertToChecked((TBigInteger)(object)truncated, out result!);
			} else if (checkType == 1) {
				BigInteger min = BigInteger.CreateSaturating(TInteger.MinValue);
				BigInteger max = BigInteger.CreateSaturating(TInteger.MaxValue);
				if (truncated < min) {
					result = (TOther)(object)TInteger.MinValue;
					return true;
				} else if (truncated > max) {
					result = (TOther)(object)TInteger.MaxValue;
					return true;
				}
				return TBigInteger.TryConvertToChecked((TBigInteger)(object)truncated, out result!);
			} else if (checkType == 2) {
				BigInteger min = BigInteger.CreateTruncating(TInteger.MinValue);
				BigInteger max = BigInteger.CreateTruncating(TInteger.MaxValue);
				if (truncated < min) {
					result = (TOther)(object)TInteger.MinValue;
					return true;
				} else if (truncated > max) {
					result = (TOther)(object)TInteger.MaxValue;
					return true;
				}
				return TBigInteger.TryConvertToChecked((TBigInteger)(object)truncated, out result!);
			}
			throw new ArgumentOutOfRangeException(nameof(checkType));
		}


		static BigDecimal() {
			BigDecimal e = 1;
			BigDecimal factorialI = 1;
			for (int i = 1; i < 100; i++) {
				factorialI *= i;
				e += One / factorialI;
			}
			E = e;

			// Way easier than implementing the algorithm myself.
			Pi = Parse("3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989");
			InversePi = One / Pi;
			Tau = Pi * 2;

			BigDecimal ln_two = -ApproximateNaturalLog(OneHalf); // ln(x/y) = ln(x) - ln(y). 1/0.5 => ln(1) - ln(0.5) => 0 - ln(0.5)
			BigDecimal ln_oneAndQuarter = ApproximateNaturalLog(XPointY(1, 25));
			BigDecimal ln_twoAndHalf = ln_two + ln_oneAndQuarter;
			BigDecimal ln_five = ln_two + ln_twoAndHalf;
			BigDecimal ln_ten = ln_two + ln_five;
			Ln2 = ln_two;
			Ln10 = ln_ten;
			Ln100 = ln_ten + ln_ten;

			HalfMinValue = Half.MinValue;
			HalfMaxValue = Half.MaxValue;
			HalfEpsilonValue = Half.Epsilon;

			FloatMinValue = float.MinValue;
			FloatMaxValue = float.MaxValue;
			FloatEpsilonValue = float.Epsilon;

			DoubleMinValue = double.MinValue;
			DoubleMaxValue = double.MaxValue;
			DoubleEpsilonValue = double.Epsilon;
		}
	}
}
