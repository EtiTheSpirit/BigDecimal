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

namespace Star3D.Maths.Numbers {

	/// <summary>
	/// Represents a decimal value with arbitrary position. This can be used for near-perfect
	/// accuracy computations.
	/// <para/>
	/// <strong>DANGER:</strong> This type is <strong>extremely slow compared to ordinary numeric types</strong>. Handle with care.
	/// <para/>
	/// Originally written by <c>Jan Christoph Bernack</c> for the public domain, and upgraded
	/// by Xan for more specific usage in Starlike3D.
	/// </summary>
	public struct BigDecimal :
		INumber<BigDecimal>,
		IAdditionOperators<BigDecimal, BigDecimal, BigDecimal>,
		IAdditiveIdentity<BigDecimal, BigDecimal>,
		IIncrementOperators<BigDecimal>,
		IDecrementOperators<BigDecimal>,
		IDivisionOperators<BigDecimal, BigDecimal, BigDecimal>,
		IEquatable<BigDecimal>,
		IComparable,
		IComparable<BigDecimal>,
		IEqualityOperators<BigDecimal, BigDecimal, bool>,
		IMultiplicativeIdentity<BigDecimal, BigDecimal>,
		IMultiplyOperators<BigDecimal, BigDecimal, BigDecimal>,
		ISubtractionOperators<BigDecimal, BigDecimal, BigDecimal>,
		IUnaryPlusOperators<BigDecimal, BigDecimal>,
		IUnaryNegationOperators<BigDecimal, BigDecimal>,
		IModulusOperators<BigDecimal, BigDecimal, BigDecimal>,
		IComparisonOperators<BigDecimal, BigDecimal, bool> 
	{

		/// <summary>
		/// The maximum precision of division operations, measured as 10^DIVISION_PRECISION. 
		/// This protects against irrational numbers being infinitely long.
		/// </summary>
		public const int DIVISION_PRECISION = 80;

		/// <inheritdoc/>
		public static BigDecimal One { get; } = new BigDecimal(1, 0);

		/// <inheritdoc/>
		public static int Radix { get; } = 10;

		/// <inheritdoc/>
		public static BigDecimal Zero { get; } = default;

		/// <summary>
		/// The mathematical constant <see langword="e"/>, computed up to <c>50!</c> (50 iterations).
		/// </summary>
		public static BigDecimal E { get; }

		/// <summary>
		/// The mathematical constant <see langword="pi"/>, computed up to 100 decimal places.
		/// </summary>
		public static BigDecimal Pi { get; }

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


		/// <inheritdoc/>
		public static BigDecimal AdditiveIdentity => Zero;

		/// <inheritdoc/>
		public static BigDecimal MultiplicativeIdentity => One;

		/// <summary>
		/// The value representing -1.
		/// </summary>
		public static BigDecimal NegativeOne { get; } = new BigDecimal(-1, 0);

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
		/// Separates the whole number from its decimal part. XPointY(7, 125) would be 7.125
		/// </summary>
		/// <param name="x"></param>
		/// <param name="y"></param>
		/// <returns></returns>
		public static BigDecimal XPointY(BigInteger x, BigInteger y) {
			int nDigits = NumberOfDigits(y);
			BigDecimal frac = new BigDecimal(y, -nDigits);
			return x + frac;
		}

		/// <summary>
		/// Separates the whole number from its decimal part. XPointY(7, 125) would be 7.125
		/// </summary>
		/// <param name="x"></param>
		/// <param name="y"></param>
		/// <param name="exponent"></param>
		/// <returns></returns>
		public static BigDecimal XPointY(BigInteger x, BigInteger y, int exponent = 0) {
			int nDigits = NumberOfDigits(y);
			BigDecimal frac = new BigDecimal(y, -nDigits);
			BigDecimal result = x + frac;
			result.Exponent += exponent;
			return result;
		}

		/// <summary>
		/// Removes trailing zeros on the mantissa of this instance.
		/// </summary>
		[DebuggerStepThrough]
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
		public static BigDecimal Truncate(BigDecimal value, int precision) {
			// Self-reminder that structs are copied by value, so <value> here is a new instance because it is not an in parameter
			value.TruncateSelf(precision);
			return value;
		}

		/// <summary>
		/// Truncate this value
		/// </summary>
		/// <param name="precision"></param>
		/// <returns></returns>
		private void TruncateSelf(int precision) {
			// save some time because the number of digits is not needed to remove trailing zeros
			NormalizeSelf();
			// remove the least significant digits, as long as the number of digits is higher than the given Precision
			while (NumberOfDigits(Mantissa) > precision) {
				Mantissa /= 10;
				Exponent++;
			}
			// normalize again to make sure there are no trailing zeros left
			NormalizeSelf();
		}

		/// <summary>
		/// Truncate the number so that up to the provided amount of digits after the decimal are shown.
		/// If the number has less than the provided amount, it will <strong>not</strong> add additional zeros.
		/// </summary>
		/// <returns>The truncated number</returns>
		[DebuggerStepThrough]
		public static BigDecimal TruncateToPlaces(BigDecimal value, int decimalDigits) {
			return Truncate(value, NumberOfDigits(value.Mantissa) + value.Exponent + decimalDigits);
		}

		/// <summary>
		/// Cuts the length of the value down to <see cref="DIVISION_PRECISION"/>.
		/// </summary>
		/// <returns></returns>
		[DebuggerStepThrough]
		public static BigDecimal Truncate(BigDecimal value) {
			return Truncate(value, DIVISION_PRECISION);
		}

		/// <summary>
		/// Removes the decimal component of the value.
		/// </summary>
		/// <returns></returns>
		[DebuggerStepThrough]
		public static BigDecimal Floor(BigDecimal value) {
			return Truncate(value, NumberOfDigits(value.Mantissa) + value.Exponent);
		}

		/// <summary>
		/// Removes the decimal component of the value.
		/// </summary>
		/// <returns></returns>
		[DebuggerStepThrough]
		public static BigInteger FloorToInt(BigDecimal value) {
			return Truncate(value, NumberOfDigits(value.Mantissa) + value.Exponent).Mantissa;
		}

		/// <summary>
		/// Rounds up, away from zero.
		/// </summary>
		/// <returns></returns>
		[DebuggerStepThrough]
		public static BigDecimal Ceil(BigDecimal value) {
			BigDecimal ofst = value < 0 ? -1 : 1;
			return Floor(value + ofst);
		}

		/// <summary>
		/// Rounds value away from zero.
		/// </summary>
		/// <param name="value"></param>
		/// <returns></returns>
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
			// Returns a binary representation of a Decimal. The return value is an
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
			value = Truncate(value, 15);
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
		public static explicit operator int(BigDecimal value) {
			return (int)(value.Mantissa * BigInteger.Pow(10, value.Exponent));
		}

		/// <inheritdoc/>
		public static explicit operator uint(BigDecimal value) {
			return (uint)(value.Mantissa * BigInteger.Pow(10, value.Exponent));
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
			var exponentChange = DIVISION_PRECISION - (NumberOfDigits(dividend.Mantissa) - NumberOfDigits(divisor.Mantissa));
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
		/// Raises <c>e</c> to the power of <paramref name="exponent"/>.
		/// </summary>
		/// <param name="exponent"></param>
		/// <returns></returns>
		public static BigDecimal Exp(double exponent) {
			var tmp = (BigDecimal)1;
			while (Math.Abs(exponent) > 100) {
				var diff = exponent > 0 ? 100 : -100;
				tmp *= Math.Exp(diff);
				exponent -= diff;
			}
			return tmp * Math.Exp(exponent);
		}

		/// <summary>
		/// Raises <paramref name="basis"/> to the power of <paramref name="exponent"/> as a <see cref="BigDecimal"/>.
		/// </summary>
		/// <param name="basis"></param>
		/// <param name="exponent"></param>
		/// <returns></returns>
		public static BigDecimal Pow(double basis, double exponent) {
			var tmp = (BigDecimal)1;
			while (Math.Abs(exponent) > 100) {
				var diff = exponent > 0 ? 100 : -100;
				tmp *= Math.Pow(basis, diff);
				exponent -= diff;
			}
			return tmp * Math.Pow(basis, exponent);
		}

		/// <summary>
		/// Returns <c>n!</c>
		/// </summary>
		/// <param name="n"></param>
		/// <returns></returns>
		public static BigInteger Factorial(BigInteger n) {
			ArgumentOutOfRangeException.ThrowIfNegative(n);
			if (n == 0) return 1;
			BigInteger c = n - 1;
			while (c > 1) {
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
		public static BigDecimal FallingFactorial(BigDecimal x, BigInteger n) {
			ArgumentOutOfRangeException.ThrowIfNegative(n);
			BigDecimal xOrg = x;
			BigDecimal result = x;
			BigDecimal nIdx = 1;
			while (n > 1) {
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
		public static BigDecimal Abs(BigDecimal value) {
			if (value < 0) return value * -1;
			return value;
		}

		/// <summary>
		/// Slowly evaluates the square root of the provided value.
		/// </summary>
		/// <param name="value">The value to get the square root of.</param>
		/// <param name="maxIterations">The amount of iterations done to approach the result.</param>
		/// <param name="errorMargin">The tested square can be within this amount of <paramref name="value"/> to return early.</param>
		/// <returns></returns>
		/// <exception cref="NotSupportedException"></exception>
		public static BigDecimal ApproximateSqrt(BigDecimal value, uint maxIterations = 1000, BigDecimal errorMargin = default) {
			if (value < 0) throw new NotSupportedException("Imaginary numbers are not supported.");
			ArgumentOutOfRangeException.ThrowIfNegativeOrZero(maxIterations);
			ArgumentOutOfRangeException.ThrowIfNegative(errorMargin);
			BigDecimal leastI;
			BigDecimal mostI;

			bool invert = false;
			if (value < 1) {
				invert = true;
				value = 1 / value;
			}

			// Some quick checks:
			BigDecimal startingI = 2;
			BigDecimal lastStartingI = 1;
			BigDecimal lastStartingI2 = 1;
			do {
				BigDecimal test = startingI * startingI;
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
			for (uint i = 0; i < maxIterations; i++) {
				mag = mostI - leastI;
				if (mag == 0) break; // Exact match?

				halfMag = mag / 2;
				BigDecimal ofst = leastI + halfMag;
				BigDecimal sqr = ofst * ofst;
				if (sqr < value) {
					// Undershot
					leastI += halfMag;
				} else if (sqr > value) {
					// Overshot
					mostI -= halfMag;
				} else if (sqr == value || (errorMargin != 0 && Abs(sqr - value) < errorMargin)) {
					break;
				}
			}
			mag = mostI - leastI;
			halfMag = mag / 2;
			BigDecimal result = leastI + halfMag;
			if (invert) result = 1 / result;
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
		public readonly string ToStringScientific() {
			return string.Concat(Mantissa.ToString(), "E", Exponent);
		}

		/// <summary>
		/// Defaults to <see cref="ToStringScientific"/>
		/// </summary>
		/// <returns></returns>
		public override readonly string ToString() => ToStringScientific();

		/// <inheritdoc/>
		public readonly bool Equals(BigDecimal other) {
			return other.Mantissa.Equals(Mantissa) && other.Exponent == Exponent;
		}

		/// <inheritdoc/>
		readonly bool IEquatable<BigDecimal>.Equals(BigDecimal other) {
			return other.Mantissa.Equals(Mantissa) && other.Exponent == Exponent;
		}

		/// <inheritdoc/>
		public readonly override bool Equals(object? obj) {
			return obj is BigDecimal bd && Equals(bd);
		}

		/// <inheritdoc/>
		public readonly override int GetHashCode() {
			unchecked {
				return Mantissa.GetHashCode() * 397 ^ Exponent;
			}
		}

		/// <inheritdoc/>
		public readonly int CompareTo(object? obj) {
			if (obj is not BigDecimal) {
				throw new ArgumentException("Invalid object type.", nameof(obj));
			}
			return CompareTo((BigDecimal)obj);
		}

		/// <inheritdoc/>
		public readonly int CompareTo(BigDecimal other) {
			return this < other ? -1 : this > other ? 1 : 0;
		}

		/// <summary>
		/// Parses the provided string into a <see cref="BigDecimal"/>.
		/// </summary>
		/// <param name="value"></param>
		/// <returns></returns>
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
		public static bool IsCanonical(BigDecimal value) {
			_ = BigInteger.DivRem(value.Mantissa, 10, out BigInteger remainder);
			return remainder != 0;
		}

		/// <inheritdoc/>
		public static bool IsComplexNumber(BigDecimal value) => false;

		/// <inheritdoc/>
		public static bool IsEvenInteger(BigDecimal value) {
			if (value.Exponent != 0) return false;
			return value.Mantissa.IsEven;
		}

		/// <inheritdoc/>
		public static bool IsFinite(BigDecimal value) => true;

		/// <inheritdoc/>
		public static bool IsImaginaryNumber(BigDecimal value) => false;

		/// <inheritdoc/>
		public static bool IsInfinity(BigDecimal value) => false;

		/// <inheritdoc/>
		public static bool IsInteger(BigDecimal value) => value.Exponent == 0;

		/// <inheritdoc/>
		public static bool IsNaN(BigDecimal value) => false;

		/// <inheritdoc/>
		public static bool IsNegative(BigDecimal value) => BigInteger.IsNegative(value.Mantissa);

		/// <inheritdoc/>
		public static bool IsNegativeInfinity(BigDecimal value) => false;

		/// <inheritdoc/>
		public static bool IsNormal(BigDecimal value) {
			_ = BigInteger.DivRem(value.Mantissa, 10, out BigInteger remainder);
			return remainder != 0;
		}

		/// <inheritdoc/>
		public static bool IsOddInteger(BigDecimal value) {
			if (value.Exponent != 0) return false;
			return !value.Mantissa.IsEven;
		}

		/// <inheritdoc/>
		public static bool IsPositive(BigDecimal value) => BigInteger.IsPositive(value.Mantissa);

		/// <inheritdoc/>
		public static bool IsPositiveInfinity(BigDecimal value) => false;

		/// <inheritdoc/>
		public static bool IsRealNumber(BigDecimal value) => true;

		/// <inheritdoc/>
		public static bool IsSubnormal(BigDecimal value) => value.Mantissa.IsZero;

		/// <inheritdoc/>
		public static bool IsZero(BigDecimal value) => value.Mantissa.IsZero;

		/// <inheritdoc/>
		public static BigDecimal MaxMagnitude(BigDecimal x, BigDecimal y) {
			if (BigInteger.IsNegative(x.Mantissa)) x = Abs(x);
			if (BigInteger.IsNegative(y.Mantissa)) y = Abs(y);
			if (x > y) return x;
			return y;
		}

		/// <inheritdoc/>
		public static BigDecimal MaxMagnitudeNumber(BigDecimal x, BigDecimal y) {
			if (BigInteger.IsNegative(x.Mantissa)) x = Abs(x);
			if (BigInteger.IsNegative(y.Mantissa)) y = Abs(y);
			if (x > y) return x;
			return y;
		}

		/// <inheritdoc/>
		public static BigDecimal MinMagnitude(BigDecimal x, BigDecimal y) {
			if (BigInteger.IsNegative(x.Mantissa)) x = Abs(x);
			if (BigInteger.IsNegative(y.Mantissa)) y = Abs(y);
			if (x < y) return x;
			return y;
		}

		/// <inheritdoc/>
		public static BigDecimal MinMagnitudeNumber(BigDecimal x, BigDecimal y) {
			if (BigInteger.IsNegative(x.Mantissa)) x = Abs(x);
			if (BigInteger.IsNegative(y.Mantissa)) y = Abs(y);
			if (x < y) return x;
			return y;
		}

		/// <inheritdoc/>
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
		public static BigDecimal Parse(string s, NumberStyles style, IFormatProvider? provider) {
			return Parse(s.AsSpan(), style, provider);
		}

		/// <inheritdoc/>
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

		/// <inheritdoc/>
		public static bool TryParse([NotNullWhen(true)] string? s, NumberStyles style, IFormatProvider? provider, [MaybeNullWhen(false)] out BigDecimal result) {
			return TryParse(s.AsSpan(), style, provider, out result);
		}

		/// <inheritdoc/>
		public bool TryFormat(Span<char> destination, out int charsWritten, ReadOnlySpan<char> format, IFormatProvider? provider) {
			string decimalPoint = NumberFormatInfo.GetInstance(provider).NumberDecimalSeparator ?? ".";

			if (format != null) {
				Span<System.Range> substrings = stackalloc System.Range[2];
				int splitLength = format.Split(substrings, "R");
				if (splitLength == 2) {
					ReadOnlySpan<char> enforceDecsS = format[substrings[0]];
					ReadOnlySpan<char> maxLengthS = format[substrings[1]];
					int minDecs = 0;
					int maxLength = -1;
					if (enforceDecsS.Length > 0 && !int.TryParse(enforceDecsS, out minDecs)) {
						charsWritten = 0;
						return false;
					}
					if (maxLengthS.Length > 0 && !int.TryParse(maxLengthS, out maxLength)) {
						charsWritten = 0;
						return false;
					}
					string result = ToStringDetailed(minDecs, int.Min(destination.Length, maxLength), decimalPoint);
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
		/// Format this string using <c>xRy</c> as a format argument to get the full detail, where 
		/// x is the number of decimal places at a minimum and y is the length of the string.
		/// If x is omitted, it is 0. If y is omitted, it is infinite.
		/// </summary>
		/// <param name="format"></param>
		/// <param name="formatProvider"></param>
		/// <returns></returns>
		public readonly string ToString(string? format, IFormatProvider? formatProvider) {
			const string MESSAGE = "Expecting format string to be null, or in the form of xRy where x is the number of decimal places at a minimum, and y is the maximum string length, both of which may be omitted. The R tag will instruct it to use full detail. A null format option will export as scientific notation.";
			string decimalPoint = NumberFormatInfo.GetInstance(formatProvider).NumberDecimalSeparator ?? ".";

			if (format != null) {
				string[] split = format.Split('R');
				if (split.Length == 2) {
					string enforceDecsS = split[0];
					string maxLengthS = split[1];
					int minDecs = 0;
					int maxLength = -1;
					if (enforceDecsS.Length > 0 && !int.TryParse(enforceDecsS, out minDecs)) {
						throw new FormatException(MESSAGE);
					}
					if (maxLengthS.Length > 0 && !int.TryParse(maxLengthS, out maxLength)) {
						throw new FormatException(MESSAGE);
					}
					return ToStringDetailed(minDecs, maxLength, decimalPoint);
				} else {
					throw new FormatException(MESSAGE);
				}
			} else {
				return ToString();
			}
		}

		/// <inheritdoc/>
		public static BigDecimal Parse(ReadOnlySpan<char> s, IFormatProvider? provider) {
			return Parse(s, NumberStyles.None, provider);
		}

		/// <inheritdoc/>
		public static bool TryParse(ReadOnlySpan<char> s, IFormatProvider? provider, [MaybeNullWhen(false)] out BigDecimal result) {
			return TryParse(s, NumberStyles.None, provider, out result);
		}

		/// <inheritdoc/>
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
				return IntCast<TOther, sbyte, BigInteger>(ref value, 0, out result);
			} else if (result is byte) {
				return IntCast<TOther, byte, BigInteger>(ref value, 0, out result);
			} else if (result is short) {
				return IntCast<TOther, short, BigInteger>(ref value, 0, out result);
			} else if (result is ushort) {
				return IntCast<TOther, ushort, BigInteger>(ref value, 0, out result);
			} else if (result is int) {
				return IntCast<TOther, int, BigInteger>(ref value, 0, out result);
			} else if (result is uint) {
				return IntCast<TOther, uint, BigInteger>(ref value, 0, out result);
			} else if (result is long) {
				return IntCast<TOther, long, BigInteger>(ref value, 0, out result);
			} else if (result is ulong) {
				return IntCast<TOther, ulong, BigInteger>(ref value, 0, out result);
			} else if (result is Int128) {
				return IntCast<TOther, Int128, BigInteger>(ref value, 0, out result);
			} else if (result is UInt128) {
				return IntCast<TOther, UInt128, BigInteger>(ref value, 0, out result);
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
			throw new NotImplementedException();
		}

		/// <inheritdoc/>
		static bool INumberBase<BigDecimal>.TryConvertToTruncating<TOther>(BigDecimal value, out TOther result) {
			throw new NotImplementedException();
		}
		private static bool IntCast<TOther, TInteger, TBigInteger>(ref readonly BigDecimal value, byte checkType, out TOther result) 
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
				if (truncated < min || truncated > max) throw new OverflowException();
				return TBigInteger.TryConvertToChecked((TBigInteger)(object)truncated, out result!);
			} else if (checkType == 2) {
				BigInteger min = BigInteger.CreateTruncating(TInteger.MinValue);
				BigInteger max = BigInteger.CreateTruncating(TInteger.MaxValue);
				if (truncated < min || truncated > max) throw new OverflowException();
				return TBigInteger.TryConvertToChecked((TBigInteger)(object)truncated, out result!);
			}
			throw new ArgumentOutOfRangeException(nameof(checkType));
		}


		static BigDecimal() {
			BigDecimal e = 0;
			for (int i = 0; i < 50; i++) {
				e += One / Factorial(i);
			}
			E = e;
			Pi = Parse("3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989");
			Tau = Pi * 2;

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
