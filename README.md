# BigDecimal

This was originally made by *Jan Christoph Bernack* in the public domain. I have since adapted it to implement several new features, including:
* Implementation of `INumber<T>`
* Definition of Euler's constant `BigDecimal.E`
* Definition of Pi `BigDecimal.Pi`
* Definition of Tau `BigDecimal.Tau`
* Implementation of `Sqrt` (slow)
* Implementation of `Factorial`
* Implementation of `FallingFactorial`
* Implementation of casts to/from all primitive integer types.
* Implementation of casts to/from `decimal` using the native format of the type.
* Implementation of casts to/from `Int128` and `UInt128`
* (Re)implementation of casts to/from `Half`, `float`, and `double` which use the native format of the type for roundtrip conversion accuracy.
* Implementation of `ToStringDetailed`, which outputs the entire value with all decimal places. It has optional parameters to limit the length.
* `INumber<T>` formatting provides a custom format option `xRy(T)` to output the real value, where `x` is the minimum number of decimal places and `y` is the maximum length of the *returned string* (not the numeric digits!)
  * This applies to `string.Format` too.
  * Typically `y` will be omitted.
  * To print with *at least* 10 decimal places of accuracy, use `10R`
  * To print with *exactly* 10 decimal places of accuracy, use `10RT`
* Implementation of `Parse` and `TryParse`.
