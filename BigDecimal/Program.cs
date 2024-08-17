using Star3D.Maths.Numbers;

namespace BigDecimalTester {
	internal class Program {
		static void Main(string[] args) {
			decimal value = -123456789.123456789m;
			BigDecimal test = new BigDecimal(value);
			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("                System Decimal: " + value);
			Console.ForegroundColor = ConsoleColor.DarkGreen;
			Console.WriteLine("                   Exact Value: " + test.ToStringDetailed());
			Console.WriteLine();

			Half value2 = (Half)0.4325f;
			BigDecimal test2 = value2;
			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("              16 bit precision: " + value2);
			Console.ForegroundColor = ConsoleColor.DarkGreen;
			Console.WriteLine("Exact Value (with float error): " + test2.ToStringDetailed());
			Console.ForegroundColor = ConsoleColor.DarkGray;
			Console.WriteLine(" 16 bit precision (from exact): " + (Half)test2);
			Console.ForegroundColor = ConsoleColor.White;
			Console.WriteLine(" 32 bit precision (from exact): " + (float)test2);
			Console.WriteLine(" 64 bit precision (from exact): " + (double)test2);
			Console.WriteLine();

			float value3 = -0.62443325f;
			BigDecimal test3 = value3;
			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("              32 bit precision: " + value3);
			Console.ForegroundColor = ConsoleColor.DarkGreen;
			Console.WriteLine("Exact Value (with float error): " + test3.ToStringDetailed());
			Console.ForegroundColor = ConsoleColor.White;
			Console.WriteLine(" 16 bit precision (from exact): " + (Half)test3);
			Console.ForegroundColor = ConsoleColor.DarkGray;
			Console.WriteLine(" 32 bit precision (from exact): " + (float)test3);
			Console.ForegroundColor = ConsoleColor.White;
			Console.WriteLine(" 64 bit precision (from exact): " + (double)test3);
			Console.WriteLine();

			double value4 = 0.4589158984D;
			BigDecimal test4 = value4;
			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("              64 bit precision: " + value4);
			Console.ForegroundColor = ConsoleColor.DarkGreen;
			Console.WriteLine("Exact Value (with float error): " + test4.ToStringDetailed());
			Console.ForegroundColor = ConsoleColor.White;
			Console.WriteLine(" 16 bit precision (from exact): " + (Half)test4);
			Console.WriteLine(" 32 bit precision (from exact): " + (float)test4);
			Console.ForegroundColor = ConsoleColor.DarkGray;
			Console.WriteLine(" 64 bit precision (from exact): " + (double)test4);
			Console.ForegroundColor = ConsoleColor.White;
			Console.WriteLine();
		}
	}
}
