#!/usr/bin/env python3

from subprocess import run
from unittest import main, TestCase

__unittest = True


class Tests(TestCase):
    def into_test(self, file, expected):
        result = run(
            f"\"$WD\"/scripts/run.sh \"$WD\"/ex/{file}.oc".encode(),
            capture_output=True,
            shell=True,
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout.decode(), f"{expected}\n")

    def test_alloc(self):
        self.into_test("alloc", "13701 -123 -1")

    def test_collatz(self):
        self.into_test("collatz", "3,10,5,16,8,4,2,1")

    def test_divmod(self):
        self.into_test("divmod", "\n".join([
            "2,2",
            "-3,14",
            "-3,-14",
            "2,-2",
            "2,0",
            "-2,0",
            "-2,0",
            "2,0",
            "1,16",
            "-2,2",
            "-2,-2",
            "1,-16",
        ]))

    def test_early_return(self):
        self.into_test("early_return", "returned early")

    def test_even_odd(self):
        self.into_test("even_odd", "1234567 is odd")

    def test_fib_tco(self):
        self.into_test("fib_tco", 12586269025)

    def test_function_pointer(self):
        self.into_test("function_pointer", -123)


if __name__ == "__main__":
    main()
