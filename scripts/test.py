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

    def test_fib_tco(self):
        self.into_test("fib_tco", 12586269025)

    def test_fib_lazy(self):
        self.into_test("fib_lazy", 610)

    def test_function_pointer(self):
        self.into_test("function_pointer", -123)

    def test_pack_unpack(self):
        self.into_test("pack_unpack", "\n".join([
            "first 123456",
            "second path!",
            "1 2 3",
        ]))

    def test_return_if(self):
        self.into_test("return_if", "!")


if __name__ == "__main__":
    main()
