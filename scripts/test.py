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

    def test_fib(self):
        self.into_test("fib", 12586269025)

    def test_pack_unpack(self):
        self.into_test("pack_unpack", "\n".join([
            "first 123456",
            "second path!",
            "1 2 3",
        ]))


if __name__ == "__main__":
    main()
