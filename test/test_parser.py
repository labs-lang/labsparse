import unittest
from labsparse import labs_parser
from labsparse.labs_ast import Attr


class TestArrays(unittest.TestCase):
    def test_offset(self):
        # self.assertTrue(True)
        parsed = labs_parser.LHS.parseString("x[5,3]").asList()[0]
        self.assertTrue(hasattr(parsed, Attr.OFFSET))
        self.assertIsInstance(parsed[Attr.OFFSET], list)
        self.assertEqual(len(parsed[Attr.OFFSET]), 2)
        self.assertEqual(parsed[Attr.OFFSET][0].as_labs(), "5")
        self.assertEqual(parsed[Attr.OFFSET][1].as_labs(), "3")
