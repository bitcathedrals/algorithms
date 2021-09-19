from algorithms.longest_norepeat_substring import Solution


class TestLongest:
    @property
    def code(self):
        return Solution()

    def test_one(self):
        test = "abcabcbb"
        expected = 3

        result = self.code.lengthOfLongestSubstring(test)

        assert result == expected

    def test_unknown(self):
        test = "pwwkew"
        expected = 3

        result = self.code.lengthOfLongestSubstring(test)

        assert result == expected
