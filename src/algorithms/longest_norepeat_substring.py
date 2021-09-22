# Longest Substring Without Repeating Characters
# written by Mike Mattie (c) 2021

from collections import OrderedDict


def printer(func):
    def wrapper(*args, **kwargs):
        print("args: " + ",".join(map(repr, args)))

        result = func(*args, **kwargs)

        print("return: " + repr(result))

        return result

    return wrapper


class Solution(object):

    def precompute_occurence_table(self):
        occur_map = [None] * len(self.string)

        table = {}
        index = 0

        for char in self.string:
            if char in table:
                table[char].append(index)
            else:
                table[char] = [index]

            index += 1

        for char in table:
            occurrences = iter(table[char])

            occur_position = next(occurrences)

            try:
                while True:
                    occur_next = next(occurrences)

                    occur_map[occur_position] = occur_next

                    occur_position = occur_next

            except StopIteration:
                pass

            # terminate the sequence
            occur_map[occur_position] = len(self.string)

        self.occur_map = occur_map

#    @printer
    def find_longest(self):
        occur_map = self.occur_map
        nearest = len(self.string)

        index = self.index

        remaining = len(self.string) - index

        if remaining < self.longest:
            return self.longest

        if remaining < 1:
            return self.longest

        # this will only find the first occurrence but that's ok
        # because the window shift will move the starting position
        while index < nearest:
            next_occur = occur_map[index]
            nearest = nearest if nearest < next_occur else next_occur

            index += 1

        distance = index - self.index
        self.longest = max(self.longest, distance)

        return self.longest

    def forwardLookingPreCompute(self, s):
        """
        :type s: str
        :rtype: int
        """
        self.index = 0
        self.longest = 0

        self.string = s
        self.end = len(self.string)

        self.precompute_occurence_table()

        while self.index < self.end:
            self.find_longest()

            if (self.end - self.index) < self.longest:
                break

            self.index += 1

        return self.longest

# remembering or backward looking version

    def longest_with_history(self, start):
        string = self.string
        index = start
        end = self.end
        seen = set()

        while index < end:
            char = string[index]
            if char in seen:
                return index - start

            seen.add(char)
            index += 1

        return index - start

    def backwardLookingMemoize(self, s):
        """
        :type s: str
        :rtype: int
        """

        index = 0
        longest = 0

        self.string = s
        self.end = len(self.string)

        seen = ()

        while index < self.end:
            length = self.longest_with_history(index)

            longest = length if length > longest else longest

            index += 1

        return longest

# forgetting version

# we keep a seen, if we see it again, instead of stopping the whole substring match,
# drop the character from seen, and restart after the previous occurance. go through
# the whole string this way in one pass.

    def longest_with_forgetting(self, index):
        string = self.string
        end = self.end

        seen = OrderedDict()

        longest = 0

        while index < end:
            char = string[index]

#            print(f"char is: {char} index is: {index} longest is: {longest} seen is: {repr(seen)}")

            if char in seen:
                last = seen[char]

                if last == index:
                    index += 1
                    continue

                table_size = len(seen)
                longest = longest if table_size < longest else table_size

                remove = []

                for i_char, i_idx in seen.items():
                    if i_idx <= last:
                        remove.append(i_char)
                    else:
                        break

                for entry in remove:
                    del seen[entry]

                seen[char] = index

                index = last + 1
                continue

            seen[char] = index

            index += 1

        longest = max(longest, len(seen))
        return longest

    def lengthOfLongestSubstring(self, s):
        """
        :type s: str
        :rtype: int
        """
        index = 0

        self.string = s
        self.end = len(self.string)

        return self.longest_with_forgetting(0)


def run(test_case):
    test = Solution()

    result = test.lengthOfLongestSubstring(test_case)

    print("longest is: " + str(result))

    return result