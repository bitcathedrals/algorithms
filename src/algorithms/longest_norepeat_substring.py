# Longest Substring Without Repeating Characters
# written by Mike Mattie (c) 2021

def printer(func):
    def wrapper(*args, **kwargs):
        print("args: " + ",".join(map(repr, args)))

        result = func(*args, **kwargs)

        print("return: " + repr(result))

        return result

    return wrapper


class Solution(object):

    def precompute_occurence_table(self):
        self.occurrence_map = [None] * len(self.string)

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

                    self.occurrence_map[occur_position] = occur_next

                    occur_position = occur_next

            except StopIteration:
                pass

            # terminate the sequence
            self.occurrence_map[occur_position] = len(self.string)

#    @printer
    def update_nearest(self, index):
        self.nearest = min(self.nearest, self.occurrence_map[index])
        return self.nearest

#    @printer
    def find_longest(self):
        self.nearest = len(self.string)

        index = self.index

        remaining = len(self.string) - index

        if remaining < self.longest:
            return self.longest

        if remaining < 1:
            return self.longest

        # this will only find the first occurance but that's ok
        # because the window shift will move the starting position
        while index < self.nearest:
            self.update_nearest(index)

            index += 1

        distance = index - self.index
        self.longest = max(self.longest, distance)

        return self.longest

    def lengthOfLongestSubstring(self, s):
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


def run(test_case):
    test = Solution()

    result = test.lengthOfLongestSubstring(test_case)

    print("longest is: " + str(result))

    return result