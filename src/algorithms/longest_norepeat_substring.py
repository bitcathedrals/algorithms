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

    def precompute_collision_map(self):
        self.table = {}
        index = 0

        for char in self.string:
            if char in self.table:
                self.table[char].append(index)
            else:
                self.table[char] = [index]

            index += 1

        for char in self.table:
            self.table[char].sort()

        self.nearest = len(self.string)
        self.end = self.nearest

#    @printer
    def update_nearest(self, char, index):
        positions = iter(self.table[char])

        next_index = None

        try:
            search = True
            while search:
                pos = next(positions)

                if pos < index:
                    continue

                if pos == index:
                    search = False

            next_index = next(positions)

        except StopIteration as ex:
            return None

        self.nearest = min(self.nearest, next_index)

        return self.nearest

#    @printer
    def find_substring(self):
        self.nearest = self.end

        index = self.index

        remaining = self.end - index

        if remaining < self.longest:
            return self.longest

        if remaining < 1:
            return self.longest

        while index < self.nearest:
            char = self.string[index]
            self.update_nearest(char, index)

            index += 1

        distance = (index - self.index)
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
        self.precompute_collision_map()

        while self.index < self.end:
            length = self.find_substring()

            if (self.end - self.index) < self.longest:
                break

            self.index += 1

        return self.longest


def run(test_case):
    test = Solution()

    result = test.lengthOfLongestSubstring(test_case)

    print("longest is: " + str(result))

    return result