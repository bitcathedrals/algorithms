def printer(func):
    def wrapper(*args, **kwargs):
        print("args: " + ",".join(map(repr, args)))
#        print("kwargs: " + ",".join(map(repr, kwargs)))

        result = func(*args, **kwargs)

        print("retval: " + repr(result))

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

    def update_nearest(self, position):
        self.nearest = min(self.nearest, position)

        return self.nearest

#    @printer
    def find_collision(self, char, index):
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

        return self.update_nearest(next_index)

#    @printer
    def find_substring(self):
        self.nearest = self.end

        index = self.index

        remaining = self.end - index

        if remaining < self.longest:
            return False, self.longest

        if remaining < 1:
            return False, self.longest

        while index < self.nearest:
            char = self.string[index]
            collision = self.find_collision(char, index)

#            print("collide: " + repr(collision))

            index += 1

        distance = (index - self.index)
        self.longest = max(self.longest, distance)

        return True, self.longest

    def lengthOfLongestSubstring(self, s):
        """
        :type s: str
        :rtype: int
        """
        self.index = 0
        self.longest = 0

        self.string = s
        self.precompute_collision_map()

#        print("foo: " + repr(self.table))

        while self.index < self.end:
            found, length = self.find_substring()

            if (self.end - self.index) < self.longest:
                break

            self.index += 1

        return self.longest


def run_test():
    run_one = "abcabcbb"

    run_hard = "pwwkew"

    test = Solution()

    print("longest is: " + str(test.lengthOfLongestSubstring(run_one)))
