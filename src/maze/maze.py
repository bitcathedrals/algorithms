# maze.py - written by Mike Mattie
# a small program to navigate a maze from a starting position to a goal position.
# the output should be a path if any through the maze.

# assumption: the starting point is in the upper left corner.
# assumption: the goal is in the lower right corner.

from typing import List


# steps are positions on the map that are not walls
# with moves associated with them.


class Step:
    # enumerate the directions that are valid moves
    EAST = 0
    SOUTH = 1
    WEST = 2
    NORTH = 3

    def __init__(self,
                 x: int, y: int,
                 previous,
                 in_bounds,
                 moves: List[int]):
        self.x = x
        self.y = y
        self.moves = sorted(moves, reverse=True)

        # initializing the first step
        if not previous:
            return

        if not in_bounds(self.x, self.y):
            raise ValueError(f"Step X,Y [{self.x}],[{self.y}] is out of bounds.")

        if not moves:
            raise ValueError(f"Step X,Y [{self.x}],[{self.y}] has no moves.")

        if in_bounds(self.x - 1, self.y) \
                and self.x - 1 == previous.x \
                and self.y == previous.y:
            previous.delete_move(self.EAST)
            self.delete_move(self.WEST)

        if in_bounds(self.x + 1, self.y) \
                and self.x + 1 == previous.x \
                and self.y == previous.y:
            previous.delete_move(self.WEST)
            self.delete_move(self.EAST)

        if in_bounds(self.x, self.y - 1) \
                and self.x == previous.x \
                and self.y - 1 == previous.y:
            previous.delete_move(self.SOUTH)
            self.delete_move(self.NORTH)

        if in_bounds(self.x, self.y + 1) \
                and self.x == previous.x \
                and self.y + 1 == previous.y:
            previous.delete_move(self.NORTH)
            self.delete_move(self.SOUTH)

    def delete_move(self, move: int):
        if move in self.moves:
            del self.moves[move]

    def pop_move(self):
        if not self.moves:
            raise ValueError(f"Step X,Y [{self.x}],[{self.y}] attempting to pop no moves")

        return self.moves.pop()

    def has_move(self):
        if self.moves:
            return True

        return False

    def relative_coordinates_for_direction(self, direction: int):
        if direction == self.EAST:
            return 1, 0

        if direction == self.SOUTH:
            return 0, 1

        if direction == self.WEST:
            return -1, 0

        if direction == self.NORTH:
            return 0, -1

    def next_move(self, map_unmarked, map_in_bounds, map_next_moves):

        while self.has_move():
            direction = self.pop_move()
            x_ofs, y_ofs = self.relative_coordinates_for_direction(direction)

            next_x = self.x + x_ofs
            next_y = self.y + y_ofs

            if not map_in_bounds(next_x, next_y):
                # this should be impossible but I would log it and drop it.
                continue

            if map_unmarked(next_x, next_y):
                return Step(next_x, next_y,
                            self,
                            map_in_bounds, map_next_moves(next_x, next_y))

        return None

    def __str__(self):
        return f"Step = ({self.x}, {self.y})"


class MazeMap:
    EMPTY = 0
    WALL = 1
    MARKED = 2

    def __init__(self, maze, width, height):
        self.maze = maze

        self.width = width
        self.height = height

        self.goal_x = width - 1
        self.goal_y = height - 1

    def starting_step(self):
        if not self.is_unmarked(0, 0):
            raise ValueError("Starting Point 0,0 is marked")

        start = Step(0, 0, None, None, self.get_moves(0, 0))

        if not start.has_move():
            raise ValueError("Starting Point 0,0 has no moves")

        return start

    def in_bounds(self, x, y):
        if x >= 0 \
            and x < self.width \
            and y >= 0 \
            and y < self.height:
            return True
        return False

    def bounds_fn(self):
        return lambda x, y: self.in_bounds(x, y)

    def is_unmarked(self, x, y):
        if self.maze[x][y] == 0:
            return True
        return False

    def unmarked_fn(self):
        return lambda x, y: self.is_unmarked(x, y)

    def is_goal(self, x, y):
        if x == self.goal_x and y == self.goal_y:
            return True

        return False

    def get_moves(self, x, y):
        valid_moves = []

        if self.in_bounds(x - 1, y) and self.is_unmarked(x - 1, y):
            valid_moves.append(Step.WEST)

        if self.in_bounds(x + 1, y) and self.is_unmarked(x + 1, y):
            valid_moves.append(Step.EAST)

        if self.in_bounds(x, y - 1) and self.is_unmarked(x, y - 1):
            valid_moves.append(Step.NORTH)

        if self.in_bounds(x, y + 1) and self.is_unmarked(x, y + 1):
            valid_moves.append(Step.SOUTH)

        return valid_moves

    def moves_fn(self):
        return lambda x,y: self.get_moves(x, y)

    def mark(self, x, y):
        self.maze[x][y] = self.MARKED

    def mark_step(self, step):
        self.maze[step.x][step.y] = self.MARKED


class Explorer:
    # create a map from the given input
    # initialize the current step and the stack
    def __init__(self, maze: MazeMap):
        self.maze = maze

        self.path = [maze.starting_step()]

        self.maze.mark(0, 0)

    def find_path(self):
        while self.path:
            # print(f"step is: {self.path[-1].x},{self.path[-1].y}")

            if self.maze.is_goal(self.path[-1].x, self.path[-1].y):
                return [str(x) for x in self.path]

            next_step = self.path[-1].next_move(self.maze.unmarked_fn(),
                                                self.maze.bounds_fn(),
                                                self.maze.moves_fn())

            if not next_step:
                self.path.pop()
                continue

            self.maze.mark_step(next_step)
            self.path.append(next_step)

        return []


maze_simple = MazeMap([[0, 0, 1, 0, 1],
                       [1, 0, 1, 1, 1],
                       [0, 0, 0, 0, 1],
                       [1, 1, 1, 0, 0],
                       [0, 0, 0, 0, 0]], 5, 5)

explorer = Explorer(maze_simple)

path = explorer.find_path()

if not path:
    print("Path could not be found!")
else:
    print("\n".join(path))

