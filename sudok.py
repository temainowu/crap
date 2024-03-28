# ===============================
# =           Imports           =
# ===============================

try:
    import numpy as np
except ImportError:
    raise ImportError("Numpy is not installed.")

import random
import time


# ===============================
# =           Classes           =
# ===============================

class SudokuSolver:
    def possible(self, x, y, n):
        for i in range(9):
            if self.grid[i][x] == n:
                return False
        for i in range(9):
            if self.grid[y][i] == n:
                return False
        x0 = (x // 3) * 3
        y0 = (y // 3) * 3
        for i in range(3):
            for j in range(3):
                if self.grid[y0+i][x0+j] == n:
                    return False
        return True

    def solve(self):
        for y in range(9):
            for x in range(9):
                if self.grid[y][x] == 0:
                    for i in range(1, 10):
                        if self.possible(x, y, i):
                            self.grid[y][x] = i
                            self.solve()
                            self.grid[y][x] = 0
                    return
        print(np.matrix(self.grid))
        self.correctSolves += 1

    def __init__(self, inGrid):
        if len(inGrid) == 0:
            raise ValueError("Empty in grid.")
        self.grid = inGrid
        self.correctSolves = 0
        start_time = time.time()
        self.solve()
        solve_time = round((time.time() - start_time), 6)
        print("Found", self.correctSolves, "different solutions in", solve_time, "seconds.")


# ====================================
# =           Main Section           =
# ====================================

if __name__ == "__main__":
    grid = [[0, 0, 9, 0, 0, 0, 0, 0, 0],
            [3, 4, 8, 0, 0, 0, 5, 0, 0],
            [0, 0, 0, 0, 4, 0, 3, 0, 0],
            [0, 0, 0, 1, 0, 0, 2, 7, 0],
            [2, 0, 0, 3, 0, 4, 0, 0, 5],
            [0, 4, 8, 0, 0, 6, 0, 0, 0],
            [0, 0, 6, 0, 1, 0, 0, 0, 0],
            [0, 0, 7, 0, 0, 0, 6, 2, 9],
            [0, 0, 0, 0, 0, 5, 0, 0, 0]]
    solver = SudokuSolver(grid)