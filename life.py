import numpy, random

grid = numpy.ndarray((40,74), dtype = numpy.bool)

def will_live(cell, number_of_neighbours):
    return number_of_neighbours == 3 or number_of_neighbours == 2 and cell

def local_population(grid, coords):
    x, y = coords
    return numpy.sum(grid[max(x-1, 0):x+2, max(y-1, 0):y+2])

def count_live_neighbours(grid, coords):
    x, y = coords
    return local_population(grid, coords) - grid[x][y]

def next_generation(grid):
    new_grid = numpy.copy(grid)
    for x, col in enumerate(grid):
        for y, cell in enumerate(col):
            new_grid[x][y] = will_live(cell, count_live_neighbours(grid, (x, y)))
    return new_grid

def output(grid):
    print('\033[H\x48\033[2J', end=None)
    print('\n'.join('\033[3' + str(1+i%7) + 'm' +
                    ''.join(['😺 ' if cell else '  ' for cell in row])
                    for i, row in enumerate(grid)))

for i in range(len(grid)):
    for j in range(len(grid[i])):
        grid[i][j] = random.choice([True, False])

while True:
    output(grid)
    grid = next_generation(grid)
