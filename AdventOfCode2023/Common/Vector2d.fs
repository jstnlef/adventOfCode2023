module AdventOfCode2023.Common.Vector2d

let determinant (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

let length (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
