-- Generated code -- CC0 -- No Rights Reserved -- http://www.redblobgames.com/grids/hexagons/

local hexagon = {}

hexagon.EVEN = 1
hexagon.ODD = -1

function hexagon.Point(x, y)
    return { x = x, y = y }
end

function hexagon.Hex(q, r, s)
    assert(not (math.floor(0.5 + q + r + s) ~= 0), "q + r + s must be 0")
    return { q = q, r = r, s = s }
end

local hex_directions = {
    hexagon.Hex(1, 0, -1),
    hexagon.Hex(1, -1, 0),
    hexagon.Hex(0, -1, 1),
    hexagon.Hex(-1, 0, 1),
    hexagon.Hex(-1, 1, 0),
    hexagon.Hex(0, 1, -1)
}

local hex_diagonals = {
    hexagon.Hex(2, -1, -1),
    hexagon.Hex(1, -2, 1),
    hexagon.Hex(-1, -1, 2),
    hexagon.Hex(-2, 1, 1),
    hexagon.Hex(-1, 2, -1),
    hexagon.Hex(1, 1, -2)
}

function hexagon.hex_add(a, b)
    return hexagon.Hex(a.q + b.q, a.r + b.r, a.s + b.s)
end

function hexagon.hex_subtract(a, b)
    return hexagon.Hex(a.q - b.q, a.r - b.r, a.s - b.s)
end

function hexagon.hex_scale(a, k)
    return hexagon.Hex(a.q * k, a.r * k, a.s * k)
end

function hexagon.hex_rotate_left(a)
    return hexagon.Hex(-a.s, -a.q, -a.r)
end

function hexagon.hex_rotate_right(a)
    return hexagon.Hex(-a.r, -a.s, -a.q)
end

function hexagon.hex_direction(direction)
    return hex_directions[1 + direction]
end

function hexagon.hex_neighbor(hex, direction)
    return hexagon.hex_add(hex, hexagon.hex_direction(direction))
end

function hexagon.hex_diagonal_neighbor(hex, direction)
    return hexagon.hex_add(hex, hex_diagonals[1 + direction])
end

function hexagon.hex_length(hex)
    return math.floor((math.abs(hex.q) + math.abs(hex.r) + math.abs(hex.s)) / 2)
end

function hexagon.hex_distance(a, b)
    return hexagon.hex_length(hexagon.hex_subtract(a, b))
end

function hexagon.hex_round(h)
    local qi = math.floor(math.floor(0.5 + h.q))
    local ri = math.floor(math.floor(0.5 + h.r))
    local si = math.floor(math.floor(0.5 + h.s))
    local q_diff = math.abs(qi - h.q)
    local r_diff = math.abs(ri - h.r)
    local s_diff = math.abs(si - h.s)
    if q_diff > r_diff and q_diff > s_diff then
        qi = -ri - si
    else
        if r_diff > s_diff then
            ri = -qi - si
        else
            si = -qi - ri
        end
    end
    return hexagon.Hex(qi, ri, si)
end

function hexagon.hex_lerp(a, b, t)
    return hexagon.Hex(a.q * (1.0 - t) + b.q * t, a.r * (1.0 - t) + b.r * t, a.s * (1.0 - t) + b.s * t)
end

function hexagon.hex_linedraw(a, b)
    local N = hexagon.hex_distance(a, b)
    local a_nudge = hexagon.Hex(a.q + 1e-06, a.r + 1e-06, a.s - 2e-06)
    local b_nudge = hexagon.Hex(b.q + 1e-06, b.r + 1e-06, b.s - 2e-06)
    local results = {}
    local step = 1.0 / math.max(N, 1)
    for i = 0, N do
        table.insert(results, hexagon.hex_round(hexagon.hex_lerp(a_nudge, b_nudge, step * i)))
    end
    return results
end

function hexagon.OffsetCoord(col, row)
    return { col = col, row = row }
end

function hexagon.qoffset_from_cube(offset, h)
    local col = h.q
    local row = h.r + math.floor((h.q + offset * (bit.band(h.q, 1))) / 2)
    if offset ~= hexagon.EVEN and offset ~= hexagon.ODD then
        error("offset must be hexagonal.EVEN (+1) or hexagonal.ODD (-1)")
    end
    return hexagon.OffsetCoord(col, row)
end

function hexagon.qoffset_to_cube(offset, h)
    local q = h.col
    local r = h.row - math.floor((h.col + offset * (bit.band(h.col, 1))) / 2)
    local s = -q - r
    if offset ~= hexagon.EVEN and offset ~= hexagon.ODD then
        error("offset must be hexagonal.EVEN (+1) or hexagonal.ODD (-1)")
    end
    return hexagon.Hex(q, r, s)
end

function hexagon.roffset_from_cube(offset, h)
    local col = h.q + math.floor((h.r + offset * (bit.band(h.r, 1))) / 2)
    local row = h.r
    if offset ~= hexagon.EVEN and offset ~= hexagon.ODD then
        error("offset must be hexagonal.EVEN (+1) or hexagonal.ODD (-1)")
    end
    return hexagon.OffsetCoord(col, row)
end

function hexagon.roffset_to_cube(offset, h)
    local q = h.col - math.floor((h.row + offset * (bit.band(h.row, 1))) / 2)
    local r = h.row
    local s = -q - r
    if offset ~= hexagon.EVEN and offset ~= hexagon.ODD then
        error("offset must be hexagonal.EVEN (+1) or hexagonal.ODD (-1)")
    end
    return hexagon.Hex(q, r, s)
end

function hexagon.DoubledCoord(col, row)
    return { col = col, row = row }
end

function hexagon.qdoubled_from_cube(h)
    local col = h.q
    local row = 2 * h.r + h.q
    return hexagon.DoubledCoord(col, row)
end

function hexagon.qdoubled_to_cube(h)
    local q = h.col
    local r = math.floor((h.row - h.col) / 2)
    local s = -q - r
    return hexagon.Hex(q, r, s)
end

function hexagon.rdoubled_from_cube(h)
    local col = 2 * h.q + h.r
    local row = h.r
    return hexagon.DoubledCoord(col, row)
end

function hexagon.rdoubled_to_cube(h)
    local q = math.floor((h.col - h.row) / 2)
    local r = h.row
    local s = -q - r
    return hexagon.Hex(q, r, s)
end

function hexagon.Orientation(f0, f1, f2, f3, b0, b1, b2, b3, start_angle)
    return { f0 = f0, f1 = f1, f2 = f2, f3 = f3, b0 = b0, b1 = b1, b2 = b2, b3 = b3, start_angle = start_angle }
end

hexagon.layout_pointy = hexagon.Orientation(math.sqrt(3.0), math.sqrt(3.0) / 2.0, 0.0, 3.0 / 2.0, math.sqrt(3.0) / 3.0, -1.0 / 3.0, 0.0, 2.0 / 3.0, 0.5)
hexagon.layout_flat = hexagon.Orientation(3.0 / 2.0, 0.0, math.sqrt(3.0) / 2.0, math.sqrt(3.0), 2.0 / 3.0, 0.0, -1.0 / 3.0, math.sqrt(3.0) / 3.0, 0.0)

function hexagon.Layout(orientation, size, origin)
    return { orientation = orientation, size = size, origin = origin }
end

function hexagon.hex_to_pixel(layout, h)
    local M = layout.orientation
    local size = layout.size
    local origin = layout.origin
    local x = (M.f0 * h.q + M.f1 * h.r) * size.x
    local y = (M.f2 * h.q + M.f3 * h.r) * size.y
    return hexagon.Point(x + origin.x, y + origin.y)
end

function hexagon.pixel_to_hex(layout, p)
    local M = layout.orientation
    local size = layout.size
    local origin = layout.origin
    local pt = hexagon.Point((p.x - origin.x) / size.x, (p.y - origin.y) / size.y)
    local q = M.b0 * pt.x + M.b1 * pt.y
    local r = M.b2 * pt.x + M.b3 * pt.y
    return hexagon.Hex(q, r, -q - r)
end

function hexagon.hex_corner_offset(layout, corner)
    local M = layout.orientation
    local size = layout.size
    local angle = 2.0 * math.pi * (M.start_angle - corner) / 6.0
    return hexagon.Point(size.x * math.cos(angle), size.y * math.sin(angle))
end

function hexagon.polygon_corners(layout, h)
    local corners = {}
    local center = hexagon.hex_to_pixel(layout, h)
    for i = 0, 5 do
        local offset = hexagon.hex_corner_offset(layout, i)
        table.insert(corners, hexagon.Point(center.x + offset.x, center.y + offset.y))
    end
    return corners
end

-- Tests

function hexagon.complain(name)
    print("FAIL ", name)
end

function hexagon.equal_hex(name, a, b)
    if not (a.q == b.q and a.s == b.s and a.r == b.r) then
        hexagon.complain(name)
    end
end

function hexagon.equal_offsetcoord(name, a, b)
    if not (a.col == b.col and a.row == b.row) then
        hexagon.complain(name)
    end
end

function hexagon.equal_doubledcoord(name, a, b)
    if not (a.col == b.col and a.row == b.row) then
        hexagon.complain(name)
    end
end

function hexagon.equal_int(name, a, b)
    if not (a == b) then
        hexagon.complain(name)
    end
end

function hexagon.equal_hex_array(name, a, b)
    hexagon.equal_int(name, #a, #b)
    for i = 0, #a - 1 do
        hexagon.equal_hex(name, a[1 + i], b[1 + i])
    end
end

-- TESTS

-- TESTS
local function test_hex_arithmetic()
    hexagon.equal_hex("hex_add", hexagon.Hex(4, -10, 6), hexagon.hex_add(hexagon.Hex(1, -3, 2), hexagon.Hex(3, -7, 4)))
    hexagon.equal_hex("hex_subtract", hexagon.Hex(-2, 4, -2), hexagon.hex_subtract(hexagon.Hex(1, -3, 2), hexagon.Hex(3, -7, 4)))
end

local function test_hex_direction()
    hexagon.equal_hex("hex_direction", hexagon.Hex(0, -1, 1), hexagon.hex_direction(2))
end

local function test_hex_neighbor()
    hexagon.equal_hex("hex_neighbor", hexagon.Hex(1, -3, 2), hexagon.hex_neighbor(hexagon.Hex(1, -2, 1), 2))
end

local function test_hex_diagonal()
    hexagon.equal_hex("hex_diagonal", hexagon.Hex(-1, -1, 2), hexagon.hex_diagonal_neighbor(hexagon.Hex(1, -2, 1), 3))
end

local function test_hex_distance()
    hexagon.equal_int("hex_distance", 7, hexagon.hex_distance(hexagon.Hex(3, -7, 4), hexagon.Hex(0, 0, 0)))
end

local function test_hex_rotate_right()
    hexagon.equal_hex("hex_rotate_right", hexagon.hex_rotate_right(hexagon.Hex(1, -3, 2)), hexagon.Hex(3, -2, -1))
end

local function test_hex_rotate_left()
    hexagon.equal_hex("hex_rotate_left", hexagon.hex_rotate_left(hexagon.Hex(1, -3, 2)), hexagon.Hex(-2, -1, 3))
end

local function test_hex_round()
    local a = hexagon.Hex(0.0, 0.0, 0.0)
    local b = hexagon.Hex(1.0, -1.0, 0.0)
    local c = hexagon.Hex(0.0, -1.0, 1.0)
    hexagon.equal_hex("hex_round 1", hexagon.Hex(5, -10, 5), hexagon.hex_round(hexagon.hex_lerp(hexagon.Hex(0.0, 0.0, 0.0), hexagon.Hex(10.0, -20.0, 10.0), 0.5)))
    hexagon.equal_hex("hex_round 2", hexagon.hex_round(a), hexagon.hex_round(hexagon.hex_lerp(a, b, 0.499)))
    hexagon.equal_hex("hex_round 3", hexagon.hex_round(b), hexagon.hex_round(hexagon.hex_lerp(a, b, 0.501)))
    hexagon.equal_hex("hex_round 4", hexagon.hex_round(a), hexagon.hex_round(hexagon.Hex(a.q * 0.4 + b.q * 0.3 + c.q * 0.3, a.r * 0.4 + b.r * 0.3 + c.r * 0.3, a.s * 0.4 + b.s * 0.3 + c.s * 0.3)))
    hexagon.equal_hex("hex_round 5", hexagon.hex_round(c), hexagon.hex_round(hexagon.Hex(a.q * 0.3 + b.q * 0.3 + c.q * 0.4, a.r * 0.3 + b.r * 0.3 + c.r * 0.4, a.s * 0.3 + b.s * 0.3 + c.s * 0.4)))
end

local function test_hex_linedraw()
    hexagon.equal_hex_array("hex_linedraw", { hexagon.Hex(0, 0, 0), hexagon.Hex(0, -1, 1), hexagon.Hex(0, -2, 2), hexagon.Hex(1, -3, 2), hexagon.Hex(1, -4, 3), hexagon.Hex(1, -5, 4) }, hexagon.hex_linedraw(hexagon.Hex(0, 0, 0), hexagon.Hex(1, -5, 4)))
end

local function test_layout()
    local h = hexagon.Hex(3, 4, -7)

    local flat = hexagon.Layout(hexagon.layout_flat, hexagon.Point(10.0, 15.0), hexagon.Point(35.0, 71.0))
    hexagon.equal_hex("layout", h, hexagon.hex_round(hexagon.pixel_to_hex(flat, hexagon.hex_to_pixel(flat, h))))

    local pointy = hexagon.Layout(hexagon.layout_pointy, hexagon.Point(10.0, 15.0), hexagon.Point(35.0, 71.0))
    hexagon.equal_hex("layout", h, hexagon.hex_round(hexagon.pixel_to_hex(pointy, hexagon.hex_to_pixel(pointy, h))))
end

local function test_offset_roundtrip()
    local a = hexagon.Hex(3, 4, -7)
    local b = hexagon.OffsetCoord(1, -3)
    hexagon.equal_hex("conversion_roundtrip even-q", a, hexagon.qoffset_to_cube(hexagon.EVEN, hexagon.qoffset_from_cube(hexagon.EVEN, a)))
    hexagon.equal_offsetcoord("conversion_roundtrip even-q", b, hexagon.qoffset_from_cube(hexagon.EVEN, hexagon.qoffset_to_cube(hexagon.EVEN, b)))
    hexagon.equal_hex("conversion_roundtrip odd-q", a, hexagon.qoffset_to_cube(hexagon.ODD, hexagon.qoffset_from_cube(hexagon.ODD, a)))
    hexagon.equal_offsetcoord("conversion_roundtrip odd-q", b, hexagon.qoffset_from_cube(hexagon.ODD, hexagon.qoffset_to_cube(hexagon.ODD, b)))
    hexagon.equal_hex("conversion_roundtrip even-r", a, hexagon.roffset_to_cube(hexagon.EVEN, hexagon.roffset_from_cube(hexagon.EVEN, a)))
    hexagon.equal_offsetcoord("conversion_roundtrip even-r", b, hexagon.roffset_from_cube(hexagon.EVEN, hexagon.roffset_to_cube(hexagon.EVEN, b)))
    hexagon.equal_hex("conversion_roundtrip odd-r", a, hexagon.roffset_to_cube(hexagon.ODD, hexagon.roffset_from_cube(hexagon.ODD, a)))
    hexagon.equal_offsetcoord("conversion_roundtrip odd-r", b, hexagon.roffset_from_cube(hexagon.ODD, hexagon.roffset_to_cube(hexagon.ODD, b)))
end

local function test_offset_from_cube()
    hexagon.equal_offsetcoord("offset_from_cube even-q", hexagon.OffsetCoord(1, 3), hexagon.qoffset_from_cube(hexagon.EVEN, hexagon.Hex(1, 2, -3)))
    hexagon.equal_offsetcoord("offset_from_cube odd-q", hexagon.OffsetCoord(1, 2), hexagon.qoffset_from_cube(hexagon.ODD, hexagon.Hex(1, 2, -3)))
end

local function test_offset_to_cube()
    hexagon.equal_hex("offset_to_cube even-", hexagon.Hex(1, 2, -3), hexagon.qoffset_to_cube(hexagon.EVEN, hexagon.OffsetCoord(1, 3)))
    hexagon.equal_hex("offset_to_cube odd-q", hexagon.Hex(1, 2, -3), hexagon.qoffset_to_cube(hexagon.ODD, hexagon.OffsetCoord(1, 2)))
end

local function test_doubled_roundtrip()
    local a = hexagon.Hex(3, 4, -7)
    local b = hexagon.DoubledCoord(1, -3)
    hexagon.equal_hex("conversion_roundtrip doubled-q", a, hexagon.qdoubled_to_cube(hexagon.qdoubled_from_cube(a)))
    hexagon.equal_doubledcoord("conversion_roundtrip doubled-q", b, hexagon.qdoubled_from_cube(hexagon.qdoubled_to_cube(b)))
    hexagon.equal_hex("conversion_roundtrip doubled-r", a, hexagon.rdoubled_to_cube(hexagon.rdoubled_from_cube(a)))
    hexagon.equal_doubledcoord("conversion_roundtrip doubled-r", b, hexagon.rdoubled_from_cube(hexagon.rdoubled_to_cube(b)))
end

local function test_doubled_from_cube()
    hexagon.equal_doubledcoord("doubled_from_cube doubled-q", hexagon.DoubledCoord(1, 5), hexagon.qdoubled_from_cube(hexagon.Hex(1, 2, -3)))
    hexagon.equal_doubledcoord("doubled_from_cube doubled-r", hexagon.DoubledCoord(4, 2), hexagon.rdoubled_from_cube(hexagon.Hex(1, 2, -3)))
end

local function test_doubled_to_cube()
    hexagon.equal_hex("doubled_to_cube doubled-q", hexagon.Hex(1, 2, -3), hexagon.qdoubled_to_cube(hexagon.DoubledCoord(1, 5)))
    hexagon.equal_hex("doubled_to_cube doubled-r", hexagon.Hex(1, 2, -3), hexagon.rdoubled_to_cube(hexagon.DoubledCoord(4, 2)))
end

function hexagon.test_all()
    print('TEST ALL')
    test_hex_arithmetic()
    test_hex_direction()
    test_hex_neighbor()
    test_hex_diagonal()
    test_hex_distance()
    test_hex_rotate_right()
    test_hex_rotate_left()
    test_hex_round()
    test_hex_linedraw()
    test_layout()
    test_offset_roundtrip()
    test_offset_from_cube()
    test_offset_to_cube()
    test_doubled_roundtrip()
    test_doubled_from_cube()
    test_doubled_to_cube()
end

return hexagon
