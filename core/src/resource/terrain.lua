-- Static W3E vertex heights, in world units. No water, walkable objects or
-- client-side terrain deformations are included.
return function(data)
    local magic, version, _, _, groundCount, pos = string.unpack("<c4I4BI4I4", data)
    assert(magic == "W3E!", "Invalid W3E signature")
    assert(version == 11, "Unsupported W3E version: " .. version)
    pos = pos + groundCount * 4
    local cliffCount
    cliffCount, pos = string.unpack("<I4", data, pos)
    pos = pos + cliffCount * 4
    local width, height, offsetX, offsetY
    width, height, offsetX, offsetY, pos = string.unpack("<I4I4ff", data, pos)
    assert(width >= 2 and height >= 2, "Invalid W3E dimensions")
    assert(width * height <= (#data - pos + 1) // 7, "Truncated W3E vertices")
    local heights = {}
    for y = 1, height do
        local row = {}
        for x = 1, width do
            local ground, _, _, _, cliff
            ground, _, _, _, cliff, pos = string.unpack("<I2I2BBB", data, pos)
            row[x] = (ground - 8192) / 4 + ((cliff & 15) - 2) * 128
        end
        heights[y] = row
    end
    return { width = width, height = height, offsetX = offsetX, offsetY = offsetY, heights = heights }
end
