use rlua::{Function, Lua};

#[test]
fn reads_static_terrain_vertices() {
    Lua::new().context(|ctx| {
        let read: Function = ctx.load(include_str!("../src/resource/terrain.lua")).eval().unwrap();
        ctx.globals().set("readTerrain", read).unwrap();
        ctx.load(r#"
            local function header(version, width, height)
                return string.pack('<c4I4BI4I4c4I4c4I4I4ff',
                    'W3E!', version, string.byte('L'), 0, 1, 'Ldrt', 1, 'CLdi', width, height, -128, -256)
            end
            local function vertex(ground, cliff)
                return string.pack('<I2I2BBB', ground, 65535, 255, 255, cliff)
            end
            local vertices = vertex(8192, 2) .. vertex(8193, 0xF3)
                .. vertex(7680, 1) .. vertex(8704, 4)
                .. vertex(8192, 2) .. vertex(8192, 2)
            local data = header(11, 2, 3) .. vertices
            local terrain = readTerrain(data)
            assert(terrain.width == 2 and terrain.height == 3)
            assert(terrain.offsetX == -128 and terrain.offsetY == -256)
            assert(terrain.heights[1][1] == 0)
            assert(terrain.heights[1][2] == 128.25)
            assert(terrain.heights[2][1] == -256)
            assert(terrain.heights[2][2] == 384)
            assert(terrain.heights[3][2] == 0)
            assert(not pcall(readTerrain, data:sub(1, -2)))
            assert(not pcall(readTerrain, 'BAD!' .. data:sub(5)))
            assert(not pcall(readTerrain, header(12, 2, 3) .. vertices))
            assert(not pcall(readTerrain, header(11, 0, 3) .. vertices))
        "#).exec().unwrap();
    });
}
