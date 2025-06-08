-- Build script utilities for warpack

warpack.compiletime = true

function log(...)
    local args = { ... }
    for k, v in pairs(args) do
        args[k] = tostring(v)
    end
    io.stderr:write("> " .. table.concat(args, " ") .. "\n")
end

-- macro support
function define(id, value)
    _G.id = function()
    end

    if type(value) == "function" then
        warpack.registerMacro(id, value)
    else
        warpack.registerMacro(id, function()
            return value
        end)
    end
end

warpack.registerMacro("macro_define", define)

warpack.registerMacro("include", function(path)
    local content, err = fs.readFile(path)

    if not content then
        log("Failed to run include macro: ", err)
        error(err)
    end

    return content
end)

function macro_define(id, value)
    define(id, value)
end
function compiletime(v)
    if type(v) == "function" then
        v = v()
    end
    if type(v) == "table" and type(v.compiletime) == "function" then
        v = v:compiletime()
    end
    return v
end

-- hooks

local preScriptBuildHooks = {}
local postScriptBuildHooks = {}
local postMapBuildHooks = {}
local postRunHooks = {}

local function callHooks(t, ...)
    for _, v in ipairs(t) do
        local r = v.callback(...)
        if r ~= nil then
            return r
        end
    end
end

local function addHook(hookTable, hookName, callback)
    for _, v in ipairs(hookTable) do
        if v.name == hookName then
            v.callback = callback
            return
        end
    end
    table.insert(hookTable, { name = hookName, callback = callback })
end

function warpack.addPreScriptBuildHook(name, callback)
    addHook(preScriptBuildHooks, name, callback)
end

function warpack.addPostScriptBuildHook(name, callback)
    addHook(postScriptBuildHooks, name, callback)
end

function warpack.addPostMapBuildHook(name, callback)
    addHook(postMapBuildHooks, name, callback)
end

function warpack.addPostRunHook(name, callback)
    addHook(postRunHooks, name, callback)
end

-- map library

local mapMeta = {}
mapMeta.__index = mapMeta

-- Reads a file from the map and returns its contents as a string if successful
function mapMeta:readFile(path)
    path = fs.normalize(path)

    local added = self.added[path]

    if added then
        if added.kind == "string" then
            return added.contents
        elseif added.kind == "file" then
            return fs.readFile(added.path)
        end
    end

    if self.kind == "mpq" then
        return self.archive:readFile(path)
    elseif self.kind == "dir" then
        return fs.readFile(self.path .. path)
    end
end

local addDirRecursive
local function addDirRecursive(self, path, basePath)
    local files, dirs = assert(fs.readDir(path))

    for _, file in pairs(files) do
        local relativePath = file:sub(#basePath + 2)
        self:addFileDisk(relativePath, file)
    end

    for _, dir in pairs(dirs) do
        addDirRecursive(self, dir, basePath)
    end
end

function mapMeta:addDir(path)
    addDirRecursive(self, path, fs.absolutize(path))
end

-- Adds a file to the map, as a lua string
-- This doesn't modify the map in any way, it only adds the file to be written when either
-- map:writeToDir() or map:writeToMpq() is called
function mapMeta:addFileString(path, contents)
    self.added[fs.normalize(path)] = {
        kind = "string",
        contents = contents
    }
end

-- Adds a file to the map, reading the contents from another file on the disk
-- This doesn't modify the map in any way, it only adds the file to be written when either
-- map:writeToDir() or map:writeToMpq() is called
function mapMeta:addFileDisk(archivePath, filePath)
    self.added[fs.normalize(archivePath)] = {
        kind = "file",
        path = filePath
    }
end

-- Writes the map to a directory
-- Any files added to the map via map:addFileString() or map:addFileDisk() will be
-- written at this stage
function mapMeta:writeToDir(path)
    if self.kind == "dir" then
        fs.copyDir(self.path, path)
    elseif self.kind == "mpq" then
        self.archive:extractTo(path)
    end

    for _, v in ipairs(self.addedDirs) do
        fs.copyDir(v, path)
    end

    for k, v in pairs(self.added) do
        if v.kind == "string" then
            fs.writeFile(path .. k:gsub("%\\", "/"), v.contents)
        elseif v.kind == "file" then
            fs.copyFile(v.path, path .. k:gsub("%\\", "/"))
        end
    end
end

-- Writes the map to an mpq archive
-- Any files added to the map via map:addFileString() or map:addFileDisk() will be
-- written at this stage
function mapMeta:writeToMpq(path)
    local creator = mpq.create()

    if self.kind == "dir" then
        local success, errorMsg = creator:addFromDir(self.path)
        if not success then
            log("Couldn't add directory " .. self.path .. " to archive: " .. errorMsg)
        end
    elseif self.kind == "mpq" then
        local success, errorMsg = creator:addFromMpq(self.archive)
        if not success then
            log("Couldn't add files from another archive: " .. errorMsg)
        end
    end

    for _, v in ipairs(self.addedDirs) do
        creator:addFromDir(v)
    end

    for k, v in pairs(self.added) do
        if v.kind == "string" then
            creator:add(k, v.contents)
        elseif v.kind == "file" then
            local success, errorMsg = creator:addFromFile(k, v.path)
            if not success then
                log("Couldn't add file " .. k .. " to archive: " .. errorMsg)
            end
        end
    end

    return creator:write(path)
end

local objectExtensions = {
    "w3a", "w3t", "w3u", "w3b", "w3d", "w3h", "w3q"
}

function mapMeta:initObjectStorage(ext)
    local data = self:readFile("war3map." .. ext)
    local dataSD = self:readFile("war3mapSkin." .. ext)
    local dataHD = self:readFile("_HD.w3mod\\war3mapSkin." .. ext)

    local storage = objdata.newStore(ext)

    if data then
        storage:readFromString(data)
    end
    if dataSD then
        storage:readFromString(dataSD, dataHD and "sd" or nil)
    end
    if dataHD then
        storage:readFromString(dataHD, "hd")
    end

    return storage
end

-- Initializes object storages for the map
function mapMeta:initObjects()
    local objects = {}
    self.objects = objects

    for _, v in pairs(objectExtensions) do
        local data = self:initObjectStorage(v)
        objects[data.typestr] = data
    end

    local lightningData = objdata.newStore("lightning")
    objects[lightningData.typestr] = lightningData

    local soundData = objdata.newStore("sound")
    objects[soundData.typestr] = soundData
end

function mapMeta:commitObjectStorage(storage)
    if storage.isDirty then
        if storage.typestr == "lightning" then
            local dataSD = storage:writeToString("sd")
            self:addFileString("Splats\\LightningData.slk", dataSD)
            local dataHD = storage:writeToString("hd")
            self:addFileString("_HD.w3mod\\Splats\\LightningData.slk", dataHD)
        else if storage.typestr == "sound" then
            local dataSD = storage:writeToString("sd")
            self:addFileString("UI\\SoundInfo\\AbilitySounds.slk", dataSD)
            local dataHD = storage:writeToString("hd")
            self:addFileString("_HD.w3mod\\UI\\SoundInfo\\AbilitySounds.slk", dataHD)
        else
            local data = storage:writeToString()
            self:addFileString("war3map." .. storage.ext, data)
            local dataSD = storage:writeToString("sd")
            self:addFileString("war3mapSkin." .. storage.ext, dataSD)
            local dataHD = storage:writeToString("hd")
            self:addFileString("_HD.w3mod\\war3mapSkin." .. storage.ext, dataHD)
            self:addFileString("war3mapSkin.txt", (self:readFile("war3mapSkin.txt") or "") .. storage:writeSkinToString())
        end
    end
end

function mapMeta:commitObjects()
    for _, v in pairs(self.objects) do
        self:commitObjectStorage(v)
    end
end

local stringsMeta = {}
stringsMeta.__index = stringsMeta

local locales = { "ruRU", "frFR" }

function stringsMeta:includeFile(path)
    local strings, err = self._map:readFile(path)
    if err then
        error(err)
    end
    for key, value in string.gmatch(strings, "%s*(%S+)%s*\"(.-)\",") do
        self:set(key, value)
    end
    for _, locale in ipairs(locales) do
        local localeStrings = self._map:readFile("_Locales\\" .. locale .. ".w3mod\\" .. path)
        if localeStrings then
            for key, value in string.gmatch(localeStrings, "%s*(%S+)%s*\"(.-)\",") do
                self:set(key, value, locale)
            end
        end
    end
end

function stringsMeta:loadTOC(path)
    local toc, err = self._map:readFile(path)
    if err then
        error(err)
    end
    for file in toc:gmatch("([^\r\n]+)\r?\n?") do
        self:includeFile(file)
    end
end

function stringsMeta:localized(key)
    local id = self._stringIds[key]
    if not id then
        id = self._maxStringId + 1
        self._maxStringId = id
        self._stringIds[key] = id
    end
    return "TRIGSTR_" .. string.format("%.3d", id)
end

function stringsMeta:set(key, value, locale)
    local id = self._stringIds[key]
    if not id then
        id = self._maxStringId + 1
        self._maxStringId = id
        self._stringIds[key] = id
    end
    if not locale or not self._strings[id] then
        self._strings[id] = value
    end
    if locale then
        self._localeStrings[locale] = self._localeStrings[locale] or {}
        self._localeStrings[locale][id] = value
    end
end

function stringsMeta:get(key, locale)
    local id = self._stringIds[key]-- or string.match(key, "TRIGSTR_(%d+)")
    if locale and self._localeStrings[locale] then
        return self._localeStrings[locale][id] or self._strings[id]
    end
    return self._strings[id]
end

local function resolveString(s, tbl)
    return type(s) == "function" and s(tbl) or s
end

local stringFormat = string.format
local function stringTransformer(func, ...)
    local defaultRest = { ... }
    return function(...)
        local args = { ... }
        local localized = false
        for i = 1, #args do
            local arg = args[i]
            if type(arg) == "string" then
                local id = string.match(arg, "^TRIGSTR_(%d+)$")
                if id then
                    localized = true
                    break
                end
            elseif type(arg) == "table" then
                for _, value in pairs(arg) do
                    if type(value) == "string" then
                        local id = string.match(arg, "^TRIGSTR_(%d+)$")
                        if id then
                            localized = true
                            break
                        end
                    end
                end
                if localized then
                    break
                end
            end
        end
        if not localized or not currentMap then
            return func(...)
        end
        local strings = currentMap.strings
        local newId = strings._maxStringId + 1
        strings._maxStringId = newId
        strings._strings[newId] = function(tbl)
            local localeArgs = {}
            for i = 1, #args do
                local arg = args[i]
                if type(arg) == "string" then
                    local id = string.match(arg, "^TRIGSTR_(%d+)$")
                    if id then
                        id = tonumber(id)
                        localeArgs[i] = resolveString(tbl[id] or strings._strings[id], tbl) or arg
                    else
                        localeArgs[i] = arg
                    end
                elseif type(arg) == "table" then
                    local localeTable = {}
                    for key, value in pairs(arg) do
                        local id = string.match(value, "^TRIGSTR_(%d+)$")
                        if id then
                            id = tonumber(id)
                            localeTable[key] = resolveString(tbl[id] or strings._strings[id], tbl) or value
                        else
                            localeTable[key] = value
                        end
                    end
                    localeArgs[i] = localeTable
                else
                    localeArgs[i] = arg
                end
            end
            return func(table.unpack(localeArgs))
        end
        return "TRIGSTR_" .. stringFormat("%.3d", newId), table.unpack(defaultRest)
    end
end

string.format = stringTransformer(string.format)
string.gsub = stringTransformer(string.gsub, 0)
string.lower = stringTransformer(string.lower)
string.rep = stringTransformer(string.rep)
string.reverse = stringTransformer(string.reverse)
string.sub = stringTransformer(string.sub)
string.upper = stringTransformer(string.upper)

function warpack.openMap(name)
    local map = {
        added = {},
        addedDirs = {}
    }
    local mapPath = name

    if not fs.exists(mapPath) then
        return false, "map does not exist"
    end

    if fs.isDir(mapPath) then
        map.kind = "dir"

        map.path = mapPath .. "/"
    elseif fs.isFile(mapPath) then
        map.kind = "mpq"

        local archive, errorMsg = mpq.open(mapPath)

        if not archive then
            return false, errorMsg
        end

        map.archive = archive
    else
        return false, "map path is not a file or directory"
    end

    setmetatable(map, mapMeta)

    map:initObjects()

    return map
end

-- default build functionality

-- Describes the folder layout used by warpack.
-- Can be changed on a per-project basis.
warpack.layout = {
    mapsDirectory = "maps/",
    srcDirectories = { "src/", "lib/" },
    targetDirectory = "target/"
}

_G.WarMap = {
    TileSet =  {
        Ashenvale = "A",
        Barrens = "B",
        Felwood = "C",
        Dungeon = "D",
        LordaeronFall = "F",
        Underground = "G",
        LordaeronSummer = "L",
        Northrend = "N",
        VillageFall = "Q",
        Village = "V",
        LordaeronWinter = "W",
        Dalaran = "X",
        Cityscape = "Y",
        SunkenRuins = "Z",
        Icecrown = "I",
        DalaranRuins = "J",
        Outland = "O",
        BlackCitadel = "K",
    }
}

-- This is the default map build procedure
-- Takes a single "build command" specifying
-- what and how to build.
function warpack.buildMap(buildCommand)
    _G.lastBuildCommand = buildCommand

    local map, mapScript
    local mapName = buildCommand.input
    local output = buildCommand.output
    local outputType = buildCommand.target

    if not (outputType == "script" or outputType == "mpq" or outputType == "dir") then
        return false, "Output type must be one of 'mpq', 'dir' or 'script'"
    end

    if mapName == nil and (outputType == "mpq" or outputType == "dir") then
        return false, "Output type " .. outputType .. " requires an input map, but none was specified"
    end

    log("Received build command");
    log("    Input: " .. tostring(mapName))
    log("    Retain map script: " .. tostring(buildCommand.retainMapScript))
    log("    Output type: " .. buildCommand.target)
    log("    Target platform: " .. buildCommand.platform)

    local races = { "selectable", "human", "orc", "undead", "night elf" }
    local raceIds = {
        ["selectable"] = 0,
        ["human"] = 1,
        ["orc"] = 2,
        ["undead"] = 3,
        ["night elf"] = 4,
    }

    local controllers = { "none", "user", "computer", "neutral", "rescuable" }
    local controllerIds = {
        ["none"] = 0,
        ["user"] = 1,
        ["computer"] = 2,
        ["neutral"] = 3,
        ["rescuable"] = 4
    }

    local fogs = { "linear", "exponential 1", "exponential 2" }
    local fogIds = {
        ["linear"] = 1,
        ["exponential 1"] = 2,
        ["exponential 2"] = 3
    }

    if mapName ~= nil then
        local loadedMap, errorMsg = warpack.openMap(warpack.layout.mapsDirectory .. mapName)
        if errorMsg ~= nil then
            loadedMap, errorMsg = warpack.openMap(mapName)
            if errorMsg ~= nil then
                return false, "Could not load map " .. mapName .. ": " .. errorMsg
            end
        end
        log("Loaded map " .. mapName)

        if buildCommand.retainMapScript then
            local loadedScript, errorMsg = loadedMap:readFile("war3map.lua")
            if errorMsg ~= nil then
                log("WARN: Could not extract script from map " .. mapName .. ": " .. errorMsg)
                log("WARN: Map script won't be included in the final artifact")
            else
                log("Loaded map script from " .. mapName)
                mapScript = loadedScript
            end
        end

        local stringIds = {}
        local maxStringId = 0

        local function readStrings(file)
            local strings = {}
            for id, value in string.gmatch(file, "STRING (%d+).-{\r?\n?(.-)\r?\n?}") do
                id = tonumber(id)
                strings[id] = value
                stringIds["TRIGSTR_" .. string.format("%.3d", id)] = id
                maxStringId = math.max(maxStringId, id)
            end
            return strings
        end

        local loadedStrings, errorMsg = loadedMap:readFile("war3map.wts")
        if errorMsg ~= nil then
            return false, "Could not load map strings file (war3map.wts): "..errorMsg
        end

        local strings = readStrings(loadedStrings)
        local localeStrings = {}
        for _, locale in ipairs(locales) do
            local loadedLocaleStrings = loadedMap:readFile("_Locales\\" .. locale .. ".w3mod\\war3map.wts")
            if loadedLocaleStrings then
                localeStrings[locale] = readStrings(loadedLocaleStrings)
            end
        end

        loadedMap.strings = setmetatable({
            _map = loadedMap,
            _stringIds = stringIds,
            _maxStringId = maxStringId,
            _strings = strings,
            _localeStrings = localeStrings
        }, stringsMeta)

        local loadedEnv, errorMsg = loadedMap:readFile("war3map.w3e")
        if errorMsg ~= nil then
            return false, "Could not load map environment file (war3map.w3e): "..errorMsg
        end

        local _, w3eVersion, mainTileSet, _, tileSetLength, pos = string.unpack("<c4iBii", loadedEnv)
        loadedMap.mainTileSet = string.char(mainTileSet)
        local tileSet = {}
        for i = 1, tileSetLength do
            tileSet[i], pos = string.unpack(">i", loadedEnv, pos)
        end
        loadedMap.tileSet = tileSet

        local loadedInfo, errorMsg = loadedMap:readFile("war3map.w3i")
        if errorMsg ~= nil then
            return false, "Could not load map info file (war3map.w3i): "..errorMsg
        end

        local w3iVersion, saveCount, editorVersion, pos = string.unpack("<iii", loadedInfo)

        local warcraftVersion = nil
        if w3iVersion >= 28 then
            warcraftVersion, pos = string.unpack("<c16", loadedInfo, pos)
        end

        local name, author, description, suggestedPlayers, pos = string.unpack("<zzzz", loadedInfo, pos)

        local bounds, flags, mainGroundType, pos = string.unpack("<c56iB", loadedInfo, pos)

        local loadingScreenId, loadingScreenPath, loadingScreenText, loadingScreenTitle, loadingScreenSubtitle, pos = string.unpack("<izzzz", loadedInfo, pos)
        local loadingScreen = {}
        loadingScreen.type = loadingScreenId == -1 and (loadingScreenPath == "" and "default" or "custom") or "campaign"
        loadingScreen.id = loadingScreenId
        loadingScreen.title = loadingScreenTitle
        loadingScreen.subtitle = loadingScreenSubtitle
        loadingScreen.text = loadingScreenText
        loadingScreen.path = loadingScreenPath

        local dataSetId, pos = string.unpack("<i", loadedInfo, pos)
        local dataSet = dataSetId == 0 and "default" or (dataSetId == "1" and "custom" or "melee")

        local prologuePath, prologueText, prologueTitle, prologueSubtitle, pos = string.unpack("<zzzz", loadedInfo, pos)
        local prologue = {}
        prologue.title = prologueTitle
        prologue.subtitle = prologueSubtitle
        prologue.text = prologueText
        prologue.path = prologuePath

        local fogId, fogStartZ, forEndZ, fogDensity, fogRed, fogGreen, fogBlue, fogAlpha, pos = string.unpack("<ifffBBBB", loadedInfo, pos)
        local terrainFog = {
            type = fogId == 0 and "none" or fogs[fogId],
            zStart = fogStartZ,
            zEnd = forEndZ,
            density = fogDensity,
            color = {
                reg = fogRed,
                green = fogGreen,
                blue = fogBlue,
                alpha = fogAlpha
            }
        }

        local weatherId, pos = string.unpack("<i", loadedInfo, pos)

        local soundEnv, lightEnv, pos = string.unpack("<zB", loadedInfo, pos)

        local waterRed, waterGreen, waterBlue, waterAlpha, pos = string.unpack("<BBBB", loadedInfo, pos)
        local waterTintingColor = {
            red = waterRed,
            green = waterGreen,
            blue = waterBlue,
            alpha = waterAlpha
        }

        local scriptingLanguage = nil
        if w3iVersion >= 28 then
            scriptingLanguage, pos = string.unpack("<i", loadedInfo, pos)
        end

        local playersPrefix = nil
        if w3iVersion >= 31 then
            playersPrefix, pos = string.unpack("<c8", loadedInfo, pos)
        end

        local maxPlayers, pos = string.unpack("<i", loadedInfo, pos)
        local players = {}
        for _ = 1, maxPlayers do
            local id, controllerId, raceId, fixedStart, playerName, startX, startY, allyLow, allyHigh, newPos = string.unpack("<iiiizffii", loadedInfo, pos)
            local enemyLow, enemyHigh = nil, nil
            if w3iVersion >= 31 then
                enemyLow, enemyHigh, newPos = string.unpack("<ii", loadedInfo, newPos)
            end
            players[id] = {
                name = playerName,
                race = races[raceId + 1],
                controller = controllers[controllerId + 1],
                fixedStartLocation = fixedStart ~= 0,
                startLocationX = startX,
                startLocationY = startY,
                allyLow = allyLow,
                allyHigh = allyHigh,
                enemyLow = enemyLow,
                enemyHigh = enemyHigh,
            }
            pos = newPos
        end
        local forces = {}
        local maxForces
        maxForces, pos = string.unpack("<i", loadedInfo, pos)
        for i = 1, maxForces do
            local flags, mask, forceName, newPos = string.unpack("<iiz", loadedInfo, pos)
            local forcePlayers = {}
            for id = 0, 23 do
                if (mask & (1 << id)) ~= 0 then
                    forcePlayers[#forcePlayers + 1] = id
                end
            end
            forces[i] = {
                name = forceName,
                allied = flags & 0x1 ~= 0,
                alliedVictory = flags & 0x2 ~= 0,
                shareVision = flags & 0x4 ~= 0,
                shareUnitControl = flags & 0x10 ~= 0,
                shareAdvancedUnitControl = flags & 0x20 ~= 0,
                players = forcePlayers,
            }
            pos = newPos
        end
        if flags & 0x40 == 0 then
            forces = {}
            maxForces = 0
        end
        local postfix = string.sub(loadedInfo, pos)

        loadedMap.w3iVersion = w3iVersion
        loadedMap.saveCount = saveCount
        loadedMap.editorVersion = editorVersion
        loadedMap.warcraftVersion = warcraftVersion
        loadedMap.name = name
        loadedMap.author = author
        loadedMap.description = description
        loadedMap.suggestedPlayers = suggestedPlayers
        loadedMap.bounds = bounds
        loadedMap.flags = flags
        loadedMap.mainGroundType = mainGroundType
        loadedMap.loadingScreen = loadingScreen
        loadedMap.dataSet = dataSet
        loadedMap.prologue = prologue
        loadedMap.terrainFog = terrainFog
        loadedMap.globalWeatherId = weatherId
        loadedMap.customSoundEnvironment = soundEnv
        loadedMap.customLightEnvironment = lightEnv
        loadedMap.waterTintingColor = waterTintingColor
        loadedMap.scriptingLanguage = scriptingLanguage
        loadedMap.playersPrefix = playersPrefix
        loadedMap.players = players
        loadedMap.forces = forces
        loadedMap.infoPostfix = postfix

        map = loadedMap
    end

    if map == nil then
        log("Building in script-only mode")
    end

    if mapScript == nil then
        log("Building without including original map script")
    end

    _G.currentMap = map
    _G.TARGET_PLATFORM = buildCommand.platform

    mapScript = callHooks(preScriptBuildHooks, map, mapScript) or mapScript

    local script, errorMsg = warpack.compileScript {
        srcDirectories = warpack.layout.srcDirectories,
        mapScript = mapScript or "",
        platform = buildCommand.platform
    }

    if errorMsg ~= nil then
        return false, "Map build failed: "..errorMsg
    end

    script = callHooks(postScriptBuildHooks, map, script) or script

    if map ~= nil then
        local function writeStrings(localeStrings)
            localeStrings = localeStrings or {}
            local strings = {}
            for id, value in pairs(map.strings._strings) do
                strings[#strings + 1] = "STRING " .. tostring(id) .. "\r\n{\r\n" .. resolveString(localeStrings[id] or value, localeStrings) .. "\r\n}\r\n"
            end
            return table.concat(strings, "\r\n")
        end

        map:addFileString("war3map.wts", writeStrings())

        for locale, strings in pairs(map.strings._localeStrings) do
            map:addFileString("_Locales\\" .. locale .. ".w3mod\\war3map.wts", writeStrings(strings))
        end

        local players = {}
        for id, player in pairs(map.players) do
            local playerStr = {}
            table.insert(playerStr, string.pack(
                "<iiiizffii",
                id,
                controllerIds[player.controller],
                raceIds[player.race],
                player.fixedStartLocation and 1 or 0,
                player.name,
                player.startLocationX,
                player.startLocationY,
                player.allyLow or 0,
                player.allyHigh or 0
            ))
            if map.w3iVersion >= 31 then
                table.insert(playerStr, string.pack("<ii", player.enemyLow or 0, player.enemyHigh or 0))
            end
            players[#players + 1] = table.concat(playerStr)
        end
        table.sort(players)

        local forces = {}
        for _, force in ipairs(map.forces) do
            local flags = 0
            if force.allied then
                flags = flags | 0x1
            end
            if force.alliedVictory then
                flags = flags | 0x2
            end
            if force.shareVision then
                flags = flags | 0x4
            end
            if force.shareUnitControl then
                flags = flags | 0x10
            end
            if force.shareAdvancedUnitControl then
                flags = flags | 0x20
            end

            local mask = 0
            for _, id in ipairs(force.players) do
                mask = mask | (1 << id)
            end

            forces[#forces + 1] = string.pack("<iiz", flags, mask, force.name)
        end
        if next(forces) then
            map.flags = map.flags | 0x40
        end

        local loadingScreen = map.loadingScreen
        local prologue = map.prologue
        local terrainFog = map.terrainFog
        local terrainFogColor = terrainFog.color or {
            red = 255,
            green = 255,
            blue = 255,
            alpha = 255
        }
        local waterTintingColor = map.waterTintingColor

        local w3iVersion = map.w3iVersion
        local w3i = {}
        table.insert(w3i, string.pack("<iii", w3iVersion, map.saveCount, map.editorVersion))
        if w3iVersion >= 28 then
            table.insert(w3i, string.pack("<c16", map.warcraftVersion or "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"))
        end
        table.insert(w3i, string.pack(
            "zzzzc56iBizzzzizzzzifffBBBBizBBBBB",
            map.name,
            map.author,
            map.description,
            map.suggestedPlayers,
            map.bounds,
            map.flags,
            map.mainGroundType,
            loadingScreen.type == "campaign" and map.loadingScreen.id or -1,
            loadingScreen.path or "",
            loadingScreen.text or "",
            loadingScreen.title or "",
            loadingScreen.subtitle or "",
            map.dataSet == "default" and 0 or (map.dataSet == "custom" and 1 or 2),
            prologue.path,
            prologue.text,
            prologue.title,
            prologue.subtitle,
            terrainFog.type == "none" and 0 or fogIds[map.terrainFog.type],
            terrainFog.zStart or 0,
            terrainFog.zEnd or 0,
            terrainFog.density or 0,
            terrainFogColor.red or 255,
            terrainFogColor.green or 255,
            terrainFogColor.blue or 255,
            terrainFogColor.alpha or 255,
            map.globalWeatherId,
            map.customSoundEnvironment,
            map.customLightEnvironment,
            waterTintingColor.red or 255,
            waterTintingColor.green or 255,
            waterTintingColor.blue or 255,
            waterTintingColor.alpha or 255
        ))
        if w3iVersion >= 28 then
            table.insert(w3i, string.pack("<i", 1)) -- scripting language: Lua
        end
        if w3iVersion >= 31 then
            table.insert(w3i, string.pack("<c8", map.playersPrefix or "\0\0\0\0\0\0\0\0"))
        end
        table.insert(w3i, string.pack("<i", #players))
        table.insert(w3i, table.concat(players))
        table.insert(w3i, string.pack("<i", #forces))
        table.insert(w3i, table.concat(forces))
        table.insert(w3i, map.infoPostfix)

        map:addFileString("war3map.w3i", table.concat(w3i))
        map:addFileString("war3map.lua", script)
        map:commitObjects()
    end

    callHooks(postMapBuildHooks, map)

    log("Successfully built the map")

    local artifact = {}

    local result, errorMsg
    if outputType == "script" then
        log("Writing artifact [script] to " .. warpack.layout.targetDirectory .. "war3map.lua")
        artifact.type = "script"
        artifact.path = output or warpack.layout.targetDirectory .. "war3map.lua"
        artifact.content = script
        result, errorMsg = fs.writeFile(warpack.layout.targetDirectory .. "war3map.lua", script)
    elseif outputType == "mpq" then
        artifact.type = "mpq"
        artifact.path = output or warpack.layout.targetDirectory .. mapName
        log("Writing artifact [mpq] to " .. artifact.path)
        result, errorMsg = map:writeToMpq(artifact.path)
    elseif outputType == "dir" then
        artifact.type = "dir"
        artifact.path = output or warpack.layout.targetDirectory .. mapName .. ".dir/"
        log("Writing artifact [dir] to " .. artifact.path)
        result, errorMsg = map:writeToDir(artifact.path)
    end

    if errorMsg then
        return false, "Saving the artifact failed: "..errorMsg
    else
        log("Build complete!")
        return artifact
    end
end

-- arg parsing
local args = warpack.getScriptArgs()

arg = {
    exists = function(arg_name)
        for _, v in pairs(args) do
            if v == arg_name then
                return true
            end
        end
        return false
    end,
    value = function(arg_name)
        local arg_pos
        for i, v in ipairs(args) do
            if v == arg_name then
                arg_pos = i
                break
            end
        end

        if arg_pos ~= nil and #args > arg_pos then
            return args[arg_pos + 1]
        end
    end
}

-- default handler

local handlerSuppressed = false

function warpack.suppressDefaultHandler()
    handlerSuppressed = true
end

-- The default handler for "build" and "run" commands in warpack
-- Will parse the arguments and invoke warpack.buildMap()
function warpack.defaultHandler()
    if handlerSuppressed then
        return
    end

    local mapArg = arg.value("--map") or arg.value("-m")
    local output = arg.value("--output") or arg.value("-o")
    local outputType = arg.value("--target") or arg.value("-t") or "mpq"
    local noKeepScript = arg.exists("--no-map-script") or false
    local platform = arg.value("--platform") or arg.value("-p") or "reforged"

    for _, v in pairs(warpack.layout.srcDirectories) do
        package.path = package.path .. ";./" .. v .. "/?.lua"
    end

    local artifact = assert(warpack.buildMap {
        input = mapArg,
        output = output,
        target = outputType,
        retainMapScript = not noKeepScript,
        platform = platform
    })

    if warpack.runMode() == "run" then
        if not artifact or artifact.type == "script" then
            log("WARN: Runmap was requested, but the current build did not produce a runnable artifact...")
        elseif warpack.runConfig == nil then
            log("WARN: Runmap was requested, but warpack.runConfig is nil!")
        else
            log("Runmap was requested, running the map...")
            warpack.runMap(artifact.path)
        end
    end
end

function warpack.runMap(path)
    local _, errorMsg = warpack.runWarcraft(path, warpack.runConfig)
    if errorMsg ~= nil then
        log("WARN: Running the map failed.")
        log(errorMsg)
        return
    end

    callHooks(postRunHooks)
end