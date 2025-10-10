--[[ warpack map header start ]]
warpack = warpack or {}
warpack.modules = {}

warpack.initialized = warpack.initialized or false

do
    function _G.print(...)
        local args = { ... }
        local msgs = {}

        for i, v in ipairs(args) do
            msgs[i] = tostring(v)
        end

        local msg = table.concat(msgs, " ")
        DisplayTimedTextToPlayer(GetLocalPlayer(), 0, 0, 60, msg)
    end

    warpack.hooks = warpack.hooks or {
        ["reload::before"] = {},
        ["reload::after"] = {},
    }

    function warpack.hookCall(hookName)
        for _, callback in ipairs(warpack.hooks[hookName]) do
            callback()
        end
    end

    function warpack.addHook(hookName, callback)
        if not warpack.hooks[hookName] then
            error(("can't register non-existent warpack hook '%s'"):format(hookName))
        end

        table.insert(warpack.hooks[hookName], warpack.wrapSafeCall(callback))
    end

    local handleError = function (err)
        print("ERROR: " .. err)
    end

    function warpack.setErrorHandler(errorHandler)
        handleError = errorHandler
    end

    local cocreate = coroutine.create
    local coresume = coroutine.resume
    local pcall = pcall

    local function safeCall(callback, ...)
        local success, err = pcall(callback, ...)

        if not success then
            handleError(err)
            return false
        else
            return true, err
        end
    end

    warpack.safeCall = safeCall

    function warpack.wrapSafeCall(callback)
        return function(...)
            safeCall(callback, ...)
        end
    end

    function warpack.wrapThread(callback)
        return function(...)
            coresume(cocreate(function(...)
                local success, err = pcall(callback, ...)

                if not success then
                    handleError(err)
                end
            end), ...)
        end
    end

    _G.require = function(name, optional)
        local module = warpack.modules[name]

        if module ~= nil then
            if module.initialized then
                return module.cached or module.error and error(module.error)
            else
                module.initialized = true
                local compiled, err = load(module.source, "module " .. name)
                if not compiled then
                    module.error = "failed to compile module " .. name .. ": " .. err
                    error(module.error)
                end

                local success, v = pcall(compiled)
                if not success then
                    module.error = v
                    error(v)
                end
                module.cached = v
                return module.cached
            end
        elseif not optional then
            error("module not found " .. name)
        end
    end

    local mapInit = false

    local mainSuppressed = false
    local configSuppressed = false
    local afterMapInit = {}
    local afterMain = {}

    function warpack.afterMapInit(callback)
        if not mapInit then
            afterMapInit[#afterMapInit + 1] = callback
        else
            callback()
        end
    end

    function warpack.afterMain(callback)
        if not warpack.initialized then
            afterMain[#afterMain + 1] = callback
        else
            callback()
        end
    end

    function warpack.suppressDefaultMain()
        mainSuppressed = true
    end

    function warpack.suppressDefaultConfig()
        configSuppressed = true
    end

    function warpack.init()
        if not warpack.initialized then
            warpack.oldMain = main or function() end
            warpack.oldConfig = config or function() end

            if warpack.modules["lualib_bundle"] then
                local __TS__Promise
                local __TS__InstanceOf
                do
                    local lualib_bundle = require("lualib_bundle")
                    __TS__Promise = lualib_bundle.__TS__Promise
                    __TS__InstanceOf = lualib_bundle.__TS__InstanceOf
                end
                local function handlePromiseError(_, err)
                    handleError(err)
                end
                safeCall = function (callback, ...)
                    local success, result = pcall(callback, ...)
                    if not success then
                        handleError(result)
                        return false
                    elseif __TS__InstanceOf(result, __TS__Promise) then
                        return true, result:catch(handlePromiseError)
                    end
                    return true, result
                end
                warpack.safeCall = safeCall
            end

            local success, err
            function _G.main()
                mapInit = true

                if warpack.modules["root"] and not success then
                    print("|c00ff0000CRITICAL ERROR:|r Root script failed to load:\n")
                    print(err)
                end

                for i = 1, #afterMapInit do
                    afterMapInit[i]()
                end
                afterMapInit = nil

                if not mainSuppressed then
                    warpack.safeCall(warpack.oldMain)
                end
                if warpack.modules["main"] then
                    warpack.safeCall(require, "main")
                end

                warpack.initialized = true

                for i = 1, #afterMain do
                    afterMain[i]()
                end
            end

            function _G.config()
                if not configSuppressed then
                    warpack.safeCall(warpack.oldConfig)
                end
                if warpack.modules["config"] then
                    warpack.safeCall(require, "config")
                end
            end

            if warpack.modules["root"] then
                success, err = pcall(require, "root")
            end
        else
            warpack.hookCall("reload::before")
            warpack.hooks["reload::before"] = {}
            warpack.hooks["reload::after"] = {}
            local success, error = pcall(require, "main")
            if not success then
                print("|c00ff0000CRITICAL ERROR:|r Main map script failed to reload:\n")
                print(tostring(error))
                return
            end
            warpack.hookCall("reload::after")
        end
    end
end
--[[ warpack map header end ]]