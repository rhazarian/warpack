/** @noselfinfile */

declare namespace warpack {
    function afterMapInit(callback: () => void): void

    function afterMain(callback: () => void): void

    function setErrorHandler(handler: (this: void, err: any) => any): void

    function safeCall<This, Args extends any[], R>(
        f: (this: This, ...args: Args) => R,
        context: This,
        ...args: Args
    ): LuaMultiReturn<[false] | [true, R]>

    /**
     * Simple alternative to `pcall`, will safely call the
     * provided function and print the error if any occured,
     * or return the function result if succeeded.
     * @runtime
     */
    function safeCall<Args extends any[], R>(f: (...args: Args) => R, ...args: Args): LuaMultiReturn<[false] | [true, R]>

    /**
     * Wraps the provided function in a `safeCall`, such that
     * when the returned function is called, any produced
     * errors will be handled by `safeCall`.
     * @runtime
     */
    function wrapSafeCall<C extends (this: void, ...args: any[]) => any>(f: C): C

    function wrapThread<Args extends any[], R>(f: (...args: Args) => R): (...args: Args) => void

    /**
     * Suppresses the default main function, if there exists one.
     * Should be called from within the `init` module.
     */
    function suppressDefaultMain(): void

    /**
     * Suppresses the default config function, if there exists one.
     * Should be called from within the `init` module.
     */
    function suppressDefaultConfig(): void

    /**
     * Flag to indicate whether the currently executing code
     * is running at compiletime.
     *
     * This can be used in shared code to distinguish between
     * runtime and compiletime executions.
     * @compiletime
     */
    const compiletime: boolean

    /**
     * Configuration used by Warpack to determine how to launch
     * Warcraft III when running Warpack via `warpack run`.
     * @compiletime
     */
    let runConfig: RunMapOptions

    /**
     * Configuration used by Warpack to get info about the directory
     * layout of the project.
     * @compiletime
     */
    let layout: WarpackLayout

    /**
     * Adds a hook to be called before the map script has been built by Warpack.
     * The callback receives the map object being built and the map's original script, if one exists.
     * The callback can be used to override the original map script by returning a string.
     *
     * `name` is used to distinguish between hooks when building maps multiple times.
     * @compiletime
     */
    function addPreScriptBuildHook(
        name: string,
        callback: (map: WarMap, mapScript: string) => void
    ): void

    /**
     * Adds a hook to be called after the map script has been built by Warpack.
     * The callback receives the map object being built and the map's script after it has been compiled by Warpack.
     * The callback can be used to override the compiled map script by returning a string.
     *
     * `name` is used to distinguish between hooks when building maps multiple times.
     * @compiletime
     */
    function addPostScriptBuildHook(
        name: string,
        callback: (map: WarMap, compiledScript: string) => void
    ): void

    /**
     * Adds a hook to be called after the map has been built by Warpack, but before it was written out to disk as either MPQ or dir.
     * The callback receives the map object that has just been built.
     *
     * `name` is used to distinguish between hooks when building maps multiple times.
     * @compiletime
     */
    function addPostMapBuildHook(name: string, callback: (map: WarMap) => void): void

    /**
     * Adds a hook to be called after the game has been launched by Warpack.
     *
     * `name` is used to distinguish between hooks when building maps multiple times.
     * @compiletime
     */
    function addPostRunHook(name: string, callback: () => void): void

    /**
     * Opens a Warcraft III map at the specified path.
     * Warpack will auto-detect whether the map is in directory format or MPQ format.
     *
     * If loading the map failed for whatever reason, will return `false` and an error message instead.
     * @compiletime
     */
    function openMap(name: string): WarMap | LuaMultiReturn<[false, string]>

    /**
     * Compiles a script with the given parameters.
     *
     * Will try to locate "main", "init" and "config" modules as entry points.
     * @compiletime
     */
    function compileScript(options: CompileOptions): string | LuaMultiReturn<[false, string]>

    /**
     * Builds a map with the given command.
     *
     * @compiletime
     */
    function buildMap(command: BuildCommand): BuildArtifact | false

    /**
     * Launches Warcraft III with the map at the specified path, and
     * using @see warpack.runConfig for configuration.
     * @compiletime
     */
    function runMap(path: string): void

    /**
     * Returns the argumen string passed to the build script.
     * E.g. if Warpack was launched with
     * `warpack run -- --map mpq.w3x --output dir`
     * this will return the string
     * ` --map mpq.w3x --output dir`
     * @compiletime
     */
    function getScriptArgs(): string

    /**
     * Suppress default build handler.
     * Used for custom workflows.
     * @compiletime
     */
    function suppressDefaultHandler(): void
}

declare namespace fs {
    /**
     * Reads the file at the specified path, returning its contents
     * as a string if successful, or false and an error if failed.
     * @compiletime
     */
    function readFile(path: string): LuaMultiReturn<[string] | [false, string]>

    /**
     * Writes a file at the specified path and content,
     * returning nothing if successful, and false and an error if failed.
     * @compiletime
     */
    function writeFile(path: string, content: string): LuaMultiReturn<[undefined] | [false, string]>

    /**
     * Copies a file from a source to another destiantion,
     * returning nothing if successful, and false and an error if failed.
     * @compiletime
     */
    function copyFile(from: string, to: string): LuaMultiReturn<[undefined] | [false, string]>

    /**
     * Copies one directory into another, recursively. Returns true if successful,
     * false and an error otherwise.
     *
     * Specifically, contents in `from/` will appear at `to/`, e.g. a file like
     * `from/a/b/c.txt` will be copied to `to/a/b/c.txt`, if copying from `from` to `to`.
     *
     * @compiletime
     */
    function copyDir(from: string, to: string): LuaMultiReturn<[true] | [false, string]>

    /**
     * Reads directories and files at the specified directory,
     * returning them as two arrays, first files, then directories if successful,
     * or false and an error if failed
     *
     * @compiletime
     */
    function readDir(path: string): LuaMultiReturn<[[string], [string]] | [false, string]>

    /**
     * Tests if the specified path is a directory.
     *
     * @compiletime
     */
    function isDir(path: string): boolean

    /**
     * Tests if the specified path is a file.
     *
     * @compiletime
     */
    function isFile(path: string): boolean

    /**
     * Tests if anything exists at the specified path.
     *
     * @compiletime
     */
    function exists(path: string): boolean

    /**
     * 'Absolutizes' a path, meaning that it takes a relative
     * path and transforms it into an absolute path, regardless
     * of whether or not this path actually exists.
     *
     * Returns false and an error if failed.
     *
     * @compiletime
     */
    function absolutize(path: string): LuaMultiReturn<[string] | [false, string]>

    /**
     * 'Watches' a file for changes, calling the specified callback
     * each time a change occurs, with the new file contents as the argument.
     *
     * Will only exit if an error occurs.
     * @compiletime
     */
    function watchFile(path: string, callback: (data: string) => void): LuaMultiReturn<[false, string]>

    /**
     * Removes a file,
     * returning nothing if successful, and false and an error if failed.
     * @compiletime
     */
    function removeFile(path: string): LuaMultiReturn<[undefined] | [false, string]>

    /**
     * Removes a directory,
     * returning nothing if successful, and false and an error if failed.
     *
     * Only removes empty directories.
     * @compiletime
     */
    function removeDir(path: string): LuaMultiReturn<[undefined] | [false, string]>
}

declare namespace mpq {
    /**
     * Creates a new MPQ builder.
     * @compiletime
     */
    function create(): MpqBuilder

    /**
     * Opens an MPQ archive for reading,
     * returning false and an error if failed.
     * @compiletime
     */
    function open(path: string): LuaMultiReturn<[MpqViewer] | [false, string]>
}

declare namespace arg {
    /**
     * Returns true if the given arg appeared in the build script arguments.
     *
     * E.g. `arg.exists("--flag")` will check if the command has had `--flag` passed to it.
     * @compiletime
     */
    function exists(arg: string): boolean

    /**
     * Returns the value of the given arg if it appeared in the build script arguments.
     *
     * E.g. `arg.value("--map")` will return `mpq.w3x` if the command has had `--map mpq.w3x` passed to it.
     * @compiletime
     */
    function value(arg: string): string | undefined
}

/**
 * A map build request.
 * Encapsulates possible parameters to ask of Warpack when building a map.
 * @compiletime
 */
interface BuildCommand {
    /**
     * Path to the map file. Can be omitted to make a script-only, mapless compilation.
     */
    input?: string

    /**
     * Type of artifact to produce.
     */
    output: "script" | "mpq" | "dir"

    /**
     * Whether to retain the original map script during building.
     */
    retainMapScript: boolean
}

/**
 * A build artifact produced by a map compilation step.
 * @compiletime
 */
interface BuildArtifact {
    /**
     * Type of artifact produced.
     */
    type: "script" | "mpq" | "dir"

    /**
     * Filesystem path to the produced artifact.
     */
    path: string

    /**
     * If the produced artifact was a script, this will be its contents.
     */
    content?: string
}

/**
 * A view into an MPQ archive.
 *
 * @compiletime
 */
declare interface MpqViewer {
    /**
     * Reads a file from this MPQ archive.
     *
     * Returns false and an error if failed.
     */
    readFile(path: string): LuaMultiReturn<[string] | [false, string]>

    /**
     * Reads a list of all files from this MPQ archive.
     */
    files(): [string]

    /**
     * Extracts all the files in this archive to the specified path.
     *
     * Return false an an error if failed.
     */
    extractTo(path: string): LuaMultiReturn<[true] | [false, string]>
}

/**
 * Possible options to specify when adding a file to an MPQ archive.
 * @compiletime
 */
declare interface MpqAddOptions {
    /**
     * Encrypts the file using MPQ encryption.
     *
     * Not actually useful, just a waste of CPU time.
     */
    encrypt: boolean

    /**
     * Compresses the file. True by default.
     */
    compress: boolean

    includeInListfile: boolean
}

/**
 * A builder of MPQ archives.
 *
 * @compiletime
 */
declare interface MpqBuilder {
    /**
     * Adds a file to the MPQ archive with the specified path and content.
     */
    add(path: string, content: string, options?: MpqAddOptions): void

    /**
     * Adds a file to the MPQ archive from disk.
     *
     * Returns false and an error if reading the file failed.
     */
    addFromFile(
        archivePath: string,
        diskPath: string,
        options?: MpqAddOptions
    ): LuaMultiReturn<[true] | [false, string]>

    /**
     * Adds the contents of the specified directory to the MPQ archive.
     *
     * Returns false and an error if reading the directory (or its files) failed.
     */
    addFromDir(path: string, options?: MpqAddOptions): LuaMultiReturn<[true] | [false, string]>

    /**
     * Adds the contents of another MPQ archive to this one.
     *
     * Returns false and an error if adding the contents failed.
     */
    addFromMpq(other: MpqViewer, options?: MpqAddOptions): LuaMultiReturn<[true] | [false, string]>

    /**
     * Builds and writes this archive to the specified path.
     *
     * Returns false and an error if writing failed.
     */
    write(path: string): LuaMultiReturn<[true] | [false, string]>
}

/**
 * Directory layout used by the default buildscript.
 *
 * @compiletime
 */
declare interface WarpackLayout {
    /**
     * Where to find maps. This is used by the command line
     * to avoid having to type `--map maps/my.w3x` all the time.
     */
    mapsDirectory: string

    /**
     * Directories to be tried, in order, by the module resolution
     * algorithm of Warpack when compiling the script.
     */
    srcDirectories: string[]

    /**
     * Where to put build artifacts.
     */
    targetDirectory: string
}

/**
 * Options that are passed to @see warpack.runMap
 *
 * @compiletime
 */
declare interface RunMapOptions {
    /**
     * Command used to launch Warcraft III.
     *
     * This can be anything so long as your system can properly execute this command.
     */
    command: string

    /**
     * Workaround for Linux users. If you are launching Warcraft III via Wine, this parameter
     * can be used to prefix all map paths with it. This can turn a path like
     * `/path/to/map` into `Z:/path/to/map`.
     */
    prefix?: string

    /**
     * Additional arguments to be passed to Warcraft III. This can be anything.
     * Multi-word arguments should be separated.
     *
     * E.g: ["-windowmode", "windowed"] instead of ["-windowmode windowed"]
     */
    args?: string[]
}

/**
 * Options that are passed to @see warpack.compileScript
 *
 * @compiletime
 */
declare interface CompileOptions {
    /**
     * Directories to look for modules in.
     *
     * Order determines priority.
     */
    srcDirectories: [string]

    /**
     * The map script, if any, to include in the compilation process.
     *
     * Will be included verbatim in the resulting lua file.
     */
    mapScript: string
}

type objectType = "unit" | "item" | "ability" | "destructable" | "doodad" | "buff" | "upgrade"
type objectExt = "w3a" | "w3t" | "w3u" | "w3b" | "w3d" | "w3h" | "w3q"

/**
 * A Warcraft III object data entity. Represents anything from units to doodads.
 *
 * @compiletime
 */
declare interface WarObject {
    /** All fields available on this object */
    all: string[]

    /** This object's id as a string. */
    id: string

    /** Parent's id as a string. Undefined if this object has no parent, i.e. it is defined by Warcraft III data itself. */
    parentId: string | undefined

    /** This object's type. */
    type: objectType

    /**
     * Clones this object, creating an independent instance of it.
     *
     * This instance can then be assigned into a `WarObjects` object
     * to insert it into the map.
     */
    clone(): WarObject

    /**
     * Gets a field on this object.
     *
     * Two syntaxes are accepted for the `field` parameter:
     * 1) SLK-like, where fields have a 'proper' name, sometimes postfixed with
     * a number to indicate which level of the field to get/set. Examples include,
     * `Name`, `DataA1`, `DataC10`, etc.
     * 2) Raw, using a four-character rawid, optionally with a `+X` postfix
     * to indicate which level to use, for fields which can have multiple levels.
     * E.g.: `unam`, `xxxx+1`, `xxxx+10`
     *
     * The returned value will be automatically converted to the appropriate type.
     * Warcraft III has 4 fundamental types for fields:
     * real - standard float value
     * unreal - float value clamped between 0.0 and 1.0
     * int - standard signed integer value
     * string - null-terminated string
     *
     * Setting a field into null/undefined will reset it to its default value.
     */
    getField(field: string): string | number | undefined

    /**
     * Sets a field on this object.
     *
     * See `getField` for field name syntax.
     *
     * Setting a field into null/undefined will reset it to its default value.
     */
    setField(field: string, value: string | number | undefined): void

    /**
     * Alternative syntax for setField/getField. Due to TypeScript limitations, this doesn't really
     * work that well in TypeScript, however, it is available and may be more convenient in pure Lua projects.
     */
    [id: string]: any
}

/**
 * Encapsulates all Warcraft III objects of a particular type in a map.
 *
 * @compiletime
 */
declare interface WarObjects {
    /** All Warcraft III objects of this type in the map. Meant for iteration. */
    all: Record<string, WarObject | undefined>

    /** Extension string for this Warcraft III object type. */
    ext: objectExt

    /** Type string for this Warcraft III object type. */
    typestr: objectType

    /** Marker whether any object data here has been modified since loading. Used by Warpack to prevent writing out unmodified object stores. */
    isDirty: boolean

    /**
     * Reads Warcraft III objects from the supplied string into this object.
     * The expected format is that of Warcraft III object data files, which is the same for all object data types.
     */
    readFromString(data: string): void

    /**
     * Writes the Warcraft III objects contained in this object out into the string.
     * The resulting format is that of Warcraft III object data files, which is the same for all object data types.
     */
    writeToString(): string

    /**
     * Gets a reference to a Warcraft III object inside this storage.
     *
     * Index is a rawid. Upon getting an object, you get a
     * reference which you can use to mutate it.
     *
     * For example:
     *
     * // getting a reference to an object
     * let myFoo = currentMap.objects.units['hfoo'] as WarObject
     * // this will mutate the 'hfoo' unit
     * myFoo['Name'] = "Crab"
     * // this will create a clone of the "Crab" 'hfoo' with the id 'xxxx'
     * currentMap.objects.units['xxxx'] = myFoo
     *
     * // this will get a clone instead, which won't modify the original object
     * let myPea = (currentMap.objects.units['hpea'] as WarObject).clone()
     * // this will only modify the clone now
     * myPea['Name'] = "Arnold"
     * currentMap.objects.units['yyyy'] = myPea
     */
    getObject(rawid: string): WarObject

    /**
     * Sets a Warcraft III object into this storage.
     *
     * Index is a rawid.
     *
     * When setting an object, Warpack will clone the target
     * and set the clone's id to that specified here.
     *
     * Setting an object into null/undefined will reset it to defaults for a stock
     * object, and delete it for a custom object.
     *
     * See `getObject` for example usage.
     */
    setObject(rawid: string, object: WarObject): void

    /**
     * Creates a new Warcraft III object inside this storage and returns a reference to it.
     */
    newObject(rawid: string, rawparentid: string): WarObject

    /**
     * Alternative syntax for setObject/getObject. Due to limitations in TypeScript,
     * it's not very useful in TypeScript, however can still be used in pure Lua projects
     * where it may be more convenient.
     */
    [id: string]: any
}

/**
 * Encapsulates all objects of all types in a map.
 *
 * @compiletime
 */
declare interface WarMapObjects {
    ability: WarObjects
    item: WarObjects
    unit: WarObjects
    destructable: WarObjects
    doodad: WarObjects
    buff: WarObjects
    upgrade: WarObjects
    lightning: WarObjects
    sound: WarObjects
}

type Locale = "ruRU" | "frFR"

declare namespace WarMap {
    export enum TileSet {
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

/**
 * Encapsulates a Warcraft III map during the compilation stage.
 *
 * @compiletime
 */
declare interface WarMap {
    /** Reference to all object storage in this map. */
    objects: WarMapObjects

    name: string
    author: string
    description: string
    suggestedPlayers: string
    players: {
        [id: number]: {
            name: string
            race: "human" | "orc" | "undead" | "night elf"
            controller: "user" | "computer" | "neutral" | "rescuable"
            fixedStartLocation: boolean
            startLocationX: number
            startLocationY: number
        }
    }
    forces: {
        name: string
        allied: boolean
        alliedVictory: boolean
        shareVision: boolean
        shareUnitControl: boolean
        shareAdvancedUnitControl: boolean
        players: number[]
    }[]
    dataSet: "default" | "custom" | "melee"
    terrainFog:
        | {
              type: "none"
          }
        | {
              type: "linear" | "exponential 1" | "exponential 2"
              zStart: number
              zEnd: number
              density: number
              color: {
                  red: number
                  green: number
                  blue: number
                  alpha: number
              }
          }
    waterTintingColor: {
        red: number
        green: number
        blue: number
        alpha: number
    }
    loadingScreen:
        | {
              type: "default"
          }
        | {
              type: "campaign"
              id: number
              title: string
              subtitle: string
              text: string
          }
        | {
              type: "custom"
              path: string
          }

    readonly mainTileSet: WarMap.TileSet
    readonly tileSet: ReadonlyArray<number>

    readonly strings: {
        includeFile(path: string): void
        loadTOC(path: string): void

        localized(key: string): string

        set(key: string, value: string, locale?: Locale): void
        get(key: string, locale?: Locale): string | undefined
    }

    /**
     * Reads a file from the map. Returns the file contents as a
     * string upon success, or false + an error message upon failure.
     *
     * This function will also "see" any files previously added to the map
     * via `addFileString`, `addFileDisk` or `addDir`.
     */
    readFile(path: string): LuaMultiReturn<[string] | [false, string]>

    /**
     * Adds a directory to the map, preserving the file hierarcy.
     * Note: the directory will only be written when writeToDir() or writeToMpq() has been called.
     */
    addDir(path: string): void

    /**
     * Adds a file to the map with the specified content as a string.
     * Note: the file will only be written when writeToDir() or writeToMpq() has been called.
     */
    addFileString(archivePath: string, contents: string): void

    /**
     * Adds a file to the map, reading it from disk at the specified lcoation.
     * Note: the file will only be written when writeToDir() or writeToMpq() has been called.
     */
    addFileDisk(archivePath: string, diskPath: string): void

    /**
     * Writes the map and all manually added files to the specified directory.
     */
    writeToDir(path: string): void

    /**
     * Writes the map and all manually added files to an MPQ archive at the specified path.
     */
    writeToMpq(path: string): void

    /**
     * Initializes an object storage for the specified extension type.
     * This will read objects already present in the map if appropriate.
     */
    initObjectStorage(ext: objectExt): void

    /**
     * Initializes all object storages in the map.
     */
    initObjects(): void

    /**
     * Writes out all the object storages to their respective files in the map.
     */
    commitObjectStorage(objects: WarObjects): void
}

/**
 * Last build command issued.
 * @compiletime
 */
declare const lastBuildCommand: BuildCommand

/**
 * Currently compiling map, if any.
 * @compiletime
 */
declare const currentMap: WarMap | undefined

/**
 * Logs the provided arguments to stderr.
 * @compiletime
 */
declare function log(...args: any[]): void

/**
 * A macro function gets all its inputs evaluated at compiletime,
 * and returns either null or string.
 *
 * If it returned a string, its content will be
 * injected unmodified into the resulting map script.
 *
 * Keep in mind that the injected code will be interpreted
 * as Lua, not TypeScript.
 *
 * @compiletime
 */
declare interface MacroFunction {
    (...args: any[]): string | null
}

type AnyCompiletime = string | number | Compiletime<AnyCompiletime>

declare interface Compiletime<T> {
    compiletime(): T
}

/**
 * If `arg` is a function, runs it, otherwise, evaluates `arg`
 * itself, at compiletime, and injects the computed result
 * into the source, formatted as a Lua value.
 *
 * Specifically:
 * * if the result is a table, it will format it as a table,
 * * if the result is a string, it will format it as a string,
 * * if the result is a number, it will format it as a number.
 *
 * If the result evaluated to nothing, then nothing is injected into the source.
 * @macro
 * @compiletime
 */
declare function compiletime<R>(arg: Compiletime<R> | (() => Compiletime<R>)): R
declare function compiletime<R extends string | number | object | null | undefined | void>(
    arg: R | (() => R)
): R
declare function postcompile<R extends string | number | object | null | undefined | void>(
    arg: () => R
): R

declare const enum Platform {
    REFORGED = "reforged",
    UJ_API = "ujapi",
}

declare const TARGET_PLATFORM: Platform

/**
 * Reads the file at `path`, and if it exists, includes its contents
 * into the source, unmodified.
 *
 * Will cause a compilation error if the file could not be read.
 * @macro
 * @compiletime
 */
declare function include(path: string): string

/**
 * Creates a new macro with the specified name and handler function.
 * @see MacroFunction for information about the handler.
 *
 * After the registration, any raw function call in the form of `name`()
 * will be interpreted as a macro call, and Warpack will call the
 * specified handler function.
 * @macro
 * @compiletime
 */
// eslint-disable-next-line @typescript-eslint/camelcase
declare function macro_define(name: string, handler: MacroFunction): void
