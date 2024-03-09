import { arch as getArch, platform as getPlatform } from "os";

/**
 * Returns the executable path for warpack located inside node_modules
 * The naming convention is warpack-${os}-${arch}
 * If the platform is `win32` or `cygwin`, executable will include a `.exe` extension
 * @see https://nodejs.org/api/os.html#osarch
 * @see https://nodejs.org/api/os.html#osplatform
 * @example "x/xx/node_modules/warpack-darwin-arm64"
 */
export async function getExePath() {
    const platform = getPlatform();
    const arch = getArch();

    let os = platform as string;
    let extension = "";

    if (platform === "win32" || platform === "cygwin") {
        os = "windows";
        extension = ".exe";
    }

    try {
        // Since the bin will be located inside `node_modules`, we can simply call import.meta.resolve
        return import.meta.resolve(
            `@warpack/${os}-${arch}/bin/warpack${extension}`,
        );
    } catch (e) {
        throw new Error(
            `Couldn't find warpack binary inside node_modules for ${os}-${arch} (${e})`,
        );
    }
}