import { existsSync } from "node:fs";
import { arch as getArch, platform as getPlatform } from "node:os";

/**
 * Returns the executable path for warpack.
 * Git installations from the `release` branch bundle it as `bin/${os}-${arch}/warpack`;
 * registry releases get it from the `@warpack/${os}-${arch}` platform package.
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

    const bundled = new URL(`../bin/${os}-${arch}/warpack${extension}`, import.meta.url);
    if (existsSync(bundled)) {
        return bundled.href;
    }

    try {
        // Since the bin will be located inside `node_modules`, we can simply call import.meta.resolve
        return import.meta.resolve(
            `@warpack/${os}-${arch}/bin/warpack${extension}`,
        );
    } catch (e) {
        throw new Error(
            `Couldn't find warpack binary for ${os}-${arch}: neither bundled in bin/ (git installations must use the release branch) nor inside node_modules (${e})`,
        );
    }
}