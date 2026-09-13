var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
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
export function getExePath() {
    return __awaiter(this, void 0, void 0, function* () {
        const platform = getPlatform();
        const arch = getArch();
        let os = platform;
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
            return import.meta.resolve(`@warpack/${os}-${arch}/bin/warpack${extension}`);
        }
        catch (e) {
            throw new Error(`Couldn't find warpack binary for ${os}-${arch}: neither bundled in bin/ (git installations must use the release branch) nor inside node_modules (${e})`);
        }
    });
}
