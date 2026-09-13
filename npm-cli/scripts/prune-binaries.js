// The release branch commits binaries for every platform under bin/<os>-<arch>/.
// Run by `prepare` on git installation, before npm packs the package, so that
// only the binary for the installing platform ends up in node_modules.

import { existsSync, readdirSync, rmSync } from "node:fs";
import { arch as getArch, platform as getPlatform } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const binDir = join(dirname(fileURLToPath(import.meta.url)), "..", "..", "bin");

const platform = getPlatform();
const os = platform === "win32" || platform === "cygwin" ? "windows" : platform;
const current = `${os}-${getArch()}`;

if (existsSync(binDir)) {
    for (const entry of readdirSync(binDir)) {
        if (entry !== current) {
            rmSync(join(binDir, entry), { recursive: true, force: true });
        }
    }
}
