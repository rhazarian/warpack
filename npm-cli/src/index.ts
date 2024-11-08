import { execa, type Options as ExecaOptions, type Result } from "execa";
import { fileURLToPath } from "node:url";
import { getExePath } from "./get-exe-path.js";

export async function runWarpack(args: string[], execaOptions?: ExecaOptions): Promise<Result> {
    const exePath = await getExePath();

    return execa(fileURLToPath(exePath), args, {
        stdio: "inherit",
        ...execaOptions,
    });
}
