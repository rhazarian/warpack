#!/usr/bin/env node

import { runWarpack } from "./index.js";

async function run() {
    const args = process.argv.slice(2);
    const processResult = await runWarpack(args);

    process.exit(processResult.exitCode ?? 0);
}

void run();
