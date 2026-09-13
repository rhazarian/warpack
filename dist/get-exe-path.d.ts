/**
 * Returns the executable path for warpack.
 * Git installations from the `release` branch bundle it as `bin/${os}-${arch}/warpack`;
 * registry releases get it from the `@warpack/${os}-${arch}` platform package.
 * If the platform is `win32` or `cygwin`, executable will include a `.exe` extension
 * @see https://nodejs.org/api/os.html#osarch
 * @see https://nodejs.org/api/os.html#osplatform
 * @example "x/xx/node_modules/warpack-darwin-arm64"
 */
export declare function getExePath(): Promise<string>;
