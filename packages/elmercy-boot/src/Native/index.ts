import { Elm } from "../Main";
import * as fs from "fs";
import * as path from "path";
import chalk from "chalk";

export type Options = {
  projectPath: string;
  watch: boolean;
  outputDir?: string;
};

export function boot({ projectPath, watch, outputDir }: Options) {
  const program = Elm.Main.init({ flags: null });

  const root = projectPath;
  const distRoot = outputDir || path.join(root, "elm-stuff", "elmercy-boot");

  program.ports.flushResult.subscribe(async result => {
    console.log(`${chalk.underline.bgGreen("Writing files:")}`);

    result.forEach(([filePath, content]) => {
      const fullPath = `${distRoot}/${filePath}`;

      ensureDirExists(fullPath);
      fs.writeFileSync(fullPath, content);

      console.log(`${filePath}`);
    });
  });

  if (watch) {
  }

  initConfig();

  function initConfig() {
    const plugins = ["Elmercy.Plugin.Basic", "Elmercy.Plugin.SPA"];

    program.ports.initConfig.send(plugins);
  }
}

function ensureDirExists(filePath: string) {
  return fs.mkdirSync(path.dirname(filePath), {
    recursive: true
  });
}
