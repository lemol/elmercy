import { Elm } from "../Main";
import * as fs from "fs";
import * as glob from "glob";
import * as path from "path";
import chalk from "chalk";

export function buildApp(projectPath: string) {
  const program = Elm.Main.init({ flags: null });

  const root = `${projectPath}/src`;
  const distRoot = `${projectPath}/elm-stuff/app-stuff`;

  program.ports.requestSourcePaths.subscribe(async () => {
    const files = glob
      .sync(`/**/*.elm`, {
        sync: true,
        ignore: [`/App/**`, "**/elm-stuff/**/*"],
        root
      })
      .map(x => path.relative(root, x));

    console.log(`${chalk.underline.bgBlue("Read files:")}`);
    console.log(`${files.join("\n")}`);

    program.ports.readSourcePaths.send(files);
  });

  program.ports.requestSourceCode.subscribe(async filePath => {
    const contents = fs.readFileSync(path.join(root, filePath)).toString();

    program.ports.readSourceCode.send([filePath, contents]);
  });

  program.ports.writeResult.subscribe(async result => {
    console.log(`${chalk.underline.bgGreen("Writing files:")}`);

    result.forEach(([filePath, content]) => {
      const fullPath = `${distRoot}/${filePath}`;

      ensureDirExists(fullPath);
      fs.writeFileSync(fullPath, content);

      console.log(`${filePath}`);
    });
  });
}

function ensureDirExists(filePath: string) {
  return fs.mkdirSync(path.dirname(filePath), {
    recursive: true
  });
}
