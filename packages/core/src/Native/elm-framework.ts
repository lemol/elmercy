import { Elm } from "../Main";
import * as fs from "fs";
import * as glob from "glob";
import * as path from "path";
import meow from "meow";
import chalk from "chalk";

const version = require("../../package.json")["version"];

const cli = meow(
  `
	Usage
	  $ foo [project path] [options]

	Options
    --version, -v               show the version

	Examples
	  $ foo --version
	  ${version}
`,
  {
    flags: {
      version: {
        type: "boolean",
        alias: "v"
      }
    }
  }
);

const program = Elm.Main.init({ flags: null });

const simplePathProject = path.relative(process.cwd(), cli.input[0] || ".");
console.log(simplePathProject);
console.log(process.cwd());

const root = `${simplePathProject}/src`;

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
    const fullPath = `${root}/${filePath}`;

    ensureDirExists(fullPath);
    fs.writeFileSync(fullPath, content);

    console.log(`${filePath}`);
  });
});

function ensureDirExists(filePath: string) {
  return fs.mkdirSync(path.dirname(filePath), {
    recursive: true
  });
}
