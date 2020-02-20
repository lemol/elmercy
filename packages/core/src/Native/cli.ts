import * as path from "path";
import meow from "meow";
import { buildApp } from "./";

const version = require("../../package.json")["version"];

const cli = meow(
  `
	Usage
	  $ elmercy [project path] [options]

	Options
    --version, -v                       show the version
    --output-dir DIR,  -o               output directory
    --watch, w                          watch for files change

	Examples
	  $ elmercy --version
	  ${version}
`,
  {
    flags: {
      version: {
        type: "boolean",
        alias: "v"
      },
      "output-dir": {
        type: "string",
        alias: "o"
      },
      watch: {
        type: "boolean",
        alias: "w"
      }
    }
  }
);

const watch = cli.flags.watch;
const projectPath = path.relative(process.cwd(), cli.input[0] || ".");
const outputDir =
  cli.flags["output-dir"] &&
  path.relative(process.cwd(), cli.flags["output-dir"]);

buildApp({ projectPath, outputDir, watch });
