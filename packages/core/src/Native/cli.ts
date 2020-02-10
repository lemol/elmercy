import * as path from "path";
import meow from "meow";
import { buildApp } from "./";

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

const projectPath = path.relative(process.cwd(), cli.input[0] || ".");

buildApp(projectPath);
