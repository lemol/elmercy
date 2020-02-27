import { Elm } from "./Main";

export function initApp({ projectPath, watch, outputDir }) {
  return Elm.Main.init({ flags: null });
}
