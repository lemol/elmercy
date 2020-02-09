// WARNING: Do not manually modify this file. It was generated using:
// https://github.com/dillonkearns/elm-typescript-interop
// Type definitions for Elm ports

export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
        requestSourcePaths: {
          subscribe(callback: (data: null) => void): void
        }
        requestSourceCode: {
          subscribe(callback: (data: string) => void): void
        }
        writeResult: {
          subscribe(callback: (data: [string, string][]) => void): void
        }
        readSourcePaths: {
          send(data: string[]): void
        }
        readSourceCode: {
          send(data: [string, string]): void
        }
      };
    }
    export function init(options: {
      node?: HTMLElement | null;
      flags: null;
    }): Elm.Main.App;
  }
}