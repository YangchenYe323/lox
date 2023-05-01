import React, { useEffect, useRef, useState } from "react";
import Terminal, { ColorMode, TerminalOutput } from "react-terminal-ui";
import { Editor } from "@monaco-editor/react";
import { Button } from "@mui/material";
import SendIcon from "@mui/icons-material/Send";
import dynamic from "next/dynamic";

const defaultCode = `class List {
  init(val, next) {
    this.val = val;
    this.next = next;
  }

  to_str() {
    if this.next == nil {
      return "" + this.val;
    } else {
      return "" + this.val + " -> " + this.next.to_str();
    }
  }
}

var tail = List(0);
var mid = List(1, tail);
var head = List(2, mid);
head.to_str()
`;

const Home = dynamic({
  ssr: false,
  loader: async () => {
    const rlox = await import("../../../rlox_wasm/pkg");
    const handle = new rlox.InterpreterHandle();

    return () => {
      const [terminalLineData, setTerminalLineData] = useState<
        TerminalOutput[]
      >([]);
      const [code, setCode] = useState(defaultCode);

      const codeOutput = (code: string) => {
        return (
          <TerminalOutput>
            <span className="text-neutral-400 text-xs">{code}</span>
          </TerminalOutput>
        );
      };

      const errorOutput = (output: string) => {
        return (
          <TerminalOutput>
            <span className="text-red-400">{output}</span>
          </TerminalOutput>
        );
      };

      const successOutput = (output: string) => {
        return (
          <TerminalOutput>
            <span className="text-gray-200">{`> ${output}`}</span>
          </TerminalOutput>
        );
      };

      const onCompile = () => {
        let result = handle.interprete(code);
        let c = codeOutput(code);
        let output = result.success()
          ? successOutput(result.output())
          : errorOutput(result.output());
        setTerminalLineData([...terminalLineData, c, output]);
        setCode("");
      };

      return (
        <div className="h-full w-full">
          <div className="fixed top-0 flex items-center justify-start w-full h-20 min-h-20 bg-cyan-200">
            <Button
              className="w-40 h-1/2"
              variant="contained"
              onClick={onCompile}
              endIcon={<SendIcon />}
              color="secondary"
            >
              Compile
            </Button>
          </div>
          <div className="flex flex-col w-full h-full mt-20">
            <div className="flex flex-row h-2/3 w-full">
              <Editor
                height="100%"
                defaultLanguage="c"
                value={code}
                onChange={(code) => {
                  let c = code || "";
                  setCode(c);
                }}
              />
              <Terminal
                height="100%"
                name="Output of the Lox Interpreter"
                colorMode={ColorMode.Dark}
              >
                {terminalLineData}
              </Terminal>
            </div>
          </div>
        </div>
      );
    };
  },
});

export default Home;
