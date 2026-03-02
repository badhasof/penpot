#!/usr/bin/env npx tsx
/**
 * Penpot CSS Tool — CLI for reading and applying CSS to shapes on the canvas.
 *
 * Connects to Shadow-CLJS nREPL (port 3447) and evaluates ClojureScript
 * in the browser runtime. No MCP server or plugin needed — just dev mode.
 *
 * Usage:
 *   npx tsx scripts/penpot-css-tool.ts get                  # Print all page CSS
 *   npx tsx scripts/penpot-css-tool.ts apply <file.css>     # Apply CSS from file
 *   echo '.Frame-xxx { width: 500px; }' | npx tsx scripts/penpot-css-tool.ts apply
 *
 * Requires: Penpot dev mode (shadow-cljs watch) + workspace open in browser.
 */

import * as net from "net";

const NREPL_PORT = parseInt(process.env.PENPOT_NREPL_PORT ?? "3447", 10);
const NREPL_HOST = process.env.PENPOT_NREPL_HOST ?? "localhost";

// ─── Bencode ──────────────────────────────────────────────────────────

function bencodeEncode(val: any): Buffer {
  if (typeof val === "string") {
    const buf = Buffer.from(val, "utf-8");
    return Buffer.concat([Buffer.from(`${buf.length}:`), buf]);
  }
  if (typeof val === "number") {
    return Buffer.from(`i${Math.floor(val)}e`);
  }
  if (Array.isArray(val)) {
    return Buffer.concat([
      Buffer.from("l"),
      ...val.map(bencodeEncode),
      Buffer.from("e"),
    ]);
  }
  if (typeof val === "object" && val !== null) {
    const keys = Object.keys(val).sort();
    const parts: Buffer[] = [Buffer.from("d")];
    for (const k of keys) {
      parts.push(bencodeEncode(k));
      parts.push(bencodeEncode(val[k]));
    }
    parts.push(Buffer.from("e"));
    return Buffer.concat(parts);
  }
  throw new Error(`Cannot bencode: ${typeof val}`);
}

interface DecodeResult {
  value: any;
  rest: Buffer;
}

function bencodeDecode(buf: Buffer): DecodeResult {
  if (buf.length === 0) throw new Error("Empty buffer");
  const c = String.fromCharCode(buf[0]);

  if (c === "i") {
    const end = buf.indexOf(0x65, 1);
    if (end === -1) throw new Error("Unterminated integer");
    const num = parseInt(buf.subarray(1, end).toString(), 10);
    return { value: num, rest: buf.subarray(end + 1) };
  }

  if (c === "l") {
    const items: any[] = [];
    let rest = buf.subarray(1);
    while (rest.length > 0 && rest[0] !== 0x65) {
      const r = bencodeDecode(rest);
      items.push(r.value);
      rest = r.rest;
    }
    return { value: items, rest: rest.subarray(1) };
  }

  if (c === "d") {
    const dict: Record<string, any> = {};
    let rest = buf.subarray(1);
    while (rest.length > 0 && rest[0] !== 0x65) {
      const kr = bencodeDecode(rest);
      const vr = bencodeDecode(kr.rest);
      dict[kr.value] = vr.value;
      rest = vr.rest;
    }
    return { value: dict, rest: rest.subarray(1) };
  }

  if (c >= "0" && c <= "9") {
    const colonIdx = buf.indexOf(0x3a);
    if (colonIdx === -1) throw new Error("Invalid string");
    const len = parseInt(buf.subarray(0, colonIdx).toString(), 10);
    const start = colonIdx + 1;
    if (start + len > buf.length) throw new Error("Incomplete string");
    const str = buf.subarray(start, start + len).toString("utf-8");
    return { value: str, rest: buf.subarray(start + len) };
  }

  throw new Error(`Unknown bencode type: ${c}`);
}

// ─── nREPL Client ─────────────────────────────────────────────────────

class NReplClient {
  private socket!: net.Socket;
  private buffer = Buffer.alloc(0);
  private pending = new Map<
    string,
    { resolve: (msgs: any[]) => void; reject: (e: Error) => void; msgs: any[] }
  >();
  private msgCounter = 0;

  async connect(port: number, host: string): Promise<void> {
    return new Promise((resolve, reject) => {
      this.socket = net.createConnection({ port, host }, () => resolve());
      this.socket.on("error", (err) => reject(err));
      this.socket.on("data", (data) => this.onData(data));
    });
  }

  private onData(data: Buffer) {
    this.buffer = Buffer.concat([this.buffer, data]);

    // Try to decode complete messages from buffer
    while (this.buffer.length > 0) {
      try {
        const r = bencodeDecode(this.buffer);
        const msg = r.value;
        this.buffer = r.rest;

        const id = msg.id;
        const entry = this.pending.get(id);
        if (!entry) continue;
        entry.msgs.push(msg);
        const status: string[] = msg.status ?? [];
        if (status.includes("done")) {
          this.pending.delete(id);
          entry.resolve(entry.msgs);
        }
      } catch {
        break; // incomplete data, wait for more
      }
    }
  }

  private send(msg: Record<string, any>): Promise<any[]> {
    const id = String(++this.msgCounter);
    msg.id = id;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject, msgs: [] });
      this.socket.write(bencodeEncode(msg));
      setTimeout(() => {
        if (this.pending.has(id)) {
          this.pending.delete(id);
          reject(new Error("nREPL timeout (30s)"));
        }
      }, 30000);
    });
  }

  async clone(): Promise<string> {
    const msgs = await this.send({ op: "clone" });
    const session = msgs.find((m) => m["new-session"])?.["new-session"];
    if (!session) throw new Error("Failed to clone nREPL session");
    return session;
  }

  async eval(
    code: string,
    session: string
  ): Promise<{ value?: string; out?: string; err?: string }> {
    const msgs = await this.send({ op: "eval", code, session });
    let value: string | undefined;
    let out = "";
    let err = "";
    for (const m of msgs) {
      if (m.value !== undefined) value = m.value;
      if (m.out) out += m.out;
      if (m.err) err += m.err;
      if (m.ex) err += m.ex;
    }
    return { value, out, err };
  }

  close() {
    this.socket?.destroy();
  }
}

// ─── Runtime Discovery ────────────────────────────────────────────────

async function findWorkspaceRuntime(
  client: NReplClient,
  session: string
): Promise<number> {
  // Find the workspace runtime in a single nREPL round-trip
  const findResult = await client.eval(
    `(let [rts (shadow.cljs.devtools.api/repl-runtimes :main)
           ids (map :client-id rts)]
       (first
         (for [id ids
               :let [r (shadow.cljs.devtools.api/cljs-eval :main "(str (.-location js/window))" {:client-id id})]
               :when (.contains (str r) "/workspace")]
           id)))`,
    session
  );
  if (findResult.value && findResult.value !== "nil") {
    return parseInt(findResult.value);
  }

  throw new Error(
    `No workspace tab found. ${ids.length} runtime(s) connected but none are in a workspace.\nOpen a project in the Penpot workspace.`
  );
}

// ─── CLJS Eval via cljs-eval ──────────────────────────────────────────

function extractCljsResult(raw: string): string {
  // cljs-eval returns EDN: {:results ["value"], :out "", :err "", :ns cljs.user}
  // Extract the first result string
  const m = raw.match(/:results \["([\s\S]*?)"\](?:,| )/);
  if (!m) return raw;
  // Unescape the doubly-escaped string
  return m[1].replace(/\\n/g, "\n").replace(/\\"/g, '"').replace(/\\\\/g, "\\");
}

async function cljsEval(
  client: NReplClient,
  session: string,
  runtimeId: number,
  jsCode: string
): Promise<string> {
  // Escape the JS code for embedding in a Clojure string
  const escaped = jsCode.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
  const code = `(shadow.cljs.devtools.api/cljs-eval :main "${escaped}" {:client-id ${runtimeId}})`;

  const result = await client.eval(code, session);
  if (result.err) {
    throw new Error(`CLJS eval failed: ${result.err}`);
  }
  return extractCljsResult(result.value ?? "");
}

// ─── Commands ─────────────────────────────────────────────────────────

async function withRepl<T>(
  fn: (
    client: NReplClient,
    session: string,
    runtimeId: number
  ) => Promise<T>
): Promise<T> {
  const client = new NReplClient();
  try {
    await client.connect(NREPL_PORT, NREPL_HOST);
  } catch (e: any) {
    throw new Error(
      `Cannot connect to nREPL at ${NREPL_HOST}:${NREPL_PORT}.\nIs Penpot dev mode running? (shadow-cljs watch)\n${e.message}`
    );
  }

  try {
    const session = await client.clone();
    const runtimeId = await findWorkspaceRuntime(client, session);
    return await fn(client, session, runtimeId);
  } finally {
    client.close();
  }
}

async function getPageCss(): Promise<string> {
  return withRepl(async (client, session, runtimeId) => {
    const css = await cljsEval(
      client,
      session,
      runtimeId,
      "(js/debug.get_page_css)"
    );
    return css;
  });
}

async function getPageShapes(): Promise<string> {
  return withRepl(async (client, session, runtimeId) => {
    return await cljsEval(
      client,
      session,
      runtimeId,
      "(js/debug.get_page_shapes)"
    );
  });
}

async function moveShape(idSuffix: string, x: number, y: number): Promise<void> {
  return withRepl(async (client, session, runtimeId) => {
    const result = await cljsEval(
      client,
      session,
      runtimeId,
      `(js/debug.move_shape "${idSuffix}" ${x} ${y})`
    );
    console.log(result);
  });
}

async function applyCss(newCss: string): Promise<void> {
  return withRepl(async (client, session, runtimeId) => {
    // Escape CSS for embedding inside a JS string inside a CLJS string
    const escaped = newCss
      .replace(/\\/g, "\\\\")
      .replace(/"/g, '\\"')
      .replace(/\n/g, "\\n")
      .replace(/\r/g, "");

    const result = await cljsEval(
      client,
      session,
      runtimeId,
      `(js/debug.apply_css "${escaped}")`
    );
    console.log(result);
  });
}

// ─── CLI ──────────────────────────────────────────────────────────────

async function readStdin(): Promise<string> {
  const chunks: Buffer[] = [];
  for await (const chunk of process.stdin) {
    chunks.push(chunk);
  }
  return Buffer.concat(chunks).toString("utf-8");
}

async function main() {
  const [command, ...args] = process.argv.slice(2);

  switch (command) {
    case "get": {
      const css = await getPageCss();
      process.stdout.write(css);
      break;
    }
    case "shapes": {
      const info = await getPageShapes();
      process.stdout.write(info + "\n");
      break;
    }
    case "move": {
      const [id, xStr, yStr] = args;
      if (!id || !xStr || !yStr) {
        console.error("Usage: penpot-css-tool.ts move <id-suffix> <x> <y>");
        process.exit(1);
      }
      await moveShape(id, parseFloat(xStr), parseFloat(yStr));
      break;
    }
    case "apply": {
      let css: string;
      if (args[0]) {
        const fs = await import("fs/promises");
        css = await fs.readFile(args[0], "utf-8");
      } else if (!process.stdin.isTTY) {
        css = await readStdin();
      } else {
        console.error("Usage: penpot-css-tool.ts apply <file.css>");
        console.error("   or: echo '...' | penpot-css-tool.ts apply");
        process.exit(1);
      }
      await applyCss(css);
      break;
    }
    default: {
      console.log("Penpot CSS Tool");
      console.log("");
      console.log("Commands:");
      console.log("  get              Print CSS for all shapes on the current page");
      console.log("  shapes           Print shape info (name, position, size, parent)");
      console.log("  move <id> <x> <y>  Move a shape to absolute coordinates");
      console.log("  apply <file>     Apply CSS changes from a file");
      console.log("  apply            Apply CSS changes from stdin");
      console.log("");
      console.log("Examples:");
      console.log("  npx tsx scripts/penpot-css-tool.ts get");
      console.log("  npx tsx scripts/penpot-css-tool.ts get > page.css");
      console.log("  npx tsx scripts/penpot-css-tool.ts apply modified.css");
      console.log(
        "  echo '.Frame-abc { width: 500px; }' | npx tsx scripts/penpot-css-tool.ts apply"
      );
      break;
    }
  }
}

main().then(
  () => process.exit(0),
  (err) => {
    console.error("Error:", err.message);
    process.exit(1);
  }
);
