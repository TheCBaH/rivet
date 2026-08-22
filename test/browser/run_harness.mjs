// Playwright driver for the real-browser smoke harness (.ai/asm_plan.md M5,
// the deferred browser-harness item). This script only controls browser
// process lifecycle - launch, navigate, wait for a terminal result, read it,
// exit. All assembler logic and the jsoo/melange-vs-native comparison run
// inside the real browser page (harness.html), which is what keeps "no
// Node/POSIX polyfills" meaningful: Node here is a browser-automation
// controller, not a stand-in JS runtime for the code under test.
//
// Usage: node run_harness.mjs <path-to-generated-work-dir>/harness.html
import { chromium } from "playwright";
import path from "node:path";
import { pathToFileURL } from "node:url";

const htmlPath = process.argv[2];
if (!htmlPath) {
  console.error("usage: node run_harness.mjs <path-to-harness.html>");
  process.exit(2);
}

const TERMINAL_TIMEOUT_MS = 15000;

async function main() {
  const browser = await chromium.launch();
  const page = await browser.newPage();

  // Captured regardless of outcome, so a failure is always diagnosable from
  // this script's own stdout, not just from reading harness.html's #result.
  page.on("console", (msg) => console.log(`[console:${msg.type()}] ${msg.text()}`));
  page.on("pageerror", (err) => console.log(`[pageerror] ${err}`));

  await page.goto(pathToFileURL(path.resolve(htmlPath)).href);

  let timedOut = false;
  try {
    await page.waitForFunction(() => document.title !== "PENDING", null, {
      timeout: TERMINAL_TIMEOUT_MS,
    });
  } catch {
    timedOut = true;
  }

  if (timedOut) {
    console.log(`[driver] TIMEOUT: title still PENDING after ${TERMINAL_TIMEOUT_MS}ms`);
    await browser.close();
    process.exit(1);
  }

  const title = await page.title();
  const result = await page.textContent("#result");
  console.log(`[driver] title: ${title}`);
  console.log(`[driver] result: ${result}`);

  await browser.close();
  process.exit(title === "PASS" ? 0 : 1);
}

main().catch((err) => {
  console.error("[driver] fatal:", err);
  process.exit(1);
});
