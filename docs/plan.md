# Moonbit SSG Parallelization Plan

This document is intended as a reference for coding agents implementing the project while reading GitHub repositories.

---

## 1. Goals

### 1-1. Technical Goals

* Parallelize the Markdown→HTML SSG build written in Moonbit using **multi-core parallelization**.
* Target: "Build time shouldn't scale linearly as page count increases."
* Achieve this at the **user level** without extending Moonbit core (VM or language runtime).

### 1-2. Model Concept

* Rather than a process model, think of it as a "**task system that concurrently executes per-page build jobs**".
* Instead of sharing state directly:
  * "Parent holds the job queue"
  * "Workers (processes or Worker Threads) fetch jobs"
  * "Workers return success/failure to parent"
  * Based on a **message passing model**.

### 1-3. Relationship with JS

* Want to **minimize JS dependencies**.
* However, pragmatically considering:
  * **JS backend + Node `worker_threads`** for parallelization
  * **Native backend + `@process` for multi-process execution**
* Will evaluate both approaches, ultimately adopting one (or both).

---

## 2. Current State

### 2-1. SSG Current State

* Markdown → HTML SSG implemented in Moonbit (single-threaded).
* Expected flow:
  1. Enumerate `content/**/*.md`
  2. Load Markdown files individually
  3. Parse/HTML conversion (CPU heavy)
  4. Apply templates
  5. Write to `dist/**/*.html`
* As page count increases, sequential processing causes CPU bottlenecks and longer build times.

### 2-2. Moonbit async / Insights from Maria (Summary)

* `moonbitlang/async` provides **event loop + structured concurrency + various primitives**.
  * `with_task_group`, `spawn_bg`, `Semaphore`, `aqueue.Queue`, `sleep`, `retry`, `@process.*`, etc.
* Maria is an **async task system** & agent implementation built on top of `async`.
  * Useful as a reference for "task management + external process calls + IO" done purely in Moonbit.
* However, `async` is a single-threaded event loop, so:
  * **"True parallel CPU execution within a single process"** is not possible.
  * True parallelization requires "multi-process or multiple OS threads".

---

## 3. Design Approach (Tentative)

### 3-1. Core Principles

1. **Extract per-page "rendering function" into Moonbit**
   * Example: `render_page(job: RenderJob) -> Result[Unit, Error]`
   * Input: Markdown path, output path, common config, etc.
   * Keep core SSG logic contained within Moonbit as much as possible.

2. **Create a "task system" to orchestrate builds**
   * Job queue (array or `aqueue.Queue`)
   * Workers (Moonbit tasks or child processes or worker_threads)
   * Success/failure aggregation

3. **Prepare parallelization strategies at two layers**
   * **Layer A: Single process + async (simple version)**
     * For IO-heavy or editor integration use cases.
   * **Layer B: Multi-process / Worker Threads (for serious builds)**
     * For CPU-bound full SSG builds.

### 3-2. Two Concrete Routes

#### Route 1: Native backend + `@process` (Moonbit only)

* Implement in Moonbit:
  * `ssg-worker`: Subcommand that builds a single page.
  * `ssg-orchestrator`: Reads job list, uses `@process.run` / `collect_output_merged` to launch `ssg-worker` up to N in parallel.
* Control concurrent process count with `Semaphore` or `TaskGroup`.
* Pros:
  * Almost no JS dependency.
  * Similar structure to Maria (`@process` for external commands), easy to reference.
* Cons:
  * Process startup overhead exists (but generally acceptable for SSG).

#### Route 2: JS backend + Node `worker_threads`

* Build Moonbit with JS backend, configure from Node:
  * Prepare `renderFile(inputPath, outputPath)` function in `moonbit/dist/ssg.js`.
  * Node main thread creates file list and worker pool.
  * Each Worker calls `renderFile` and returns results.
* Pros:
  * Easy to leverage Node ecosystem (glob, chokidar, etc.).
  * Easy to build watch, dev server, etc. on JS side.
* Cons:
  * Need to understand Moonbit-JS bridging (async state, etc.).
  * Somewhat opposite to "minimize JS dependency" motivation.

---

## 4. Investigation Tasks (For Coding Agent)

### 4-1. Moonbit async / Maria Related

* [ ] Open `moonbitlang/async` GitHub repository and focus on:
  * `src/async.mbt`: `with_task_group`, `spawn_bg`, `Semaphore`, `Queue` and other public APIs.
  * `@process` modules: `run`, `collect_output_merged`, `spawn` interfaces.
* [ ] Read Maria GitHub (`moonbit-maria`):
  * Extract task management and job queue implementation (Agent main loop, task registration, external command calls).
  * Note patterns for "task reuse / cancellation / error handling".

### 4-2. SSG Core Library API Design

* [ ] Organize current SSG codebase, define **minimal API for single page rendering**:
  * Input: `input_path: String`, `output_path: String`, `config: Config`, etc.
  * Output: `Result[Unit, RenderError]` (separate struct if including warnings).
* [ ] Decompose `render_page` into pure functions within Moonbit:
  * `load_markdown(path) -> String`
  * `parse_markdown(src) -> Ast`
  * `render_ast(ast, ctx) -> String`
  * `write_html(path, html) -> Unit`
* [ ] Verify this API is easily callable from both native and JS backends.

### 4-3. Route 1 (Native + @process) Verification Tasks

* [ ] Implement `ssg-worker` subcommand in Moonbit:
  * `ssg worker --input input.md --output dist/input.html` format, or pass job as JSON argument.
* [ ] Write sample implementation calling own `ssg-worker` from Moonbit program using `@process.collect_output_merged`.
* [ ] Write code limiting concurrent processes (= core count or arbitrary value) with `Semaphore` + `with_task_group`.
* [ ] Test with small Markdown set, verify success/failure behavior (exit code, stderr).

### 4-4. Route 2 (JS backend + worker_threads) Verification Tasks

* [ ] Build SSG core with JS backend, verify Node can import it.
* [ ] Create minimal sample calling `renderFile(input, output)` from Node.
* [ ] Write simple "Job → Worker → Result" cycle sample using `worker_threads`.
* [ ] Verify flow for propagating failed page errors from Worker to main, setting exit code to 1.

### 4-5. Performance and Behavior Verification

* [ ] With small-to-medium content set:
  * Sequential execution
  * Route 1 (multi-process)
  * Route 2 (worker_threads)
  * Compare build times and CPU usage.
* [ ] Record scaling behavior and issues (process limits, IO bottlenecks, etc.) as page count increases.

---

## 5. Implementation Notes

* **Don't over-share state**:
  * SSG pages are typically independent.
  * "Export metadata to JSON beforehand, each job reads it" is safer and simpler than "share giant global state".

* **Design error handling and logging from the start**:
  * How to express success/failure (`Result` / exit code / JSON response, etc.).
  * Standardize log format (job id, path, error message).

* **Keep future UI / HTTP expansion in mind**:
  * If wanting to evolve into "daemon + HTTP API" like Maria:
    * Extract build orchestrator core logic as pure functions or independent modules.
    * Enable common usage from CLI / HTTP / Node CLI for easier extension.

---

This document organizes:
* What we want to do (SSG multi-core parallelization)
* Design approach (task system + per-job parallelization)
* What to read and build (async/Maria/SSG core API/@process/worker_threads)

This should help coding agents proceed without confusion.
