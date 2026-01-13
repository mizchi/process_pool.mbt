# mizchi/process_pool

A Moonbit process pool for js/native

```
moon add mizchi/process_pool
```

## Features

- Run multiple external processes in parallel with configurable concurrency limits
- Timeout support for individual jobs
- Callback support for job completion notifications
- Cross-platform:
  - [x] `native`: moonbitlang/async
    - Semaphore-based concurrency control
  - [x] `js`: node:child_process
  - [ ] `wasm`, `wasm-gc` with WASI

inspired by `moonbitlang/maria` daemon

## Usage

### Basic Example

```mbt async test
let pool = @process_pool.ProcessPool::new(max_workers=4)
let results = pool.run_all([
  @process_pool.job("echo", ["hello"]),
  @process_pool.job("echo", ["world"]),
])

assert_eq(results.length(), 2)
assert_eq(results[0].exit_code, 0)
assert_eq(results[1].exit_code, 0)
```

### With Timeout

```mbt async test
let pool = @process_pool.ProcessPool::new(max_workers=2)
let results = pool.run_all([
  @process_pool.job("sleep", ["10"], timeout=100),  // 100ms timeout
])

assert_eq(results.length(), 1)
assert_true(results[0].timed_out)
```

### With Error Checking

```mbt async test
let pool = @process_pool.ProcessPool::new(max_workers=2)
let mut caught = false

// Raises JobError if any job fails
ignore(pool.run_all_checked([
  @process_pool.job("echo", ["hello"]),
  @process_pool.job("false", []),  // This will fail
])) catch {
  @process_pool.JobError::ProcessFailed(..) => {
    caught = true
  }
  _ => ()
}

assert_true(caught)
```

### With Completion Callback

```mbt async test
let count = @ref.new(0)
let callback = @process_pool.OnJobComplete(fn(_result) {
  count.update(fn(n) { n + 1 })
})

let pool = @process_pool.ProcessPool::new(
  max_workers=4,
  on_complete=callback,
)

let results = pool.run_all([
  @process_pool.job("echo", ["task1"]),
  @process_pool.job("echo", ["task2"]),
])

assert_eq(results.length(), 2)
assert_eq(count.val, 2)
```

### Map Pattern (for SSG, batch processing, etc.)

```mbt async test
let pool = @process_pool.ProcessPool::new(max_workers=4)
let items = ["a", "b", "c"]

let results = pool.map(items, fn(item) {
  @process_pool.job("echo", [item])
})

assert_eq!(results.length(), 3)
for result in results {
  assert_eq(result.exit_code, 0)
}
```

## API

### Types

#### `Job`
```mbt check
///|
let _a : @process_pool.Job = @process_pool.job("echo", ["hello"])
```

#### `JobResult`
The result of a job execution containing exit code, stdout, stderr, and timeout status.

#### `JobError`
Error type raised by `run_all_checked` when a job fails or times out.

### Functions

#### `job(cmd, args, cwd?, timeout?) -> Job`
Helper function to create a job.

#### `ProcessPool::new(max_workers~ = 4, on_complete?) -> ProcessPool`
Create a new process pool with the specified maximum worker count.

#### `ProcessPool::run_all(jobs) -> Array[JobResult]`
Run all jobs in parallel and return results. Job order is preserved.

#### `ProcessPool::run_all_checked(jobs) -> Array[JobResult]`
Run all jobs and raise `JobError` if any job fails or times out.

#### `ProcessPool::map(items, to_job) -> Array[JobResult]`
Transform items into jobs and run them in parallel.

#### `now() -> Int64`
Get current time in milliseconds (useful for timing measurements).

## Benchmark

The `example/` directory contains a benchmark that demonstrates parallel speedup by word-counting multiple documents.

### Running the Benchmark

```bash
# 1. Generate test documents (16 files, ~5000 words each)
node example/scripts/generate_docs.mjs

# 2. Run the benchmark
cd example && moon run --target native .
```

### Results

Processing 16 documents (~80,000 words total):

| Workers | Time | Speedup |
|---------|------|---------|
| 1 | 632ms | baseline |
| 2 | 315ms | 2.00x |
| 4 | 194ms | 3.25x |
| 8 | 160ms | 3.95x |

The benchmark shows near-linear speedup with additional workers, demonstrating effective parallel process execution.

## Platform Support

| Platform | Status |
|----------|--------|
| Native   | Full support |
| JS (Node.js) | Full support |
| WASM     | Not implemented |
| WASM-GC  | Not implemented |

### JS Semaphore Behavior Note

The JS implementation uses a cooperative multitasking model (async/await), which behaves differently from the native implementation's OS-level synchronization. Due to JavaScript's event loop scheduling, the semaphore may not strictly enforce concurrency limits in all scenarios. See `lib_js.mbt` for details.

## License

MIT
