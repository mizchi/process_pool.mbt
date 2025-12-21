# process_pool

A Moonbit process pool library for parallel process execution.

## Features

- Run multiple external processes in parallel with configurable concurrency limits
- Semaphore-based concurrency control
- Timeout support for individual jobs
- Callback support for job completion notifications
- Cross-platform: Native and Node.js (JS backend)

## Installation

Add to your `moon.mod.json`:

```json
{
  "deps": {
    "mizchi/process_pool": "0.1.0"
  }
}
```

## Usage

### Basic Example

```moonbit
async fn main {
  let pool = @process_pool.ProcessPool::new(max_workers=4)
  let results = pool.run_all([
    @process_pool.job("echo", ["hello"]),
    @process_pool.job("echo", ["world"]),
  ])

  for result in results {
    println(result.stdout)
  }
}
```

### With Timeout

```moonbit
async fn main {
  let pool = @process_pool.ProcessPool::new(max_workers=2)
  let results = pool.run_all([
    @process_pool.job("sleep", ["10"], timeout=1000),  // 1 second timeout
  ])

  for result in results {
    if result.timed_out {
      println("Job timed out!")
    }
  }
}
```

### With Error Checking

```moonbit
async fn main {
  let pool = @process_pool.ProcessPool::new(max_workers=2)

  // Raises JobError if any job fails
  let results = pool.run_all_checked([
    @process_pool.job("echo", ["hello"]),
    @process_pool.job("false", []),  // This will fail
  ]) catch {
    JobError::ProcessFailed(job~, exit_code~, ..) => {
      println("Job failed: \{job.cmd} with exit code \{exit_code}")
    }
    JobError::ProcessTimedOut(job~, timeout~, ..) => {
      println("Job timed out: \{job.cmd} after \{timeout}ms")
    }
  }
}
```

### With Completion Callback

```moonbit
async fn main {
  let callback = OnJobComplete(fn(result) {
    println("Completed: \{result.job.cmd}")
  })

  let pool = @process_pool.ProcessPool::new(
    max_workers=4,
    on_complete=callback,
  )

  let results = pool.run_all([
    @process_pool.job("echo", ["task1"]),
    @process_pool.job("echo", ["task2"]),
  ])
}
```

### Map Pattern (for SSG, batch processing, etc.)

```moonbit
async fn main {
  let pool = @process_pool.ProcessPool::new(max_workers=4)
  let files = ["page1.md", "page2.md", "page3.md"]

  let results = pool.map(files, fn(file) {
    @process_pool.job("render", [file])
  })
}
```

## API

### Types

#### `Job`
```moonbit
pub struct Job {
  cmd : String
  args : Array[String]
  cwd : String?
  timeout : Int?  // Timeout in milliseconds
}
```

#### `JobResult`
```moonbit
pub struct JobResult {
  job : Job
  exit_code : Int
  stdout : String
  stderr : String
  timed_out : Bool
}
```

#### `JobError`
```moonbit
pub suberror JobError {
  ProcessFailed(job~ : Job, exit_code~ : Int, stdout~ : String, stderr~ : String)
  ProcessTimedOut(job~ : Job, timeout~ : Int, stdout~ : String, stderr~ : String)
}
```

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
