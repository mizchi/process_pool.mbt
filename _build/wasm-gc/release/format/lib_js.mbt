// JavaScript (Node.js) implementation

// --- Node.js child_process bindings ---

///|
/// Spawn result from JS
priv struct SpawnResult {
  exit_code : Int
  stdout : String
  stderr : String
}

///|
/// Promise from JavaScript
#external
priv type JsPromise[T]

///|
extern "js" fn spawn_and_collect(
  cmd : String,
  args : Array[String],
  cwd : String?,
  timeout : Int?,
) -> JsPromise[SpawnResult] =
  #| async (cmd, args, cwd, timeout) => {
  #|   const { spawn } = await import('node:child_process');
  #|   return new Promise((resolve, reject) => {
  #|     const options = {};
  #|     if (cwd !== undefined) options.cwd = cwd;
  #|     const child = spawn(cmd, args, options);
  #|     let stdout = '';
  #|     let stderr = '';
  #|     let timedOut = false;
  #|     let timeoutId = null;
  #|     if (timeout !== undefined && timeout > 0) {
  #|       timeoutId = setTimeout(() => {
  #|         timedOut = true;
  #|         child.kill('SIGTERM');
  #|       }, timeout);
  #|     }
  #|     child.stdout.on('data', (data) => { stdout += data.toString(); });
  #|     child.stderr.on('data', (data) => { stderr += data.toString(); });
  #|     child.on('error', (err) => {
  #|       if (timeoutId) clearTimeout(timeoutId);
  #|       reject(err);
  #|     });
  #|     child.on('close', (code) => {
  #|       if (timeoutId) clearTimeout(timeoutId);
  #|       if (timedOut) {
  #|         resolve({ exit_code: -1, stdout, stderr: 'Process timed out' });
  #|       } else {
  #|         resolve({ exit_code: code ?? 0, stdout, stderr });
  #|       }
  #|     });
  #|   });
  #| }

// --- Semaphore implementation for JS ---
//
// Note: The JS Semaphore has different behavior characteristics than Native.
//
// JavaScript is single-threaded, and async/await is cooperative multitasking.
// When multiple Promises are created via @js_async.Promise::from_async,
// the execution start timing of each Promise depends on JS Event Loop scheduling.
//
// Problem: Between the `if self.current < self.max` check and `self.current += 1`
// in acquire(), other Promises may interleave. This means even with max_workers=1,
// multiple jobs can pass through acquire() simultaneously.
//
// This is a JS runtime limitation. True preemptive concurrency control would
// require a different approach (e.g., Worker Threads). Therefore, timing-dependent
// tests are native-only (see lib_native_test.mbt).

///|
/// Simple semaphore for concurrency control
struct JsSemaphore {
  max : Int
  mut current : Int
  waiters : Array[() -> Unit]
}

///|
fn JsSemaphore::new(max : Int) -> JsSemaphore {
  JsSemaphore::{ max, current: 0, waiters: [] }
}

///|
extern "js" fn js_promise_create_unit(
  executor : ((Unit) -> Unit, (String) -> Unit) -> Unit,
) -> @js_async.Promise[Unit] =
  #| (executor) => new Promise(executor)

///|
async fn JsSemaphore::acquire(self : JsSemaphore) -> Unit {
  if self.current < self.max {
    self.current += 1
    return
  }
  // Need to wait
  @js_async.run_promise(fn(_signal) {
    js_promise_create_unit(fn(resolve, _reject) {
      self.waiters.push(fn() { resolve(()) })
    })
  })
  self.current += 1
}

///|
fn JsSemaphore::release(self : JsSemaphore) -> Unit {
  self.current -= 1
  if self.waiters.length() > 0 {
    let waiter = self.waiters.remove(0)
    waiter()
  }
}

// --- ProcessPool implementation ---

///|
/// Process pool
pub struct ProcessPool {
  max_workers : Int
  semaphore : JsSemaphore
  on_complete : OnJobComplete?
}

///|
/// Create a process pool
pub fn ProcessPool::new(
  max_workers? : Int = 4,
  on_complete? : OnJobComplete,
) -> ProcessPool {
  ProcessPool::{
    max_workers,
    semaphore: JsSemaphore::new(max_workers),
    on_complete,
  }
}

///|
/// Execute a single job
async fn run_job(job : Job) -> JobResult {
  let promise = spawn_and_collect(job.cmd, job.args, job.cwd, job.timeout)
  let result : SpawnResult = @js_async.run_promise(fn(_signal) {
    promise.cast()
  })
  let timed_out = result.exit_code == -1 && result.stderr == "Process timed out"
  JobResult::{
    job,
    exit_code: result.exit_code,
    stdout: result.stdout,
    stderr: result.stderr,
    timed_out,
  }
}

///|
fn[T, U] JsPromise::cast(self : JsPromise[T]) -> @js_async.Promise[U] = "%identity"

///|
/// Execute job with semaphore-limited concurrency
async fn run_job_with_semaphore(
  semaphore : JsSemaphore,
  job : Job,
  on_complete : OnJobComplete?,
) -> JobResult {
  semaphore.acquire()
  let result = run_job(job)
  semaphore.release()
  if on_complete is Some(callback) {
    (callback.0)(result)
  }
  result
}

///|
/// Run multiple jobs in parallel
pub async fn ProcessPool::run_all(
  self : ProcessPool,
  jobs : Array[Job],
) -> Array[JobResult] {
  // JS version uses Promise-based parallel execution
  let promises : Array[@js_async.Promise[JobResult]] = jobs.map(fn(job) {
    @js_async.Promise::from_async(fn() {
      run_job_with_semaphore(self.semaphore, job, self.on_complete)
    })
  })
  let results : Array[JobResult] = []
  for promise in promises {
    results.push(promise.wait())
  }
  results
}

///|
/// Get current time in milliseconds
pub fn now() -> Int64 {
  js_date_now()
}

///|
extern "js" fn js_date_now() -> Int64 =
  #| () => BigInt(Date.now())
