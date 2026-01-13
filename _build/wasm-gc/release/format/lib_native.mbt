// Native (C backend) implementation

///|
/// Process pool
pub struct ProcessPool {
  max_workers : Int
  semaphore : @async.Semaphore
  on_complete : OnJobComplete? // Callback on job completion
}

///|
/// Create a process pool
///
/// Parameters:
/// - `max_workers`: Maximum number of concurrent processes (default: 4)
/// - `on_complete`: Callback invoked on job completion (optional)
pub fn ProcessPool::new(
  max_workers? : Int = 4,
  on_complete? : OnJobComplete,
) -> ProcessPool {
  ProcessPool::{
    max_workers,
    semaphore: @async.Semaphore::new(max_workers),
    on_complete,
  }
}

///|
/// Execute a single job (internal function)
async fn run_job(job : Job) -> JobResult {
  let decoder = @encoding.decoder(UTF8)
  match job.timeout {
    Some(timeout) => {
      let result = @async.with_timeout_opt(timeout, () => @process.collect_output(
        job.cmd,
        job.args,
        cwd?=job.cwd,
      ))
      match result {
        Some((exit_code, stdout, stderr)) => {
          let stdout_str = decoder.decode_lossy(stdout.binary())
          let stderr_str = decoder.decode_lossy(stderr.binary())
          JobResult::{
            job,
            exit_code,
            stdout: stdout_str,
            stderr: stderr_str,
            timed_out: false,
          }
        }
        None =>
          JobResult::{
            job,
            exit_code: -1,
            stdout: "",
            stderr: "Process timed out",
            timed_out: true,
          }
      }
    }
    None => {
      let (exit_code, stdout, stderr) = @process.collect_output(
        job.cmd,
        job.args,
        cwd?=job.cwd,
      )
      let stdout_str = decoder.decode_lossy(stdout.binary())
      let stderr_str = decoder.decode_lossy(stderr.binary())
      JobResult::{
        job,
        exit_code,
        stdout: stdout_str,
        stderr: stderr_str,
        timed_out: false,
      }
    }
  }
}

///|
/// Execute job with semaphore-limited concurrency (internal function)
async fn run_job_with_semaphore(
  semaphore : @async.Semaphore,
  job : Job,
  on_complete : OnJobComplete?,
) -> JobResult {
  semaphore.acquire()
  defer semaphore.release()
  let result = run_job(job)
  if on_complete is Some(callback) {
    (callback.0)(result)
  }
  result
}

///|
/// Run multiple jobs in parallel and return all results
///
/// Waits for all jobs to complete and returns the results array.
/// Job order is preserved matching the input order.
pub async fn ProcessPool::run_all(
  self : ProcessPool,
  jobs : Array[Job],
) -> Array[JobResult] {
  @async.with_task_group(group => {
    let tasks = jobs.map(job => group.spawn(() => run_job_with_semaphore(
      self.semaphore,
      job,
      self.on_complete,
    )))
    let results = []
    for task in tasks {
      results.push(task.wait())
    }
    results
  })
}

///|
/// Get current time in milliseconds
pub fn now() -> Int64 {
  @async.now()
}
