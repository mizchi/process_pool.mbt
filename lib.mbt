// process_pool: Moonbit Process Pool Library
//
// A task runner for parallel process execution.
//
// Design:
// - Semaphore limits concurrent execution count
// - TaskGroup provides structured concurrency
// - @process.run executes child processes
//
// Example:
//   let pool = @process_pool.ProcessPool::new(max_workers=4)
//   let results = pool.run_all([
//     @process_pool.job("echo", ["hello"]),
//     @process_pool.job("echo", ["world"]),
//   ])
//
// Supported platforms:
// - native: Full functionality
// - js: Full functionality in Node.js environment
// - wasm/wasm-gc: Not implemented (abort)

///|
/// Job definition
pub struct Job {
  cmd : String
  args : Array[String]
  cwd : String?
  timeout : Int? // Timeout in milliseconds
}

///|
/// Job execution result
pub struct JobResult {
  job : Job
  exit_code : Int
  stdout : String
  stderr : String
  timed_out : Bool // Whether the job timed out
}

///|
/// Job execution error
pub suberror JobError {
  ProcessFailed(
    job~ : Job,
    exit_code~ : Int,
    stdout~ : String,
    stderr~ : String
  )
  ProcessTimedOut(
    job~ : Job,
    timeout~ : Int,
    stdout~ : String,
    stderr~ : String
  )
}

///|
/// Callback invoked on job completion
pub(all) struct OnJobComplete((JobResult) -> Unit)

///|
/// Helper function to create a job
///
/// Parameters:
/// - `cmd`: Command to execute
/// - `args`: Command arguments
/// - `cwd`: Working directory (optional)
/// - `timeout`: Timeout in milliseconds (optional)
pub fn job(
  cmd : String,
  args : Array[String],
  cwd? : String,
  timeout? : Int,
) -> Job {
  Job::{ cmd, args, cwd, timeout }
}

///|
/// Run multiple jobs in parallel and raise an exception on error
///
/// If any job fails (exit_code != 0) or times out,
/// raises JobError.
pub async fn ProcessPool::run_all_checked(
  self : ProcessPool,
  jobs : Array[Job],
) -> Array[JobResult] {
  let results = self.run_all(jobs)
  for result in results {
    if result.timed_out {
      raise JobError::ProcessTimedOut(
        job=result.job,
        timeout=result.job.timeout.unwrap_or(0),
        stdout=result.stdout,
        stderr=result.stderr,
      )
    }
    if result.exit_code != 0 {
      raise JobError::ProcessFailed(
        job=result.job,
        exit_code=result.exit_code,
        stdout=result.stdout,
        stderr=result.stderr,
      )
    }
  }
  results
}

///|
/// Transform input data into jobs and run them in parallel
///
/// Useful for SSG and similar use cases where jobs are generated
/// from file lists and executed in parallel.
pub async fn[T] ProcessPool::map(
  self : ProcessPool,
  items : Array[T],
  to_job : (T) -> Job,
) -> Array[JobResult] {
  let jobs = items.map(to_job)
  self.run_all(jobs)
}
