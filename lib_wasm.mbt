// WebAssembly (wasm, wasm-gc) stub implementation
// These backends are not yet implemented.

/// Process pool (not implemented)
pub struct ProcessPool {
  max_workers : Int
  on_complete : OnJobComplete?
}

/// Create a process pool
pub fn ProcessPool::new(
  max_workers~ : Int = 4,
  on_complete? : OnJobComplete,
) -> ProcessPool {
  ProcessPool::{ max_workers, on_complete }
}

/// Run multiple jobs in parallel (not implemented)
pub async fn ProcessPool::run_all(
  self : ProcessPool,
  jobs : Array[Job],
) -> Array[JobResult] {
  ignore(self)
  ignore(jobs)
  abort("ProcessPool::run_all is not implemented for wasm/wasm-gc backend")
}
