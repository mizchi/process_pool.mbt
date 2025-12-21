// Native-only tests: Timing-dependent tests
//
// These tests are native-only because the JS Semaphore has different behavior.
//
// Reason:
// - Native: @async.Semaphore provides true OS-level synchronization
// - JS: Single-threaded + cooperative multitasking (async/await)
//       Due to Event Loop scheduling during Promise creation,
//       multiple Promises may pass through acquire() check simultaneously
//
// See lib_js.mbt Semaphore implementation comments for details

///|
async test "max_workers limits concurrency" {
  let pool = ProcessPool::new(max_workers=1)
  let start = now()
  let jobs = [
    job("sleep", ["0.1"]),
    job("sleep", ["0.1"]),
  ]
  let results = pool.run_all(jobs)
  let elapsed = now() - start

  assert_eq(results.length(), 2)
  for result in results {
    assert_eq(result.exit_code, 0)
  }
  // max_workers=1 means sequential execution, 2 * 100ms = 200ms+ expected
  assert_true(elapsed >= 150L)
}

///|
async test "parallel execution is faster" {
  let pool = ProcessPool::new(max_workers=4)
  let start = now()
  let jobs = [
    job("sleep", ["0.1"]),
    job("sleep", ["0.1"]),
    job("sleep", ["0.1"]),
    job("sleep", ["0.1"]),
  ]
  let results = pool.run_all(jobs)
  let elapsed = now() - start

  assert_eq(results.length(), 4)
  for result in results {
    assert_eq(result.exit_code, 0)
  }
  // max_workers=4 means parallel execution, should finish in ~100ms + overhead
  assert_true(elapsed < 300L)
}
