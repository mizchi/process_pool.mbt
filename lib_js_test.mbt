///|
async test "ProcessPool runs jobs in parallel (js)" {
  let pool = ProcessPool::new(max_workers=2)
  let jobs = [
    job("echo", ["hello"]),
    job("echo", ["world"]),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 2)
  assert_eq(results[0].exit_code, 0)
  assert_eq(results[1].exit_code, 0)
}

///|
async test "ProcessPool handles timeout (js)" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("sleep", ["5"], timeout=50),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 1)
  assert_true(results[0].timed_out)
}

///|
async test "stdout is captured correctly (js)" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("echo", ["hello world"]),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results[0].stdout.trim_space().to_string(), "hello world")
}

///|
async test "stderr is captured correctly (js)" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("sh", ["-c", "echo error message >&2"]),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results[0].stderr.trim_space().to_string(), "error message")
}

///|
async test "run_all_checked raises on failure (js)" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("false", []),
  ]
  let raised = @ref.new(false)
  ignore(pool.run_all_checked(jobs)) catch {
    JobError::ProcessFailed(_) => raised.val = true
    _ => ()
  }
  assert_true(raised.val)
}
