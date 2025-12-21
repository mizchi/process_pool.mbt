// 共通テスト: native と js の両方で実行される

///|
async test "ProcessPool runs jobs in parallel" {
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
async test "ProcessPool handles timeout" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("sleep", ["5"], timeout=50),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 1)
  assert_true(results[0].timed_out)
}

///|
async test "ProcessPool callback is called" {
  let count = @ref.new(0)
  let pool = ProcessPool::new(
    max_workers=2,
    on_complete=fn(_result) { count.val += 1 },
  )
  let jobs = [
    job("echo", ["a"]),
    job("echo", ["b"]),
    job("echo", ["c"]),
  ]
  ignore(pool.run_all(jobs))
  assert_eq(count.val, 3)
}

///|
async test "ProcessPool map works" {
  let pool = ProcessPool::new(max_workers=2)
  let items : Array[Int] = [1, 2, 3]
  let results = pool.map(items, fn(n) {
    job("echo", [n.to_string()])
  })
  assert_eq(results.length(), 3)
  for result in results {
    assert_eq(result.exit_code, 0)
  }
}

///|
async test "ProcessPool run_all_checked raises on failure" {
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

///|
async test "stdout is captured correctly" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("echo", ["hello world"]),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results[0].stdout.trim_space().to_string(), "hello world")
}

///|
async test "stderr is captured correctly" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("sh", ["-c", "echo error message >&2"]),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results[0].stderr.trim_space().to_string(), "error message")
}

///|
async test "cwd parameter works" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("pwd", [], cwd="/tmp"),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results[0].exit_code, 0)
  assert_true(results[0].stdout.contains("/tmp"))
}

///|
async test "empty job list returns empty results" {
  let pool = ProcessPool::new(max_workers=2)
  let jobs : Array[Job] = []
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 0)
}

///|
async test "run_all_checked raises on timeout" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("sleep", ["10"], timeout=50),
  ]
  let raised = @ref.new(false)
  ignore(pool.run_all_checked(jobs)) catch {
    JobError::ProcessTimedOut(_) => raised.val = true
    _ => ()
  }
  assert_true(raised.val)
}

///|
async test "mixed success and failure jobs" {
  let pool = ProcessPool::new(max_workers=2)
  let jobs = [
    job("echo", ["success1"]),
    job("false", []),
    job("echo", ["success2"]),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 3)
  assert_eq(results[0].exit_code, 0)
  assert_eq(results[1].exit_code, 1)
  assert_eq(results[2].exit_code, 0)
}

///|
async test "callback receives correct results" {
  let received : Array[String] = []
  let pool = ProcessPool::new(
    max_workers=1,
    on_complete=fn(result) {
      received.push(result.stdout.trim_space().to_string())
    },
  )
  let jobs = [
    job("echo", ["first"]),
    job("echo", ["second"]),
    job("echo", ["third"]),
  ]
  ignore(pool.run_all(jobs))

  assert_eq(received.length(), 3)
  assert_true(received.contains("first"))
  assert_true(received.contains("second"))
  assert_true(received.contains("third"))
}

///|
async test "job with both stdout and stderr" {
  let pool = ProcessPool::new(max_workers=1)
  let jobs = [
    job("sh", ["-c", "echo out && echo err >&2"]),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results[0].exit_code, 0)
  assert_true(results[0].stdout.contains("out"))
  assert_true(results[0].stderr.contains("err"))
}

///|
async test "large number of jobs" {
  let pool = ProcessPool::new(max_workers=4)
  let jobs : Array[Job] = []
  for i in 0..<20 {
    jobs.push(job("echo", [i.to_string()]))
  }
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 20)
  for result in results {
    assert_eq(result.exit_code, 0)
  }
}
