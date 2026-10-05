// Common tests: Run on both native and js

///|
async test "ProcessPool runs jobs in parallel" {
  let pool = @process_pool.ProcessPool::new(max_workers=2)
  let jobs = [
    @process_pool.job("echo", ["hello"]),
    @process_pool.job("echo", ["world"]),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 2)
  assert_eq(results[0].exit_code, 0)
  assert_eq(results[1].exit_code, 0)
}

///|
async test "ProcessPool handles timeout" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("sleep", ["5"], timeout=50)]
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 1)
  assert_true(results[0].timed_out)
}

///|
async test "ProcessPool callback is called" {
  let count = @ref.new(0)
  let pool = @process_pool.ProcessPool::new(max_workers=2, on_complete=fn(
    _result,
  ) {
    count.val += 1
  })
  let jobs = [
    @process_pool.job("echo", ["a"]),
    @process_pool.job("echo", ["b"]),
    @process_pool.job("echo", ["c"]),
  ]
  ignore(pool.run_all(jobs))
  assert_eq(count.val, 3)
}

///|
async test "ProcessPool map works" {
  let pool = @process_pool.ProcessPool::new(max_workers=2)
  let items : Array[Int] = [1, 2, 3]
  let results = pool.map(items, fn(n) {
    @process_pool.job("echo", [n.to_string()])
  })
  assert_eq(results.length(), 3)
  for result in results {
    assert_eq(result.exit_code, 0)
  }
}

///|
async test "ProcessPool run_all_checked raises on failure" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("false", [])]
  let raised = @ref.new(false)
  ignore(pool.run_all_checked(jobs)) catch {
    @process_pool.JobError::ProcessFailed(_) => raised.val = true
    _ => ()
  }
  assert_true(raised.val)
}

///|
async test "stdout is captured correctly" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("echo", ["hello world"])]
  let results = pool.run_all(jobs)
  assert_eq(results[0].stdout.trim().to_owned(), "hello world")
}

///|
async test "stderr is captured correctly" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("sh", ["-c", "echo error message >&2"])]
  let results = pool.run_all(jobs)
  assert_eq(results[0].stderr.trim().to_owned(), "error message")
}

///|
async test "cwd parameter works" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("pwd", [], cwd="/tmp")]
  let results = pool.run_all(jobs)
  assert_eq(results[0].exit_code, 0)
  assert_true(results[0].stdout.contains("/tmp"))
}

///|
async test "empty job list returns empty results" {
  let pool = @process_pool.ProcessPool::new(max_workers=2)
  let jobs : Array[@process_pool.Job] = []
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 0)
}

///|
async test "run_all_checked raises on timeout" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("sleep", ["10"], timeout=50)]
  let raised = @ref.new(false)
  ignore(pool.run_all_checked(jobs)) catch {
    @process_pool.JobError::ProcessTimedOut(_) => raised.val = true
    _ => ()
  }
  assert_true(raised.val)
}

///|
async test "mixed success and failure jobs" {
  let pool = @process_pool.ProcessPool::new(max_workers=2)
  let jobs = [
    @process_pool.job("echo", ["success1"]),
    @process_pool.job("false", []),
    @process_pool.job("echo", ["success2"]),
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
  let pool = @process_pool.ProcessPool::new(max_workers=1, on_complete=fn(
    result,
  ) {
    received.push(result.stdout.trim().to_owned())
  })
  let jobs = [
    @process_pool.job("echo", ["first"]),
    @process_pool.job("echo", ["second"]),
    @process_pool.job("echo", ["third"]),
  ]
  ignore(pool.run_all(jobs))
  assert_eq(received.length(), 3)
  assert_true(received.contains("first"))
  assert_true(received.contains("second"))
  assert_true(received.contains("third"))
}

///|
async test "job with both stdout and stderr" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("sh", ["-c", "echo out && echo err >&2"])]
  let results = pool.run_all(jobs)
  assert_eq(results[0].exit_code, 0)
  assert_true(results[0].stdout.contains("out"))
  assert_true(results[0].stderr.contains("err"))
}

///|
async test "large number of jobs" {
  let pool = @process_pool.ProcessPool::new(max_workers=4)
  let jobs : Array[@process_pool.Job] = []
  for i in 0..<20 {
    jobs.push(@process_pool.job("echo", [i.to_string()]))
  }
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 20)
  for result in results {
    assert_eq(result.exit_code, 0)
  }
}

///|
async test "default max_workers" {
  // Test default value when max_workers is not specified
  let pool = @process_pool.ProcessPool::new()
  let jobs = [@process_pool.job("echo", ["test"])]
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 1)
  assert_eq(results[0].exit_code, 0)
}

///|
async test "run_all_checked raises on first failure" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [
    @process_pool.job("echo", ["before"]),
    @process_pool.job("false", []), // Should fail here
    @process_pool.job("echo", ["after"]),
  ]
  let error_job_cmd = @ref.new("")
  ignore(pool.run_all_checked(jobs)) catch {
    @process_pool.JobError::ProcessFailed(job~, ..) =>
      error_job_cmd.val = job.cmd
    _ => ()
  }
  assert_eq(error_job_cmd.val, "false")
}

///|
async test "JobError contains correct exit_code" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("sh", ["-c", "exit 42"])]
  let captured_code = @ref.new(0)
  ignore(pool.run_all_checked(jobs)) catch {
    @process_pool.JobError::ProcessFailed(exit_code~, ..) =>
      captured_code.val = exit_code
    _ => ()
  }
  assert_eq(captured_code.val, 42)
}

///|
async test "JobError contains stdout and stderr" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [
    @process_pool.job("sh", ["-c", "echo output && echo error >&2 && exit 1"]),
  ]
  let captured_stdout = @ref.new("")
  let captured_stderr = @ref.new("")
  ignore(pool.run_all_checked(jobs)) catch {
    @process_pool.JobError::ProcessFailed(stdout~, stderr~, ..) => {
      captured_stdout.val = stdout
      captured_stderr.val = stderr
    }
    _ => ()
  }
  assert_true(captured_stdout.val.contains("output"))
  assert_true(captured_stderr.val.contains("error"))
}

///|
async test "JobResult preserves original job" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let original_job = @process_pool.job(
    "echo",
    ["preserve test"],
    cwd="/tmp",
    timeout=1000,
  )
  let jobs = [original_job]
  let results = pool.run_all(jobs)
  assert_eq(results[0].job.cmd, "echo")
  assert_eq(results[0].job.args[0], "preserve test")
  assert_eq(results[0].job.cwd, Some("/tmp"))
  assert_eq(results[0].job.timeout, Some(1000))
}

///|
async test "multiple timeouts" {
  let pool = @process_pool.ProcessPool::new(max_workers=2)
  let jobs = [
    @process_pool.job("sleep", ["10"], timeout=30),
    @process_pool.job("sleep", ["10"], timeout=30),
  ]
  let results = pool.run_all(jobs)
  assert_eq(results.length(), 2)
  assert_true(results[0].timed_out)
  assert_true(results[1].timed_out)
}

///|
async test "special characters in arguments" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("echo", ["hello world", "foo\tbar", "a'b\"c"])]
  let results = pool.run_all(jobs)
  assert_eq(results[0].exit_code, 0)
  assert_true(results[0].stdout.contains("hello world"))
}

///|
async test "environment inherits from parent" {
  let pool = @process_pool.ProcessPool::new(max_workers=1)
  let jobs = [@process_pool.job("sh", ["-c", "echo $HOME"])]
  let results = pool.run_all(jobs)
  assert_eq(results[0].exit_code, 0)
  // Verify HOME environment variable is inherited
  assert_true(results[0].stdout.trim().to_owned().length() > 0)
}
