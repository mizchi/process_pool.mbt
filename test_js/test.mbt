///|
test "hello returns js implementation" {
  let result = @lib.hello()
  assert_eq(result, "hello from js")
}

///|
test "ProcessPool can be created" {
  let _pool = @lib.new_pool(max_workers=2)
}

///|
async test "run_all returns results for all jobs" {
  let pool = @lib.new_pool(max_workers=2)
  let jobs = [
    @types.job("echo", ["hello"]),
    @types.job("echo", ["world"]),
  ]
  let results = @lib.run_all(pool, jobs)
  assert_eq(results.length(), 2)
}
