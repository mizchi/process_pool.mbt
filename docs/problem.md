# Virtual Package + Async Function: Linker Error

## Summary

When using a **virtual package** with **async functions** in the implementation package, the moonc linker fails with a `Key_not_found` error. The linker looks for async state machine types in the virtual package's namespace (`lib.xxx.State`) instead of the implementation package's namespace (`impl_native.xxx.State`).

## Environment

- moonc version: v0.6.35+dd17327ed
- Target: native

## Reproduction Steps

### 1. Project Structure

```
proc_pool/
├── lib/                    # Virtual package
│   ├── moon.pkg.json
│   ├── pkg.mbti
│   └── lib.mbt
├── impl_native/            # Implementation package
│   ├── moon.pkg.json
│   └── impl.mbt
├── test_native/            # Test package
│   ├── moon.pkg.json
│   └── test.mbt
└── types/                  # Shared types
    ├── moon.pkg.json
    └── types.mbt
```

### 2. Virtual Package Definition

**lib/moon.pkg.json:**
```json
{
  "import": ["mizchi/proc_pool/types"],
  "virtual": {
    "has-default": true
  }
}
```

**lib/pkg.mbti:**
```
package "mizchi/proc_pool/lib"

import(
  "mizchi/proc_pool/types"
)

fn hello() -> String
fn new_pool(max_workers? : Int) -> ProcessPool
async fn run_all(ProcessPool, Array[@types.Job]) -> Array[@types.JobResult]

type ProcessPool
```

**lib/lib.mbt:** (default implementation)
```moonbit
pub fn hello() -> String {
  "hello from default"
}

pub struct ProcessPool {
  max_workers : Int
}

pub fn new_pool(max_workers~ : Int = 4) -> ProcessPool {
  ProcessPool::{ max_workers }
}

pub async fn run_all(
  pool : ProcessPool,
  jobs : Array[@types.Job],
) -> Array[@types.JobResult] {
  ignore(pool)
  ignore(jobs)
  abort("run_all is not implemented.")
}
```

### 3. Implementation Package

**impl_native/moon.pkg.json:**
```json
{
  "import": [
    "mizchi/proc_pool/types",
    "moonbitlang/async",
    {"path": "moonbitlang/async/process", "alias": "process"},
    "moonbitlang/x/encoding"
  ],
  "implement": "mizchi/proc_pool/lib",
  "targets": {
    "impl.mbt": ["native"]
  }
}
```

**impl_native/impl.mbt:**
```moonbit
pub fn hello() -> String {
  "hello from native"
}

pub struct ProcessPool {
  semaphore : @async.Semaphore
}

pub fn new_pool(max_workers~ : Int = 4) -> ProcessPool {
  ProcessPool::{
    semaphore: @async.Semaphore::new(max_workers),
  }
}

pub async fn run_all(
  pool : ProcessPool,
  jobs : Array[@types.Job],
) -> Array[@types.JobResult] {
  let decoder = @encoding.decoder(UTF8)

  @async.with_task_group(fn(group) {
    let tasks = jobs.map(fn(job) {
      group.spawn(fn() {
        pool.semaphore.acquire()
        defer pool.semaphore.release()
        // ... process execution logic
      })
    })
    // ... collect results
  })
}
```

### 4. Test Package

**test_native/moon.pkg.json:**
```json
{
  "import": [
    "mizchi/proc_pool/lib",
    "mizchi/proc_pool/types",
    "moonbitlang/async"
  ],
  "overrides": ["mizchi/proc_pool/impl_native"],
  "targets": {
    "test.mbt": ["native"]
  }
}
```

### 5. Run Test

```bash
moon test --target native -p mizchi/proc_pool/test_native
```

## Error Message

```
Error: Moonc.Basic_hash_string.Key_not_found("$mizchi/proc_pool/lib.run_all.lambda/60.State")
```

The linker is looking for `lib.run_all.lambda/60.State` but the async state machine is actually defined in `impl_native`.

## Analysis

The issue appears to be in how the linker resolves async function state machine types when using virtual packages:

1. **Non-async functions work correctly**: `hello()` and `new_pool()` work fine with virtual packages.
2. **Async functions fail at link time**: When the implementation package contains async functions, the linker looks for state machine types in the virtual package's namespace (`lib.xxx.State`) rather than the implementation package's namespace (`impl_native.xxx.State`).

## Workaround

Currently, there is no known workaround. The options are:

1. **Avoid virtual packages with async functions**: Use conditional compilation (`#cfg`) instead
2. **Use sync-only functions in virtual packages**: Lose the ability to have async implementations

## Expected Behavior

The linker should resolve async state machine types from the implementation package's namespace when `overrides` is specified, not from the virtual package's namespace.

## Related

- MoonBit Virtual Package Blog: https://www.moonbitlang.com/blog/virtual-package
