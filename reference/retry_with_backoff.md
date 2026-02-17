# Retry with Backoff

Retries a function call with exponential backoff.

## Usage

``` r
retry_with_backoff(
  f,
  max_attempts = 3,
  initial_delay = 0.1,
  max_delay = 5,
  on_error = NULL
)
```

## Arguments

- f:

  Function to call.

- max_attempts:

  Integer. Maximum retry attempts.

- initial_delay:

  Numeric. Initial delay in seconds.

- max_delay:

  Numeric. Maximum delay in seconds.

- on_error:

  Function. Called on each error with (attempt, error).

## Value

Result of f() or error on final failure.
