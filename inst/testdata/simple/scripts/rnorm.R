jsonlite::write_json(
  rnorm(
    jsonlite::read_json(
      inputs$n,
      simplifyVector = TRUE
    )
  ),
  outputs$sample
)
