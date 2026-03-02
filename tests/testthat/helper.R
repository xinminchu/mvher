# Helper: build a tiny record dataset for tests
make_records <- function() {
  data.frame(
    id        = 1:6,
    entity_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    name      = c("Alice Smith", "Alice Smyth", NA, "Bob Jones", "", "Carol White"),
    city      = c("London", "Londen", "Paris", NA, "", "Rome"),
    stringsAsFactors = FALSE
  )
}

# Helper: build a small pairs table
make_pairs <- function() {
  data.frame(
    id1 = c(1, 1, 2, 3, 5),
    id2 = c(2, 3, 4, 4, 6)
  )
}
