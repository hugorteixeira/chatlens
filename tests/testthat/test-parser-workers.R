test_that("vectorized datetime parsing preserves supported formats", {
  dmy <- c(
    "01/02/2025 13:14:15",
    "01/02/2025 13:14",
    "01/02/2025, 13:14:15",
    "01/02/2025, 13:14",
    "01/02/25 13:14",
    "01/02/25, 13:14",
    "01/02/2025 01:14 PM",
    "01/02/2025, 01:14 PM",
    "01/02/25 01:14 PM",
    "01/02/25, 01:14 PM"
  )

  parsed <- chatlens:::.clh_parse_datetimes(dmy, tz = "UTC", date_order = "dmy")

  expect_length(parsed, length(dmy))
  expect_false(anyNA(parsed))
  expect_true(all(format(parsed, "%Y-%m-%d %H:%M:%S", tz = "UTC") %in%
    c("2025-02-01 13:14:00", "2025-02-01 13:14:15")))

  mdy <- chatlens:::.clh_parse_datetimes(
    "02/01/2025 13:14",
    tz = "UTC",
    date_order = "mdy"
  )
  expect_equal(format(mdy, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2025-02-01 13:14:00")
})

test_that("parser workers preserve message order and multiline text", {
  lines <- c(
    "01/01/2025, 10:00 - Alice: first line",
    "continued line",
    "01/01/2025, 10:01 - Bob: Contato.vcf (arquivo anexado)",
    "01/01/2025, 10:02 - Alice: final"
  )

  serial <- chatlens:::.clh_whatsapp_parse_lines(lines, workers = 1L)
  workers <- if (.Platform$OS.type == "windows") 1L else 2L
  parallel <- chatlens:::.clh_whatsapp_parse_lines(lines, workers = workers)

  expect_identical(parallel, serial)
  expect_equal(serial$message_id, 1:3)
  expect_equal(serial$text[1], "first line\ncontinued line")
  expect_equal(unname(serial$attachment_type[2]), "contact")
})

test_that("worker resolution validates input and caps automatic workers", {
  expect_error(
    chatlens:::.clh_whatsapp_resolve_workers(0, task_count = 100),
    "positive whole number"
  )
  expect_error(
    chatlens:::.clh_whatsapp_resolve_workers(1.5, task_count = 100),
    "positive whole number"
  )
  expect_equal(chatlens:::.clh_whatsapp_resolve_workers(NULL, task_count = 10), 1L)

  auto <- chatlens:::.clh_whatsapp_resolve_workers(NULL, task_count = 100000)
  expect_gte(auto, 1L)
  expect_lte(auto, 4L)
})
