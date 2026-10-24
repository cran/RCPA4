as_file_url <- function(path) {
  paste0(
    "file:///",
    gsub("\\\\", "/", normalizePath(path, winslash = "/", mustWork = TRUE))
  )
}

test_that("getC imports delimited and RDS files", {
  source_data <- data.frame(id = 1:2, label = c("first", "second"))
  csv_file <- tempfile(fileext = ".csv")
  compressed_csv_file <- tempfile(fileext = ".csv.gz")
  csv2_file <- tempfile(fileext = ".csv2")
  tsv_file <- tempfile(fileext = ".tsv")
  rds_file <- tempfile(fileext = ".rds")
  on.exit(unlink(c(csv_file, compressed_csv_file, csv2_file, tsv_file, rds_file)), add = TRUE)
  utils::write.csv(source_data, csv_file, row.names = FALSE)
  compressed_connection <- gzfile(compressed_csv_file, "wt")
  utils::write.csv(source_data, compressed_connection, row.names = FALSE)
  close(compressed_connection)
  utils::write.csv2(source_data, csv2_file, row.names = FALSE)
  utils::write.table(source_data, tsv_file, sep = "\t", row.names = FALSE)
  saveRDS(source_data, rds_file)

  expect_equal(quiet_run(getC(csv_file, confirm = FALSE)), source_data)
  expect_equal(quiet_run(getC(as_file_url(csv_file), confirm = FALSE)), source_data)
  expect_equal(quiet_run(getC(compressed_csv_file, confirm = FALSE)), source_data)
  expect_equal(quiet_run(getC(csv2_file, confirm = FALSE)), source_data)
  expect_equal(quiet_run(getC(tsv_file, confirm = FALSE)), source_data)
  expect_equal(quiet_run(getC(rds_file, confirm = FALSE)), source_data)
  expect_equal(quiet_run(getC(as_file_url(rds_file), confirm = FALSE)), source_data)
})

test_that("getC imports SAS transport extensions", {
  source_data <- data.frame(id = 1:2, value = c(3, 4))
  xpt_file <- tempfile(fileext = ".xpt")
  xport_file <- tempfile(fileext = ".xport")
  on.exit(unlink(c(xpt_file, xport_file)), add = TRUE)
  haven::write_xpt(source_data, xpt_file)
  haven::write_xpt(source_data, xport_file)

  expect_equal(quiet_run(getC(xpt_file, confirm = FALSE)), source_data)
  expect_equal(quiet_run(getC(xport_file, confirm = FALSE)), source_data)
})

test_that("getC imports SAS data files", {
  source_data <- data.frame(id = 1:2, value = c(3, 4))
  sas_file <- tempfile(fileext = ".sas7bdat")
  on.exit(unlink(sas_file), add = TRUE)
  suppressWarnings(haven::write_sas(source_data, sas_file))

  expect_equal(quiet_run(getC(sas_file, confirm = FALSE)), source_data)
})

test_that("getC imports single-object native R files", {
  source_data <- data.frame(id = 1:2, value = c(3, 4))
  native_files <- c(
    tempfile(fileext = ".rda"),
    tempfile(fileext = ".rdata"),
    tempfile(fileext = ".rdat")
  )
  on.exit(unlink(native_files), add = TRUE)

  for (native_file in native_files) {
    save(source_data, file = native_file)
    expect_equal(quiet_run(getC(native_file, confirm = FALSE)), source_data)
  }
  expect_equal(quiet_run(getC(as_file_url(native_files[[1]]), confirm = FALSE)), source_data)
})

test_that("getC returns multiple native R objects as a named list", {
  first_object <- data.frame(id = 1)
  second_object <- data.frame(id = 2)
  native_file <- tempfile(fileext = ".rda")
  on.exit(unlink(native_file), add = TRUE)
  save(first_object, second_object, file = native_file)

  expect_message(
    imported <- getC(native_file, confirm = FALSE),
    "contains 2 objects"
  )
  expect_named(imported, c("first_object", "second_object"))
  expect_equal(imported$first_object, first_object)
  expect_equal(imported$second_object, second_object)
})

test_that("getC stops for unsupported file extensions", {
  unsupported_file <- tempfile(fileext = ".unknown")
  on.exit(unlink(unsupported_file), add = TRUE)
  file.create(unsupported_file)

  expect_error(
    quiet_run(getC(unsupported_file, confirm = FALSE)),
    "not a recognized data file type"
  )
})

test_that("getC validates file paths before importing", {
  expect_error(
    quiet_run(getC("missing-file.csv", confirm = FALSE)),
    "could not find a data file"
  )
  expect_error(
    quiet_run(getC(c("first.csv", "second.csv"), confirm = FALSE)),
    "one nonempty file path"
  )
})
