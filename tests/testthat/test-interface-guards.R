test_that("testmeansC enforces dv/iv xor x1/x2 interfaces", {
  expect_error(
    quiet_run(
      testmeansC(
        dv = states$vep20_turnout, iv = states$south,
        x1 = states$vep20_turnout, x2 = 50,
        plot = FALSE, ci.table = FALSE
      )
    ),
    "choose either dv/iv or x1/x2"
  )
  
  expect_error(
    quiet_run(testmeansC(x1 = states$vep20_turnout, plot = FALSE, ci.table = FALSE)),
    "missing required argument\\(s\\): x2"
  )
})

test_that("testpropsC enforces dv/iv xor x1/x2 and one-sample bounds", {
  expect_error(
    quiet_run(
      testpropsC(
        dv = states$south, iv = states$region,
        x1 = states$south, x2 = states$south,
        response = 1, plot = FALSE, ci.table = FALSE
      )
    ),
    "choose either dv/iv or x1/x2"
  )
  
  expect_error(
    quiet_run(testpropsC(x1 = states$south, x2 = 1.2, response = 1, plot = FALSE, ci.table = FALSE)),
    "Please correct this function's response argument or choose another x1"
  )
  
  expect_error(
    quiet_run(testpropsC(x1 = states$south, x2 = 1.2, response = "South", plot = FALSE, ci.table = FALSE)),
    "between 0 and 1"
  )
})
