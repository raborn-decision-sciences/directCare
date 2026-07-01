test_that("report assembly functions are stubbed pending implementation", {
  expect_error(build_report_data(), class = "dcPlanR_not_implemented")
  expect_error(render_plan_report(list(), tempfile()), class = "dcPlanR_not_implemented")
})
