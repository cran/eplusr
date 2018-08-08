context("Job methods")

install_ep88()
example <- copy_example()

job <- eplus_job(example$idf, example$epw)

test_that("can run the simulation", {
    expect_output(job$run())
})

test_that("can get status of simulation", {
    expect_equal(job$status(),
        list(run_before = TRUE, changed_after = FALSE, terminated = FALSE,
            successful = TRUE, alive = FALSE, wait = TRUE)
    )
})

test_that("can return the output directory", {
    expect_equal(job$output_dir(), dirname(example$idf))
})

test_that("can return simulation errors", {
    expect_is(job$errors(), "ErrFile")
})

# clean
clean_wd(example$idf)
unlink(c(example$idf, example$epw))
