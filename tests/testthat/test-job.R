test_that("Job methods", {
    skip_on_cran()

    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    path_epw <- path_eplus_weather(LATEST_EPLUS_VER, "USA_CO_Golden-NREL.724666_TMY3.epw")

    expect_s3_class(job <- eplus_job(path_idf, path_epw), "EplusJob")

    expect_equal(job$version(), numeric_version(LATEST_EPLUS_VER))
    expect_output(job$print())

    # can get job status
    expect_equal(
        job$status(),
        list(run_before = FALSE, alive = FALSE, terminated = NA,
            successful = NA, changed_after = NA)
    )

    # can run job in waiting mode
    expect_s3_class(job$run(wait = TRUE, echo = FALSE), "EplusJob")

    # can refresh job status
    expect_equal(job$status(),
        list(run_before = TRUE, alive = FALSE, terminated = FALSE,
            successful = TRUE, changed_after = FALSE)
    )

    # can kill job
    expect_false(job$kill())

    # can read ERR file
    path_idf <- copy_eplus_example(LATEST_EPLUS_VER, "5Zone_Transformer.idf")
    job <- eplus_job(path_idf, path_epw)
    expect_s3_class({job$run(echo = FALSE);job$errors()}, "ErrFile")
    expect_s3_class(job$errors(info = TRUE), "ErrFile")
    expect_silent({err <- job$errors()})
    expect_equal(names(err), c("index", "envir_index", "envir",
        "level_index", "level", "message"
    ))
    expect_equal(attr(err, "eplus_version"), numeric_version(LATEST_EPLUS_VER))
    expect_equal(attr(err, "eplus_build"), ALL_EPLUS_RELEASE_COMMIT$commit[1])
    # New EnergyPlus version has removed the IDD version in ERR file
    expect_equal(attr(err, "idd_version"), NA)
    expect_equal(attr(err, "successful"), TRUE)
    expect_equal(attr(err, "terminated"), FALSE)

    # can retrieve simulation data
    idf <- read_idf(path_idf)
    job <- idf$run(path_epw, dir = NULL, echo = FALSE)
    # can get all table names
    expect_equal(length(job$list_table()), 44L)

    # can read table
    expect_error(job$read_table("a"), "no such table")
    expect_s3_class(job$read_table("Zones"), "data.table")

    # can read report data dictionary
    expect_s3_class(job$report_data_dict(), "data.table")

    # can read report data
    expect_equal(nrow(job$report_data()), 3840L)
    expect_equal(nrow(job$report_data(name = job$report_data_dict()[is.na(key_value), name])), 1344L)
    expect_equal(nrow(job$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(nrow(job$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate")),
        192L
    )
    expect_equal(year(job$report_data(
        "TRANSFORMER 1", "Transformer Load Loss Rate", year = 2010)$datetime),
        rep(2010, 192)
    )
    expect_equal(lubridate::tz(job$report_data(tz = "Asia/Shanghai")$datetime),
        "Asia/Shanghai"
    )
    expect_equal(job$report_data(case = "test")$case, rep("test", 3840))
    expect_equal(names(job$report_data(all = TRUE)),
        c("case", "datetime", "month", "day", "hour", "minute", "dst", "interval",
          "simulation_days", "day_type", "environment_name",
          "environment_period_index", "is_meter", "type", "index_group",
          "timestep_type", "key_value", "name", "reporting_frequency",
          "schedule_name", "units", "value"
        )
    )
    expect_equal(nrow(job$report_data(period = seq(
        lubridate::ymd_hms("2019-01-14 0:0:0"), lubridate::ymd_hms("2019-01-15 0:0:0"), "15 min")
    )), 1900)
    expect_equal(nrow(job$report_data(month = 1)), 1920)
    expect_equal(nrow(job$report_data(month = 1, hour = 1)), 80)
    expect_equal(nrow(job$report_data(minute = 0)), 960)
    expect_equal(nrow(job$report_data(interval = 15)), 3840)
    expect_equal(nrow(job$report_data(simulation_days = 1)), 3840)
    expect_equal(nrow(job$report_data(day_type = "Tuesday")), 3840)
    expect_equal(nrow(job$report_data(environment_name = "WINTERDAY")), 1920)

    expect_true(job == job)
    expect_false(job != job)

    skip_on_os("mac")
    # can get path
    expect_equal(job$path(), c(idf = path_idf, epw = path_epw))
    expect_equal(job$path("idf"), c(path_idf))
    expect_equal(job$path("epw"), c(path_epw))

    # can get output dir
    expect_equal(job$output_dir(), normalizePath(dirname(path_idf)))

    # can get output file path
    expect_equal(
        job$locate_output(".err"),
        normalizePath(file.path(tempdir(), "5Zone_Transformer.err"))
    )

    # can list all output files
    expect_equal(length(job$list_files()), 57L)
    expect_equal(length(job$list_files(full = TRUE)), 57L)
    expect_equal(length(job$list_files(simplify = TRUE)), 23L)
    expect_equal(length(job$list_files(simplify = TRUE, full = TRUE)), 23L)
    expect_equal(normalizePath(unique(dirname(job$list_files(simplify = TRUE, full = TRUE)))), job$output_dir())

    clean_wd(path_idf)
    unlink(c(path_idf, file.path(tempdir(), basename(path_epw))))
})

# vim: set fdm=marker:
