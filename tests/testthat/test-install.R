test_that("Install", {
    skip_on_cran()
    expect_equal(as.character(avail_eplus()), names(.globals$eplus))
    if (is_avail_eplus(8.8)) expect_error(install_eplus(8.8, local = TRUE))
    else install_eplus(8.8, local = TRUE)

    # test if patch on EnergyPlus v9.1 and above works
    if (!is_avail_eplus(9.4)) install_eplus(9.4, local = TRUE)
    expect_true(is_avail_eplus(9.4))
})
