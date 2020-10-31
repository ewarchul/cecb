testthat::test_that("ECDF values are less than 1", {
    # Given
    idpath = 
        "../testdata/benchmark-1"
    config = 
        list(dim = 10, probs = 1:30, reps = 51)
    # When
    out = 
        get_dfr(idpath, config)
    # Then
    testthat::expect_true(all(out$Value < 1))      
})

testthat::test_that("Function computes correctly ECDF values", {
    # Given
    df = 
      tibble::tibble(
        Method = rep("benchmark-1", 14),
        Bstep = c(0.04, 0.08, 0.12,
          0.2, 0.4, 0.8, 1.2, 1.6,
          2, 2.4, 2.8, 3.2, 3.6, 4),
        Value = 
          c(
            0.05405183, 0.14723708, 0.22416015, 0.37605923, 0.63814617, 0.78360071,
            0.80578637, 0.83392294, 0.85701357, 0.86899767, 0.87434526, 0.87678596,
            0.87818456, 0.87963801)
         )
    idpath = 
        "../testdata/benchmark-1"
    config = 
        list(dim = 10, probs = 1:30, reps = 51)
    # When
    out = 
        get_dfr(idpath, config)
    # Then
    testthat::expect_equal(out, df)      
})

testthat::test_that("Function is able to deal with different data formats", {
    # Given
    idpaths =
        c( 
            "../testdata/benchmark-1",
            "../testdata/benchmark-2"
    )
    expected = 
        list(
          txt_format = "../testdata/benchmark-1",
          json_format = "../testdata/benchmark-2"
        )
    # When
    out = 
        split_formats(idpaths)
    # Then
    testthat::expect_equal(out, expected)
})


testthat::test_that("Function properly computes area under curve", {
    # Given
    func_values = 
        tibble::tibble(
            x = 0:10,
            y = rep(10, 11),
            name = "constant"
        ) %>%
            dplyr::bind_rows(
                tibble::tibble(
                    x = 0:10,
                    y = (0:10)^2,
                    name = "quadratic"
            ))
    expected =
        tibble::tribble(
            ~name, ~Aoc,
            "constant_hand", 100,
            "quadratic_hand", 333
        )
    # When
    aocs = 
        func_values %>%
            compute_aoc("name", "x", "y") %>%
            dplyr::select(c("name", "Aoc")) 
    diffs = 
        dplyr::bind_rows(expected, aocs)
    # Then
    testthat::expect_true(all(abs(expected$Aoc - aocs$Aoc) < 5))

})