#' Generate table with benchmark results
#' 
#' @param idpath path to benchmark results files :: String
#' @param problems list of function indices :: [Int] 
#' @param dim dimensionality
#' @param type type of budget step counter: {M, m} :: String
#' @param ... extra params for `kbl()` 
#' @export

get_resultTable = function(idpath, problems, dim, type = "m", ...) {
    table = problems %>%
     purrr::map(function(p){
         datapath = stringr::str_glue("{idpath}/{type}/{type}-{p}-D-{dim}.txt")
         df = readr::read_delim(datapath, ",", col_names = FALSE, col_types = cols(.default = col_double()))
         df %>%
            dplyr::last() %>%
            tibble::enframe() %>%
            dplyr::transmute(
                Function = p,
                Best = base::min(value),
                Worst = base::max(value),
                Median = stats::median(value),
                Mean = base::mean(value),
                Std = stats::sd(value)
            ) %>%
            dplyr::slice(dplyr::n())
     })  %>% 
     purrr::reduce(dplyr::bind_rows)
     kableExtra::kbl(table, digits = 20, ...) %>%
        kableExtra::kable_classic(full_width = F)
}
