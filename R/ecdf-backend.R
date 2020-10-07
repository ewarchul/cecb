#' Check file format
#'
#' @description Function checks if given `filepath` has JSON or TXT format.
#' @param filepath :: [character]
#' @return type of format :: [character] 

check_format = function(filepath) {
	has_json = 
		base::list.files(filepath) %>%
		purrr::detect(stringr::str_detect, "json")
	if (!is.null(has_json))
		"JSON"
	else
		"TXT"
}

#' Benchmark results data frame
#'
#' @description Function generates ready to plot data frame with benmchark results.
#' @param idpath filepath to benchmark data :: [character]
#' @param config map with benchmark config: :: [dim :: integer, probnums :: [integer], reps :: integer]
#' @return data frame with benchmark data :: tibble

get_dfr = function(idpath, config) {
	if (missing(config)) {
			cli::cli_alert_danger("User has to provide map with benchmark configuration. For more details check documentation.")
			base::stop("Missing config.")
		}

	if (check_format(idpath) == "JSON")
		generate_dfr(idpath, config, load_result_json)
	else 
		generate_dfr(idpath, config, load_result_txt)
}

#' Compute ECDF values
#' 
#' @description 
#' Function computes ECDF values and put results into data frame.
#' @param benchmark_data previously loaded and parsed data from files :: [numeric]
#' @param idpath filepath to benchmark results :: character
#' @param probnums problems for which ECDF will be computed :: [integer]
#' @param rep benchmark repetitions :: integer
#' @return data frame with ECDF values

compute_ecdf = function(benchmark_data, idpath, probnums, rep) {
	bsteps = 
    c(0.01, 0.02, 0.03, 0.05, seq(0.1, 1.0, 0.1))*log10(10000)
  ecdf_vals = purrr::map(benchmark_data, get_ecdf)
	ecdf_ms = get_ms(ecdf_vals, rep)
	get_mincnt(
						 extract_id(idpath),
						 benchmark_data,
						 ecdf_vals,
						 probnums,
						 bsteps,
						 rep,
						 .max_succ = ecdf_ms
	)
}


generate_dfr = function(idpaths, config, format_handler) {
	c(dim, probnums, rep) %<~% 
		config
	results = 
    purrr::map(probnums, format_handler, idpaths, dim) 
	compute_ecdf(results, idpaths, probnums, rep) 
}

#' 
#'
#'
#'

parse_json = function(json, probnum, dim) {
	json %>% 
		purrr::pluck(as.character(dim), as.character(probnum)) %>%
		purrr::flatten() %>%
		purrr::flatten_dbl()
}

#' Extract id 
#'
#' @description Function extracts benchmarks ids from list of filepaths. 
#' @param idpaths list of filepaths to benchmark :: [character] 
#' @return list with raw ids of benchamrks :: [character]

extract_id = function(idpaths) {
	idpaths %>%
			purrr::map(stringr::str_extract, "([^/]+$)")
}


#' Load data in TXT format
#' 
#' @description
#' Function reads results of given benchmark from TXT file.
#' @param probnum problem number :: [integer]
#' @param idpaths benchmark ids :: [character]
#' @param dim dimensionality of problem :: integer
#' @export

load_result_txt = function(probnum, idpaths, dim) {
    idpaths %>%
    purrr::map(function(id) {
			filepath = 
				stringr::str_glue("{id}/M/M-{probnum}-D-{dim}.txt")
      read.table(file = filepath, sep = ",")
      }) %>% 
    purrr::set_names(extract_id(idpaths))
}


#' Load data in JSON format
#' 
#' @description
#' Function reads results of given benchmark from JSON file.
#' @param probnum problem number :: [integer]
#' @param idpaths benchmark ids :: [character]
#' @param dim dimensionality of problem :: integer
#' @export

load_result_json = function(probnum, idpaths, dim) {
idpaths %>%
    purrr::map(function(id) {
			bench_id = 
				extract_id(id)
			filepath = 
				stringr::str_glue("{id}/{bench_id}.json")
      jsonlite::read_json(path = filepath) %>%
				parse_json(probnum, dim) %>%
				matrix(nrow = 14) %>%
				data.frame()
      }) %>%
    purrr::set_names(extract_id(idpaths))
}

