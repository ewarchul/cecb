config_path = 
	"./test-config.yml"

testthat::test_that("function correctly parses yaml file", {
	parsed_config = 
		cecb::parse_yaml_config(config_path)
	config_fields =
		length(parsed_config)
	number_of_dims = 
		parsed_config$dims
	cpu_usage =
		parsed_config$cpupc
	number_of_methods = 
		length(parsed_config$methods)

	testthat::expect_equal(config_fields, 11)
	testthat::expect_equal(number_of_dims, 2)
	testthat::expect_equal(cpu_usage, 0.75)
	testthat::expect_equal(number_of_methods, 2)
})

testthat::test_that("function properly extracts names of algorithms", {
	parsed_config = 
		parse_yaml_config(config_path)
	alg_names = 
		cecb::extract_names(2, parsed_config$methods)
	testthat::expect_equal(alg_names, c("cma_es_quant", "cma_es_quant"))
})


testthat::test_that("function is able to distinguish correct and incorrect yaml configs", {
	parsed_config = 
		parse_yaml_config(config_path)
	testthat::expect_true(verify_config_names(parsed_config))
})





	




