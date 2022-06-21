test_that("parse_pandoc_highlight_theme() works", {

  # On GitHub Actions..
  gha_dir <- Sys.getenv("RUNNER_TEMP")
  on_gha <- gha_dir != ""
  if(on_gha){
    json <- readLines(file.path(gha_dir, "tango.theme"))
  }else{
    pandoc_path <- Sys.getenv("RSTUDIO_PANDOC")
    pandoc_path <- gsub("Program Files", "Progra~1", pandoc_path)
    if(pandoc_path == ""){
      stop("`pandoc_path` is nonexistent", call. = FALSE)
    }
    cmd <- paste(file.path(pandoc_path, "pandoc"),  "--print-highlight-style tango")
    json <- system(cmd, intern = TRUE)
  }

  j <- csasdown:::parse_pandoc_highlight_theme(json)
  expect_true(all(lengths(purrr::map(j[[3]], ~{as.logical(grep("text-color", .x))}))))
  expect_identical(names(j), c("text-color", "background-color", "sections-list"))
  expect_identical(class(j), "list")
  expect_true(is.na(j[[1]]))
  expect_identical(j[[2]], "#f8f8f8")
  expect_identical(names(j[[3]]), c("Other", "Attribute",
                                    "SpecialString", "Annotation",
                                    "Function", "String",
                                    "ControlFlow", "Operator",
                                    "Error", "BaseN",
                                    "Alert", "Variable",
                                    "Extension", "Preprocessor",
                                    "Information", "VerbatimString",
                                    "Warning", "Documentation",
                                    "Import", "Char",
                                    "DataType", "Float",
                                    "Comment", "CommentVar",
                                    "Constant", "SpecialChar",
                                    "DecVal", "Keyword"))
  # Select a few random Toks to check for tango
  op_list <- j[[3]]$Alert
  expect_identical(op_list[[1]], c("text-color", "#ef2929"))
  expect_identical(op_list[[2]], c("background-color", "null"))
  expect_identical(op_list[[3]], c("bold", "false"))
  expect_identical(op_list[[4]], c("italic", "false"))
  expect_identical(op_list[[5]], c("underline", "false"))

  op_list <- j[[3]]$Function
  expect_identical(op_list[[1]], c("text-color", "#000000"))
  expect_identical(op_list[[2]], c("background-color", "null"))
  expect_identical(op_list[[3]], c("bold", "false"))
  expect_identical(op_list[[4]], c("italic", "false"))
  expect_identical(op_list[[5]], c("underline", "false"))

  op_list <- j[[3]]$Keyword
  expect_identical(op_list[[1]], c("text-color", "#204a87"))
  expect_identical(op_list[[2]], c("background-color", "null"))
  expect_identical(op_list[[3]], c("bold", "true"))
  expect_identical(op_list[[4]], c("italic", "false"))
  expect_identical(op_list[[5]], c("underline", "false"))

  op_list <- j[[3]]$Operator
  expect_identical(op_list[[1]], c("text-color", "#ce5c00"))
  expect_identical(op_list[[2]], c("background-color", "null"))
  expect_identical(op_list[[3]], c("bold", "true"))
  expect_identical(op_list[[4]], c("italic", "false"))
  expect_identical(op_list[[5]], c("underline", "false"))

  back_json <- json
  json <- gsub("text-color", "", json)
  json <- gsub("background-color", "", json)
  j <- csasdown:::parse_pandoc_highlight_theme(json)
  expect_false(any(lengths(purrr::map(j[[3]], ~{as.logical(grep("text-color", .x))}))))
  expect_true(is.na(j[[1]]))
  expect_true(is.na(j[[2]]))
})
