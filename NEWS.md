# csasdown

# csasdown 0.1.0

* A new function `csasdown::render()` will auto-detect your document type (*resdoc*, *resdoc-b*, *sr*, *techreport*) and render accordingly. We recommend using this over `bookdown::render_book()`. The 'Knit' button in RStudio should properly choose `csasdown::render()`.

* New YAML options are required for this render method; an error will be produced when you run `render()` explaining which ones you are missing. These new YAML options are new French options and are required to render the document. Even if you don't plan on using French, you must enter them with some (any) default text. They are:
   - `french_month`
   - `french_region`
   - `french_address`
   - `french_title`
   - `french_abstract`
   
* The four example documents (*resdoc*, *resdoc-b*, *sr*, *techreport*) contain all the current YAML options.

* There is a new example document called **resdoc-b** which you can get by running `csasdown::draft("resdoc-b")` and then rendering with `csasdown::render()`. That document is built with and explains all the new features and the new `render()` function.

* If you're using an old version of **index.Rmd** in your project you should modify the `knit:` YAML tag to the following:
  ```
  knit: (function(input, ...) {
        csasdown::render('_bookdown.yml', doc_type = csasdown::get_doc_type())`
        })
  ```
* If you're using an old version of **index.Rmd** in your project you should delete these chunks of code from it:
  ```
  meta <- rmarkdown::metadata$output
  if (length(grep("pdf", names(meta)))) {
    french <- meta$`csasdown::resdoc_pdf`$french
    prepub <- meta$`csasdown::resdoc_pdf`$prepub
  } else if (length(grep("word", names(meta)))) {
    french <- meta$`csasdown::resdoc_word`$french
    prepub <- meta$`csasdown::resdoc_word`$prepub
  }
  csl <- "csl/csas.csl"
  if (french) {
    csl <- "csl/csas-french.csl"
    options(OutDec = ",")
  }
  ```
  and
  ```
  ---
  csl: `r csl`    
  ---
  ```

# csasdown 0.0.10.9000

* Reduce space above title in SRs #207

* Implement 2022 CSAS formatting updates #204

* Add ISBN and Cat number to Res Docs (new mandatory fields)

* Add `gsub(" :", "~:", x)` to catch dangling `:` in French

# csasdown 0.0.8.9001

* Started recording NEWS.md

* Added `run_pdflatex()` for cases where latexmk doesn't run pdflatex enough times to update page numbers for long table of contents. #151

* Decreased spacing in Res Doc abstracts based on CSAP request. #147

* Added Tech Report cover and 2nd pages to match new format exactly. This requires modifying a .docx file. #146

# csasdown 0.0.0.9000

* Adapted huskydown for CSAS Res Docs
