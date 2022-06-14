# csasdown

# csasdown 0.1.0

* A new render method called `csasdown::render()` will auto-detect your document type (resdoc, resdoc-b, sr, techreport) and render accordingly.
* New YAML options are required for this render method, an error will be produced when you run render() explaining which ones you are missing. They are all new French options and are required to render the document. Even if you don't plan on using French, you must enter them with some (any) default text. They are:
   - french_month
   - french_region
   - french_address
   - french_title
   - french_abstract
* There is a new example document called **resdoc-b** which you can get by running `csasdown::draft("resdoc-b")` and then rendering with `csasdown::render()`. This document is built with and explains all the new features and the new `render()` function.

# csasdown 0.0.10.9000

* Add ISBN and Cat number to Res Docs (new mandatory fields)
* Add `gsub(" :", "~:", x)` to catch dangling `:` in French

# csasdown 0.0.8.9001

* Started recording NEWS.md

* Added `run_pdflatex()` for cases where latexmk doesn't run pdflatex enough times to update page numbers for long table of contents. #151

* Decreased spacing in Res Doc abstracts based on CSAP request. #147

* Added Tech Report cover and 2nd pages to match new format exactly. This requires modifying a .docx file. #146

# csasdown 0.0.0.9000

* Adapted huskydown for CSAS Res Docs
