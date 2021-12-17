# csasdown

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
