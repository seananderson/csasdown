# csasdown

[![Travis build status](https://travis-ci.org/pbs-assess/csasdown.svg?branch=master)](https://travis-ci.org/pbs-assess/csasdown)

csasdown is an R package that uses the bookdown package to generate Canadian Science Advisory Secretariat (CSAS) Research Documents ("Res Docs") in PDF or Word format using R Markdown. It is based on Chester Ismay's thesisdown package and Ben Marwick's huskydown package.

Slides from a recent workshop on csasdown [PDF](https://www.dropbox.com/s/7m23mh3yfhk5ah8/csasdown-slides.pdf?dl=1).

## Initial setup

Using csasdown has some prerequisites, such as Pandoc, LaTeX, and fonts. To compile PDF documents using R, you need to have Pandoc, LaTeX and several related packages installed. If you have a recent version of  [RStudio](http://www.rstudio.com/products/rstudio/download/), then you already have Pandoc and don't need to do anything more about that.

1) You will need to install LaTeX:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

2) Install the csasdown package: 

```r
# install.packages("devtools")
devtools::install_github("pbs-assess/csasdown")
```

3) Run this line in your R console to create a new Research Document from the built-in template:

```r
rmarkdown::draft("index.Rmd", template = "resdoc", 
  package = "csasdown", create_dir = FALSE, edit = FALSE)
```

Or, if you want to create the template files inside a subfolder, run:

```r
rmarkdown::draft("index.Rmd", template = "resdoc", 
  package = "csasdown", create_dir = TRUE, edit = FALSE)
```

Then look in the `index` folder. Rename the folder if you'd like.

## Day-to-day writing of a Research Document

You need to edit the individual chapter R Markdown files to write your Research Document. While writing, you should `git commit` your work frequently, after every major activity on your Research Document. For example, every few paragraphs or section of text, and after major step of analysis development. You should `git push` at the end of each work session before you leave your computer or change task. For gentle novice-friendly guide to getting starting with using Git with R and RStudio, see <http://happygitwithr.com/>.

## Rendering

To render your Res Doc into a PDF or Word document, open `index.Rmd` in RStudio and then click the "knit" button:

<img src="screenshots/knit.png" width="400">

To change the output formats between PDF, Word, and gitbook look at the `output:` field in `index.Rmd`and comment-out the formats you don't want. You can include multiple.

Alternatively, if you're not using RStudio, you can run this from the R console, assuming your have set the main directory (the one with the `index.Rmd` file) as your working directory:

```r
bookdown::render_book("index.Rmd")
```

The PDF or Word file of your Research Document will be deposited in the `_book/` directory.

<img src="screenshots/example-titlepage.png" width="450">

<img src="screenshots/example-page.png" width="450">

Run

```r
csasdown::add_resdoc_titlepage()
```

if you want to add a CSAS .docx title page to the .docx file or manually add this title page yourself after.

## Components

The following components are ones you should edit to customize your Research Document:

### `_bookdown.yml`

This is the main configuration file for your Research Document. It determines what .Rmd files are included in the output, and in what order. Arrange the order of your chapters in this file and ensure that the names match the names in your folders. If you add new chapters/sections, add them here.

### `index.Rmd`

This file contains all the meta information that goes at the beginning of your
document. You'll need to edit this to put your name on the first page, add the title of your Research Document, etc.

### `01-chap1.Rmd`, `02-chap2.Rmd`, etc.

These are the .Rmd files for each chapter/section of your report. Write your report in these.

### `bib/`

Store your bibliography (as BibTeX files) here. You might look at the [citr addin](https://github.com/crsh/citr) and [Zotero](https://www.zotero.org/) to efficiently manage and insert citations.

### `csl/`

Specific style files for bibliographies should be stored here. If you're writing a CSAS Research Document, you'll want to use the included `csas.csl`, which is based on the CJFAS (Canadian Journal of Fisheries and Aquatic Sciences) `.csl` file.

### `figure/` and `data/`

Store pre-made figures and data here and reference them in your R Markdown files. See the [bookdown book](https://bookdown.org/yihui/bookdown/) for details on cross-referencing items using R Markdown.

## Related projects

This project has drawn directly on code and ideas in the following:

- <https://github.com/benmarwick/huskydown>
- <https://github.com/ismayc/thesisdown>

## Contributing

If you would like to contribute to this project, please start by reading our [Guide to Contributing](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
