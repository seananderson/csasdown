# csasdown

[![Travis build status](https://travis-ci.org/pbs-assess/csasdown.svg?branch=master)](https://travis-ci.org/pbs-assess/csasdown)
[![Coverage status](https://codecov.io/gh/pbs-assess/csasdown/branch/master/graph/badge.svg)](https://codecov.io/github/pbs-assess/csasdown?branch=master)

csasdown is an R package that uses the bookdown package to generate Canadian Science Advisory Secretariat (CSAS) documents in PDF or Word format using R Markdown. It is based on Chester Ismay's thesisdown package and Ben Marwick's huskydown package.

Slides from a short workshop on csasdown last year [[PDF](https://www.dropbox.com/s/7m23mh3yfhk5ah8/csasdown-slides.pdf?dl=1)].

If you use csasdown to write a report, please let us know so we can add it to the list of publications using the package. This helps us justify spending our work time on its development. You can post an issue, make a pull request adding it to this readme, or send us a quick email (contact details [here](https://github.com/pbs-assess/csasdown/blob/c76731e8580bb12da9324e56ff5e56a8d4901aeb/DESCRIPTION#L5-L8)).

Please check the [wiki](https://github.com/pbs-assess/csasdown/wiki) for hints and FAQs.

## Publications which were prepared using csasdown

Pacific Herring stock assessment Science Response in [English](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-eng.html) and [French](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-fra.html).

## Initial setup

To compile PDF documents using R, you need to have Pandoc, LaTeX and several related packages installed. If you have a recent version of  [RStudio](http://www.rstudio.com/products/rstudio/download/), then you already have Pandoc.

1) You will need to install LaTeX if you do not have it already. csasdown works with TeX Live on Unix or MikTeX on Windows, so you do not need to install [tinytex](https://yihui.org/tinytex/#for-r-users) if you already have one of these; however, you may find tinytex more convenient regardless. tinytex automates the process of downloading necessary packages for you whereas MikTeX often involves manual intervention on DFO networks. If you choose to install tinytex, you may choose to remove TeX Live or MikTeX first to keep things simple.

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

2) Install the csasdown package:

```r
# install.packages("devtools")
devtools::install_github("pbs-assess/csasdown")
```

3) Create a new project in a new directory to hold your document project and all the files that csasdown creates. If you're using RStudio: File -> New Project -> New Directory -> New Project. Check the box **Open in new session** (lower left). If you are going to use GitHub version control, check the box **Create a git repository**. Click **Create Project**. A new RStudio project will open up, and will have its working directory set to the new document project's directory. To check this, type ```getwd()``` in the console.

4) Run this line in your R console to create a new Research Document from the built-in template in whatever your working directory is:

```r
csasdown::draft("resdoc")
```

You can do the same for a Technical Report:

```r
csasdown::draft("techreport")
```

or for a Science Response:

```r
csasdown::draft("sr")
```

## Day-to-day writing

You need to edit the individual chapter R Markdown files to write your report. While writing, you should `git commit` your work frequently. For a gentle novice-friendly guide to getting starting with using Git with R and RStudio, see <http://happygitwithr.com/>.

## Rendering

To render your report into a PDF or Word document, open `index.Rmd` in RStudio and then click the "knit" button:

<img src="screenshots/knit.png" width="400">

To change the output formats between PDF and Word look at the `output:` field in `index.Rmd`and comment out the format you don't want.

Alternatively, if you're not using RStudio, you can run this from the R console, assuming your have set the main directory (the one with the `index.Rmd` file) as your working directory:

```r
bookdown::render_book("index.Rmd")
```

The rendered PDF or Word file of your report will be deposited in the `_book/` directory.

<img src="screenshots/example-titlepage.png" width="450">

<img src="screenshots/example-page.png" width="450">

If you want to add a CSAS-formatted .docx title page to a Res Doc, edit the file `templates/RES2016-eng-titlepage.docx` as desired and run the command:

```r
csasdown::add_resdoc_docx_titlepage()
```

This will attach the title page to the beginning of the Word document.

## Components

The following components are ones you should edit to customize your report:

### `_bookdown.yml`

This is the main configuration file for your report. It determines what .Rmd files are included in the output, and in what order. Arrange the order of your chapters in this file and ensure that the names match the names in your folders. If you add new sections, add them here.

### `index.Rmd`

This file contains all the meta information that goes at the beginning of your document. You'll need to edit this to put your name on the first page, add the title of your report, etc.

### `01-chap1.Rmd`, `02-chap2.Rmd`, etc.

These are the .Rmd files for each chapter/section of your report. Write your report in these.

### `bib/`

Store your bibliography (as BibTeX files) here. You might look at the [citr addin](https://github.com/crsh/citr) and [Zotero](https://www.zotero.org/) to efficiently manage and insert citations.

### `csl/`

Style files for bibliographies should be stored here. You will want to use the included `csas.csl`, which is based on the CJFAS (Canadian Journal of Fisheries and Aquatic Sciences) `.csl` file.

### `figure/` and `data/`

Store pre-made figures and data here and reference them in your R Markdown files. See the [bookdown book](https://bookdown.org/yihui/bookdown/) for details on cross-referencing items using R Markdown.

### `templates/`

This contains any `.docx` or `.tex` files that are need to compile the documents. With the exception of the title page file, you shouldn't have to edit any of these files.

## Related projects

This project has drawn directly on code and ideas in the following:

- <https://github.com/benmarwick/huskydown>
- <https://github.com/ismayc/thesisdown>

## Contributing

If you would like to contribute to this project, please start by reading our [Guide to Contributing](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
