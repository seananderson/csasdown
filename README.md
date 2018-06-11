# csasdown

csaddown uses the bookdown R package to generate CSAS Research Documents (Res Docs) in PDF, Word, or HTML using R Markdown. Based on Chester Ismay's thesisdown package and Ben Marwick's huskydown package.

### Initial setup

Using csasdown has some prerequisites, such as Pandoc, LaTeX and some fonts. To compile PDF documents using R, you need to have Pandoc, LaTeX and several related packages installed. If you have a recent version of  [RStudio](http://www.rstudio.com/products/rstudio/download/), then you already have Pandoc and don't need to do anything more about that.

Next is LaTeX. By far the easiest way to install LaTeX on any platform is with the [`tinytex`](https://yihui.name/tinytex/) package:

```r
install.packages(c("tinytex", "rmarkdown"))
tinytex::install_tinytex()
# after restarting RStudio, confirm that you have LaTeX with 
tinytex:::is_tinytex()
```

### Starting to write as Res Doc

To use csasdown from [RStudio](http://www.rstudio.com/products/rstudio/download/):

1) Ensure that you have already installed LaTeX and are using the latest version of [RStudio](http://www.rstudio.com/products/rstudio/download/). You can use csasdown without RStudio. For example, you can write the Rmd files in your favourite text editor (e.g. [Atom](https://atom.io/), [Notepad++](https://notepad-plus-plus.org/)). But RStudio is probably the easiest tool for writing both R code and text in your Res Doc. 

2) Install the csasdown package: 

```r
# install.packages("remotes")
remotes::install_github("pbs-assess/csasdown")
```

3) Run this line in your R console to create a new Res Doc from the template:

```r
rmarkdown::draft("index.Rmd", template = "resdoc", 
  package = "csasdown", create_dir = FALSE, edit = FALSE)
```

Or, if you want to create the template files inside a subfolder, run:

```r
rmarkdown::draft("index.Rmd", template = "resdoc", 
  package = "csasdown", create_dir = TRUE, edit = FALSE)
```

Then look in the `index` folder. Rename it if you'd like.

### Day-to-day writing of your Res Doc

You need to edit the individual chapter R Markdown files to write your Res Doc. While writing, you should `git commit` your work frequently, after every major activity on your Res Doc. For example, every few paragraphs or section of text, and after major step of analysis development. You should `git push` at the end of each work session before you leave your computer or change task. For gentle novice-friendly guide to getting starting with using Git with R and RStudio, see <http://happygitwithr.com/>.

## Rendering

To render your Res Doc into a PDF or Word document, open `index.Rmd` in RStudio and then click the "knit" button:

<img src="screenshots/knit.png" width="664">

To change the output formats between PDF, Word, and gitbook look at the `output:` field in `index.Rmd`and comment-out the formats you don't want. You can include multiple.

Alternatively, if you're not using RStudio, you can use this from the R console, assuming your have set the main directory (the one with the `index.Rmd` file) as your working directory:

```r
bookdown::render_book("index.Rmd", csasdown::resdoc_pdf())
```

The PDF or Word file of your Res Doc will be deposited in the `_book/` directory.

## Components

The following components are ones you should edit to customize your Res Doc:

### `_bookdown.yml`

This is the main configuration file for your Res Doc. It determines what Rmd files are included in the output, and in what order. Arrange the order of your chapters in this file and ensure that the names match the names in your folders. If you add new chapters/sections, add them here.

### `index.Rmd`

This file contains all the meta information that goes at the beginning of your
document. You'll need to edit this to put your name on the first page, add the title of your Res Doc, etc.

### `01-chap1.Rmd`, `02-chap2.Rmd`, etc.

These are the Rmd files for each chapter/section of your Res Doc. Write your Res Doc in these.

### `bib/`

Store your bibliography (as BibTeX files) here. You might look at the [citr addin](https://github.com/crsh/citr) and [Zotero](https://www.zotero.org/) to efficiently manage and insert citations.

### `csl/`

Specific style files for bibliographies should be stored here. If you're writing a CSAS Res Doc, you'll want to use the included `csas.csl`, which is based on the CJFAS `.csl` file.

### `figure/` and `data/`

Store your figures and data here and reference them in your R Markdown files. See the [bookdown book](https://bookdown.org/yihui/bookdown/) for details on cross-referencing items using R Markdown.

## Related projects

This project has drawn directly on code and ideas in the following:

- <https://github.com/benmarwick/huskydown>
- <https://github.com/ismayc/thesisdown>

## Contributing

If you would like to contribute to this project, please start by reading our [Guide to Contributing](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
