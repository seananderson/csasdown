# csasdown

Uses the bookdown R package to generate CSAS Research Documents in
PDF, Word, or HTML using R Markdown. 
Based on Chester Ismay's thesisdown package and
Ben Marwick's huskydown package.

### Initial setup

Using **csasdown** has some prerequisites, such as Pandoc, LaTeX and some fonts. To compile PDF documents using **R**, you need to have Pandoc, LaTeX and several related packages installed. If you have a recent version of  [RStudio](http://www.rstudio.com/products/rstudio/download/), then you already have Pandoc and don't need to do anything more about that. 

Next is LaTeX. By far the easiest way to install LaTeX on any platform is with the [`tinytex`](https://yihui.name/tinytex/) package:

```r
install.packages(c('tinytex', 'rmarkdown'))
tinytex::install_tinytex()
# after restarting RStudio, confirm that you have LaTeX with 
tinytex:::is_tinytex()
```

### Starting to write as Res Doc

To use **csasdown** from [RStudio](http://www.rstudio.com/products/rstudio/download/):

1) Ensure that you have already installed LaTeX and the fonts described above, and are using the latest version of [RStudio](http://www.rstudio.com/products/rstudio/download/). You can use csasdown without RStudio. For example, you can write the Rmd files in your favourite text editor (e.g. [Atom](https://atom.io/), [Notepad++](https://notepad-plus-plus.org/)). But RStudio is probably the easiest tool for writing both R code and text in your Res Doc. 

2) Install the **bookdown** and **csasdown** packages: 

```r
# install.packages("devtools")
devtools::install_github("rstudio/bookdown")
devtools::install_github("pbs-assess/csasdown")
```

3) Use the **New R Markdown** dialog to select **CSAS-ResDoc**, here are the steps, and a screenshot below:

File -> New File -> R Markdown... then choose 'From template', then choose 'CSAS-ResDoc, and enter `index` as the **Name**. Note that this will currently only **Knit** if you name the directory `index` at this step. 

Or if you're not using RStudio, run this line in your R console to create a new Res Doc from the template:

```r
rmarkdown::draft("index.Rmd", template = "resdoc", package = "csasdown", create_dir = TRUE)
```

### Day-to-day writing of your Res Doc

You need to edit the individual chapter R Markdown files to write your Res Doc. 

You can write in the Rmd files without RStudio (in fact RStudio lacks some conveniences for writing, such as live spell-checking and live word count). So you may prefer to do some writing and editing your Rmd files in your favourite text editor. I frequently use [Atom](https://atom.io/), [Notepad++](https://notepad-plus-plus.org/), and [Emacs](https://www.gnu.org/software/emacs/). But I come back to RStudio to create the PDF and work on the R code in my documents. 

While writing, you should `git commit` your work frequently, after every major activity on your Res Doc. For example, every few paragraphs or section of text, and after major step of analysis development. You should `git push` at the end of each work session before you leave your computer or change task. For gentle novice-friendly guide to getting starting with using Git with R and RStudio, see <http://happygitwithr.com/>.

## Rendering

To render your Res Doc into a PDF, open `index.Rmd` in RStudio and then click the "knit" button. To change the output formats between PDF, gitbook and Word , look at the `output:` field in `index.Rmd`and comment-out the formats you don't want.

Alternatively, if you're not using RStudio, you can use this from the R console, assuming your have set the `'index/` directory as your working directory:

```r
bookdown::render_book("index.Rmd", csasdown::resdoc_pdf())
```

The PDF file of your Res Doc will be deposited in the `_book/` directory.

## Components

The following components are ones you should edit to customize your Res Doc:

### `_bookdown.yml`

This is the main configuration file for your Res Doc. It determines what Rmd files are included in the output, and in what order. Arrange the order of your chapters in this file and ensure that the names match the names in your folders. 

### `index.Rmd`

This file contains all the meta information that goes at the beginning of your
document. You'll need to edit this to put your name on the first page, the title of your Res Doc, etc.

### `01-chap1.Rmd`, `02-chap2.Rmd`, etc.

These are the Rmd files for each chapter in your dissertation. Write your Res Doc in these. If you're writing in RStudio, you may find the [wordcount addin](https://github.com/pbs-assess/wordcountaddin) useful for getting word counts and readability statistics in R markdown documents. You might also enjoy writing in these Rmd files with [Atom](https://atom.io/), [Notepad++](https://notepad-plus-plus.org/), [Sublime Text](https://www.sublimetext.com/), and [Emacs](https://www.gnu.org/software/emacs/). 

### `bib/`

Store your bibliography (as bibtex files) here. We recommend using the [citr addin](https://github.com/crsh/citr) and [Zotero](https://www.zotero.org/) to efficiently manage and insert citations. 

### `csl/`

Specific style files for bibliographies should be stored here. A good source for
citation styles is https://github.com/citation-style-language/styles#readme

### `figure/` and `data/`

Store your figures and data here and reference them in your R Markdown files. See the [bookdown book](https://bookdown.org/yihui/bookdown/) for details on cross-referencing items using R Markdown.

## Related projects

This project has drawn directly on code and ideas in the following:

- https://github.com/benmarwick/huskydown    
- https://github.com/UWIT-IAM/UWThesis    
- https://github.com/stevenpollack/ucbthesis  
- https://github.com/suchow/Dissertate    
- https://github.com/SeungkiKwak/Kwak_S_PhD_thesis    
- https://github.com/dhalperi/uwthesis-tweaked     

Other relevant projects:

- Ed Berry's blog post ['Writing your thesis with bookdown'](https://eddjberry.netlify.com/post/writing-your-thesis-with-bookdown/), Posted on September 25, 2017    
- Rosanna van Hespen's ([@rosannavhespen](https://twitter.com/rosannavhespen?lang=en)) five blog posts on ['Writing your thesis with R Markdown'](https://rosannavanhespenresearch.wordpress.com/2016/02/03/writing-your-thesis-with-r-markdown-1-getting-started/)
- [thesisdowndss](https://github.com/mine-cetinkaya-rundel/thesisdowndss) by Mine Cetinkaya-Rundel at Duke University    
- [beaverdown](https://github.com/zkamvar/beaverdown) by Zhian Kamvar at Oregon State University

## Contributing

If you would like to contribute to this project, please start by reading our [Guide to Contributing](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

<!--
To update the PDF template stored in inst/ assuming we are at top level:

rmarkdown::draft('index.Rmd', template = 'thesis', package = 'csasdown', create_dir = TRUE, edit = FALSE)

setwd('index')

bookdown::render_book('index.Rmd', csasdown::thesis_pdf(latex_engine = 'xelatex'))

-->

