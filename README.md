# csasdown

[![Travis build status](https://travis-ci.org/pbs-assess/csasdown.svg?branch=master)](https://travis-ci.org/pbs-assess/csasdown)
[![Coverage status](https://codecov.io/gh/pbs-assess/csasdown/branch/master/graph/badge.svg)](https://codecov.io/github/pbs-assess/csasdown?branch=master)

csasdown is an R package that uses the bookdown package to generate Canadian Science Advisory Secretariat (CSAS) documents in PDF or Word format using R Markdown. It is based on Chester Ismay's thesisdown package and Ben Marwick's huskydown package.

Please check the [wiki](https://github.com/pbs-assess/csasdown/wiki) for hints and FAQs.

Slides from a short workshop on csasdown last year [[PDF](https://www.dropbox.com/s/7m23mh3yfhk5ah8/csasdown-slides.pdf?dl=1)].

If you use csasdown to write a report, please let us know so we can add it to the list of publications using the package. This helps us justify spending our work time on its development. You can post an issue, make a pull request adding it to this readme, or send us a quick email (contact details [here](https://github.com/pbs-assess/csasdown/blob/c76731e8580bb12da9324e56ff5e56a8d4901aeb/DESCRIPTION#L5-L8)).

## Publications prepared using csasdown

Haggarty, D.R., Huynh, Q.C., Forrest, R.E., Anderson, S.C., Bresch, M.J., Keppel, E.A. In review.
Evaluation of potential rebuilding strategies for Inside Yelloweye Rockﬁsh (*Sebastes ruberrimus*) in British Columbia. DFO Can. Sci. Advis. Sec. Res. Doc. In review/nnn. v + 135 p.
[[GitHub repository]](https://github.com/pbs-assess/yelloweye-inside)

Anderson, S.C., Forrest, R.E., Huynh, Q.C., Keppel, E.A. In review. A management procedure
framework for groundﬁsh in British Columbia. DFO Can. Sci. Advis. Sec. Res. Doc. In review/nnn. v + 133 p.
[[GitHub repository]](https://github.com/pbs-assess/gfmp)

DFO. 2020. Stock status update with application of management procedures for Pacifc Herring
(*Clupea pallasii*) in British Columbia: Status in 2019 and forecast for 2020. DFO Can. Sci.
Advis. Sec. Sci. Resp. 2020/004. 
[[English]](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-eng.html)
[[French]](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-fra.html)
[[GitHub repository]](https://github.com/pbs-assess/herringsr)

Hansen, S.C., Obradovich, S.G., Rooper, C.N., Waddell, B.J., Nichol, L.M., MacNeill, S., and Barton, L.L. 2020.
Index Site Surveys time series (1978–2018) based on survey methodology and environmental variability. Can. Tech. Rep. Fish. Aquat. Sci. 3330: vii + 110 p.
[English](https://waves-vagues.dfo-mpo.gc.ca/Library/40849296.pdf)

Forrest, R.E., Anderson, S.C., Grandin, C.J., and Paul J. Starr. In press. Assessment of Pacific Cod (*Gadus macrocephalus*) for Hecate Strait and Queen Charlotte Sound (Area 5ABCD), and West Coast Vancouver Island (Area 3CD) in 2018. DFO Can. Sci. Advis. Sec. Res. Doc. In press/nnn. iv + 204 p.

Swain, D.P., Ricard, D., Rolland, N., and Aubry, É. 2019. Assessment of the southern Gulf of
St. Lawrence Atlantic Cod (*Gadus morhua*) stock of NAFO Div. 4T and 4Vn (November to
April), March 2019. DFO Can. Sci. Advis. Sec. Res. Doc. 2019/038. iv + 105 p.
[[English]](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_038-eng.html)
[[French]](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_038-fra.html)

Anderson, S.C., Keppel, E.A., Edwards, A.M. 2019. A reproducible data synopsis 
for over 100 species of British Columbia groundfish. DFO Can. Sci. Advis. Sec.
Res. Doc. 2019/041. vii + 321 p.
[[English]](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-eng.html),
[[French]](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-fra.html),
[[GitHub repository]](https://github.com/pbs-assess/gfsynopsis)

## Initial setup

To compile PDF documents using R, you need to have Pandoc, LaTeX and several related packages installed. If you have a recent version of  [RStudio](http://www.rstudio.com/products/rstudio/download/), then you already have Pandoc.

1. You will need to install LaTeX if you do not have it already. Read [this Wiki page](https://github.com/pbs-assess/csasdown/wiki/Latex-Installation-for-csasdown) for a detailed description of this procedure.

2. Install the csasdown package:

```r
install.packages("devtools")
devtools::install_github("pbs-assess/csasdown")
```

3. Create a new project in a new directory to hold your document project and all the files that csasdown creates. If you're using RStudio: click File -> New Project -> New Directory -> New Project, then type the name of the project in the **Directory name** box. Check the box **Open in new session**. If you are going to use GitHub version control (or if you are not sure), check the box **Create a git repository**. Click **Create Project**. A new RStudio project will open up, and will have its working directory set to the new document project's directory. To check this, type ```getwd()``` in the console.

4. Run this line in your R console to create a new Research Document from the built-in template in whatever your working directory is:

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

5. Render the document right away to make sure everything works by opening the file **index.Rmd** and clicking the **knit** button. Once completed, a preview pane showing the PDF document will appear. The location of the PDF is in the **_book** directory. See the *Rendering* section below for more information.

6. Read the output PDF carefully and compare with what is written in the Rmd files. This will help you understand more quickly how the document is put together and how you might want to structure your document.

7. *(Optional but recommended)* To make the initial commit on your GitHub page, first make a new, blank repository on GitHub. Open your git client software, navigate to the working directory of your new project and type the following commands:

```git add *``` to add all the new files you created in step 4.

```git commit -m "Initial commit"```

```git remote add origin URL``` where URL is the new repository URL as copied from the GitHub site. For example the URL for the csasdown project is https://github.com/pbs-assess/csasdown. This URL can usually be copied from your browser's URL line at the top and pasted into your command line.

```git remote -v``` verifies you entered the correct URL and it is bound to **origin**

```git push --set-upstream origin master```

Look on your GitHub repository and you should see all the new files there. Your git client is now set up to push and fetch from your repository on GitHub.

## Day-to-day writing

You need to edit the individual chapter R Markdown files to write your report. While writing, you should `git commit` your work frequently. For a gentle novice-friendly guide to getting starting with using Git with R and RStudio, see <http://happygitwithr.com/>.

## Rendering

***
**Render the document often!**

This can't be stressed enough. Every time you add something new, render the document to make sure you didn't break the build. It is much easier to find the problem when only one small known change was made since the last time it was rendered. 
***

To render your report into a PDF or Word document, open `index.Rmd` in RStudio and then click the "knit" button:

<img src="screenshots/knit.png" width="400">

To change the output formats between PDF and Word look at the YAML header part of `index.Rmd`  (the part between the two sets of triple dashes) and change this:

```
output:
 csasdown::resdoc_pdf:
   french: false
```

to this:

```
output:
 csasdown::resdoc_word:
   french: false
```

**Notes**
* This is also the only place you should be changing your document language.
* Replace `resdoc_pdf` and `resdoc_word` with `sr_pdf`, `sr_word`, `techreport_pdf`, or `techreport_word` for other document types.

Alternatively, if you're not using RStudio, you can run this from the R console, assuming your have set the main directory (the one with the `index.Rmd` file) as your working directory:

```r
bookdown::render_book("index.Rmd")
```

This method of rendering also allows you to insert `browser()` calls in your code and stop compilation to debug. It also does *not* open a preview viewer once finished, so you will have to navigate to the `_book/` directory and open it up manually.

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

This is the main configuration file for your report. It determines what `.Rmd` files are included in the output, and in what order. Arrange the order of your chapters in this file and ensure that the names match the names in your folders. If you add new `.Rmd` files, add them here. You may comment out some files while working on others by placing a `#` in front of them. This will stop compilation of those files, reducing the time to compile while working on another file.

### `index.Rmd`

This file contains all the meta information that goes at the beginning of your document. You'll need to edit this to put your name on the first page, add the title of your report, etc. **The name of this file cannot be changed.**

### `01-chap1.Rmd`, `02-chap2.Rmd`, etc.

These are the .Rmd files for each chapter/section of your report. Write your report in these. You can delete any or all of these and create as many of your own as you wish, but if you do you must change the **_bookdown.yml** file accordingly.

### `bib/`

Store your bibliography (as BibTeX files) here. You might look at the [citr addin](https://github.com/crsh/citr) and [Zotero](https://www.zotero.org/) to efficiently manage and insert citations.

### `csl/`

Style files for bibliographies should be stored here. You will want to use the included `csas.csl`, which is based on the CJFAS (Canadian Journal of Fisheries and Aquatic Sciences) `.csl` file.

### `figure/` and `data/`

Store pre-made figures and data here and reference them in your R Markdown files. See the [bookdown book](https://bookdown.org/yihui/bookdown/) for details on cross-referencing items using R Markdown.

### `templates/`

This contains any `.docx` or `.tex` files that are need to compile the documents. With the exception of the title page file, you shouldn't have to edit any of these files.

## Debugging the LaTeX output on its own

***Advanced topic***

Sometimes a document will knit perfectly but there will be an error in the LaTeX part. It can be very helpful fix the LaTeX error manually, then implement that fix into your R code. The easiest way to do this is to run the following from the root directory of your project (where your .Rproj file is located):

```r
  root_dir <- getwd()
  tmp_dir <- create_tempdir_for_latex(type = "resdoc", where = "r")
  # or tmp_dir <- create_tempdir_for_latex(type = "sr", where = "r")
  # or tmp_dir <- create_tempdir_for_latex(type = "techreport", where = "r")
  setwd(tmp_dir)
  tinytex::latexmk("resdoc.tex")
  # or tinytex::latexmk("sr.tex")
  # or tinytex::latexmk("techreport.tex")
```

This code creates a temporary directory somewhere on your file system. In Windows this will be in `C:\Users\your_user_name\AppData\Local\Temp` unless you provide a directory to the *tmp_dir* argument in the `create_tempdir_for_latex()` function.

The `where = "r"` part in the `create_tempdir_for_latex(type = "resdoc", where = "r")` call tells the function to look for the `resdoc.tex` file in the root directory of your project. If the compilation of the document fails for any reason, this file is left in the root directory. If it succeeds, it is found in the `_book` subdirectory. In that case you would use `create_tempdir_for_latex(type = "resdoc", where = "b")`. Usually you want to debug when compilation fails, so you would usually use `where = "r"` (the default).

When you run the `tinytex::latexmk("resdoc.tex")` command from within that temporary directory, LaTeX will be run on the `resdoc.tex` file and the PDF or Word file will be generated. You can debug LaTeX errors here by modifying the .tex file directly and re-running the `tinytex::latexmk("resdoc.tex")` function to see if it compiles. Once it does it is up to you to figure out what R code in your project needs to be modified to fix this issue. Note this directory is garbage-collected by R meaning it will be destroyed once the R session is closed. Don't write any code in the .tex file that you intend to keep.

When you are done, return to your project's root directory:
```r
  setwd(root_dir)
```

## Related projects

This project has drawn directly on code and ideas in the following:

- <https://github.com/benmarwick/huskydown>
- <https://github.com/ismayc/thesisdown>

## Contributing

If you would like to contribute to this project, please start by reading our [Guide to Contributing](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
