# csasdown <img src='man/figures/csasdown-logo.png' align="right" height="139" />

> Reproducible CSAS Reports with R Markdown

<!-- badges: start -->
[![R-CMD-check](https://github.com/pbs-assess/csasdown/workflows/R-CMD-check/badge.svg)](https://github.com/pbs-assess/csasdown/actions)
<!-- badges: end -->

<!-- [![Coverage status](https://codecov.io/gh/pbs-assess/csasdown/branch/master/graph/badge.svg)](https://codecov.io/github/pbs-assess/csasdown?branch=master) -->

csasdown is an R package that facilitates generating Canadian Science Advisory Secretariat (CSAS) documents in PDF or Word format using R Markdown and bookdown.

Please check the [wiki](https://github.com/pbs-assess/csasdown/wiki) for hints and FAQs.

Slides from a short workshop on csasdown: [[PDF](https://www.dropbox.com/s/7m23mh3yfhk5ah8/csasdown-slides.pdf?dl=1)].

Have a problem? If it's a bug or feature request please post it as a [GitHub Issue](https://github.com/pbs-assess/csasdown/issues). If it's a quick question on csasdown use, find us on [DFO MS Teams](https://teams.microsoft.com/l/channel/19%3ae15d7a8e776b418b8e4975a4c9c5f93f%40thread.skype/R%2520-%2520csasdown?groupId=50a32c0d-d5fe-4368-b95b-4beaaa1ba1a1&tenantId=1594fdae-a1d9-4405-915d-011467234338).

**If you use csasdown to write a report, please let us know so we can add it to the list of publications using the package. This helps us justify spending our work time on its development. You can post an issue, make a pull request adding it to this readme, send us a quick email (contact details [here](https://github.com/pbs-assess/csasdown/blob/c76731e8580bb12da9324e56ff5e56a8d4901aeb/DESCRIPTION#L5-L8)), or find us on [DFO MS Teams](https://teams.microsoft.com/l/channel/19%3ae15d7a8e776b418b8e4975a4c9c5f93f%40thread.skype/R%2520-%2520csasdown?groupId=50a32c0d-d5fe-4368-b95b-4beaaa1ba1a1&tenantId=1594fdae-a1d9-4405-915d-011467234338).**

## Initial setup

To compile PDF documents using R, you need to have Pandoc, LaTeX, and several related packages installed. If you have a recent version of  [RStudio](http://www.rstudio.com/products/rstudio/download/), then you already have Pandoc.

1. You will need to install LaTeX if you do not have it already. Read [this Wiki page](https://github.com/pbs-assess/csasdown/wiki/Latex-Installation-for-csasdown) for a detailed description of this procedure. Most likely you will want to use the R package tinytex.

2. Install the csasdown package:

```r
install.packages("remotes")
remotes::install_github("pbs-assess/csasdown")
```

3. Create a new project in a new directory to hold your document project and all the files that csasdown creates. If you're using RStudio: click File -> New Project -> New Directory -> New Project, then type the name of the project in the **Directory name** box. Check the box **Open in new session**. If you are going to use GitHub version control (or if you are not sure), check the box **Create a git repository**. Click **Create Project**. A new RStudio project will open up, and will have its working directory set to the new document project's directory.

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

Note that the `techreport` example contains a lot of information on getting started with R Markdown and should be the first one you render if you are new to `csasdown`. The `resdoc` example contains other examples.

5. Render the document right away to make sure everything works by opening the file **index.Rmd** and clicking the **knit** button in RStudio. Once completed, a preview pane showing the PDF document will appear. The location of the PDF is in the **_book** directory. See the *Rendering* section below for more information.

6. Read the output PDF carefully and compare with what is written in the .Rmd files. This will help you understand more quickly how the document is put together and how you might want to structure your document.

7. *(Optional but recommended)* Create a blank repository on GitHub, commit your changes, and push to GitHub. New to Git? Start with <https://happygitwithr.com/>.

<!--
Open your git client software, navigate to the working directory of your new project and type the following commands:

```git add *``` to add all the new files you created in step 4.

```git commit -m "Initial commit"```

```git remote add origin URL``` where URL is the new repository URL as copied from the GitHub site.

```git remote -v``` verifies you entered the correct URL and it is bound to **origin**

```git push --set-upstream origin master```

Look on your GitHub repository and you should see all the new files there. Your git client is now set up to push and fetch from your repository on GitHub.
-->

8. Need to make an English *and* French version? csasdown has support for both. Also see csasdown's sister package [rosettafish](https://github.com/pbs-assess/rosettafish).

## Publications prepared using csasdown

DFO. 2021. Status Update of Pacifc Cod (*Gadus macrocephalus*) for West Coast Vancouver
Island (Area 3CD), and Hecate Strait and Queen Charlotte Sound (Area 5ABCD) in 2020.
DFO Can. Sci. Advis. Sec. Sci. Resp. 2021/002. 
[[English version](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2021/2021_002-eng.html)], [[French version](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2021/2021_002-fra.html)]. [[GitHub repository](https://github.com/pbs-assess/pacific-cod-2020)]

DFO. 2021. Stock status update with application of management procedures for Pacific Herring
(*Clupea pallasii*) in British Columbia: Status in 2020 and forecast for 2021. DFO Can. Sci.
Advis. Sec. Sci. Resp. 2021/001.
[[English](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2021/2021_001-eng.html)], [[French](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2021/2021_001-fra.html)]

Anderson, E.D., King, J.R. and Zubkowski, T.B. 2021. Ecosystem-Based Juvenile Pacific Salmon
(*Oncorhynchus* spp.) Survey on the North Coast of British Columbia, October 6-16, 2020. Can.
Data Rep. Fish. Aquat. Sci. 1331: vi + 36 p.
[[English]](http://publications.gc.ca/site/eng/9.894954/publication.html)

Obradovich, S.G., Hansen, S.C., Zhang, Z., MacNeill, S., Nichol, L.M., Rooper, C.N., St. Germain,
C., Curtis, D.L., Waddell, B.J., and Barton, L.L. In press. Pre-COSEWIC review of DFO
information on Northern Abalone (*Haliotis kamtschatkana*) along the Pacific Coast of Canada.
DFO Can. Sci. Advis. Sec. Res. Doc. In press/nnn. iv + 74 p.

Ouellette-Plante, J., Chabot, D., Nozères, C. et Bourdages, H. 2020. Régimes alimentaires de poissons démersaux provenant des relevés écosystémiques du NGCC Teleost dans l’estuaire et le nord du golfe du Saint-Laurent, août 2015-2017. Rapp. tech. can. sci. halieut. aquat. 3383: v + 124 p. [[French](https://waves-vagues.dfo-mpo.gc.ca/Library/40889191.pdf)] [[English](https://waves-vagues.dfo-mpo.gc.ca/Library/40889178.pdf)]

Holt, C.A., Freshwater, C., Holt, K., and Huang, A.-M. 2020. A quantitative tool for evaluating rebuilding plans for Pacific salmon. Can. Tech. Rep. Fish. Aquat. Sci. 3402: v + 26 p. [[English](https://waves-vagues.dfo-mpo.gc.ca/Library/40889385.pdf)]

DFO. 2020. Population status update for the northern resident killer whale (*Orcinus Orca*) in 2019. DFO Can. Sci. Advis. Sec. Sci. Resp. 2020/040. [[English](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_040-eng.html)] [[French](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_040-fra.html)]

Hansen, S.C., Obradovich, S.G., Rooper, C.N., Waddell, B.J., Nichol, L.M., MacNeill, S., and Barton, L.L. 2020. Identifying variables for standardization of the Northern Abalone (Haliotis kamtschatkana) Index Site Surveys time series (1978-2018) based on survey methodology and environmental variability. Can. Tech. Rep. Fish. Aquat. Sci. 3330: vii + 110 p. [[English](https://waves-vagues.dfo-mpo.gc.ca/Library/40849296.pdf)]

Boldt, J., Anderson, E., King, J., Dennis-Bohm, H., Zubkowski, T., and Flostrand, L. 2020. Integrated Pelagic Ecosystem Survey on the Vancouver Island Continental Shelf, June 15 - July 15, 2019. Can. Tech. Rep. Fish. Aquat. Sci. 3339: vii + 85 p. [[English](https://waves-vagues.dfo-mpo.gc.ca/Library/40843476.pdf)]

Lacko, L.C. and Acheson, S.M. and Connors, B.M. 2020. Summary of the Annual 2018 and 2019 Sablefish (*Anoplopoma fimbria*) Trap Surveys, October 9 - November 19, 2018 and October 8 - November 25, 2019. Can. Tech. Rep. Fish. Aquat. Sci. nnn: viii + 63 p. [[GitHub Repository]](https://github.com/sabledata/surveyreport)

DFO. 2020. Evaluating the robustness of candidate management procedures in the BC Sablefish (*Anoplopoma fibria*) fishery for 2019-2020. DFO Can. Sci. Advis. Sec. Sci. Resp. 2020/025. [[GitHub Repository]](https://github.com/samueldnj/SablefishSR2019)

Haggarty, D.R., Huynh, Q.C., Forrest, R.E., Anderson, S.C., Bresch, M.J., Keppel, E.A. In press.
Evaluation of potential rebuilding strategies for Inside Yelloweye Rockﬁsh (*Sebastes ruberrimus*) in British Columbia. DFO Can. Sci. Advis. Sec. Res. Doc. Accepted at Regional Peer Review meeting in June 2020. v + 135 p.
[[GitHub repository](https://github.com/pbs-assess/yelloweye-inside)]

Anderson, S.C., Forrest, R.E., Huynh, Q.C., Keppel, E.A. In press. A management procedure
framework for groundﬁsh in British Columbia. DFO Can. Sci. Advis. Sec. Res. Doc. 
Accepted at Regional Peer Review meeting in June 2020. v + 133 p.
[[GitHub repository](https://github.com/pbs-assess/gfmp)]

DFO. 2020. Stock status update with application of management procedures for Pacifc Herring
(*Clupea pallasii*) in British Columbia: Status in 2019 and forecast for 2020. DFO Can. Sci.
Advis. Sec. Sci. Resp. 2020/004. 
[[English](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-eng.html)]
[[French](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ScR-RS/2020/2020_004-fra.html)]
[[GitHub repository](https://github.com/pbs-assess/herringsr)]

Forrest, R.E., Anderson, S.C., Grandin, C.J., and Paul J. Starr. In press. Assessment of Pacific Cod (*Gadus macrocephalus*) for Hecate Strait and Queen Charlotte Sound (Area 5ABCD), and West Coast Vancouver Island (Area 3CD) in 2018. DFO Can. Sci. Advis. Sec. Res. Doc. 2020/070 iv + 204 p. [[English version](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_070-eng.html)], [[French version](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_070-fra.html)]. [[GitHub repository](https://github.com/pbs-assess/pacific-cod-2018)]

Swain, D.P., Ricard, D., Rolland, N., and Aubry, É. 2019. Assessment of the southern Gulf of
St. Lawrence Atlantic Cod (*Gadus morhua*) stock of NAFO Div. 4T and 4Vn (November to
April), March 2019. DFO Can. Sci. Advis. Sec. Res. Doc. 2019/038. iv + 105 p.
[[English](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_038-eng.html)]
[[French](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_038-fra.html)]

Anderson, S.C., Keppel, E.A., Edwards, A.M. 2019. A reproducible data synopsis 
for over 100 species of British Columbia groundfish. DFO Can. Sci. Advis. Sec.
Res. Doc. 2019/041. vii + 321 p.
[[English](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-eng.html)]
[[French](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2019/2019_041-fra.html)]
[[GitHub repository](https://github.com/pbs-assess/gfsynopsis)]

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

<!--
If you want to add a CSAS-formatted .docx title page to a Res Doc, edit the file `templates/RES2016-eng-titlepage.docx` as desired and run the command:

```r
csasdown::add_resdoc_docx_titlepage()
```

This will attach the title page to the beginning of the Word document.
-->

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

## Related projects

This project has drawn directly on code and ideas from the following:

- <https://github.com/benmarwick/huskydown>
- <https://github.com/ismayc/thesisdown>

[NAFOdown](https://github.com/nafc-assess/NAFOdown) is a derivative of csasdown for rendering NAFO (Northwest Atlantic Fisheries Organization) reports.

## Contributing

If you would like to contribute to this project, please start by reading our [Guide to Contributing](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
