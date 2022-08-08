install.packages("remotes")

# Packages needed for Color coded tables
install.packages(c("caret", "e1071", "janitor", "rpart", "rpart.plot"))

# Install knitr and xaringan presentation package
remotes::install_github("yihui/xfun")
remotes::install_github("yihui/knitr")
remotes::install_github("yihui/xaringan")
remotes::install_github("gadenbuie/xaringanthemer")

# Install csasdown and dependencies
remotes::install_github("pbs-assess/csasdown")
remotes::install_github("pbs-assess/rosettafish")
