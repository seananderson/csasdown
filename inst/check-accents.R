is_broken <- function(x) {
  is.na(iconv(x, to = "latin1"))
}

find_non_latin <- function(x) {
  s <- strsplit(x, " ")
  test <- lapply(s, is_broken)
  broken <- unlist(lapply(test, any))
  w <- list()
  j <- 1
  if (sum(broken)) {
    for (i in which(broken)) {
      logic <- test[[i]]
      words_broken <- s[[i]][logic]
      for (k in seq_along(words_broken)) {
        w[[j]] <- words_broken[k]
        j <- j + 1
      }
    }
    w <- unlist(w)
    len <- length(w)
    w <- paste0(w, collapse = "', '")
    punct <- c(".", "?", "!", ")", "(")
    for (j in seq_along(punct)) {
      w <- gsub(paste0("\\", punct[j]), "", w)
    }
    if (len == 1) {
      m1 <- "'{w}' has a character in it that can not be converted."
    } else {
      m1 <- "'{w}' have characters in them that can not be converted."
    }
    msg <- c(
      m1,
      "Check for accent characters.",
      "Try replacing them with versions from https://french.typeit.org/"
    )
    cli::cli_abort(msg, call = NULL)

  }
}


x <- c("This is some text.", "espèce words words words", "Some more text.")
find_non_latin(x)

x <- c("This is aaespèce some text lllespèce.", "espèce words words words", "Some more text.")
find_non_latin(x)




# ------------------------------------
# ignore me:

# broken:
stringi::stri_enc_toascii("espèce")
# "espe\032ce"

# correct:
stringi::stri_enc_toascii("espèce")
# "esp\032ce"

broken <- "espèce"
correct <- "espèce"

grepl("[^ -~]", broken)
grepl("[^ -~]", correct)

grepNonASCII <- function(x) {
  asc <- iconv(x, "latin1", "ASCII")
  ind <- is.na(asc) | asc != x
  which(ind)
}

grepNonASCII(broken)
grepNonASCII(correct)

tools::showNonASCII(broken)
tools::showNonASCII(correct)

write_utf8 <- function(text, f = tempfile()) {

  # ensure text is UTF-8
  utf8 <- enc2utf8(text)

  # create connection with UTF-8 encoding
  con <- file(f, open = "w+", encoding = "UTF-8")
  writeLines(utf8, con = con)
  close(con)

  # read back from the file just to confirm
  # everything looks as expected
  readLines(f, encoding = "UTF-8")

}
write_utf8(broken) %>% tools::showNonASCII()
write_utf8(correct) %>% tools::showNonASCII()

pryr::bits(broken)
pryr::bits(correct)



is_non_latin <- function(x) {
  is.na(iconv(x, to = "latin1"))
}


broken <- 'è'
correct <- 'è'

is_broken(broken)
is_broken(correct)


latin1_b <- iconv(broken, to = "latin1")
latin1_c <- iconv(correct, to = "latin1")

latin1_b <- iconv(broken, to = "UTF-8")
latin1_c <- iconv(correct, to = "UTF-8")

latin1_b %>% is_broken()
latin1_c %>% is_broken()



paste(latin1_b, "(latin1):", pryr::bits(latin1_b))
paste(latin1_c, "(latin1):", pryr::bits(latin1_c))

latin1_b
latin1_c

write_utf8(broken) %>% pryr::bits()
write_utf8(correct) %>% pryr::bits()
