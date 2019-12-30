# wiki_url(c("blablabla", page_zero))
wiki_url <- function(x) {
  wiki_root <- "https://en.wikipedia.org"
  loc <- str_sub(x, 1, nchar(wiki_root)) != wiki_root
  x[loc] <- paste(wiki_root, x[loc], sep = "/")
  x
}
