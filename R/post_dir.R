#' @export 
post_dir <- function(date, name, blogdir = ".") {
    paste(blogdir, "posts", paste(date, name, sep = "-"), sep = "/")
}