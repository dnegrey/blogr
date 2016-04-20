#' @export 
get_post_dirs <- function(blogdir = ".") {
    list.files(paste(blogdir, "posts", sep = "/"), full.names = TRUE)
}