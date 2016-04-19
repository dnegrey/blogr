#' @title Ensure that the \code{posts} directory exists
#' @description \code{ensure_posts} ensures the existence of the \code{posts} 
#' directory
#' @param blogdir path to the base directory of the blog
#' @return The directory given by \code{paste(blogdir, "posts", sep = "/")} is 
#' created if it does not already exist.
#' @examples 
#' ensure_posts()
#' @export 
ensure_posts <- function(blogdir = ".") {
    posts <- paste(blogdir, "posts", sep = "/")
    if (!dir.exists(posts)) {
        dir.create(posts)
    }
}