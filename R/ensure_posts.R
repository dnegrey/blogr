#' @export 
ensure_posts <- function(blogdir = ".") {
    posts <- paste(blogdir, "posts", sep = "/")
    if (!dir.exists(posts)) {
        dir.create(posts)
    }
}