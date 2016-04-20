#' @title Tag a post
#' @description \code{tag_post} tags a post
#' @param post path to the post
#' @param tags character vector of tags to apply
#' @param append logical value; add to existing tags (or replace)?
#' @return The \code{tags} file in the directory \code{post} is created if it 
#' does not already exist, and populated with the items in \code{tags}.
#' @seealso \code{\link{new_post}, \link{render_post}, \link{remove_post}}
#' @examples 
#' new_post("Hello, world!", Sys.Date(), "hello-world")
#' tag_post(post_dir(Sys.Date(), "hello-world"), c("greetings", "salutations"))
#' @export 
tag_post <- function(post, tags, append = TRUE) {
    if (!dir.exists(post)) {
        stop("the post [", post, "] does not exist")
    } else {
        ensure_tags(post)
        tagFile <- paste(post, "tags", sep = "/")
        if (!append) {
            write(tags, tagFile)
        } else {
            x <- readLines(tagFile)
            x <- unique(c(x, tags))
            x <- x[order(x)]
            write(x, tagFile)
        }
    }
}