#' @title Remove a post
#' @description \code{remove_post} removes a post
#' @param post path to the post
#' @return The directory \code{post} is removed if it exists.
#' @seealso \code{\link{new_post}}
#' @examples 
#' new_post("Hello, world!", Sys.Date(), "hello-world")
#' remove_post(post_dir(Sys.Date(), "hello-world"))
#' @export 
remove_post <- function(post) {
    if (dir.exists(post)) {
        unlink(post, recursive = TRUE, force = TRUE)
    }
}