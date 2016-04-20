#' @title Render a post to HTML
#' @description \code{render_post} renders a post to HTML
#' @param post path to the post
#' @return The post HTML file is created.
#' @seealso \code{\link{new_post}, \link{tag_post}, \link{remove_post}}
#' @examples 
#' new_post("Hello, world!", "hello-world")
#' render_post(post_dir(Sys.Date(), "hello-world"))
#' @importFrom rmarkdown render
#' @export 
render_post <- function(post) {
    if (!dir.exists(post)) {
        stop("the post [", post, "] does not exist")
    } else {
        post <- paste(post, "index.Rmd", sep = "/")
        render(post)
    }
}