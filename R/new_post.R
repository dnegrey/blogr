#' @title Create a new post
#' @description \code{new_post} creates a new post
#' @param title the title of the post
#' @param name the name of the post
#' @param date the date of the post
#' @param blogdir path to the base directory of the blog
#' @return A new post is created in the \code{posts} directory using the 
#' \code{title}, \code{date} and \code{name} that were supplied.
#' @seealso \code{\link{tag_post}, \link{render_post}, \link{remove_post}}
#' @examples 
#' new_post("Hello, world!", "hello-world")
#' @export 
new_post <- function(title, name, date = Sys.Date(), blogdir = ".") {
    pd <- post_dir(date, name, blogdir)
    if (dir.exists(pd)) {
        stop("the post [", pd, "] already exists")
    } else {
        ensure_posts(blogdir)
        dir.create(pd)
        content <- c(
            paste("#", title),
            "",
            paste0("*", format(date, "%B %d, %Y"), "*")
        )
        write(content, paste(pd, "content.Rmd", sep = "/"))
        ensure_shell(blogdir)
        shell_in <- paste(blogdir, "shell.Rmd", sep = "/")
        shell_out <- paste(pd, "index.Rmd", sep = "/")
        file.copy(shell_in, shell_out)
        ensure_tags(pd)
    }
}