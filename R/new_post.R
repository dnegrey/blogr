#' @title Create a new post
#' @description \code{new_post} creates a new post
#' @param title the title of the post
#' @param date the date of the post
#' @param name the name of the post
#' @param blogdir path to the base directory of the blog
#' @return A new post is created in the \code{posts} directory using the 
#' \code{title}, \code{date} and \code{name} that were supplied.
#' @seealso \code{\link{ensure_posts}}
#' @examples 
#' new_post("Hello, world!", Sys.Date(), "hello-world")
#' @export 
new_post <- function(title, date, name, blogdir = ".") {
    post_dir <- paste(blogdir, "posts", paste(date, name, sep = "-"), sep = "/")
    if (dir.exists(post_dir)) {
        stop("the post [", post_dir, "] already exists")
    } else {
        ensure_posts(blogdir)
        dir.create(post_dir)
    }
    content <- paste("#", title)
    write(content, paste(post_dir, "content.Rmd", sep = "/"))
    ensure_shell(blogdir)
    shell_in <- paste(blogdir, "shell.Rmd", sep = "/")
    shell_out <- paste(post_dir, "index.Rmd", sep = "/")
    file.copy(shell_in, shell_out)
}