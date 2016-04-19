#' @title Ensure that the \code{shell} Rmd document exists
#' @description \code{ensure_shell} ensures the existence of the \code{shell} 
#' Rmd document
#' @param blogdir path to the base directory of the blog
#' @return The file given by \code{paste(blogdir, "shell.Rmd", sep = "/")} is 
#' created if it does not already exist.
#' @seealso \code{\link{ensure_posts}}
#' @examples 
#' ensure_shell()
#' @export 
ensure_shell <- function(blogdir = ".") {
    shell <- paste(blogdir, "shell.Rmd", sep = "/")
    if (!file.exists(shell)) {
        x <- c("",
               '```{r child="content.Rmd"}',
               '```')
        write(x, shell)
    }
}