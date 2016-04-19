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