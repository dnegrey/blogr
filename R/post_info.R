#' @title Fetch the information about a post
#' @description \code{post_info} fetches the information about a post and 
#' returns it in a named list
#' @param post path to the post
#' @return A named list containing information about \code{post}
#' @seealso \code{\link{new_post}, \link{tag_post}}
#' @examples 
#' new_post("Hello, world!", "hello-world")
#' tag_post(post_dir(Sys.Date(), "hello-world"), c("greetings", "salutations"))
#' post_info(post_dir(Sys.Date(), "hello-world"))
#' @export 
post_info <- function(post) {
    if (!dir.exists(post)) {
        stop("the post [", post, "] does not exist")
    } else {
        pd <- strsplit(post, "/", TRUE)[[1]]
        pd <- pd[length(pd)]
        date <- as.Date(substr(pd, 1, 10))
        name <- substr(pd, 12, nchar(pd))
        contentFile <- paste(post, "content.Rmd", sep = "/")
        title <- readLines(contentFile)[1]
        title <- substr(title, 3, nchar(title))
        z <- unlist(gregexpr("\\{", title))
        z <- z[length(z)]
        z <- ifelse(z == -1, nchar(title) + 1, z)
        title <- substr(title, 1, z - 1)
        title <- trimws(title)
        tagFile <- paste(post, "tags", sep = "/")
        tags <- readLines(tagFile)
        x <- list(date = date,
                  name = name,
                  title = title,
                  tags = tags,
                  path = post)
        return(x)
    }
}