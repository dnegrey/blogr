#' @title Generate a search table for blog posts
#' @description \code{post_search} creates a searchable table of post information
#' @param blogdir path to the base directory of the blog
#' @param linkdir relative path to the blog directory to use in post hyperlinks
#' @return A \code{\link{datatable}} containing post information
#' @seealso \code{\link{post_info}}
#' @examples 
#' lapply(seq.Date(Sys.Date() - 4, Sys.Date(), 1),
#'        new_post,
#'        title = "Lorem Ipsum", 
#'        name = "lorem-ipsum")
#' tag_post(get_post_dirs()[1], c("hello", "world"))
#' post_search()
#' @importFrom DT datatable
#' @export 
post_search <- function(blogdir = ".", linkdir = "/blog") {
    x <- lapply(get_post_dirs(blogdir), post_info)
    x <- lapply(x, function(y){
        z <- y
        ulstart <- "<ul class=\"tags\">"
        ulend <- "</ul>"
        if (length(z$tags) == 0) {
            z$tags <- paste0(ulstart, ulend)
        } else {
            z$tags <- toupper(z$tags)
            z$tags <- paste0(
                ulstart,
                paste0("<li>", z$tags, "</li>", collapse = ""),
                ulend,
                collapse = ""
            )
        }
        return(z)
    })
    x <- lapply(x, as.data.frame, stringsAsFactors = FALSE)
    x <- do.call(rbind, x)
    x <- x[order(x$date, decreasing = TRUE), ]
    x$link <- gsub(paste0(blogdir, "/posts/"),
                   paste0(linkdir, "/posts/"),
                   x$path)
    x$Title <- paste0(
        "<a href=",
        '"',
        x$link,
        '">',
        x$title,
        "</a>",
        "<p>",
        "<em>",
        format(x$date, "%B %d, %Y"),
        "</em>",
        "</p>",
        "<p>",
        x$tags,
        "</p>"
    )
    x <- x[c("Title")]
    x <- datatable(
        x,
        rownames = FALSE,
        colnames = "Recent Posts",
        escape = FALSE,
        options = list(
            ordering = FALSE
        )
    )
    return(x)
}