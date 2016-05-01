#' @title Generate a search table for blog posts
#' @description \code{post_search} creates a searchable table of post information
#' @param blogdir path to the base directory of the blog
#' @param linkdir relative path to the blog directory to use in post hyperlinks
#' @param tagBackground background color to use for tags
#' @param tagColor font color to use for tags
#' @return A \code{\link{datatable}} containing post information
#' @seealso \code{\link{post_info}}
#' @examples 
#' lapply(seq.Date(Sys.Date() - 4, Sys.Date(), 1),
#'        new_post,
#'        title = "Lorem Ipsum", 
#'        name = "lorem-ipsum")
#' tag_post(get_post_dirs()[3], c("hello", "world"))
#' tag_post(get_post_dirs()[1], c("don't", "panic"))
#' post_search()
#' @importFrom DT datatable
#' @export 
post_search <- function(blogdir = ".",
                        linkdir = "/blog",
                        tagBackground = "#3399F3",
                        tagColor = "#FFFFFF") {
    x <- lapply(get_post_dirs(blogdir), post_info)
    x <- lapply(x, function(y){
        z <- y
        ulstart <- "<ul style=\"list-style: none; padding-left: 0;\">"
        ulend <- "</ul>"
        listart <- paste0(
            "<li ",
            "style=\"",
            "list-style-type: none;",
            "display: inline-block;",
            "height: 24px;",
            "line-height: 25px;",
            "font-size: 12px;",
            "margin-right: 10px;",
            "padding: 0 12px 0 12px;",
            paste0("background-color: ", tagBackground, ";"),
            paste0("color: ", tagColor, ";"),
            "-moz-border-radius-bottomleft: 16px;",
            "-webkit-border-bottom-left-radius: 16px;",
            "border-bottom-left-radius: 16px;",
            "-moz-border-radius-topleft: 16px;",
            "-webkit-border-top-left-radius: 16px;",
            "border-top-left-radius: 16px;",
            "\">"
        )
        divstart <- paste0(
            "<div ",
            "style=\"",
            "float: left;",
            "margin: 8px 4px 8px 0px;",
            "height: 8px;",
            "width: 8px;",
            "border-radius: 8px;",
            "-moz-border-radius: 8px;",
            "-webkit-border-radius: 8px;",
            paste0("background-color: ", tagColor, ";"),
            "\">"
        )
        divend <- "</div>"
        liend <- "</li>"
        if (length(z$tags) == 0) {
            z$tags <- paste0(ulstart, ulend)
        } else {
            z$tags <- toupper(z$tags)
            z$tags <- paste0(
                ulstart,
                paste0(listart,divstart, divend, z$tags, liend, collapse = ""),
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
        x$tags
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