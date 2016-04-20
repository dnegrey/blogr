#' @export 
ensure_tags <- function(post) {
    tagFile <- paste(post, "tags", sep = "/")
    if (!file.exists(tagFile)) {
        file.create(tagFile)
    }
}