source_github <- function(file_path, repo, user, token = NULL, username = user) {
    url <- paste("https://api.github.com/repos/", user, "/", repo, "/contents/", file_path, sep = "")
    if (is.null(token)) {
        httpheader <- c(
            Accept = "application/vnd.github-blob.raw"
        )
    } else {
        httpheader <- c(
            Accept = "application/vnd.github-blob.raw",
            'Authorization' = paste("token ", token, sep = "")
        )
    }
    resp <- getURL(
        url,
        httpheader = httpheader,
        useragent = username,
        header = TRUE
    )
    resp_parse <- parseHTTPHeader(resp)
    status <- resp_parse["status"]
    statusMessage <- resp_parse["statusMessage"]
    if (status != "200") stop(paste("Error:", statusMessage))
    if (status == "200") cat("OK!\n")
    content <- resp_parse[22]
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    writeLines(content, temp_file)
    source(temp_file)
}