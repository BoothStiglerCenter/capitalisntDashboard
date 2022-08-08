library(rsconnect)

error_on_missing_name <- function(name) {
    var <- Sys.getenv(name, unset = NA)
    if (is.na(var)) {
        stop(paste0("CANNOT FIND", name, "!", sep = " "), call. = FALSE)
    }
    gsub("\"", "", var)

    ### Authentication section:
    setAccountInfo(
        name = error_on_missing_name("SHINY_ACC_NAME"),
        token = error_on_missing_name("TOKEN"),
        secret = error_on_missing_name("SECERT")
    )

    ### Deploy Application to Shiny
    deployApp(
        appFiles = c("ui.R", "global.R", "server.R"),
        appName = error_on_missing_name("MASTERNAME"),
        appTitle = "Capitalisn't Dashboard"
    )
}