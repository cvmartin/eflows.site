# ShinyApp() deployment: use rsconnect::deployApp()
# if using `renv` do NOT upload `.Rprofile`

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
eflows.site::run_app()

