
get_inst_file <- function(local_path, package = pkgload::pkg_name()){
  path <- system.file(local_path, package=package)
  
  if (nchar(path) == 0){
    rlang::abort(
      class = "error_inst_file_not_found",
      message = sprintf(
        "File `%s` not found in global path `%s`",
        local_path,
        system.file(package=package)
      )
    )
  }
  path
}
