.onAttach <- function(libname, pkgname) {
  msg <- paste0("WARNING:\n",
    "This package is under development and is for internal testing only.\n",
    "Please do not further distribute.\n",
    "It will be released on GitHub when it is ready for public testing.\n",
    "It is highly appreciated if you can report any bugs you found, or\n",
    "any suggestions you have.\n",
    "Please type '?semfindr` for further information."
    )
  packageStartupMessage(msg)
}
