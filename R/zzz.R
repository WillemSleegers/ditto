.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)

  packageStartupMessage(
    "Welcome to ditto ", version, "!\n",
    "ditto provides string and text similarity metrics, including a unified\n",
    "compare_strings() interface for edit-distance, n-gram, and embedding-based\n",
    "scores.\n\n",
    "To get started, see the available vignettes:\n",
    "  vignette(\"ditto\")                  # Comparing strings with ditto\n",
    "  vignette(\"bertscore-validation\")   # Validating bertscore() vs. the reference\n",
    "  browseVignettes(\"ditto\")          # List all vignettes"
  )
}
