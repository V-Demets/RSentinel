.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(i18n$t("Welcome to shinycnes. To use the package from a GUI, launch"),
    " > cnes_gui()",
    "Documentation: https://pobsteta.github.io/cnes\n",
    i18n$t("IMPORTANT: shinycnes depends on some external tools;"),
    i18n$t("before using it, it is strongly recommended to run function"), " > check_cnes_deps()",
    i18n$t("to check them and install the missing ones."),
    sep = "\n"
  ))
}
