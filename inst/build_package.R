#--------------------------------------
# Re-build the R-Package after changes
#--------------------------------------
library(devtools)

# =====================================
# Rebuild Package
rm(list = ls())
devtools::load_all()
devtools::document()
devtools::test()  # Run tests
devtools::check(vignettes = FALSE) # Operating system test
# devtools::build_manual() # Time intensive


# =====================================
# Remove and re install package locally
# Ctrl + Shift + F10 to restart RStudio
remove.packages("MANI.CPBT")
install.packages(getwd(), repos = NULL, type = "source")
