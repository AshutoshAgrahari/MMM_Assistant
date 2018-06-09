# source: https://github.com/ficonsulting/RInno
# If you don't have development tools, install them
# install.packages("devtools"); 
# require(devtools)
# 
# # Use install_github to get RInno
# devtools::install_github("ficonsulting/RInno",  build_vignettes = TRUE)

# Require Package
require(RInno)

# Build an installer
create_app(app_name = "ME_Win")
compile_iss()
