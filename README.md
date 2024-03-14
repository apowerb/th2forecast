# Configure repo to create R package
## Update package name and title
* Replace **RPACKAGENAME** with your target package name
* Replace  **R_PACKAGE_TITLE** and **R_PACKAGE_DESCRIPTION**

### [Description file](./DESCRIPTION)
### [workflow](./.github/workflow/test_r_package.yaml)
### [App config](./inst/golem-config.yml)

## CI/CD and Unit testing

* use the following function to include all packages depedncies **usethis::use_package("dplyr")**

Further Documentation can be found here : 
*  [CI CD github actions for R](https://r-pkgs.org/testing-basics.html)
*  Unit Testing using [**testthat**](https://testthat.r-lib.org/)

### Github Actions 

* Replace 

### Dockerfile
* Use the following command to create Dockerfile to execute Testing pipeline

`write(dockerfiler::dock_from_desc(FROM = "rocker/shiny-verse")$Dockerfile,"Dockerfile")` 

### Initialize Test framework
`usethis::use_test("my_function.R")` 


