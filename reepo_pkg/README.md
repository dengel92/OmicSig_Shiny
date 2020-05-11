### files
* R: functions
* man: manual (documentation of functions)
* NAMESPACE: don't know what it does yet
* DESCRIPTION: description of this package
* README.md: manually created

### notes
Put functions into R folder with documentation (it's okay to have multiple functions in one `.R` file. As long as their documentation are properly formated, each function will have its own `.Rd` file in man folder).  
Run `devtools::document()`, and `.Rd` description will be automatically generated. Do not manually edit `.Rd` files.  
Now you can use your functions and use `?testFun()` to see function documentation. Or open a `.Rd` file and click "preview" button at the top.
