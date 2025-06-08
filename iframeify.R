## iframeify.R
## by Steve Myles / https://stevemyles.site/
## 10 February 2019
## updated 7 June 2025
## converts a markdown file into an iframe-able html file for embedding in
## another site such as tumblr

## load knitr and stringr packages
library(knitr)
library(stringr)
library(rmarkdown)

iframeify <- function(filename, stylesheet = "styles.css", styles = "styles.txt") {

    ## add markdown and html extensions to filename
    md_filename <- paste0(filename, ".md")
    html_filename <- paste0(filename, ".html")

    ## check whether stylesheet exists; if not, set to blank
    if (!file.exists(stylesheet))
      stylesheet = ""

    ## render
    ##knit2html(md_filename, html_filename, stylesheet = stylesheet)
    render(md_filename, output_format = html_document(css = stylesheet))
    
    ## read the html and styles files into memory; if styles does not exist,
    ## replace it with opening html <style> tag
    html <- readLines(html_filename)
    if (file.exists(styles)) {
      styles <- paste(readLines(styles), collapse = "\n")
    } else {
      styles = '<style type="text/css">'
    }

    ## replace the title, add the styles, and add 'target="_parent"' to html tags
    html <- html %>%
        str_c() %>%
        str_replace_all(c('<title>(.*?)</title>' = paste0('<title>', filename, '</title>'),
                  '<style type="text/css">' = styles,
                  'a href' = 'a target="_parent" href'))

    ## write results back to the html file
    writeLines(html, con = html_filename)

    ## remove interim .txt file
    file.remove(paste0(filename, ".txt"))
}
