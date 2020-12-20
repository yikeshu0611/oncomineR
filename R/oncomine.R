#' Process oncomine data to dataframe
#'
#' @param mhtml one or more data files of MHTML plugin
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom magrittr %>%
#' @return dataframe
#' @export
#'
oncomine <- function(mhtml){
    if (length(mhtml) == 1){
        oncomine.i(mhtml)
    }else{
        oncomine.m(mhtml)
    }
}
