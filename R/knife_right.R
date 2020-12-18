knife_right <- function(x,n){
    knife_i <- function(x,n){
        x=rev(strsplit(as.character(x),'')[[1]])
        x=rev(x[-c(1:n)])
        paste0(x,collapse = '')
    }
    x1=sapply(x, function(i) knife_i(i,n))
    names(x1)=names(x)
    x1
}
