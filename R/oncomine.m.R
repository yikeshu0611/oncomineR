oncomine.m <- function(mhtml){
    file.names <- do::Replace0(mhtml,'\\..*')
    x=lapply(mhtml, function(i) oncomine.i(i))
    for (i in seq(length(x))){
        if (file.names[i] %in% colnames(x[[i]])) next(i)
        colnames(x[[i]])[colnames(x[[i]]) == 'Legend Value'] = file.names[i]
        if (i == 1){
            hotel = x[[i]]
        }else{
            names.left = names(x[[i]])[! names(x[[i]]) %in% names(hotel)]
            if (length(names.left)>0) hotel = merge(hotel,x[[i]][,c(names.left,"Sample Name")],
                                                    by="Sample Name")
        }

    }
    hotel
}
