matchNearest <- function ( x , table ) {
    stopifnot (
        isNumericMatrix ( x ) ,
        isNumericMatrix ( table ) ,
        ncol ( x ) == ncol ( table ) )
    vapply (
        FUN.VALUE = 1L ,
        USE.NAMES = FALSE ,
        X = seq_len ( nrow ( x ) ) ,
        function ( i ) .matchNearest ( x [ i , ] , table ) ) }
