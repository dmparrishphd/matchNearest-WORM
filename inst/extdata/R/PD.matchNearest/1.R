.matchNearest <- function ( x , table ) {
    D <- t ( table ) - x
    DD <- D * D
    Q <- vapply (
        USE.NAMES = FALSE ,
        X = seq_len ( ncol ( DD ) ) ,
        FUN.VALUE = sum ( DD [ , 1 ] ) ,
        FUN = function ( j ) sum ( DD [ , j ] ) )
    which.min ( Q ) }
