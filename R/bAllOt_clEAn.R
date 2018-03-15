chEck_stAndArd_mAtrIx <-
function(x, len = ncol(x), numeric_na = NULL) {
    ROWHAVENA <- c()
    NOTNANUM <- c()
    ROWHAVEDUP <- c()
    for (i in 1:nrow(x)) {
        ii <- as.numeric(x[i, ])
        pos_not_in <- which(!ii %in% numeric_na)
        len_notna <- length(pos_not_in)
        if (len_notna < len) {
            ROWHAVENA <- append(ROWHAVENA, i)
            NOTNANUM <- append(NOTNANUM, len_notna)
        }
        ii <- ii[pos_not_in]
        length_ok <- length(ii)
        if (length_ok > 0) {
            if (anyDuplicated(ii) != 0) 
                ROWHAVEDUP <- append(ROWHAVEDUP, i)
        }
    }
    check_result <- list(ROWHAVENA, NOTNANUM, ROWHAVEDUP)
    check_result
}

fUll_dIgIt <-
function(n, p = NULL) {
    nchar_nc <- nchar(as.character(n))
    num <- as.character(1:n)
    nchar_num <- nchar(num)
    nchar_dif <- nchar_nc - nchar_num
    y <- c()
    for (i in 1:n) y[i] <- paste(rep(0, nchar_dif[i]), collapse = "")
    y <- paste(y, num, sep = "")
    if (!is.null(p)) 
        y <- paste(p, y, sep = "")
    y
}

rOw2cdc <- function(x) {
    len <- ncol(x)
    xcn <- colnames(x)
    CDC <- matrix(0, nrow = len, ncol = len)
    colnames(CDC) <- xcn
    rownames(CDC) <- xcn
    CDCNA <- CDC
    DIF <- CDC
    DIFNA <- CDC
    
    if (class(x)[1] != "data.table") 
        x <- data.table::data.table(x)
    R <- data.table::frankv(x, ties.method = "dense")
    RT <- table(R)
    tlen <- length(RT)
    ttn <- as.numeric(names(RT))
    RT <- as.numeric(RT)
    xx <- matrix(NA, nrow = tlen, ncol = ncol(x))
    for (i in 1:tlen) {
        instance <- match(ttn[i], R)
        xx[i, ] <- as.numeric(x[instance, ])  # must add as.numeric
    }
    rm(x)
    
    for (r in 1:nrow(xx)) {
        rr <- as.numeric(xx[r, ])
        r_uni <- RT[r]
        rr_which_na <- which(is.na(rr))
        num_na <- length(rr_which_na)
        if (num_na == 0) {
            # if there is no NA
            for (i in 1:len) {
                ii <- rr[i]
                for (j in 1:len) {
                  jj <- rr[j]
                  if (ii < jj) {
                    iimjj <- (jj - ii) * r_uni
                    CDC[i, j] <- CDC[i, j] + r_uni
                    CDCNA[i, j] <- CDCNA[i, j] + r_uni
                    DIF[i, j] <- DIF[i, j] + iimjj
                    DIFNA[i, j] <- DIFNA[i, j] + iimjj
                  }
                }
            }
        } else if (num_na > 0 & num_na < len) {
            rr[rr_which_na] <- len
            for (i in 1:len) {
                ii <- rr[i]
                for (j in 1:len) {
                  jj <- rr[j]
                  if (ii < jj) {
                    iimjj <- (jj - ii) * r_uni
                    CDCNA[i, j] <- CDCNA[i, j] + r_uni
                    DIFNA[i, j] <- DIFNA[i, j] + iimjj
                  }
                }
            }
        }
    }
    cdclist <- list(CDC, CDCNA, DIF, DIFNA)
    cdclist
}
