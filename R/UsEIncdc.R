bOttlEnEck <-
function(d) {
    nrc <- nrow(d)
    if (is.data.frame(d)) 
        d <- as.matrix(d)  # data.frame is dangerous
    p <- d
    p[p != 0] <- 0
    for (i in 1:nrc) {
        for (j in 1:nrc) {
            if (i != j) {
                if (d[i, j] > d[j, i]) {
                  p[i, j] <- d[i, j]
                } else {
                  p[i, j] <- 0
                }
            }
        }
    }
    for (i in 1:nrc) {
        for (j in 1:nrc) {
            if (i != j) {
                for (k in 1:nrc) {
                  if (k != i & k != j) {
                    p[j, k] <- max(p[j, k], min(p[j, i], p[i, k]))
                  }
                }
            }
        }
    }
    p
}

fInd_cdc_mAtrIx <-
function(x, dup_ok = TRUE, available = 1) {
    class1 <- class(x)[1]
    
    message("CREATING CDC MATRIX")
    if (class1 == "vote") {
        candidate <- x$candidate
        candidate_num <- x$candidate_num
        ballot_num <- x$ballot_num
        compute_cdc <- FLEXIBLE_rOw2cdc(x, ARG_dup_ok = dup_ok, ARG_available = available)
        cdc_matrix <- compute_cdc[[1]]
        dif_matrix <- compute_cdc[[2]]
        valid_ballot_num <- compute_cdc[[3]]
    } else if (class1 == "matrix") {
        message("------USE INPUT MATRIX")
        if (nrow(x) != ncol(x)) 
            stop("x must be a square matrix.")
        diag(x) <- 0  # should be 0
        cdc_matrix <- x
        dif_matrix <- NULL
        ballot_num <- NULL
        valid_ballot_num <- NULL
        ini_cn <- colnames(x)
        ini_rn <- rownames(x)
        if (is.null(ini_cn) & is.null(ini_rn)) {
            add_rcname <- fUll_dIgIt(nrow(x), "x")
            colnames(cdc_matrix) <- add_rcname
            rownames(cdc_matrix) <- add_rcname
        } else if (is.null(ini_cn) & !is.null(ini_rn)) {
            colnames(cdc_matrix) <- rownames(cdc_matrix)
        } else if (!is.null(ini_cn) & is.null(ini_rn)) {
            rownames(cdc_matrix) <- colnames(cdc_matrix)
        } else if (!is.null(ini_cn) & !is.null(ini_rn)) {
            if (!identical(ini_cn, ini_rn)) 
                stop(" Rownames and colnames of x must be the same.")
        }
        candidate <- rownames(cdc_matrix)
        candidate_num <- length(candidate)
    } else {
        cdc_matrix <- x$cdc  # for condorcet obj
        dif_matrix <- x$dif
        ballot_num <- x$ballot_num
        valid_ballot_num <- x$valid_ballot_num
        candidate <- rownames(cdc_matrix)
        candidate_num <- length(candidate)
    }  # search task finished
    
    binary_m <- if (class1 == "condorcet") 
        x$binary else cdc2bInAry(cdc_matrix)
    
    cdc_m_res <- list(input_object = class1, candidate = candidate, candidate_num = candidate_num, ballot_num = ballot_num, valid_ballot_num = valid_ballot_num, 
        cdc = cdc_matrix, dif = dif_matrix, binary = binary_m)
    return(cdc_m_res)
}

FLEXIBLE_rOw2cdc <- function(x, ARG_dup_ok, ARG_available) {
    candidate_num <- x$candidate_num
    candidate <- x$candidate
    CDC <- NULL
    
    # no need to calculate
    if (ARG_dup_ok == TRUE | length(x$row_with_dup) == 0) {
        if (ARG_available == candidate_num) {
            message("------USE CDC MATRIX IN x")
            CDC <- x$cdc
            DIF <- x$dif
            VALID_BALLOT_NUM <- nrow(x$ballot) - length(x$row_with_na)
        } else if (ARG_available == 1) {
            message("------USE CDC MATRIX WITH NA IN x")
            CDC <- x$cdc_with_na
            DIF <- x$dif_with_na
            VALID_BALLOT_NUM <- nrow(x$ballot) - length(which(x$num_non_na == 0))
        }
    }
    
    # need to recalculate
    if (is.null(CDC)) {
        message("------RECALCULATING CDC MATRIX")
        get_na_ok <- which(x$num_non_na < ARG_available)
        if (length(get_na_ok) > 0) 
            get_na_ok <- x$row_with_na[get_na_ok]
        get_dup <- if (ARG_dup_ok == TRUE) 
            integer(0) else x$row_with_dup
        should_del <- unique(c(get_dup, get_na_ok))
        length_should_del <- length(should_del)
        VALID_BALLOT_NUM <- nrow(x$ballot) - length_should_del
        if (VALID_BALLOT_NUM == 0) 
            stop("No ballot is OK.")  # maybe no usable ballot
        if (length_should_del > 0) {
            x <- x$ballot[-should_del, ]
        } else {
            x <- x$ballot
        }
        convert_v <- candidate_num  # equal to candidate number, the highest(worst) score
        
        # start to compute
        CDC <- matrix(0, nrow = candidate_num, ncol = candidate_num)
        colnames(CDC) <- candidate
        rownames(CDC) <- candidate
        DIF <- CDC
        
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
            rr[rr_which_na] <- convert_v
            for (i in 1:candidate_num) {
                ii <- rr[i]
                for (j in 1:candidate_num) {
                  jj <- rr[j]
                  if (ii < jj) {
                    iimjj <- (jj - ii) * r_uni
                    CDC[i, j] <- CDC[i, j] + r_uni
                    DIF[i, j] <- DIF[i, j] + iimjj
                  }
                }
            }
        }
    }
    
    y <- list(CDC, DIF, valid_ballot_num = VALID_BALLOT_NUM)
    return(y)
}

lock_winner <-
function(x, CAND) {
    colnames(x) <- NULL  # must do this
    nrx <- nrow(x)
    res <- list(x[1, ])
    for (i in 2:nrx) {
        ii <- x[i, ]
        iir <- rev(ii)
        have_anti <- 0
        for (j in 1:length(res)) {
            if (identical(iir, res[[j]])) 
                have_anti <- have_anti + 1
        }
        if (have_anti == 0) {
            ii1 <- ii[1]
            ii2 <- ii[2]
            bigger <- c()
            smaller <- c()
            for (k in 1:length(res)) {
                kk <- res[[k]]
                kk1 <- kk[1]
                kk2 <- kk[2]
                if (kk2 == ii1) 
                  bigger <- append(bigger, kk1)
                if (kk1 == ii2) 
                  smaller <- append(smaller, kk2)
            }
            res[[length(res) + 1]] <- ii
            if (length(bigger) > 0) {
                for (p in bigger) res[[length(res) + 1]] <- c(p, ii2)
            }
            if (length(smaller) > 0) {
                for (q in smaller) res[[length(res) + 1]] <- c(ii1, q)
            }
            res <- unique(res)
        }
    }
    res <- do.call(rbind, res)
    which_not <- as.character(res[, 2])
    # candidate who does not appear here is the winner
    which_not <- which(!CAND %in% which_not)
    which_not <- CAND[which_not]
    y <- list(res, which_not)
    y
}

RP_TIE_SOLVE <-
function(x, zeroone) {
    TIE_SOLVE <- TRUE
    DF <- data.frame("", "", 0, 0, stringsAsFactors = FALSE)
    only_num_df <- as.matrix(x[, c(3, 4)])
    only_num_df <- unique(only_num_df)
    for (i in 1:nrow(only_num_df)) {
        ii <- only_num_df[i, ]
        subi <- subset(x, x[, 3] == ii[1] & x[, 4] == ii[2])
        if (nrow(subi) == 1) {
            DF[nrow(DF) + 1, ] <- subi
        } else {
            unique1 <- uniqueN(subi[, 1])
            unique2 <- uniqueN(subi[, 2])
            if (!unique1 == 1 & !unique2 == 1) {
                for (j in 1:nrow(subi)) DF[nrow(DF) + 1, ] <- subi[j, ]
                TIE_SOLVE <- FALSE
            }
            if (unique1 == 1) {
                need_name <- subi[, 2]
                need_m <- zeroone[need_name, need_name]
                need_score <- rowSums(need_m)
				if (length(need_score) != length(unique(need_score))) TIE_SOLVE <- FALSE
                o_need <- order(need_score)
                subi <- subi[o_need, ]
                for (j in 1:nrow(subi)) DF[nrow(DF) + 1, ] <- subi[j, ]
            }
            if (unique2 == 1) {
                need_name <- subi[, 1]
                need_m <- zeroone[need_name, need_name]
                need_score <- rowSums(need_m)
				if (length(need_score) != length(unique(need_score))) TIE_SOLVE <- FALSE				
                o_need <- order(need_score, decreasing = TRUE)
                subi <- subi[o_need, ]
                for (j in 1:nrow(subi)) DF[nrow(DF) + 1, ] <- subi[j, ]
            }
        }
    }
    DF <- DF[-1, ]
    colnames(DF) <- colnames(x)
    rownames(DF) <- NULL
    return(list(TIE_SOLVE, DF))
}

sUmmAry_101 <-
function(x, rname) {
    y <- matrix(0, nrow = nrow(x), ncol = 3)
    colnames(y) <- c(-1, 0, 1)
    rownames(y) <- rname
    for (i in 1:nrow(x)) {
        ib <- append(x[i, ], c(-1, 1))
        y[i, ] <- as.numeric(table(ib)) - 1  # minus 1 from -1, 1, and the diag 0
    }
    y
}

cdc2bInAry <-
function(x) {
    nrc <- nrow(x)
    y <- matrix(0, nrow = nrc, ncol = nrc)
    colnames(y) <- colnames(x)
    rownames(y) <- rownames(x)
    for (i in 1:nrc) {
        for (j in 1:nrc) {
            if (i > j) {
                dif_ij <- x[i, j] - x[j, i]
                if (dif_ij > 0) {
                  y[i, j] <- 1
                  y[j, i] <- -1
                }
                if (dif_ij < 0) {
                  y[i, j] <- -1
                  y[j, i] <- 1
                }
            }
        }
    }
    y
}

#' @import gtools
AlllInkstrEngth=function(x, candname, keep=FALSE){
	nx=nrow(x)
	
	# expand.grid is too slow and memory-costing
    #	allcombi=expand.grid(rep(list(1: nx), nx))
    #	checkinvalid=apply(allcombi, 1, FUN=function(x) if (anyDuplicated(x) != 0) FALSE else TRUE)
    #	allcombi=allcombi[checkinvalid, ]
    #	rownames(allcombi)=NULL
    #	rm(checkinvalid)
	
	allcombi=gtools::permutations(nx, nx)
	
	dUAlsUm=function(i1, i2, datam) `[`(datam, i1, i2)
	SCORE <- rep(0, nrow(allcombi))
	for (i in 1: (nx-1)){
		for (j in 2: nx){
			if (i < j){
				SCORE=SCORE+mapply(FUN=dUAlsUm, allcombi[, i], allcombi[, j], MoreArgs=list(datam=x), SIMPLIFY=TRUE)
			}
		}
	}
	
	whichlink=which(SCORE == max(SCORE))
	linkv=SCORE[whichlink]
	WINLINK=list()
	for (r in whichlink){
		rr=as.numeric(allcombi[r, ])
		WINLINK[[length(WINLINK)+1]]=candname[rr]
	}
	truewinner=unique(unlist(lapply(WINLINK, `[`, 1)))
	WINLINK=do.call(rbind, WINLINK) # must after truewinner
	
	if (keep==FALSE){
		res=list(truewinner, WINLINK, linkv)
	} else {
		RANKLINK=cbind(allcombi, SCORE)[order(SCORE, decreasing=TRUE), ]
		colnames(RANKLINK)=c(1: (ncol(RANKLINK)-1), "score")
		rownames(RANKLINK)=NULL
		res=list(truewinner, WINLINK, linkv, RANKLINK)
	}
	res
}
