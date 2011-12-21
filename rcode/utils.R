
catln <- function(...) { cat(...,"\n")}
assert <- function(...) { stopifnot(...)}


saveWork <- function() {
	print(ls(all=TRUE,envir=.GlobalEnv))
	save( list=ls(all=TRUE,envir=.GlobalEnv), file="CAP_WORK.Rdata", compress=TRUE)	
}

loadWork <- function() {
	load("CAP_WORK.Rdata", .GlobalEnv)
}

accuracy <- function( clas, pred) {
	clas <- factor(clas, levels=c(-1,0,1))
	pred <- factor(pred, levels=c(-1,0,1))
	restable <- table( clas, pred)
	acc <- sum(diag(restable))/sum(restable)
	rm(clas,pred,restable)
	return(acc)
}
