source("R/concentration.R")
source("R/comparison.R")
near <- function(a,b) stopifnot(isTRUE(all.equal(a,b,tolerance=1e-12,check.attributes=FALSE)))
fails <- function(expr, pattern) {
  message <- tryCatch({force(expr); ""}, error=function(e) conditionMessage(e))
  stopifnot(nzchar(message), grepl(pattern,message,ignore.case=TRUE))
}

# Independent Wilcoxon distribution check for every attainable rank sum.
# pwilcox uses ascending ranks and subtracts m(m+1)/2.
for (k in 2:10) for (m in seq_len(k-1L)) {
  s <- setNames(seq_len(k),paste0("Y",seq_len(k)))
  out <- exact_concentration(s,names(s)[seq_len(m)])
  d <- out$distribution
  near(cumsum(d$probability), stats::pwilcox(d$rank_sum-m*(m+1)/2,m,k-m))
  near(sum(d$frequency), choose(k,m))
  near(out$minimum_reference,1/choose(k,m))
  near(out$p_ref,1)
  top <- exact_concentration(s,tail(names(s),m))
  near(top$p_ref,1/choose(k,m))
}

# Ties change the attainable floor, not just the displayed average rank.
s <- c(A=10,B=10,C=8)
a <- exact_concentration(s,"A")
near(a$ranks["A"],1.5);near(a$p_ref,2/3);near(a$minimum_reference,2/3)
near(exact_concentration(c(A=1,B=1,C=1),"A")$p_ref,1)
near(exact_concentration(s,names(s))$p_ref,1)
near(exact_concentration(setNames(1:19,letters[1:19]),"s")$attainable_rate,0)
near(exact_concentration(setNames(1:20,letters[1:20]),"t")$attainable_rate,.05)

# For tied statistics enumerate each labeled subset independently, then use
# the empirical distribution of its W to verify p and exact cutoff frequency.
for (s in list(c(A=5,B=5,C=3,D=2,E=2),c(A=0,B=0,C=0,D=0))) {
  for (m in seq_len(length(s))) {
    subsets <- combn(names(s),m,simplify=FALSE)
    sums <- vapply(subsets,function(a) sum(rank(-s,ties.method="average")[a]),numeric(1))
    p <- vapply(sums,function(w) mean(sums<=w),numeric(1))
    out <- exact_concentration(s,subsets[[1]])
    near(out$p_ref,p[1]);near(out$attainable_rate,mean(p<=.05))
    near(out$minimum_reference,min(p))
  }
}
fails(exact_concentration(c(A=1,B=NA),"A"),"finite")
fails(exact_concentration(c(A=1,A=2),"A"),"unique")
fails(exact_concentration(c(A=1,B=2),c("A","A")),"unique")
fails(exact_concentration(c(A=1,B=2),"C"),"present")
fails(exact_concentration(setNames(1:44,paste0("Y",1:44)),paste0("Y",1:22)),"limit")
fails(exact_concentration(c(A=1,B=2),"A",alpha=0),"alpha")

L <- data.frame(item=c("A","B","C"),t=c(10,10,8),status="ok")
stopifnot(rank_pattern(L,L$item,"A")$top_subset,
          !rank_pattern(L,L$item,"C")$top_subset,
          identical(rank_pattern(L,L$item,"C")$stronger_unreported,c("A","B")))
near(comparison_statistics(L,L$item,"negative_t"),-L$t)
fails(comparison_statistics(L,c("A","X")),"unique fitted")
L$t[3] <- NA
fails(comparison_statistics(L,L$item),"finite")
cat("PASS: 45 independent Wilcoxon checks, tied exact distributions, attainable rates, single/full reports, invalid inputs, cap and descriptive top-subset logic.\n")
