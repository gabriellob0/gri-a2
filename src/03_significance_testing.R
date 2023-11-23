gpval.lq<-function(R,tpshr.vec,ntshr.vec,ref.lq,ethp.size,p=0.05) {
  thresh.shr.vec<-ref.lq*tpshr.vec #s_gj
  ct.thresh.vec<-qbinom(p,ethp.size,thresh.shr.vec,lower.tail=FALSE)
  draws<-rmultinom(R,ethp.size,ntshr.vec) #J rows (tracts) X R columns (replications)  
  draws[draws<ct.thresh.vec]<-NA #this uses "recycling"; ct.thresh.vec is recycled into a long vector repeating sequence R times
  #Remove rows from matrix that are all NAs
  allna.rows<-apply(draws,1,function(x) sum(is.na(x))==length(x))
  thresh.shr.vec<-thresh.shr.vec[allna.rows==FALSE]
  draws<-draws[allna.rows==FALSE,]
  pvals<-apply(draws,2,function(x,yvec,N) pbinom(x,N,yvec,lower.tail=FALSE),N=ethp.size,yvec=thresh.shr.vec)
  pvals[is.na(pvals)]<-p #set NAs to p (usually 0.05)
  minp.vec<-apply(pvals,2,function(x) min(x,na.rm=TRUE))
  glbl.mnpval<-sort(minp.vec)[floor(p*R)]
}

test <- oas_with_lqs |>
  mutate(total_ethpop = sum(count), .by = ethnicity) |>
  mutate(prob = threshold * share_oa)

nat_shares <- test |>
  filter(ethnicity == "WBRI.01", loc_quo < threshold) |>
  arrange(share_ethoa) |>
  pull(share_ethoa)

test2 <- test |>
  mutate(
    qbn = qbinom(0.05, total_ethpop, prob, lower.tail = FALSE)
  ) |>
  filter(ethnicity == "CHNE.01")

#each row is a tract, ordered by the LQ of natives, only included bellow the 99th
draws <- rmultinom(n = 10, size = filter(test2, ethnicity == "CHNE.01")$total_ethpop[1], prob = nat_shares)
draws[draws < filter(test2, ethnicity == "WBRI.01")$qbn] <- NA
draws <- draws |>
  as_tibble() |>
  filter(!if_all(everything(), is.na))
         