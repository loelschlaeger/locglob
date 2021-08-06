### transformation matrix to compute differences wrt alternative i
### J: number of alternatives
### i: index i of choosen alternative

delta = function (J,i) {
  diag = diag(J-1)
  mone = rep(-1,J-1)
  if(i==1){
    return(cbind(mone,diag,deparse.level=0))
  } else if(i==J){
    return(cbind(diag,mone,deparse.level=0))
  } else{
    return(cbind(diag[,1:(i-1)],mone,diag[,i:(J-1)],deparse.level=0))
  }
}