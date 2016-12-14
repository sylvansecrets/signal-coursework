library(tm)


file1 = "C:/Users/User/Documents/GitHub/Signal-Data-Science/spelling_corpus.txt"
spelling_text = readChar(file1, file.info(file1)$size)
s = unlist(strsplit(spelling_text, "(\\s|\\n|\\r|\\_|\\')"))
s = sapply(s, tolower)
spec_replace = function(w){
  return( gsub("(,|\\.|\\d|\\&|\\;)", " ", w))
}
s = sapply(s, spec_replace)
# Choose the most likely to be true between distance 1 and distance 2

edits = function(w){
  func_li = c("deletes", "transposes", "replaces","inserts")
  word_set = vector("list",4)
  for (i in 1:4)  {word_set[[i]]=do.call(func_li[i], list(w))}
  return (unique(unlist(word_set)))
}

splits = function(w){
  n = nchar(w)
  s_li = vector(mode="list", n)
  for (i in 1:n+1){
    s_li[[i]]=c(substr(w,1,i-1),substr(w,i,n))
  }
  return (s_li)
}

word_iter = function(w, func){
  sp = splits(w)
  n=length(sp)
  w_vec = vector(mode="list", length(sp))
  for (i in 1:n){
    a = sp[[i]][1]
    b = sp[[i]][2]
    w_vec[[i]] = func(a,b)
  }
  return(unlist(w_vec))
}

deletes = function(a,b){
  paste(a,substr(b,2,nchar(b)), sep="")
}

transposes = function(a,b){
  paste(a,substr(b,2,2),substr(b,1,1),substr(b,3,nchar(b)), sep="")
}

replaces = function(a,b){
  replace_vec = letters
  for (i in 1:26){
    replace_vec[i]=paste(a,letters[i], substr(b,2,nchar(b)), sep="")
  }
  return(replace_vec)
}

inserts = function(a,b){
  insert_vec = letters
  for (i in 1:26){
    insert_vec[i]=paste(a,letters[i], substr(b,1,nchar(b)), sep="")
  }
  return(insert_vec)
}