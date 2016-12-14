##Self Assessment 2, Part 2
library('ggplot2')
##Hashmap Collision

##Counts the number of collisions
collision_detect = function(n_vec){
  collision_count = 0
  n_vec = sort(n_vec)
  for (i in seq_len(length(n_vec)-1)){
    if (n_vec[i]==n_vec[i+1])
    collision_count = collision_count+1
  }
  return(collision_count)
}

##end 13:07

##return 13:21
##Detect amount of unused hashes in n_vec, where range is the range of hashes
unused_detect = function(range,n_vec){
  unused_counter = 0
  for (i in range){
    if (!(i %in% n_vec)){
      unused_counter = unused_counter+1
    }
  }
  return(unused_counter)
}


## Iterate n_repeat times
collisions = 0
unused = 0
collision_boolean = 0

n_repeat = 10000
for (i in 1:n_repeat){
  s = sample(1:10,10,replace=TRUE)
  co = collision_detect(s)
  un = unused_detect(1:10,s)
  bo = co>0
  collisions = collisions+co
  unused = unused+un
  collision_boolean = collision_boolean+bo
}

print (
  paste(
    "The probability of hash collisions is",collision_boolean/n_repeat,". ",
    "The expected number of hash collisions is",collisions/n_repeat,". ",
    "The expected number of unused hashes is", unused/n_repeat, ".",
    "As estimated over",n_repeat,"runs",
    sep=" "
  )
)

##Rolling the dice
roll_instance = function(){
  roll_history = c()
  while (unused_detect(1:6,roll_history)>0){
    new = sample(1:6,1)
    roll_history = c(roll_history,new)
  }
  return(length(roll_history))
}

n_trials = 10000
run_sum = 0
for (i in 1:n_trials){
  run_sum = run_sum + roll_instance()
}
print(
  paste(
    "It takes",run_sum/n_trials, "rolls on average to roll 1:6, as estimated by",
    n_trials,"trials."
  )
)

##Bobo the ameoba
##Typo: should read "large number of ameobas" instead of "large of ameobas"
bobo_cap = 500
n_generation = 30
num_lineages = 100

next_gen = function(n){
  sum = 0
  m = mean(0:3)
  if (n >= bobo_cap | n==0){
    return (n*m)
  }
  for (i in seq_len(n)){
    sum = sum+sample(0:3,1)
  }
  return (sum)
}

simulate_amoeba = matrix(-1,ncol=num_lineages,nrow=n_generation)
simulate_amoeba[1,]=1
for (i in 2:n_generation){
  simulate_amoeba[i,]=sapply(simulate_amoeba[i-1,],next_gen)
}

for (i in 1:n_generation){
  simulate_amoeba[i,]= sapply(simulate_amoeba[i,],function(w){
    if(w>0){return (1)}else{return(0)}
  })
}

extinction_prob = 1-(rowSums(simulate_amoeba)/num_lineages)

extinction_evo = as.data.frame(cbind(1:n_generation,extinction_prob))
names(extinction_evo) = c("generation","extinct_prob")
ggplot(data=extinction_evo)+geom_point(aes(x=generation,y=extinct_prob))
extinction_prob[30]
##chance of dying out is about 0.41, determined at 14:03
##graph functional at 14:08