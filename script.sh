#! /bin/bash
# Script

mkdir solution

function execute_branchAndBound {
  echo "sbt run branchAndBound /InstanciasBT/bt$1.txt"
  sbt "run branchAndBound /InstanciasBT/bt$1.txt" #> solution/mrho$1/branchAndBound.txt
}

function execute_ants {
  #for i in `seq 1 10`;
   # do
      echo "$i sbt run ant /InstanciasBT/bt$1.txt"
      sbt "run ant /InstanciasBT/bt$1.txt" #> solution/mrho$1/ant$i.txt
    #done
}

function execute_genetic {
  #for i in `seq 1 10`;
   # do
      echo "sbt run genetic /InstanciasBT/bt$1.txt"
      sbt "run genetic /InstanciasBT/bt$1.txt" # > solution/mrho$1/genetic.txt
    #done
}

function execute_greedy {
  for i in `seq 1 10`;
    do
      echo "$i sbt run greedy /InstanciasBT/bt$1.txt"
      sbt "run greedy /InstanciasBT/bt$1.txt" > solution/mrho$1/greedy$i.txt
    done
}

function execute_problems {

  for i in `seq 3 12`;
    do
      # mkdir solution/mrho$i
      execute_branchAndBound $i
      # execute_greedy $i
      # execute_ants $i
      # execute_genetic $i
    done
}

execute_problems
