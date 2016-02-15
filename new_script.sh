#! /bin/bash
# Script

function execute_branchAndBoundWithAnts {
  echo "sbt run branchAndBound ant /RMarti/RND/t1d35.$1"
  sbt "run branchAndBound ant /RMarti/RND/t1d35.$1"
}

function execute_branchAndBoundWithGreedy {
  echo "sbt run branchAndBound greedy /RMarti/RND/t1d35.$1"
  sbt "run branchAndBound greedy /RMarti/RND/t1d35.$1"
}

function execute_problems {

  for i in `seq 1 25`;
    do
      execute_branchAndBoundWithAnts $i
      execute_branchAndBoundWithGreedy $i
    done
}

execute_problems
