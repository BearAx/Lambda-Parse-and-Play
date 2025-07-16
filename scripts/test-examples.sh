#!/bin/bash
set -euo pipefail

echo "Running integration tests for examples/"

# Проверяем каждый файл примера
for file in examples/*.lam; do
  echo -n "Testing $file... "
  base_file=$(basename "$file")
  
  # Запускаем и проверяем код возврата
  if ! output=$(stack exec lambda-exe -- "$file" 2>&1); then
    echo "FAILED"
    echo "Error output:"
    echo "$output"
    exit 1
  fi
  
  echo "PASSED"
done

echo "All examples passed!"
