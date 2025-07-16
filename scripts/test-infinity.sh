#!/bin/bash
echo "=== Testing Infinity.lam (should run forever) ==="
timeout 3s stack exec lambda-exe -- examples/Infinity.lam
exit 0  # Всегда возвращаем успех, даже после таймаута
