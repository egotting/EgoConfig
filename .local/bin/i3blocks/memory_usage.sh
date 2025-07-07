#!/bin/bash

# Extrai memória total e disponível em KB
mem_total_kb=$(awk '/MemTotal/ {print $2}' /proc/meminfo)
mem_avail_kb=$(awk '/MemAvailable/ {print $2}' /proc/meminfo)

# Calcula e exibe a porcentagem de RAM livre
mem_free_percent=$(awk -v avail=$mem_avail_kb -v total=$mem_total_kb 'BEGIN {printf "%.1f", (avail / total) * 100}')

echo " [ memory: $mem_free_percent% ] "

