#!/bin/bash

# lê a linha cpu do /proc/stat duas vezes com intervalo para calcular uso
read cpu user nice system idle iowait irq softirq steal guest < /proc/stat
sleep 0.5
read cpu2 user2 nice2 system2 idle2 iowait2 irq2 softirq2 steal2 guest2 < /proc/stat

prev_idle=$((idle + iowait))
idle2=$((idle2 + iowait2))

prev_non_idle=$((user + nice + system + irq + softirq + steal))
non_idle2=$((user2 + nice2 + system2 + irq2 + softirq2 + steal2))

prev_total=$((prev_idle + prev_non_idle))
total2=$((idle2 + non_idle2))

total_diff=$((total2 - prev_total))
idle_diff=$((idle2 - prev_idle))

cpu_usage=$(( (1000 * (total_diff - idle_diff) / total_diff + 5) / 10 ))

echo "[ cpu: $cpu_usage% ]"

