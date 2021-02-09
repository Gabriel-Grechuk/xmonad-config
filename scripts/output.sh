#!/usr/bin/env bash

# note: 
# * Debian/ubuntu based use /sbin/iw instead of /iw

setVolume() {
    local icon="$preIcon$postIcon"
    local value=$(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
    segmentVolume="$icon $labelColor Vol $valueColor$value"
}

setMemory() {
    local icon="$preIcon$postIcon"
    local mem_total=$(free | awk 'FNR == 2 {print $2}')
    local mem_used=$(free | awk 'FNR == 2 {print $3}')
    local value=$[$mem_used * 100 / $mem_total]
    segmentMemory="$icon $labelColor Mem $valueColor$value"
}

setDisk() {
    local icon="$preIcon$postIcon"
    local value=$(df /home -h | awk  'FNR == 2 {print $5}' | sed s/%//)
    segmentDisk="$icon $labelColor Disk $valueColor$value"
}

setCPU() {
    local icon="$preIcon$postIcon"
    
    helperCPU
    local value=$cpu_util
    
    segmentCPU="$icon $labelColor CPU $valueColor$value"
}

setSSID() {  
    local wifi=$(iw dev | grep Interface | awk '{print $2}')
    
    local value=""
    if [ \"$wifi\" ]; then 
        value=$(iw dev $wifi link | grep SSID: | awk '{print $2 $3}')
    fi

   local icon="$preIcon$postIcon"
   segmentSSID="$icon $valueColor$value"
}  

setNet() {
    helperNet
    local iconUp="$preIcon$postIcon"
    local iconDown="$preIcon$postIcon"
    segmentNet="$labelColor Net $iconUp $valueColor$TX_text $iconDown $valueColor$RX_text"
}

setUptime() {
    #  Uptime
    local icon="$preIcon$postIcon"
    local value=$(uptime -p)
    segmentUptime="$icon $labelColor Uptime $valueColor$value"
}

setHost() {
    #  Machine
    #  Home
    local icon="$preIcon$postIcon"
    
    # I prefer $(uname -r)
    local value=$(uname -n)
    
    segmentHost="$icon $labelColor Host $valueColor$value"
}

setUpdates() {
    local icon="$preIcon$postIcon"
    local value=$(pacman -Qu | wc -l)
    segmentUpdates="$icon $labelColor Updates $valueColor$value"
}

setDate() {
    local iconDate="$preIcon$postIcon"
    local iconTime="$preIcon$postIcon"
    local date=$(date +'%a %b %d')
    local time=$(date +'%H:%M:%S')
    
    case $theme in
      'bright-arrow' | 'bright-deco') 
         segmentDate=" $iconTime ^fg($colGrey200)$time, "
         segmentDate+="$iconDate ^fg($colGrey500)$date-^fg($colGrey200)$day"    
      ;;
      'dark-arrow' | 'dark-deco')
         segmentDate=" $iconTime ^fg($colGrey200)$time, "
         segmentDate+="$iconDate ^fg($colGrey500)$date-^fg($colGrey200)$day" 
      ;;
      *) # 'dark-colorful' | 'bright-colorful'
         segmentDate=" $iconTime $valueColor$time, "
         segmentDate+="$iconDate $valueColor$date"    
      ;;
    esac    
}

setMPD() {
    case $theme in
      'bright-arrow' | 'bright-deco') 
         local format="^fg(#$colGrey900)[%artist% ^fg()- ]^fg(#$colGrey300)[%title%]"
      ;;
      'dark-arrow' | 'dark-deco')
         local format="^fg(#$colGrey100)[%artist% ^fg()- ]^fg(#$colYellow500)[%title%]"
      ;;
      'bright-colorful')
         local format="^fg(#$colBlue500)[%artist% ^fg()- ]^fg(#$colPink700)[%title%]"
      ;;
      *) # 'dark-colorful'
         local format="^fg(#$colBlue300)[%artist% ^fg()- ]^fg(#$colYellow500)[%title%]"
      ;;
    esac

    local iconPlay="$preIcon$postIcon"
    local iconPause="$preIcon$postIcon"
    segmentMPD="$iconPlay "$(mpc current -f "$format")
}


helperNet() {
  local interface=$(iw dev | grep Interface | awk '{print $2}')

  if [ "$interface" ]; then 

    # Read first datapoint
    read TX_prev < /sys/class/net/$interface/statistics/tx_bytes
    read RX_prev < /sys/class/net/$interface/statistics/rx_bytes

    sleep 1

    # Read second datapoint

    read TX_curr < /sys/class/net/$interface/statistics/tx_bytes
    read RX_curr < /sys/class/net/$interface/statistics/rx_bytes

    # compute 
    local TX_diff=$((TX_curr-TX_prev))
    local RX_diff=$((RX_curr-RX_prev))

    # printout var
    TX_text=$(echo "scale=1; $TX_diff/1024" | bc | awk '{printf "%.1f", $0}')
    RX_text=$(echo "scale=1; $RX_diff/1024" | bc | awk '{printf "%.1f", $0}')
  fi; 
}

helperCPU() {
  # Read /proc/stat file (for first datapoint)
  read cpu user nice system idle iowait irq softirq steal guest< /proc/stat

  # compute active and total utilizations
  local cpu_active_prev=$((user+system+nice+softirq+steal))
  local cpu_total_prev=$((user+system+nice+softirq+steal+idle+iowait))

  # echo 'cpu_active_prev = '.cpu_active_prev

  sleep 1

  # Read /proc/stat file (for second datapoint)
  read cpu user nice system idle iowait irq softirq steal guest< /proc/stat

  # compute active and total utilizations
  local cpu_active_cur=$((user+system+nice+softirq+steal))
  local cpu_total_cur=$((user+system+nice+softirq+steal+idle+iowait))

  # compute CPU utilization (%)
  cpu_util=$((100*( cpu_active_cur-cpu_active_prev ) / (cpu_total_cur-cpu_total_prev) ))
}


