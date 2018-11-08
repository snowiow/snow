d=$(date +"%x %X")
b0=$(cat /sys/class/power_supply/BAT0/capacity)
b1=$(cat /sys/class/power_supply/BAT1/capacity)
mem=$(free -m |awk '/Mem:/ { printf "%.2f", $3/1024 }')
swap=$(free -m |awk '/Swap:/ { printf "%.2f", $3/1024 }')
proc=$(cat /proc/stat | awk '/cpu / { printf "%.2f", ($2+$4)*100/($2+$4+$5)}')

interface=$(iw dev | awk '/Interface/ {print $2}')
ssid=$(iw dev $interface link | awk 'NR==2{$1=""; print $0}' | xargs)
status_str="CPU $proc% | MEM ${mem}G | SWAP ${swap}G | BAT0 $b0% | "
if [[ ! -z $b1 ]]; then
    status_str+="BAT1 $b1% |"
fi

if [[ ! -z $interface ]]; then
    status_str+=" $ssid | "
else
    status_str+=" not connected | "
fi
status_str+=$d
echo " $status_str "
