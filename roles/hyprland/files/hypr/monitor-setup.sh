#!/bin/bash
# Dynamically assign workspaces based on connected monitors

sleep 1 # wait for monitors to be fully initialized

monitors=$(hyprctl monitors -j)

has_monitor() {
    echo "$monitors" | grep -q "\"name\": \"$1\""
}

apply_workspaces() {
    local cmds=""
    for pair in "$@"; do
        cmds+="keyword workspace ${pair};"
    done
    hyprctl --batch "$cmds"
}

if has_monitor "HDMI-A-1" && has_monitor "DP-1"; then
    # Home: eDP-1 + HDMI-A-1 + DP-2
    apply_workspaces \
        "1,monitor:eDP-1" \
        "2,monitor:HDMI-A-1" \
        "3,monitor:HDMI-A-1" \
        "4,monitor:HDMI-A-1" \
        "5,monitor:HDMI-A-1" \
        "6,monitor:HDMI-A-1" \
        "7,monitor:HDMI-A-1" \
        "8,monitor:HDMI-A-1" \
        "9,monitor:DP-1" \
        "10,monitor:DP-1"
elif has_monitor "DP-2"; then
    # Work: eDP-1 + DP-1
    apply_workspaces \
        "1,monitor:eDP-1" \
        "2,monitor:DP-2" \
        "3,monitor:DP-2" \
        "4,monitor:DP-2" \
        "5,monitor:DP-2" \
        "6,monitor:DP-2" \
        "7,monitor:DP-2" \
        "8,monitor:DP-2" \
        "9,monitor:DP-2" \
        "10,monitor:DP-2"
fi
