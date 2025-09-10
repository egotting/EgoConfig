#!/bin/bash

exec_always for id in $(xinput list | grep "pointer" | cut -d '=' -f 2 | cut -f 1); do xinput --set-prop $id 'libinput Accel Profile Enabled' 0, 1; done
exec_always for id in $(xinput list | grep "pointer" | cut -d '=' -f 2 | cut -f 1); do xinput --set-prop $id "libinput Accel Speed" 0.2; done
exec_always xset r rate 200 50
