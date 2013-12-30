#!/bin/bash

vif1=vif171.0
vif2=vif169.0

if [ $# -lt 1 ]; then
	echo "Usage: $0 inet|perf|taps|ling"
	exit
fi

case "$1" in
inet)
	echo 1 > /proc/sys/net/ipv4/ip_forward

	iptables --flush
	iptables -t nat -A POSTROUTING -o br-linc0 -j MASQUERADE
	iptables -A FORWARD -i br-linc0 -o br-linc1 -m state --state RELATED,ESTABLISHED -j ACCEPT
	iptables -A FORWARD -i br-linc1 -o br-linc0 -j ACCEPT
	iptables -A FORWARD -i br-linc0 -o br-linc2 -m state --state RELATED,ESTABLISHED -j ACCEPT
	iptables -A FORWARD -i br-linc2 -o br-linc0 -j ACCEPT

	brctl delif br-linc1 $vif1
	brctl addif br-linc1 $vif1
	brctl delif br-linc2 $vif2
	brctl addif br-linc2 $vif2
	;;

perf)
	echo 0 > /proc/sys/net/ipv4/ip_forward

	iptables --flush
	;;

taps)
	brctl addif br-linc1 tap-linc-port1
	brctl addif br-linc2 tap-linc-port2
	ifconfig tap-linc-port1 up
	ifconfig tap-linc-port2 up
	;;

ling)
	domid=`xl domid lincx`

	lingvif1=vif${domid}.1
	lingvif2=vif${domid}.2

	brctl delif xenbr0 $lingvif1
	brctl delif xenbr0 $lingvif2
	brctl addif br-linc1 $lingvif1
	brctl addif br-linc2 $lingvif2
	;;

*)
	echo "Bad option: $1"
	;;
esac

#EOF
