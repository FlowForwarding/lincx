# Introduction
This document covers the steps required to install Ubuntu 14.04 with 
Xen hypervisor so that one can install and run LINCX

# Prerequisites
>1. Assumptions: PXE/DHCP/DNS services are available on the network
>2. Download Ubuntu Netboot [image] (http://cdimage.ubuntu.com/netboot/14.04/).
This will download the packages over the Internet.  When installing, only 
select (1) base system and (2) OpenSSH Server packages.  Do not install 
Virtual host or other libraries here. No X installation is required.
>3. Assumption: Local user created has a username "localadmin"

# Install Steps

## Step 1
TFTP PXE server ```pxelinux.cfg``` directory has the following lines in the default file.
Note: I have the downloaded Ubuntu netboot image in ```images/ubuntu/ubuntu14_netboot```
directory.  ```"images"``` directory is peer to ```"pxelinux.cfg"``` directory.

```bash
UI menu.c32
MENU TITLE Boot menu for My Install Network
NOHALT 1
PROMPT 0
TIMEOUT 80
ONTIMEOUT Ubuntu Netboot Server 14.04

LABEL Ubuntu Netboot Server 14.04
MENU LABEL Ubuntu Netboot Server 14.04 x86_64
KERNEL images/ubuntu/ubuntu14_netboot/ubuntu-installer/amd64/linux
APPEND root=/dev/ram0 ramdisk_size=360000 boot=casper initrd=images/ubuntu/ubuntu14_netboot/ubuntu-installer/amd64/initrd.gz +++
IPAPPEND 1
```

## Step 2
After installation of Ubuntu, reboot the system and login as ```"localadmin"```
```bash
# sudo su
# apt-get install xen-hypervisor

To boot Xen, change settings in /etc/grub.d
# mv 10_linux 20_linux
# mv 20_linux_xen 10_linux_xen
# grep -i xen /boot/grub/grub.cfg
# sudo sed -i 's/GRUB_DEFAULT=.*\+/GRUB_DEFAULT="Xen 4.4-amd64"/' /etc/default/grub
# sudo update-grub

Setting Toolstack
# sed -i 's/TOOLSTACK=.*\+/TOOLSTACK="xl"/' /etc/default/xen
This should put “TOOLSTACK=xl” in /etc/default/xen file; If not do it manually

# vi /etc/xen/xend-config.sxp
Comment out the line: (vif-script vif-bridge)

To disable Firewall permanently,
#  ufw disable

# reboot
```

## Step 3
Setup system network interfaces
```bash
$ vi /etc/network/interfaces
```
Sample ```/etc/network/interfaces``` file
```bash
# The loopback network interface
auto lo
iface lo inet loopback

# The primary network interface
auto p2p1
iface p2p1 inet manual

auto p2p2
iface p2p2 inet static

auto p2p3
iface p2p3 inet static


auto xenbr0
iface xenbr0 inet static
 address 10.48.11.100
 netmask 255.255.255.0
 gateway 10.48.11.1
 dns-nameservers 10.48.2.5
 bridge_ports p2p1
 bridge_stp off
 bridge_fd 0

auto br1
iface br1 inet manual
  bridge_ports p2p2
  bridge_stp off
  bridge_fd 0

auto br2
iface br2 inet manual
  bridge_ports p2p3
  bridge_stp off
  bridge_fd 0
```
Restart networking services

```bash
# service networking restart
```
## Step 4
Checking if everything is installed correctly
```bash
Check if we have Xen environment
# xl list
(should respond with line Domain-0 ...)

Check if networking is correctly setup
# brctl show
```
## Step 5
Now you are ready to install LINCX.  Please follow [instructions] (https://github.com/FlowForwarding/lincx/blob/master/README.md)
