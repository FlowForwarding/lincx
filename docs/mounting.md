## Running lincx with external logs

### Install prerequisites

* diod - I/O forwarding server for 9P
* munge - authentication service to create and validate credentials

Ubuntu/Debian:

    sudo apt-get install diod munge

Arch Linux:

* [diod-git](http://aur.archlinux.org/packages/diod-git)
* [munge](https://aur.archlinux.org/packages/munge)

#### Run prerequisites 

    sudo munged
    sudo diod -E

### Configure LING

Get secret keys:

    sudo ./scripts/mungeling

The utility will print something like this:

    -secret dacfa423b8ab3ec21e89009a9d3854ac86b1b3fd 6400e7b41e31c0998eeef9ed356e9fdba13a9c21
Update REMOTE_MOUNTS option in LINGConfig.mk:

    REMOTE_MOUNTS := -secret xxxxxxxx yyyyyyyy
    REMOTE_MOUNTS += -9p 10.10.10.10 /mnt /log

- Change the '-secret' values to the actual output of  ./scripts/mungeling
- Change IP 10.10.10.10 to the real IP of your host with diod server
- Change /mnt to any other real directory on your host if you want your logs to be written there

Thats it. When you will run lincx with:

    sudo make boot
it logs will be written to specified directory.

### See also

[Erlang on Xen documentation](http://build.erlangonxen.org/mounting)
