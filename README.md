# Jaguar Land Rover HVAC demo app running on Exosense Device Stack.

# Building the Exosense JLR demo.

Follow the instructions under:

    `https://github.com/Feuerlabs/exosense_specs/blob/master/doc/exosense_demo_tutorial.pdf`

Replace meta-exodemo with meta-jlrdemo (the Yocto build layerfor
this demo).

Since the demo is installed on Tizen, we will not create a complete
image, but rather a set of RPMs that can be installed on the standard
demo.  

After the build directory has been generated, as outlined in the
meta-exosense instructions, edit the following entries in
conf/local.conf

    MACHINE ?= "qemux86"
    PACKAGE_CLASSES ?= "package_rpm"
    EXTRA_IMAGE_FEATURES = "debug-tweaks exosense"
    IMAGE_INSTALL_append = " erlang-jlrdemo"

In the build directory, do:

    bitbake erlang-jlrdemo
	

The rpms will be deposited in 

    build/tmp/deploy/rpm/i586

# Installing the Exosense JLR demo RPMs on the target system

All rpms to be copied over from the directory above to the target system are listed in

    https://github.com/Feuerlabs/jlrdemo/blob/master/tizen_rpm_list.txt

Once copied, install them all using a single `rpm -i` command


# Setting up automatic launch during boot

There is a start script for the Exosense JLR demo installed on the target under:

    /usr/lib/erlang/jlrdemo-???/priv/jlrdemo_ctl.sh

This script also installs the pcan driver kernel module (unless already loaded).

Copy this script to /usr/sbin

    /usr/lib/erlang/jlrdemo-*/priv/jlrdemo_ctl.sh /usr/sbin
	
Edit the uxlaunch systemd service in file:

     /etc/systemd/system/display-manager.service
	 
Edit the ExecStart= line so that it looks like this:

    ExecStart=/bin/sh /usr/sbin/tizenctl.sh start

**Note** The /usr/sbin/tizenctl.sh, which will start the dashboard UI,
  and the `/usr/sbin/jlrdemo.sh` script is not provided by the
  Exosense RPMs. Please see the Tizen documentation for details on
  where to source this.

# Upgrading the Exosense JLR demo

Be sure to remove the old package using `rpm -e` before installing the new version.
Also be sure to execute the following command to wipe any old setup data.

    rm -rf /root/setup



