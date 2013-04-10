# Jaguar Land Rover HVAC demo app running on Exosense Device Stack.


# Installing the pcan driver.

**Add pcan driver build instructions (Rudi)**

Load the module with

    insmod pcan.ko bitrate=0x031C

Bring up the interface with

    ifconfig can0 up


# After install, before starting the demo.

rm -rf /root/setup