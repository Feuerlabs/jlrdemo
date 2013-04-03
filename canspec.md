# Settings
Protocol: CAN15765

Timing Registers: BTR0 = 47, BTR1 = 14

Bits/sec: 125000

CAN identifiers: 29bit


# Message
FCIM_FACP_A
ID: 0x240
DLC: 8

## Front System On

Element | Value
--------|-------
Symbol | FCIM_FACP_A::FrontSystemOnCmd_MS
Startbit | 46
Length (Bit) | 1

Values:

Hex | Meaning
----|------
0x1 | On
0x0 | Off


##  Air Recirculation

Element | Value
--------|-------
Symbol|FCIM_FACP_A::RecircReq_MS
Startbit| 56
Length (Bit)|2

Values:

0x3 | Recirc Timed
0x2 | Recirc Auto
0x1 | Recirc On
0x0 | Recirc Off

## Fan (Blower) Speed

Element | Value
--------|-------
Symbol|FCIM_FACP_A::FrontBlwrSpeedCmd_MS
Startbit|24
Length (Bit)|4

Values:

Hex | Meaning
----|------
 0x0 | Off

## Air Distribution

Element | Value
--------|-------
Symbol|FCIM_FACP_A::FLHSDistrCmd_MS
Startbit| 16
Length (Bit)|3

Values:

Hex | Meaning
----|------
0x7 | Foot / Face Screen
0x6 | Face / Screen
0x5 | Foot / Screen
0x4 | Screen
0x3 | Foot / Face
0x2 | Face
0x1 | Foot
0x0 | Auto

##  Temperature Control | Left

Element | Value
--------|-------
Symbol|FCIM_FACP_A::FrontTSetLeftCmd_MS
Startbit| 32
Length (Bit)|6

Values:

Hex | Meaning
----|------
0x0 to 0x3E | Temp Deg C

## Temperature Control | Right

Element | Value
--------|-------
Symbol | FCIM_FACP_A::FrontTSetRightCmd_MS
Startbit | 40
Length (Bit) | 6

Values:

Hex | Meaning
----|------
0x0 to 0x3E | Temp Deg C

##  Aircon

Element | Value
--------|-------
Symbol | FCIM_FACP_A::ACCommand_MS
Startbit | 47
Length (Bit) | 1

Values:

Hex | Meaning
----|------
0x1 | AC On
0x0 | AC Off

## Heated Front Screen

Element | Value
--------|-------
Symbol | FCIM_FACP_A::HFSCommand_MS
Startbit | 61
Length (Bit) | 1

Values:

Hex | Meaning
----|------
0x1 | On
0x0 | Off

# Heated Rear Window

Element | Value
--------|-------
Symbol | FCIM_FACP_A::HRWCommand_MS
Startbit | 60
Length (Bit) | 1

Values:

Hex | Meaning
----|------
0x1 | On
0x0 | Off
