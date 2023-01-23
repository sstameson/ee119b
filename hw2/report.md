# Synthesis Output

This report contains a summary of the synthesis output of ISE 14.7. Synthesis
was performed with the synthesis process properties optimization goal set to
"area". All other process properties were left unchanged.

## Gate Usage

```
Device utilization summary:
---------------------------

Selected Device : 3s1200efg320-4 

 Number of Slices:                     1531  out of   8672    17%  
 Number of Slice Flip Flops:            206  out of  17344     1%  
 Number of 4 input LUTs:               2700  out of  17344    15%  
    Number used as logic:              2699
    Number used as Shift registers:       1
 Number of IOs:                          54
 Number of bonded IOBs:                  53  out of    250    21%  
    IOB Flip Flops:                      52
 Number of GCLKs:                         1  out of     24     4%  
```

## Speed

```
Clock to Setup on destination clock clk
---------------+---------+---------+---------+---------+
               | Src:Rise| Src:Fall| Src:Rise| Src:Fall|
Source Clock   |Dest:Rise|Dest:Rise|Dest:Fall|Dest:Fall|
---------------+---------+---------+---------+---------+
clk            |   88.136|         |         |         |
---------------+---------+---------+---------+---------+
```

## System Quality Factor

```
SQF = 10 * (f'_max / f_max) - l^2
    = 10 * ((1/88.136) / (1/312.635)) - 4^2
    = 19.5
```

## System Size

The original, unpipelined system used 1546 slices and the pipelined system used
1531 slices. As shown, the design size is roughly the same. This is because
each slice already has two flip-flops, so the unpipelined design simply
removes them by configuring control signals on a MUX. To add pipelining, the
synthesis tool can use a nearly identical design, but just change the control
signals to include the flip-flops in the datapath of the slice, rather than
skip them.

The slight decrease in size of the pipelined design is odd, but likely occurs
due to slight differences in optimizations made by the synthesis tool. The
important point is that adding flip-flops doesn't substantially change
area, since the slices all already have flip-flops that can be included or
removed from the datapath at no additional area cost.

When optimizing for speed instead of area, the unpipelined design is acually
substantially larger because the synthesis tool spreads logic over many
slices to avoid routing congestion. However, the results presented here are
for area optimization.
