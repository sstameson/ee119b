# Synthesis Output

This report contains a summary of the synthesis output of ISE 14.7. Synthesis was performed with the synthesis process properties optimization goal set to "area". All other process properties were left unchanged.

## Gate Usage

```
Device utilization summary:
---------------------------

Selected Device : 3s1200efg320-4 

 Number of Slices:                     1546  out of   8672    17%  
 Number of 4 input LUTs:               2734  out of  17344    15%  
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
clk            |  312.635|         |         |         |
---------------+---------+---------+---------+---------+
```
