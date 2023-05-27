
# TEMPLEATES

here are some predefine settings for different looking simulations:

*(note that PLTCHRT is always set to `TRUE`)*

### 1. High cap liquid
* note: the loading time on this one might be a bit larger*

- STRVAL         = 10000
- NUMPOS         = 1250
- NUMRUN         = 2
- maxM/T         = 100
- ROUND          = 2
- CYCLE          = 
[RANDOM, UP, UP, UUP, DW, DW, UP, UUP, DWW, DWW, RANDOM, CN, CN, RANDOM, UUP, RANDOM, UUP, UUP, UUP, UP, CN, RANDOM, UUP]
- VOL            = 200
- VOL2           = buy volume; 
                   bs;250,up;1000000 |
                   bs;250,up; 800000 |
                   sell volume;
                   bs;200, up;1100000 |
                   bs;200, up;500000
- STAT           = taker: x;40, y;35, z;15, f;10 | maker: x;40, y;40, z;10, f;10
- BookMinMax$    = min; 5000, max; 1500000
- bookMinMaxMove = min; 0.05, max; 0.5 | min; 0.06, max; 0.5 
- SPREAD         = FALSE
- TakeBidAsk     = bid; 4000, ask; 4000
- wallLikelyHood = 300
- wallAmp        = 3

### 2. Low cap illiquid

- STRVAL         = 
- NUMPOS         = 
- NUMRUN         = 
- maxM/T         =
- ROUND          =
- CYCLE          =
- VOL            =
- VOL2           =
- STAT           =
- BookMinMax$    =
- bookMinMaxMove =
- SPREAD         =
- TakeBidAsk     =
- wallLikelyHood =
- wallAmp        =

### 3. Short squeeze

- STRVAL         = 
- NUMPOS         = 
- NUMRUN         = 
- maxM/T         =
- ROUND          =
- CYCLE          =
- VOL            =
- VOL2           =
- STAT           =
- BookMinMax$    =
- bookMinMaxMove =
- SPREAD         =
- TakeBidAsk     =
- wallLikelyHood =
- wallAmp        =

### 4. Long squeeze

- STRVAL         = 
- NUMPOS         = 
- NUMRUN         = 
- maxM/T         =
- ROUND          =
- CYCLE          =
- VOL            =
- VOL2           =
- STAT           =
- BookMinMax$    =
- bookMinMaxMove =
- SPREAD         =
- TakeBidAsk     =
- wallLikelyHood =
- wallAmp        =