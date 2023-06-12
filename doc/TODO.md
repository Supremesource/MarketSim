
- Add beter statistics
- add more complex mechanic checkers, and ensure that the current ones work as intended
- more complex orderbook and volume, has to do with statistics
- more dynamic spread options
- better psychology ** optional **
- LIQUIDITY + LEVERAGE


- SYSTEM LOGIC:
first generate volume
pass into orderbook loo
orderbook loop take and initialize a generator in the orderbook loop
pass into position future
work with position future inside of that generator and subsequently pass
the possible liquidation volume inside of the volume and generator as a tekr


on 13.6
make first volume generator function truly only for volume outside of position generator
and move that position generator into the core of orderbook loop from where it is going to get activated based off of volume

if finished wihin 3 hours move onto a process where that will get passed into position future not necessary with a working integration of that
just pass it and make it be there. Also activate things like open interest and associated things from there.

conclusion for 13.6

-- VOLUME -> ORDERBOOKLOOP -> GENERATOR (X,Y) ---[PRICE + GEN]---> POSITION FUTURE

THAT WOULD BE SUCESS FOR THE DAY !

AT THE END DO:
Fronend
- ADD TIME FRAME FORMATING

