
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

completed for about 60%
-- VOLUME -> ORDERBOOKLOOP -> GENERATOR (X,Y) ---[PRICE + GEN]---> POSITION FUTURE

14.6
-- finish the green txt in util

16.6

add functional second run function and forced circulation, fix positioning writing. Then move onto fronend practicing, would be success for the day.

19.6
finish TODO in util so normal run works
pass liquidatoion volume


20.6 
Add liquidation circulation
finish testing everything and implement complex checking mechanisms

THAT WOULD BE SUCESS FOR THE DAY !


22 . 6

Take the liquidations to the end make correct writing with a corresponding data


TODO 23 . 6
pass everything from recursive base case into the aggregated stats 
take positioning out of main and add it into the generator itself
above the recursive list


then do
Optimization: Fix sequence to list transitions, fix conncat performances
finish data writing


24.6
Add multiple correctness checker mechanics


AT THE END DO:
Fronend
- ADD TIME FRAME FORMATING


