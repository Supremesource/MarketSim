romFile, takeamount,
                                              taketowalls, volumechecker,
                                              zipToTuples, positionamountcheck)

import           RunSettings
import           Statistics                  (generateRandomPosition)
import           Util                        (aggregateStats, initStats,
                                              recursiveList)




mainLoop :: Stats -> Int -> IO [(Int, VolumeSide)]
mainLoop aggregatedStats remainingRuns = do
      if remainingRuns > 0
        then do
          positions <- replicateM numPositions generateRandomPosition
          let newAggregatedStats = foldl (flip aggregateStats) aggregatedStats positions
          volumesAndSides <- Control.Monad.forM (zip [1..] positions) $ \(i, pos) -> do
            (volume, side) <- printPositionStats i pos




            return (volume, side)



          putStrLn "--------"
          printStats newAggregatedStats
          nextVolumesAndSides <- mainLoop newAggregatedStats (remainingRuns - 1)


          return (volumesAndSides ++ nextVolumesAndSides)
        else do
          printFinal aggregatedStats
          return []


    -- TODO
    -- |
    -- return the list of volumes and sides , right here not after
    -- fix the maker tuple
    -- add majority of the outputs into the data files & display in charts using elm/ javascript/ https/ css



printFinal :: Stats -> IO ()
printFinal aggregatedStats = do
  putStrLn "\n\n\n\n\n\n-------------\n\n\n you have reached the end of the generator \n\n\n📊📊AGGREGATED STATS📊📊: "
  printStats aggregatedStats

main :: IO ()
main = do


  -- CHECKING IF FILES ARE EMPTY
  isBidEmpty  <- isFileEmpty bidBookPath
  isAskEmpty  <- isFileEmpty askBookPath
  hSetBuffering stdout LineBuffering
  -- WIPING RUN == TRUE
  -- when wiping run is running the whole code is not evaluated