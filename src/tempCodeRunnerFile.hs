type OrderBook = [(Double, Int)]
type VolumeList = [(Int, VolumeSide)]
type Generator = StdGen
type FullWall = [Int]
type StartingPoint = Double
type Totakefromwall = Int
type Volume = (Int, VolumeSide)

data OrderBookData = OrderBookData
  { bidBookData :: OrderBook
  , askBookData :: OrderBook}


settingcheck :: VolumeSide -> Int -> Int -> Int -> IO ()
settingcheck vSide volumeAmount asksTotal bidsTotal = do
            let check
                    :: String
                    | vSide /=  Buy && vSide /=  Sell =
                        error "wrong volume specification !"
                    | vSide ==  Buy && volumeAmount > asksTotal =
                        error "THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | vSide ==  Sell && volumeAmount > bidsTotal =
                        error "-THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | otherwise = ""
            putStrLn check

calculateVolumes :: VolumeSide -> Int -> (Int, Int)
calculateVolumes vSide volumeAmount = 
    (if vSide == Sell then volumeAmount else 0,
     if vSide == Buy then volumeAmount else 0)

lengthChanges :: OrderBook -> OrderBook -> OrderBook -> OrderBook -> (Int, Int)
lengthChanges bidUpdateBook bidBook askUpdateBook askBook =
    (lengthchange bidUpdateBook bidBook, lengthchange askUpdateBook askBook)

startingPrices :: VolumeSide -> OrderBook -> OrderBook -> Double
startingPrices vSide bidUpdateBook askUpdateBook =
    max (if vSide == Sell then fst (head bidUpdateBook) else 0)
        (if vSide == Buy then fst (head askUpdateBook) else 0)


-- | processing the orderbook with a volume
recursiveList :: VolumeList -> OrderBook -> OrderBook -> Generator -> Generator -> FullWall -> FullWall -> StartingPoint -> Totakefromwall -> IO (OrderBook, OrderBook) 
-- | base case
recursiveList [] _ _ _ _ _ _ _ _ = return ([], [])
recursiveList (x:xs) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall -- | recursive case
         =
    orderbookLoop x bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall >>=
    \(newBidBook, newAskBook) -> do
        let (newGen1, newGen2) = (fst (split gen1), fst (split gen2)) -- create two new generators
        recursiveList xs newBidBook newAskBook newGen1 newGen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall
  where
    orderbookLoop :: Volume -> OrderBook
                  -> OrderBook -> Generator -> Generator -> FullWall -> FullWall -> StartingPoint -> Totakefromwall -> IO (OrderBook, OrderBook) 
    orderbookLoop (volumeAmount, vSide) bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall = do
            -- | local variables
            let (volumeBID, volumeASK) = calculateVolumes vSide volumeAmount
            let bidUpdateBook = orderbookChange bidBook volumeBID
            let askUpdateBook = orderbookChange askBook volumeASK
-- | how much volume took from certain order books
            let (lengthchangeASK, lengthchangeBID) = lengthChanges bidUpdateBook bidBook askUpdateBook askBook
            let startingprice = startingPrices vSide bidUpdateBook askUpdateBook
            let upMovesInsert :: [Double] = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
            let downMovesInsert :: [Double] = take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2
            let askSetupInsert :: [Double] = take lengthchangeASK (tail (infiniteList' startingprice gen1 upMovesInsert))
            let bidSetupInsert :: [Double] = take lengthchangeBID (tail (infiniteListDown' startingprice gen2 downMovesInsert))
            let maxMinLimit :: [[Int]] = [fullwallsASK, fullwallsBIDS]
            pricesASK :: [Int] <- printCustomRandomList lengthchangeASK
            pricesBID :: [Int] <- printRandomList' lengthchangeBID
            -- | the / number is how smaller the insertion will be
            let listASK :: [(Double, Int)] = zipToTuples askSetupInsert (map (round . (/ 1.10) . fromIntegral) pricesASK)
            let listBID :: [(Double, Int)] = zipToTuples bidSetupInsert (map (round . (/ 1.10) . fromIntegral) pricesBID)
            let currentbookASK :: [(Double, Int)] = if vSide ==  Buy     then askUpdateBook  else listASK ++ askBook
            let currentbookBID :: [(Double, Int)] = if vSide ==  Sell    then bidUpdateBook else listBID ++ bidBook
            let finalBookAsk :: [(Double, Int)] = if largerSpread then tail currentbookASK else currentbookASK
            let finalBookBid :: [(Double, Int)] = if largerSpread then tail currentbookBID else currentbookBID
            let toAdditional = additionalInformation finalBookAsk finalBookBid vSide volumeAmount startingPoint totakefromwall maxMinLimit lengthchangeASK lengthchangeBID bidBook askBook listASK listBID
            return (currentbookBID, currentbookASK)

additionalInformation :: OrderBook -> OrderBook -> VolumeSide -> Int -> StartingPoint -> Totakefromwall -> [[Int]] -> Int -> Int -> OrderBook -> OrderBook -> OrderBook -> OrderBook -> Double -> IO ()
additionalInformation fBookBid fBookAsk vside vamount spoint towall mxMinLimit lchangeAsk lchangeBid bbookASK bbookBID listAsk listBid sprice = do
            -- ? ADDITIONAL INFORMATION
-- | ask in total in terms of count
            let asktotal = fromIntegral (length fBookAsk)
-- | bids in total in terms of count
            let bidtotal = fromIntegral (length fBookBid)
-- | Ratio between bids and AKS
            let bidAskRatio = abs ((bidtotal - asktotal) / (bidtotal + asktotal)) :: Double
-- | Ratio is benefiting :
            let bidAskBenefit = if asktotal > bidtotal then putStr " ->  benefiting ASKS" else putStr " -> benefeting BIDS"
-- | asks in total -> $$
            let asksTotal = sumInts fBookAsk
 -- | bids in total -> $$
            let bidsTotal = sumInts fBookBid
-- | bid ask spread
            let firstelemASK = (fst . head) fBookAsk
            let firstelemBID = (fst . head) fBookBid
            let spread = spread' firstelemASK firstelemBID
-- | checking if the stats above will go through with the orderbook
-- | local check
            let check = settingcheck vside vamount asksTotal bidsTotal
    
    -- TODO make this shit more simple
            let orderbookIO =   generateOrderBook -- function to generate the orderbook
                                fBookAsk
                                fBookBid
                                spread
                                asksTotal
                                bidsTotal
                                bidAskRatio
                                bidAskBenefit
                                logPath
                                spoint
                                mxMinLimit
                                towall
                                lchangeBid
                                lchangeAsk                        
                                bbookBID
                                bbookASK
                                listAsk
                                listBid
                                (show vside)
                                vamount
                                sprice
                                pricePath
                                bidBookPath
                                askBookPath
            orderbookIO
-- | returning the orderbook
            return ()
