module Helpers
( getOpen
, getHigh
, getLow
, getClose
, newStreak
, calcPosition
) where
    getOpen :: [Float] -> Float
    getOpen candle = head candle

    getHigh :: [Float] -> Float
    getHigh candle = candle !! 1

    getLow :: [Float] -> Float
    getLow candle = candle !! 2

    getClose :: [Float] -> Float
    getClose candle = candle !! 3
 
    newStreak :: Float -> Float -> Float -> Float
    newStreak streak open close
        |   (streak >= 0) && ((close - open) > 0) =  streak + 1.0
        |   (streak >= 0) && ((close - open) < 0) =  (-1.0)
        |   (streak < 0) && ((close - open) > 0) = 1.0
        |   (streak < 0) && ((close - open) < 0) =  streak - 1.0
        |   otherwise =  streak

    calcPosition :: Float -> Float -> [Float] -> [Float] -> [Float]
    calcPosition up down position candle
        |   (money > 0) && (low < priceBase*(1 - down / 100)) = 
                let price = priceBase * (1 - down / 100)
                in [0.0, money / price, price, 1.0, streak', (money / price) * close]
        |   (quantity > 0) && (high > priceBase * (1 + up / 100)) = 
                let price = priceBase * (1 + up / 100)
                in [quantity * price, 0.0, price, -1.0, close, streak', quantity * close]
        |   otherwise =  
                let price = if money > 0 then close else priceBase
                in [money, quantity, price, 0.0, streak', money + quantity * close]
        where   money = head position
                quantity = position !! 1
                priceBase = position !! 2
                action = position !! 3
                streak = position !! 4
                open = getOpen candle 
                close = getClose candle 
                low = getLow candle
                high = getHigh candle
                streak' = newStreak streak open close
