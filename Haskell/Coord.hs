{-# LANGUAGE OverloadedStrings #-}

data Coord { x :: Double, y :: Double }

instance FromJSON Coord where
   parseJSON (Object v) = Coord    <$>
                          v .: "x" <*>
                          v .: "y"

-- A non-Object value is of the wrong type, so use mzero to fail.
   parseJSON _          = mzero
