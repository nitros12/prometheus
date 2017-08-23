{-# LANGUAGE TupleSections #-}

module System.Metrics.Prometheus.Metric.Histogram
       ( Histogram
       , HistogramSample (..)
       , Buckets
       , UpperBound
       , new
       , observe
       , sample
       , observeAndSample
       ) where


import           Data.Bool  (bool)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Map   (Map)
import qualified Data.Map   as Map


newtype Histogram = Histogram { unHistogram :: IORef HistogramSample }


type UpperBound = Double -- Inclusive upper bounds
type Buckets = Map UpperBound Double


data HistogramSample =
    HistogramSample
    { histBuckets :: Buckets
    , histSum     :: Double
    , histCount   :: Int
    }


new :: [UpperBound] -> IO Histogram
new buckets = Histogram <$> newIORef empty
  where
    empty = HistogramSample (Map.fromList $ map (, 0) (read "Infinity" : buckets)) zeroSum zeroCount
    zeroSum = 0.0
    zeroCount = 0


observeAndSample :: Double -> Histogram -> IO HistogramSample
observeAndSample x = flip atomicModifyIORef' update . unHistogram
  where
    update histData = (hist' histData, histData)
    hist' histData =
        histData { histBuckets = updateBuckets x $ histBuckets histData
                 , histSum = histSum histData + x
                 , histCount = histCount histData + 1
                 }


observe :: Double -> Histogram -> IO ()
observe x h = observeAndSample x h >> pure ()


updateBuckets :: Double -> Buckets -> Buckets
updateBuckets x = Map.mapWithKey updateBucket
  where updateBucket key val = bool val (val + 1) (x <= key)


sample :: Histogram -> IO HistogramSample
sample = readIORef . unHistogram
