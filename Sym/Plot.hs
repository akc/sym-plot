{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Plot permutations.

module Sym.Plot
    (
     Plot, plotPerm, plotPerms, savePlot
    ) where

import Diagrams.Prelude hiding (size)
import Diagrams.Backend.Cairo
import Sym.Perm (toList)
import Sym

-- | A plot is represented by Cairo diagram
type Plot = Diagram Cairo

-- An n x n grid
grid :: Int -> Plot
grid n = stripes `atop` rotateBy (1/4) stripes
  where
    stripes = centerXY . hcat' (def {_sep=1}) . replicate n . vrule $ fromIntegral n

-- | The plot of a permutation.
plotPerm :: Permutation a => a -> Plot
plotPerm w = centerXY plot `atop` grid n `atop` background
  where
    n = size w
    v = map fromIntegral . toList $ st w
    plot = position $ zip (zipWith (curry p2) [0..] v) dots
    dots = repeat $ circle 0.2 # fc black
    background = square (fromIntegral n) # fc white # lc white

-- Split a list into fixed length chunks
chunk :: Int -> [a] -> [[a]]
chunk k [] = []
chunk k xs = ys : chunk k zs where (ys, zs) = splitAt k xs

-- | The plot of a list of permutations. The first argument specifies
-- how many permutations are on each row.
plotPerms :: Permutation a => Int -> [a] -> Plot
plotPerms cols = vcat . map hcat . chunk cols . map (pad 1.1 . plotPerm)

-- | Save plot as a png, ps, pdf, or svg (determined automatically from
-- the file extension).
savePlot :: Double -> String -> Plot -> IO ()
savePlot width filePath = renderCairo filePath (mkWidth width)
