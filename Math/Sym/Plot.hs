{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Module      : Math.Sym.Plot
-- Copyright   : (c) Anders Claesson 2013
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- 
-- Plot permutations.

module Math.Sym.Plot
    (
     plotPerm, plotPerms, savePNG
    ) where

import Diagrams.Prelude
import Graphics.Rendering.Diagrams.Core
import Diagrams.Backend.Cairo.Internal
import Math.Sym (Perm (..), st, toList, size)

type DC = Diagram Cairo R2

-- An n x n grid
grid :: Int -> DC
grid n = stripes `atop` rotateBy (1/4) stripes
    where 
      stripes = centerXY . hcat' with {sep = 1} . replicate n . vrule $ fromIntegral n

-- | The plot of a permutation as a Cairo diagram.
plotPerm :: Perm a => a -> DC
plotPerm w = centerXY plot `atop` grid n `atop` background
    where
      n = size w
      v = map fromIntegral . toList $ st w
      plot = position $ zip (zipWith (curry p2) [0..] v) dots
      dots = repeat $ circle 0.2 # fc black
      background = square (fromIntegral n) # fc whitesmoke # lc white

-- Split a list into fixed length chunks
chunk :: Int -> [a] -> [[a]]
chunk k [] = []
chunk k xs = ys : chunk k zs where (ys, zs) = splitAt k xs

-- | The plot of a list of permutations as a Cairo diagram. The first
-- argument specifies how many permutations are on each row.
plotPerms :: Perm a => Int -> [a] -> DC
plotPerms cols = vcat . map hcat . chunk cols . map (pad 1.1 . plotPerm)

-- | Save a Cairo diagram as a PNG.
savePNG :: Double -> String -> DC -> IO ()
savePNG width filePath = fst . renderDia Cairo (CairoOptions filePath (Width width) PNG)
