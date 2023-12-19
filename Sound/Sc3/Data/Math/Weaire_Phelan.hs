{- |  Weaire, D.; Phelan, R. (1994),
      "A counter-example to Kelvin's conjecture on minimal surfaces",
      Phil. Mag. Lett., 69: 107–110, doi:10.1080/09500839408241577.

      <http://www.steelpillow.com/polyhedra/wp/wp.html>
-}
module Sound.Sc3.Data.Math.Weaire_Phelan where

-- | Irregular pentagonal dodecahedra
wp_12 :: Fractional n => [(n, n, n)]
wp_12 =
  let n1 = 3.14980
      n2 = n1 * 2 -- 6.2996
      n3 = n1 * 4 / 3 -- 4.1997
  in [ (n1, 0, n2)
     , (-n1, 0, n2)
     , (n3, n3, n3)
     , (0, n2, n1)
     , (-n3, n3, n3)
     , (-n3, -n3, n3)
     , (0, -n2, n1)
     , (n3, -n3, n3)
     , (n2, n1, 0)
     , (-n2, n1, 0)
     , (-n2, -n1, 0)
     , (n2, -n1, 0)
     , (n3, n3, -n3)
     , (0, n2, -n1)
     , (-n3, n3, -n3)
     , (-n3, -n3, -n3)
     , (0, -n2, -n1)
     , (n3, -n3, -n3)
     , (n1, 0, -n2)
     , (-n1, 0, -n2)
     ]

{- | Weaire Phelan 12 Faces

>>> map length wp_12_faces == replicate 12 5
True
-}
wp_12_faces :: [[Int]]
wp_12_faces =
  [ [1, 2, 3, 4, 5]
  , [16, 17, 18, 19, 20]
  , [1, 2, 8, 7, 6]
  , [2, 3, 9, 12, 8]
  , [3, 4, 14, 13, 9]
  , [4, 5, 10, 15, 14]
  , [5, 1, 6, 11, 10]
  , [16, 17, 7, 6, 11]
  , [17, 18, 12, 8, 7]
  , [18, 19, 13, 9, 12]
  , [19, 20, 15, 14, 13]
  , [20, 16, 11, 10, 15]
  ]

-- | Tetrakaidecahedra
wp_14 :: Fractional n => [(n, n, n)]
wp_14 =
  let n1 = 3.14980
      n2 = 3.70039
      n3 = n1 * 4 / 3 -- 4.19974
      n4 = 0.80026
      n5 = 5 + n4 -- 5.80026
      n6 = 6.85020
      n7 = 1.29961
  in [ (n1, n2, 5)
     , (-n1, n2, 5)
     , (-5, 0, 5)
     , (-n1, -n2, 5)
     , (n1, -n2, 5)
     , (5, 0, 5)
     , (n3, n5, n5)
     , (-n3, n5, n5)
     , (-n6, 0, n7)
     , (-n3, -n5, n5)
     , (n3, -n5, n5)
     , (n6, 0, n7)
     , (n5, n3, -n5)
     , (0, n6, -n7)
     , (-n5, n3, -n5)
     , (-n5, -n3, -n5)
     , (0, -n6, -n7)
     , (n5, -n3, -n5)
     , (n2, n1, -5)
     , (0, 5, -5)
     , (-n2, n1, -5)
     , (-n2, -n1, -5)
     , (0, -5, -5)
     , (n2, -n1, -5)
     ]

{- | Weaire Phelan 14 Faces

>>> map length wp_14_faces == replicate 2 6 ++ replicate 12 5
True
-}
wp_14_faces :: [[Int]]
wp_14_faces =
  [ [1, 2, 3, 4, 5, 6]
  , [19, 20, 21, 22, 23, 24]
  , [1, 2, 8, 14, 7]
  , [2, 3, 9, 15, 8]
  , [3, 4, 10, 16, 9]
  , [4, 5, 11, 17, 10]
  , [5, 6, 12, 18, 11]
  , [6, 1, 7, 13, 12]
  , [19, 20, 14, 7, 13]
  , [20, 21, 15, 8, 14]
  , [21, 22, 16, 9, 15]
  , [22, 23, 17, 10, 16]
  , [23, 24, 18, 11, 17]
  , [24, 19, 13, 12, 18]
  ]
