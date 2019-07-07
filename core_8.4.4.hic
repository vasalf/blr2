==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 5,385, types: 7,025, coercions: 1,492, joins: 19/136}

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main18 :: Addr#
main18 = "biclique/1500"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main17 :: [Char]
main17 = unpackCString# main18

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main23 :: Addr#
main23 = "grid/750"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main22 :: [Char]
main22 = unpackCString# main23

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main28 :: Addr#
main28 = "caterpillar/1000000"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main27 :: [Char]
main27 = unpackCString# main28

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main36 :: Addr#
main36 = "ttree/12"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main35 :: [Char]
main35 = unpackCString# main36

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main41 :: Addr#
main41 = "btree/20"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main40 :: [Char]
main40 = unpackCString# main41

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main47 :: Addr#
main47 = "path/1000000"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main46 :: [Char]
main46 = unpackCString# main47

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main53 :: Addr#
main53 = "clique/2000"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main52 :: [Char]
main52 = unpackCString# main53

Rec {
-- RHS size: {terms: 30, types: 24, coercions: 0, joins: 0/0}
$wpoly_go1 :: forall a. Int# -> Map Int a -> Bool
$wpoly_go1
  = \ (@ a) (ww :: Int#) (w :: Map Int a) ->
      case w of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case ipv1 of { I# y# ->
          case <# ww y# of {
            __DEFAULT ->
              case ==# ww y# of {
                __DEFAULT -> $wpoly_go1 ww ipv4;
                1# -> True
              };
            1# -> $wpoly_go1 ww ipv3
          }
          };
        Tip -> False
      }
end Rec }

Rec {
-- RHS size: {terms: 25, types: 22, coercions: 0, joins: 0/0}
$smember3 :: forall a. Integer -> Map Integer a -> Bool
$smember3
  = \ (@ a) (ds :: Integer) (ds1 :: Map Integer a) ->
      case ds of ds2 { __DEFAULT ->
      case ds1 of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case compareInteger ds2 ipv1 of {
            LT -> $smember3 ds2 ipv3;
            EQ -> True;
            GT -> $smember3 ds2 ipv4
          };
        Tip -> False
      }
      }
end Rec }

Rec {
-- RHS size: {terms: 66, types: 34, coercions: 0, joins: 0/0}
$w$sgo1 :: Int -> Int# -> Set Int -> Set Int
$w$sgo1
  = \ (w :: Int) (ww :: Int#) (w1 :: Set Int) ->
      case w1 of wild {
        Bin dt y l r ->
          case y of wild1 { I# y# ->
          case <# ww y# of {
            __DEFAULT ->
              case ==# ww y# of {
                __DEFAULT ->
                  case $w$sgo1 w ww r of r' { __DEFAULT ->
                  case reallyUnsafePtrEquality# r' r of {
                    __DEFAULT -> balanceR wild1 l r';
                    1# -> wild
                  }
                  };
                1# -> wild
              };
            1# ->
              case $w$sgo1 w ww l of l' { __DEFAULT ->
              case reallyUnsafePtrEquality# l' l of {
                __DEFAULT -> balanceL wild1 l' r;
                1# -> wild
              }
              }
          }
          };
        Tip -> case lazy w of dt { I# ipv -> Bin 1# dt Tip Tip }
      }
end Rec }

Rec {
-- RHS size: {terms: 81, types: 40, coercions: 0, joins: 0/0}
$w$sgo3 :: Int -> Int# -> Set Int -> Set Int
$w$sgo3
  = \ (w :: Int) (ww :: Int#) (w1 :: Set Int) ->
      case w1 of wild {
        Bin dt y l r ->
          case y of wild1 { I# y# ->
          case <# ww y# of {
            __DEFAULT ->
              case ==# ww y# of {
                __DEFAULT ->
                  case $w$sgo3 w ww r of r' { __DEFAULT ->
                  case reallyUnsafePtrEquality# r' r of {
                    __DEFAULT -> balanceR wild1 l r';
                    1# -> wild
                  }
                  };
                1# ->
                  case lazy w of wild2 { I# ipv ->
                  case reallyUnsafePtrEquality# w wild1 of {
                    __DEFAULT -> Bin dt wild2 l r;
                    1# -> wild
                  }
                  }
              };
            1# ->
              case $w$sgo3 w ww l of l' { __DEFAULT ->
              case reallyUnsafePtrEquality# l' l of {
                __DEFAULT -> balanceL wild1 l' r;
                1# -> wild
              }
              }
          }
          };
        Tip -> case lazy w of dt { I# ipv -> Bin 1# dt Tip Tip }
      }
end Rec }

Rec {
-- RHS size: {terms: 208, types: 179, coercions: 0, joins: 0/0}
$s$wsplitS :: Int -> Set Int -> (# Set Int, Set Int #)
$s$wsplitS
  = \ (w1 :: Int) (w4 :: Set Int) ->
      case w4 of {
        Bin dt y l r ->
          case y of wild1 { I# y# ->
          case w1 of wild2 { I# x# ->
          case <# x# y# of {
            __DEFAULT ->
              case ==# x# y# of {
                __DEFAULT ->
                  case $s$wsplitS wild2 r of { (# ww1, ww2 #) ->
                  case l of wild3 {
                    Bin ipv ipv1 ipv2 ipv3 ->
                      case ww1 of wild4 {
                        Bin ipv4 ipv5 ipv6 ipv7 ->
                          case <# (*# 3# ipv) ipv4 of {
                            __DEFAULT ->
                              case <# (*# 3# ipv4) ipv of {
                                __DEFAULT ->
                                  (# Bin (+# (+# ipv ipv4) 1#) wild1 wild3 wild4, ww2 #);
                                1# ->
                                  case balanceR
                                         ipv1 ipv2 (link_$slink1 ipv4 ipv5 ipv6 ipv7 ipv3 wild1)
                                  of dt1
                                  { __DEFAULT ->
                                  (# dt1, ww2 #)
                                  }
                              };
                            1# ->
                              case balanceL ipv5 (link_$slink ipv6 ipv ipv1 ipv2 ipv3 wild1) ipv7
                              of dt1
                              { __DEFAULT ->
                              (# dt1, ww2 #)
                              }
                          };
                        Tip ->
                          case link_$sinsertMax ipv ipv1 ipv2 ipv3 wild1 of dt1
                          { __DEFAULT ->
                          (# dt1, ww2 #)
                          }
                      };
                    Tip ->
                      case insertMin wild1 ww1 of dt1 { __DEFAULT -> (# dt1, ww2 #) }
                  }
                  };
                1# -> (# l, r #)
              };
            1# ->
              case $s$wsplitS wild2 l of { (# ww1, ww2 #) ->
              case ww2 of wild3 {
                Bin ipv ipv1 ipv2 ipv3 ->
                  case r of wild4 {
                    Bin ipv4 ipv5 ipv6 ipv7 ->
                      case <# (*# 3# ipv) ipv4 of {
                        __DEFAULT ->
                          case <# (*# 3# ipv4) ipv of {
                            __DEFAULT ->
                              (# ww1, Bin (+# (+# ipv ipv4) 1#) wild1 wild3 wild4 #);
                            1# ->
                              case balanceR
                                     ipv1 ipv2 (link_$slink1 ipv4 ipv5 ipv6 ipv7 ipv3 wild1)
                              of dt1
                              { __DEFAULT ->
                              (# ww1, dt1 #)
                              }
                          };
                        1# ->
                          case balanceL ipv5 (link_$slink ipv6 ipv ipv1 ipv2 ipv3 wild1) ipv7
                          of dt1
                          { __DEFAULT ->
                          (# ww1, dt1 #)
                          }
                      };
                    Tip ->
                      case link_$sinsertMax ipv ipv1 ipv2 ipv3 wild1 of dt1
                      { __DEFAULT ->
                      (# ww1, dt1 #)
                      }
                  };
                Tip ->
                  case insertMin wild1 r of dt1 { __DEFAULT -> (# ww1, dt1 #) }
              }
              }
          }
          }
          };
        Tip -> (# Tip, Tip #)
      }
end Rec }

Rec {
-- RHS size: {terms: 90, types: 87, coercions: 0, joins: 2/2}
$sunion :: Set Int -> Set Int -> Set Int
$sunion
  = \ (t1 :: Set Int) (ds :: Set Int) ->
      case ds of wild {
        Bin dt x ds1 ds2 ->
          case x of ww { I# ww1 ->
          join {
            fail :: Void# -> Set Int
            fail _
              = case t1 of wild1 {
                  Bin dt1 x1 ds5 ds3 ->
                    case x1 of ww2 { I# ww3 ->
                    join {
                      fail1 :: Void# -> Set Int
                      fail1 _
                        = case $s$wsplitS ww2 wild of { (# ww5, ww6 #) ->
                          case $sunion ds3 ww6 of r1r2 { __DEFAULT ->
                          case $sunion ds5 ww5 of l1l2 { __DEFAULT ->
                          case reallyUnsafePtrEquality# l1l2 ds5 of {
                            __DEFAULT -> link ww2 l1l2 r1r2;
                            1# ->
                              case reallyUnsafePtrEquality# r1r2 ds3 of {
                                __DEFAULT -> link ww2 l1l2 r1r2;
                                1# -> wild1
                              }
                          }
                          }
                          }
                          } } in
                    case ds5 of {
                      Bin ipv ipv1 ipv2 ipv3 -> jump fail1 void#;
                      Tip ->
                        case ds3 of {
                          Bin ipv ipv1 ipv2 ipv3 -> jump fail1 void#;
                          Tip -> $w$sgo3 ww2 ww3 wild
                        }
                    }
                    };
                  Tip -> wild
                } } in
          case ds1 of {
            Bin ipv ipv1 ipv2 ipv3 -> jump fail void#;
            Tip ->
              case ds2 of {
                Bin ipv ipv1 ipv2 ipv3 -> jump fail void#;
                Tip -> $w$sgo1 ww ww1 t1
              }
          }
          };
        Tip -> t1
      }
end Rec }

Rec {
-- RHS size: {terms: 63, types: 42, coercions: 0, joins: 0/0}
$w$sgo4
  :: forall a1.
     (a1 -> a1 -> a1) -> Int# -> a1 -> Map Int a1 -> Map Int a1
$w$sgo4
  = \ (@ a1)
      (w :: a1 -> a1 -> a1)
      (ww :: Int#)
      (w1 :: a1)
      (w3 :: Map Int a1) ->
      case w3 of {
        Bin dt ky y l r ->
          case ky of wild1 { I# y# ->
          case <# ww y# of {
            __DEFAULT ->
              case ==# ww y# of {
                __DEFAULT -> balanceR wild1 y l ($w$sgo4 w ww w1 r);
                1# -> case w y w1 of y' { __DEFAULT -> Bin dt wild1 y' l r }
              };
            1# -> balanceL wild1 y ($w$sgo4 w ww w1 l) r
          }
          };
        Tip -> case w1 of x1 { __DEFAULT -> Bin 1# (I# ww) x1 Tip Tip }
      }
end Rec }

Rec {
-- RHS size: {terms: 64, types: 42, coercions: 0, joins: 0/0}
$w$sgo2
  :: forall a1.
     (a1 -> a1 -> a1) -> Int# -> a1 -> Map Int a1 -> Map Int a1
$w$sgo2
  = \ (@ a1)
      (w :: a1 -> a1 -> a1)
      (ww :: Int#)
      (w1 :: a1)
      (w3 :: Map Int a1) ->
      case w3 of {
        Bin dt ky y l r ->
          case ky of wild1 { I# y# ->
          case <# ww y# of {
            __DEFAULT ->
              case ==# ww y# of {
                __DEFAULT -> balanceR wild1 y l ($w$sgo2 w ww w1 r);
                1# -> case w w1 y of y' { __DEFAULT -> Bin dt (I# ww) y' l r }
              };
            1# -> balanceL wild1 y ($w$sgo2 w ww w1 l) r
          }
          };
        Tip -> case w1 of x1 { __DEFAULT -> Bin 1# (I# ww) x1 Tip Tip }
      }
end Rec }

Rec {
-- RHS size: {terms: 63, types: 124, coercions: 0, joins: 0/0}
$w$sgo13
  :: forall a1.
     Int# -> Map Int a1 -> (# Map Int a1, Maybe a1, Map Int a1 #)
$w$sgo13
  = \ (@ a1) (ww :: Int#) (w :: Map Int a1) ->
      case w of {
        Bin dt kx x l r ->
          case kx of wild1 { I# y# ->
          case <# ww y# of {
            __DEFAULT ->
              case ==# ww y# of {
                __DEFAULT ->
                  case $w$sgo13 ww r of { (# ww2, ww3, ww4 #) ->
                  case link wild1 x l ww2 of lt' { __DEFAULT -> (# lt', ww3, ww4 #) }
                  };
                1# -> (# l, Just x, r #)
              };
            1# ->
              case $w$sgo13 ww l of { (# ww2, ww3, ww4 #) ->
              case link wild1 x ww4 r of gt' { __DEFAULT -> (# ww2, ww3, gt' #) }
              }
          }
          };
        Tip -> (# Tip, Nothing, Tip #)
      }
end Rec }

Rec {
-- RHS size: {terms: 92, types: 134, coercions: 0, joins: 3/3}
$sunionWith
  :: forall a. (a -> a -> a) -> Map Int a -> Map Int a -> Map Int a
$sunionWith
  = \ (@ a)
      (_f :: a -> a -> a)
      (t1 :: Map Int a)
      (ds :: Map Int a) ->
      case ds of wild {
        Bin dt k1 x ds1 ds2 ->
          case k1 of { I# ww1 ->
          join {
            fail :: Void# -> Map Int a
            fail _
              = case t1 of {
                  Bin dt1 k2 x1 ds5 ds3 ->
                    case k2 of ww2 { I# ww3 ->
                    join {
                      fail1 :: Void# -> Map Int a
                      fail1 _
                        = case $w$sgo13 ww3 wild of { (# ww5, ww6, ww7 #) ->
                          join {
                            $j :: a -> Map Int a
                            $j (x1' :: a)
                              = link
                                  ww2 x1' ($sunionWith _f ds5 ww5) ($sunionWith _f ds3 ww7) } in
                          case ww6 of {
                            Nothing -> case x1 of x1' { __DEFAULT -> jump $j x1' };
                            Just x2 -> case _f x1 x2 of x1' { __DEFAULT -> jump $j x1' }
                          }
                          } } in
                    case ds5 of {
                      Bin ipv ipv1 ipv2 ipv3 ipv4 -> jump fail1 void#;
                      Tip ->
                        case ds3 of {
                          Bin ipv ipv1 ipv2 ipv3 ipv4 -> jump fail1 void#;
                          Tip -> $w$sgo2 _f ww3 x1 wild
                        }
                    }
                    };
                  Tip -> wild
                } } in
          case ds1 of {
            Bin ipv ipv1 ipv2 ipv3 ipv4 -> jump fail void#;
            Tip ->
              case ds2 of {
                Bin ipv ipv1 ipv2 ipv3 ipv4 -> jump fail void#;
                Tip -> $w$sgo4 _f ww1 x t1
              }
          }
          };
        Tip -> t1
      }
end Rec }

-- RHS size: {terms: 41, types: 30, coercions: 0, joins: 0/1}
btree3 :: Int# -> Map Int (Set Int)
btree3
  = \ (y :: Int#) ->
      case ># 1# y of {
        __DEFAULT ->
          letrec {
            go :: Int# -> [(Int, Set Int)]
            go
              = \ (x :: Int#) ->
                  : (I# x, Bin 1# (I# (+# (*# 2# x) 1#)) Tip Tip)
                    (case ==# x y of {
                       __DEFAULT -> go (+# x 1#);
                       1# -> []
                     }); } in
          path2 $fOrdInt (go 1#);
        1# -> path2 $fOrdInt []
      }

-- RHS size: {terms: 3, types: 5, coercions: 0, joins: 0/0}
btree2 :: Map Int (Set Int)
btree2 = path2 $fOrdInt []

-- RHS size: {terms: 116, types: 48, coercions: 0, joins: 1/6}
$wbtree :: Int# -> Map Int (Set Int)
$wbtree
  = \ (ww :: Int#) ->
      let {
        i# :: Int#
        i# = -# ww 1# } in
      join {
        $j :: Int# -> Map Int (Set Int)
        $j (y :: Int#)
          = let {
              karg :: Map Int (Set Int)
              karg
                = case >=# i# 0# of {
                    __DEFAULT ->
                      let {
                        b :: Int#
                        b = negateInt# i# } in
                      case >=# b 64# of {
                        __DEFAULT -> btree3 (uncheckedIShiftRA# 1# b);
                        1# -> btree3 0#
                      };
                    1# ->
                      case >=# i# 64# of {
                        __DEFAULT -> btree3 (uncheckedIShiftL# 1# i#);
                        1# -> btree3 0#
                      }
                  } } in
            case ># 1# y of {
              __DEFAULT ->
                letrec {
                  go :: Int# -> [(Int, Set Int)]
                  go
                    = \ (x :: Int#) ->
                        : (I# x, Bin 1# (I# (*# 2# x)) Tip Tip)
                          (case ==# x y of {
                             __DEFAULT -> go (+# x 1#);
                             1# -> []
                           }); } in
                $sunionWith $sunion (path2 $fOrdInt (go 1#)) karg;
              1# -> $sunionWith $sunion btree2 karg
            } } in
      case >=# i# 0# of {
        __DEFAULT ->
          let {
            b :: Int#
            b = negateInt# i# } in
          case >=# b 64# of {
            __DEFAULT -> jump $j (uncheckedIShiftRA# 1# b);
            1# -> jump $j 0#
          };
        1# ->
          case >=# i# 64# of {
            __DEFAULT -> jump $j (uncheckedIShiftL# 1# i#);
            1# -> jump $j 0#
          }
      }

-- RHS size: {terms: 6, types: 3, coercions: 0, joins: 0/0}
btree1 :: Int -> Map Int (Set Int)
btree1 = \ (w :: Int) -> case w of { I# ww1 -> $wbtree ww1 }

-- RHS size: {terms: 1, types: 0, coercions: 5, joins: 0/0}
btree :: Int -> AdjacencyMap Int
btree = btree1 `cast` <Co:5>

Rec {
-- RHS size: {terms: 56, types: 38, coercions: 0, joins: 0/0}
$w$sgo8 :: forall a1. Int# -> a1 -> Map Int a1 -> Map Int a1
$w$sgo8
  = \ (@ a1) (ww :: Int#) (w :: a1) (w1 :: Map Int a1) ->
      case w of x1 { __DEFAULT ->
      case w1 of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case ipv1 of wild1 { I# y# ->
          case <# ww y# of {
            __DEFAULT ->
              case ==# ww y# of {
                __DEFAULT -> balanceR wild1 ipv2 ipv3 ($w$sgo8 ww x1 ipv4);
                1# -> Bin ipv (I# ww) x1 ipv3 ipv4
              };
            1# -> balanceL wild1 ipv2 ($w$sgo8 ww x1 ipv3) ipv4
          }
          };
        Tip -> Bin 1# (I# ww) x1 Tip Tip
      }
      }
end Rec }

Rec {
-- RHS size: {terms: 49, types: 36, coercions: 0, joins: 0/0}
$sinsert_$sgo1
  :: forall a1. Integer -> a1 -> Map Integer a1 -> Map Integer a1
$sinsert_$sgo1
  = \ (@ a1) (kx :: Integer) (x :: a1) (ds :: Map Integer a1) ->
      case kx of kx1 { __DEFAULT ->
      case x of x1 { __DEFAULT ->
      case ds of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case compareInteger kx1 ipv1 of {
            LT -> balanceL ipv1 ipv2 ($sinsert_$sgo1 kx1 x1 ipv3) ipv4;
            EQ -> Bin ipv kx1 x1 ipv3 ipv4;
            GT -> balanceR ipv1 ipv2 ipv3 ($sinsert_$sgo1 kx1 x1 ipv4)
          };
        Tip -> Bin 1# kx1 x1 Tip Tip
      }
      }
      }
end Rec }

Rec {
-- RHS size: {terms: 31, types: 26, coercions: 0, joins: 0/0}
$wpoly_go13 :: forall a. Int# -> Map Int a -> Maybe a
$wpoly_go13
  = \ (@ a) (ww :: Int#) (w :: Map Int a) ->
      case w of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case ipv1 of { I# y# ->
          case <# ww y# of {
            __DEFAULT ->
              case ==# ww y# of {
                __DEFAULT -> $wpoly_go13 ww ipv4;
                1# -> Just ipv2
              };
            1# -> $wpoly_go13 ww ipv3
          }
          };
        Tip -> Nothing
      }
end Rec }

Rec {
-- RHS size: {terms: 79, types: 91, coercions: 2, joins: 0/3}
$w$sdfs2
  :: Part
     -> AdjacencyMap Int -> Map Int Part -> Int# -> Maybe (Map Int Part)
$w$sdfs2
  = \ (w :: Part)
      (w1 :: AdjacencyMap Int)
      (w3 :: Map Int Part)
      (ww :: Int#) ->
      case $wpoly_go13 ww (w1 `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Part
            lvl25
              = case w of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          letrec {
            go3
              :: (Map Int Part -> Maybe (Map Int Part))
                 -> Set Int -> Map Int Part -> Maybe (Map Int Part)
            go3
              = \ (z' :: Map Int Part -> Maybe (Map Int Part)) (ds :: Set Int) ->
                  case ds of {
                    Bin dt x1 l r ->
                      case x1 of { I# ww2 ->
                      go3
                        (let {
                           k :: Map Int Part -> Maybe (Map Int Part)
                           k = go3 z' r } in
                         \ (z :: Map Int Part) ->
                           case $wpoly_go13 ww2 z of {
                             Nothing ->
                               case $w$sdfs2 lvl25 w1 z ww2 of {
                                 Nothing -> Nothing;
                                 Just x2 -> k x2
                               };
                             Just q ->
                               case q of {
                                 LeftPart ->
                                   case w of {
                                     LeftPart -> Nothing;
                                     RightPart -> k z
                                   };
                                 RightPart ->
                                   case w of {
                                     LeftPart -> k z;
                                     RightPart -> Nothing
                                   }
                               }
                           })
                        l
                      };
                    Tip -> z'
                  }; } in
          go3 Just x ($w$sgo8 ww w w3)
      }
end Rec }

-- RHS size: {terms: 42, types: 42, coercions: 3, joins: 1/2}
benchmarks_$sdetectParts1 :: AdjacencyMap Int -> Bool
benchmarks_$sdetectParts1
  = \ (g :: AdjacencyMap Int) ->
      let {
        s :: Map Int (Set Int)
        s = symmetricClosure1 $fOrdInt g } in
      joinrec {
        go :: [Int] -> Map Int Part -> Bool
        go (ds :: [Int]) (eta :: Map Int Part)
          = case ds of {
              [] -> True;
              : y ys ->
                case y of { I# ww1 ->
                case $wpoly_go13 ww1 eta of {
                  Nothing ->
                    case $w$sdfs2 LeftPart (s `cast` <Co:3>) eta ww1 of {
                      Nothing -> False;
                      Just x -> jump go ys x
                    };
                  Just a1 -> jump go ys eta
                }
                }
            }; } in
      jump go (keys_go [] s) Tip

Rec {
-- RHS size: {terms: 26, types: 24, coercions: 0, joins: 0/0}
$slookup3 :: forall a. Integer -> Map Integer a -> Maybe a
$slookup3
  = \ (@ a) (ds :: Integer) (ds1 :: Map Integer a) ->
      case ds of ds2 { __DEFAULT ->
      case ds1 of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case compareInteger ds2 ipv1 of {
            LT -> $slookup3 ds2 ipv3;
            EQ -> Just ipv2;
            GT -> $slookup3 ds2 ipv4
          };
        Tip -> Nothing
      }
      }
end Rec }

Rec {
-- RHS size: {terms: 76, types: 89, coercions: 2, joins: 0/3}
benchmarks_$sdfs3
  :: Part
     -> AdjacencyMap Integer
     -> Map Integer Part
     -> Integer
     -> Maybe (Map Integer Part)
benchmarks_$sdfs3
  = \ (p :: Part)
      (g :: AdjacencyMap Integer)
      (m :: Map Integer Part)
      (v :: Integer) ->
      case $slookup3 v (g `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Part
            lvl25
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          letrec {
            go3
              :: (Map Integer Part -> Maybe (Map Integer Part))
                 -> Set Integer -> Map Integer Part -> Maybe (Map Integer Part)
            go3
              = \ (z' :: Map Integer Part -> Maybe (Map Integer Part))
                  (ds :: Set Integer) ->
                  case ds of {
                    Bin dt x1 l r ->
                      go3
                        (let {
                           k :: Map Integer Part -> Maybe (Map Integer Part)
                           k = go3 z' r } in
                         \ (z :: Map Integer Part) ->
                           case $slookup3 x1 z of {
                             Nothing ->
                               case benchmarks_$sdfs3 lvl25 g z x1 of {
                                 Nothing -> Nothing;
                                 Just x2 -> k x2
                               };
                             Just q ->
                               case q of {
                                 LeftPart ->
                                   case p of {
                                     LeftPart -> Nothing;
                                     RightPart -> k z
                                   };
                                 RightPart ->
                                   case p of {
                                     LeftPart -> k z;
                                     RightPart -> Nothing
                                   }
                               }
                           })
                        l;
                    Tip -> z'
                  }; } in
          go3 Just x ($sinsert_$sgo1 v p m)
      }
end Rec }

-- RHS size: {terms: 39, types: 40, coercions: 3, joins: 1/2}
benchmarks_$sdetectParts2 :: AdjacencyMap Integer -> Bool
benchmarks_$sdetectParts2
  = \ (g :: AdjacencyMap Integer) ->
      let {
        s :: Map Integer (Set Integer)
        s = symmetricClosure1 $fOrdInteger g } in
      joinrec {
        go :: [Integer] -> Map Integer Part -> Bool
        go (ds :: [Integer]) (eta :: Map Integer Part)
          = case ds of {
              [] -> True;
              : y ys ->
                case $slookup3 y eta of {
                  Nothing ->
                    case benchmarks_$sdfs3 LeftPart (s `cast` <Co:3>) eta y of {
                      Nothing -> False;
                      Just x -> jump go ys x
                    };
                  Just a1 -> jump go ys eta
                }
            }; } in
      jump go (keys_go [] s) Tip

-- RHS size: {terms: 32, types: 23, coercions: 0, joins: 0/0}
lvl7
  :: Either Integer Integer
     -> Either Integer Integer -> Either Integer Integer
lvl7
  = \ (x :: Either Integer Integer) (y :: Either Integer Integer) ->
      case y of wild {
        Left a1 ->
          case x of wild1 {
            Left b1 ->
              case ltInteger# a1 b1 of {
                __DEFAULT -> wild1;
                1# -> wild
              };
            Right ipv -> wild
          };
        Right a1 ->
          case x of wild1 {
            Left ipv -> wild1;
            Right b1 ->
              case ltInteger# a1 b1 of {
                __DEFAULT -> wild1;
                1# -> wild
              }
          }
      }

-- RHS size: {terms: 32, types: 23, coercions: 0, joins: 0/0}
lvl8
  :: Either Integer Integer
     -> Either Integer Integer -> Either Integer Integer
lvl8
  = \ (x :: Either Integer Integer) (y :: Either Integer Integer) ->
      case y of wild {
        Left a1 ->
          case x of wild1 {
            Left b1 ->
              case ltInteger# a1 b1 of {
                __DEFAULT -> wild;
                1# -> wild1
              };
            Right ipv -> wild1
          };
        Right a1 ->
          case x of wild1 {
            Left ipv -> wild;
            Right b1 ->
              case ltInteger# a1 b1 of {
                __DEFAULT -> wild;
                1# -> wild1
              }
          }
      }

-- RHS size: {terms: 32, types: 23, coercions: 0, joins: 0/0}
lvl9 :: Either Integer Integer -> Either Integer Integer -> Bool
lvl9
  = \ (a1 :: Either Integer Integer)
      (b1 :: Either Integer Integer) ->
      case a1 of {
        Left a2 ->
          case b1 of {
            Left b2 ->
              case ltInteger# a2 b2 of {
                __DEFAULT -> True;
                1# -> False
              };
            Right ipv -> False
          };
        Right a2 ->
          case b1 of {
            Left ipv -> True;
            Right b2 ->
              case ltInteger# a2 b2 of {
                __DEFAULT -> True;
                1# -> False
              }
          }
      }

-- RHS size: {terms: 22, types: 21, coercions: 0, joins: 0/0}
lvl10 :: Either Integer Integer -> Either Integer Integer -> Bool
lvl10
  = \ (a1 :: Either Integer Integer)
      (b1 :: Either Integer Integer) ->
      case b1 of {
        Left a2 ->
          case a1 of {
            Left b2 -> ltInteger a2 b2;
            Right ipv -> True
          };
        Right a2 ->
          case a1 of {
            Left ipv -> False;
            Right b2 -> ltInteger a2 b2
          }
      }

-- RHS size: {terms: 32, types: 23, coercions: 0, joins: 0/0}
lvl11 :: Either Integer Integer -> Either Integer Integer -> Bool
lvl11
  = \ (a1 :: Either Integer Integer)
      (b1 :: Either Integer Integer) ->
      case b1 of {
        Left a2 ->
          case a1 of {
            Left b2 ->
              case ltInteger# a2 b2 of {
                __DEFAULT -> True;
                1# -> False
              };
            Right ipv -> False
          };
        Right a2 ->
          case a1 of {
            Left ipv -> True;
            Right b2 ->
              case ltInteger# a2 b2 of {
                __DEFAULT -> True;
                1# -> False
              }
          }
      }

-- RHS size: {terms: 22, types: 21, coercions: 0, joins: 0/0}
lvl12 :: Either Integer Integer -> Either Integer Integer -> Bool
lvl12
  = \ (a1 :: Either Integer Integer)
      (b1 :: Either Integer Integer) ->
      case a1 of {
        Left a2 ->
          case b1 of {
            Left b2 -> ltInteger a2 b2;
            Right ipv -> True
          };
        Right a2 ->
          case b1 of {
            Left ipv -> False;
            Right b2 -> ltInteger a2 b2
          }
      }

-- RHS size: {terms: 22, types: 21, coercions: 0, joins: 0/0}
lvl13
  :: Either Integer Integer -> Either Integer Integer -> Ordering
lvl13
  = \ (a1 :: Either Integer Integer)
      (b1 :: Either Integer Integer) ->
      case a1 of {
        Left a2 ->
          case b1 of {
            Left b2 -> compareInteger a2 b2;
            Right ipv -> LT
          };
        Right a2 ->
          case b1 of {
            Left ipv -> GT;
            Right b2 -> compareInteger a2 b2
          }
      }

-- RHS size: {terms: 32, types: 23, coercions: 0, joins: 0/0}
$s$fEqEither_$s$fEqEither_$c/=1
  :: Either Integer Integer -> Either Integer Integer -> Bool
$s$fEqEither_$s$fEqEither_$c/=1
  = \ (x :: Either Integer Integer) (y :: Either Integer Integer) ->
      case x of {
        Left a1 ->
          case y of {
            Left b1 ->
              case eqInteger# a1 b1 of {
                __DEFAULT -> True;
                1# -> False
              };
            Right ipv -> True
          };
        Right a1 ->
          case y of {
            Left ipv -> True;
            Right b1 ->
              case eqInteger# a1 b1 of {
                __DEFAULT -> True;
                1# -> False
              }
          }
      }

-- RHS size: {terms: 22, types: 21, coercions: 0, joins: 0/0}
lvl15 :: Either Integer Integer -> Either Integer Integer -> Bool
lvl15
  = \ (ds :: Either Integer Integer)
      (ds1 :: Either Integer Integer) ->
      case ds of {
        Left a1 ->
          case ds1 of {
            Left b1 -> eqInteger a1 b1;
            Right ipv -> False
          };
        Right a1 ->
          case ds1 of {
            Left ipv -> False;
            Right b1 -> eqInteger a1 b1
          }
      }

-- RHS size: {terms: 3, types: 3, coercions: 0, joins: 0/0}
$s$fEqEither1 :: Eq (Either Integer Integer)
$s$fEqEither1 = C:Eq lvl15 $s$fEqEither_$s$fEqEither_$c/=1

-- RHS size: {terms: 9, types: 3, coercions: 0, joins: 0/0}
$s$fOrdEither1 :: Ord (Either Integer Integer)
$s$fOrdEither1
  = C:Ord $s$fEqEither1 lvl13 lvl12 lvl11 lvl10 lvl9 lvl8 lvl7

Rec {
-- RHS size: {terms: 59, types: 59, coercions: 0, joins: 0/0}
$slookup5
  :: forall a.
     Either Integer Integer -> Map (Either Integer Integer) a -> Maybe a
$slookup5
  = \ (@ a)
      (ds :: Either Integer Integer)
      (ds1 :: Map (Either Integer Integer) a) ->
      case ds of ds2 { __DEFAULT ->
      case ds1 of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case ds2 of wild1 {
            Left a2 ->
              case ipv1 of {
                Left b2 ->
                  case compareInteger a2 b2 of {
                    LT -> $slookup5 wild1 ipv3;
                    EQ -> Just ipv2;
                    GT -> $slookup5 wild1 ipv4
                  };
                Right ipv7 -> $slookup5 wild1 ipv3
              };
            Right a2 ->
              case ipv1 of {
                Left ipv7 -> $slookup5 wild1 ipv4;
                Right b2 ->
                  case compareInteger a2 b2 of {
                    LT -> $slookup5 wild1 ipv3;
                    EQ -> Just ipv2;
                    GT -> $slookup5 wild1 ipv4
                  }
              }
          };
        Tip -> Nothing
      }
      }
end Rec }

Rec {
-- RHS size: {terms: 106, types: 102, coercions: 0, joins: 0/0}
$sinsert_$sgo2
  :: forall a1.
     Either Integer Integer
     -> a1
     -> Map (Either Integer Integer) a1
     -> Map (Either Integer Integer) a1
$sinsert_$sgo2
  = \ (@ a1)
      (kx :: Either Integer Integer)
      (x :: a1)
      (ds :: Map (Either Integer Integer) a1) ->
      case kx of kx1 { __DEFAULT ->
      case x of x1 { __DEFAULT ->
      case ds of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case kx1 of wild1 {
            Left a2 ->
              case ipv1 of wild2 {
                Left b2 ->
                  case compareInteger a2 b2 of {
                    LT -> balanceL wild2 ipv2 ($sinsert_$sgo2 wild1 x1 ipv3) ipv4;
                    EQ -> Bin ipv wild1 x1 ipv3 ipv4;
                    GT -> balanceR wild2 ipv2 ipv3 ($sinsert_$sgo2 wild1 x1 ipv4)
                  };
                Right ipv7 ->
                  balanceL wild2 ipv2 ($sinsert_$sgo2 wild1 x1 ipv3) ipv4
              };
            Right a2 ->
              case ipv1 of wild2 {
                Left ipv7 ->
                  balanceR wild2 ipv2 ipv3 ($sinsert_$sgo2 wild1 x1 ipv4);
                Right b2 ->
                  case compareInteger a2 b2 of {
                    LT -> balanceL wild2 ipv2 ($sinsert_$sgo2 wild1 x1 ipv3) ipv4;
                    EQ -> Bin ipv wild1 x1 ipv3 ipv4;
                    GT -> balanceR wild2 ipv2 ipv3 ($sinsert_$sgo2 wild1 x1 ipv4)
                  }
              }
          };
        Tip -> Bin 1# kx1 x1 Tip Tip
      }
      }
      }
end Rec }

Rec {
-- RHS size: {terms: 76, types: 143, coercions: 4, joins: 0/3}
benchmarks_$sdfs1
  :: Part
     -> AdjacencyMap (Either Integer Integer)
     -> Map (Either Integer Integer) Part
     -> Either Integer Integer
     -> Maybe (Map (Either Integer Integer) Part)
benchmarks_$sdfs1
  = \ (p :: Part)
      (g :: AdjacencyMap (Either Integer Integer))
      (m :: Map (Either Integer Integer) Part)
      (v :: Either Integer Integer) ->
      case $slookup5 v (g `cast` <Co:4>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Part
            lvl25
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          letrec {
            go3
              :: (Map (Either Integer Integer) Part
                  -> Maybe (Map (Either Integer Integer) Part))
                 -> Set (Either Integer Integer)
                 -> Map (Either Integer Integer) Part
                 -> Maybe (Map (Either Integer Integer) Part)
            go3
              = \ (z'
                     :: Map (Either Integer Integer) Part
                        -> Maybe (Map (Either Integer Integer) Part))
                  (ds :: Set (Either Integer Integer)) ->
                  case ds of {
                    Bin dt x1 l r ->
                      go3
                        (let {
                           k :: Map (Either Integer Integer) Part
                                -> Maybe (Map (Either Integer Integer) Part)
                           k = go3 z' r } in
                         \ (z :: Map (Either Integer Integer) Part) ->
                           case $slookup5 x1 z of {
                             Nothing ->
                               case benchmarks_$sdfs1 lvl25 g z x1 of {
                                 Nothing -> Nothing;
                                 Just x2 -> k x2
                               };
                             Just q ->
                               case q of {
                                 LeftPart ->
                                   case p of {
                                     LeftPart -> Nothing;
                                     RightPart -> k z
                                   };
                                 RightPart ->
                                   case p of {
                                     LeftPart -> k z;
                                     RightPart -> Nothing
                                   }
                               }
                           })
                        l;
                    Tip -> z'
                  }; } in
          go3 Just x ($sinsert_$sgo2 v p m)
      }
end Rec }

-- RHS size: {terms: 39, types: 74, coercions: 5, joins: 1/2}
benchmarks_$sdetectParts
  :: AdjacencyMap (Either Integer Integer) -> Bool
benchmarks_$sdetectParts
  = \ (g :: AdjacencyMap (Either Integer Integer)) ->
      let {
        s :: Map (Either Integer Integer) (Set (Either Integer Integer))
        s = symmetricClosure1 $s$fOrdEither1 g } in
      joinrec {
        go
          :: [Either Integer Integer]
             -> Map (Either Integer Integer) Part -> Bool
        go (ds :: [Either Integer Integer])
           (eta :: Map (Either Integer Integer) Part)
          = case ds of {
              [] -> True;
              : y ys ->
                case $slookup5 y eta of {
                  Nothing ->
                    case benchmarks_$sdfs1 LeftPart (s `cast` <Co:5>) eta y of {
                      Nothing -> False;
                      Just x -> jump go ys x
                    };
                  Just a1 -> jump go ys eta
                }
            }; } in
      jump go (keys_go [] s) Tip

Rec {
-- RHS size: {terms: 57, types: 56, coercions: 0, joins: 0/0}
$smember5
  :: forall a.
     Either Integer Integer -> Map (Either Integer Integer) a -> Bool
$smember5
  = \ (@ a)
      (ds :: Either Integer Integer)
      (ds1 :: Map (Either Integer Integer) a) ->
      case ds of ds2 { __DEFAULT ->
      case ds1 of {
        Bin ipv ipv1 ipv2 ipv3 ipv4 ->
          case ds2 of wild1 {
            Left a2 ->
              case ipv1 of {
                Left b2 ->
                  case compareInteger a2 b2 of {
                    LT -> $smember5 wild1 ipv3;
                    EQ -> True;
                    GT -> $smember5 wild1 ipv4
                  };
                Right ipv7 -> $smember5 wild1 ipv3
              };
            Right a2 ->
              case ipv1 of {
                Left ipv7 -> $smember5 wild1 ipv4;
                Right b2 ->
                  case compareInteger a2 b2 of {
                    LT -> $smember5 wild1 ipv3;
                    EQ -> True;
                    GT -> $smember5 wild1 ipv4
                  }
              }
          };
        Tip -> False
      }
      }
end Rec }

-- RHS size: {terms: 18, types: 5, coercions: 0, joins: 0/0}
$fEqPart_$c== :: Part -> Part -> Bool
$fEqPart_$c==
  = \ (ds :: Part) (ds1 :: Part) ->
      case ds of {
        LeftPart ->
          case ds1 of {
            LeftPart -> True;
            RightPart -> False
          };
        RightPart ->
          case ds1 of {
            LeftPart -> False;
            RightPart -> True
          }
      }

-- RHS size: {terms: 18, types: 5, coercions: 0, joins: 0/0}
$fEqPart_$c/= :: Part -> Part -> Bool
$fEqPart_$c/=
  = \ (eta :: Part) (eta1 :: Part) ->
      case eta of {
        LeftPart ->
          case eta1 of {
            LeftPart -> False;
            RightPart -> True
          };
        RightPart ->
          case eta1 of {
            LeftPart -> True;
            RightPart -> False
          }
      }

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$fEqPart :: Eq Part
$fEqPart = C:Eq $fEqPart_$c== $fEqPart_$c/=

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$fShowPart5 :: Addr#
$fShowPart5 = "LeftPart"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$fShowPart4 :: [Char]
$fShowPart4 = unpackCString# $fShowPart5

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$fShowPart3 :: Addr#
$fShowPart3 = "RightPart"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$fShowPart2 :: [Char]
$fShowPart2 = unpackCString# $fShowPart3

-- RHS size: {terms: 13, types: 6, coercions: 0, joins: 0/0}
$fShowPart_$cshowsPrec :: Int -> Part -> ShowS
$fShowPart_$cshowsPrec
  = \ _ (ds1 :: Part) (eta :: String) ->
      case ds1 of {
        LeftPart -> ++ $fShowPart4 eta;
        RightPart -> ++ $fShowPart2 eta
      }

-- RHS size: {terms: 12, types: 5, coercions: 0, joins: 0/0}
$fShowPart1 :: Part -> ShowS
$fShowPart1
  = \ (ds :: Part) (eta :: String) ->
      case ds of {
        LeftPart -> ++ $fShowPart4 eta;
        RightPart -> ++ $fShowPart2 eta
      }

-- RHS size: {terms: 6, types: 4, coercions: 0, joins: 0/0}
$fShowPart_$cshowList :: [Part] -> ShowS
$fShowPart_$cshowList
  = \ (ls :: [Part]) (s :: String) -> showList__ $fShowPart1 ls s

-- RHS size: {terms: 7, types: 2, coercions: 0, joins: 0/0}
$fShowPart_$cshow :: Part -> String
$fShowPart_$cshow
  = \ (x :: Part) ->
      case x of {
        LeftPart -> $fShowPart4;
        RightPart -> $fShowPart2
      }

-- RHS size: {terms: 4, types: 1, coercions: 0, joins: 0/0}
$fShowPart :: Show Part
$fShowPart
  = C:Show
      $fShowPart_$cshowsPrec $fShowPart_$cshow $fShowPart_$cshowList

-- RHS size: {terms: 7, types: 2, coercions: 0, joins: 0/0}
otherPart :: Part -> Part
otherPart
  = \ (ds :: Part) ->
      case ds of {
        LeftPart -> RightPart;
        RightPart -> LeftPart
      }

Rec {
-- RHS size: {terms: 82, types: 97, coercions: 2, joins: 0/3}
detectParts_dfs
  :: forall a.
     Ord a =>
     Part -> AdjacencyMap a -> Map a Part -> a -> Maybe (Map a Part)
detectParts_dfs
  = \ (@ a)
      ($dOrd :: Ord a)
      (p :: Part)
      (g :: AdjacencyMap a)
      (m :: Map a Part)
      (v :: a) ->
      case lookup $dOrd v (g `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Part
            lvl25
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          letrec {
            go3
              :: (Map a Part -> Maybe (Map a Part))
                 -> Set a -> Map a Part -> Maybe (Map a Part)
            go3
              = \ (z' :: Map a Part -> Maybe (Map a Part)) (ds :: Set a) ->
                  case ds of {
                    Bin dt x1 l r ->
                      go3
                        (let {
                           k :: Map a Part -> Maybe (Map a Part)
                           k = go3 z' r } in
                         \ (z :: Map a Part) ->
                           case lookup $dOrd x1 z of {
                             Nothing ->
                               case detectParts_dfs $dOrd lvl25 g z x1 of {
                                 Nothing -> Nothing;
                                 Just x2 -> k x2
                               };
                             Just q ->
                               case q of {
                                 LeftPart ->
                                   case p of {
                                     LeftPart -> Nothing;
                                     RightPart -> k z
                                   };
                                 RightPart ->
                                   case p of {
                                     LeftPart -> k z;
                                     RightPart -> Nothing
                                   }
                               }
                           })
                        l;
                    Tip -> z'
                  }; } in
          go3 Just x (insert $dOrd v p m)
      }
end Rec }

-- RHS size: {terms: 43, types: 46, coercions: 3, joins: 1/2}
detectParts :: forall a. Ord a => AdjacencyMap a -> Bool
detectParts
  = \ (@ a) ($dOrd :: Ord a) (g :: AdjacencyMap a) ->
      let {
        s :: Map a (Set a)
        s = symmetricClosure1 $dOrd g } in
      joinrec {
        go :: [a] -> Map a Part -> Bool
        go (ds :: [a]) (eta :: Map a Part)
          = case ds of {
              [] -> True;
              : y ys ->
                case lookup $dOrd y eta of {
                  Nothing ->
                    case detectParts_dfs $dOrd LeftPart (s `cast` <Co:3>) eta y of {
                      Nothing -> False;
                      Just x -> jump go ys x
                    };
                  Just a1 -> jump go ys eta
                }
            }; } in
      jump go (keys_go [] s) Tip

Rec {
-- RHS size: {terms: 49, types: 55, coercions: 0, joins: 0/0}
$w$scropHeads
  :: Integer
     -> [Integer] -> NonEmpty Integer -> (# Integer, [Integer] #)
$w$scropHeads
  = \ (ww :: Integer) (ww1 :: [Integer]) (w :: NonEmpty Integer) ->
      case ww1 of wild {
        [] ->
          (# case w of { :| a1 as -> a1 }, case w of { :| a1 as -> as } #);
        : ipv ipv1 ->
          case w of { :| ds ds1 ->
          case ds1 of wild2 {
            [] -> (# ww, wild #);
            : ipv2 ipv3 ->
              case eqInteger# ipv ipv2 of {
                __DEFAULT ->
                  case ++_$s++ (reverse1 wild2 []) ww wild of { : ww3 ww4 ->
                  (# ww3, ww4 #)
                  };
                1# -> $w$scropHeads ipv ipv1 (:| ipv2 ipv3)
              }
          }
          }
      }
end Rec }

-- RHS size: {terms: 4, types: 17, coercions: 0, joins: 0/0}
lvl16
  :: Map Integer (Part, NonEmpty Integer)
     -> (Maybe [Integer], Map Integer (Part, NonEmpty Integer))
lvl16
  = \ (eta1 :: Map Integer (Part, NonEmpty Integer)) ->
      (Nothing, eta1)

Rec {
-- RHS size: {terms: 170, types: 231, coercions: 178, joins: 0/8}
benchmarks11
  :: AdjacencyMap Integer
     -> Part
     -> NonEmpty Integer
     -> Integer
     -> PartMap Integer
     -> Identity (Maybe [Integer], PartMap Integer)
benchmarks11
  = \ (g :: AdjacencyMap Integer)
      (p :: Part)
      (l :: NonEmpty Integer)
      (v :: Integer)
      (eta :: PartMap Integer) ->
      case $slookup3 v (g `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Integer
            lvl25 = case l of { :| b bs -> b } } in
          let {
            lvl26 :: [Integer]
            lvl26 = case l of { :| b bs -> bs } } in
          let {
            lvl27 :: [Integer]
            lvl27 = : lvl25 lvl26 } in
          let {
            lvl28 :: NonEmpty Integer
            lvl28
              = case reverse1 lvl27 [] of {
                  [] -> case cycle1 of wild2 { };
                  : a1 as -> :| a1 as
                } } in
          let {
            p1 :: Part
            p1
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          letrec {
            go3
              :: MaybeT (State (PartMap Integer)) [Integer]
                 -> Set Integer -> MaybeT (State (PartMap Integer)) [Integer]
            go3
              = \ (z' :: MaybeT (State (PartMap Integer)) [Integer])
                  (ds :: Set Integer) ->
                  case ds of {
                    Bin dt x1 l1 r ->
                      let {
                        lvl29 :: NonEmpty Integer
                        lvl29 = :| x1 lvl27 } in
                      go3
                        (let {
                           ys :: MaybeT (State (PartMap Integer)) [Integer]
                           ys = go3 z' r } in
                         (\ (eta1 :: PartMap Integer) ->
                            case $slookup3 x1 eta1 of {
                              Nothing ->
                                case (benchmarks11 g p1 lvl29 x1 eta1) `cast` <Co:11> of wild3
                                { (a1, s') ->
                                case a1 of {
                                  Nothing -> (ys `cast` <Co:19>) s';
                                  Just ds1 -> wild3 `cast` <Co:12>
                                }
                                };
                              Just pv ->
                                case pv of { (p2, lu) ->
                                case p2 of {
                                  LeftPart ->
                                    case p1 of {
                                      LeftPart -> (ys `cast` <Co:19>) eta1;
                                      RightPart ->
                                        (Just
                                           (case reverse1
                                                   (: (case lu of { :| a1 as -> a1 })
                                                      (case lu of { :| a1 as -> as }))
                                                   []
                                            of {
                                              [] -> case cycle1 of wild7 { };
                                              : a1 as ->
                                                case $w$scropHeads a1 as lvl28 of
                                                { (# ww1, ww2 #) ->
                                                : ww1 ww2
                                                }
                                            }),
                                         eta1)
                                        `cast` <Co:12>
                                    };
                                  RightPart ->
                                    case p1 of {
                                      LeftPart ->
                                        (Just
                                           (case reverse1
                                                   (: (case lu of { :| a1 as -> a1 })
                                                      (case lu of { :| a1 as -> as }))
                                                   []
                                            of {
                                              [] -> case cycle1 of wild7 { };
                                              : a1 as ->
                                                case $w$scropHeads a1 as lvl28 of
                                                { (# ww1, ww2 #) ->
                                                : ww1 ww2
                                                }
                                            }),
                                         eta1)
                                        `cast` <Co:12>;
                                      RightPart -> (ys `cast` <Co:19>) eta1
                                    }
                                }
                                }
                            })
                         `cast` <Co:17>)
                        l1;
                    Tip -> z'
                  }; } in
          ((go3 (lvl16 `cast` <Co:41>) x) `cast` <Co:14>)
            ($sinsert_$sgo1 v (p, l) eta)
      }
end Rec }

-- RHS size: {terms: 50, types: 81, coercions: 14, joins: 1/2}
$w$spartMonad2
  :: AdjacencyMap Integer
     -> PartMap Integer -> (# Maybe [Integer], PartMap Integer #)
$w$spartMonad2
  = \ (w :: AdjacencyMap Integer) (w1 :: PartMap Integer) ->
      let {
        s :: Map Integer (Set Integer)
        s = symmetricClosure1 $fOrdInteger w } in
      joinrec {
        go
          :: [Integer]
             -> PartMap Integer -> (# Maybe [Integer], PartMap Integer #)
        go (ds :: [Integer]) (eta :: PartMap Integer)
          = case ds of {
              [] -> (# Nothing, eta #);
              : y ys ->
                case $smember3 y eta of {
                  False ->
                    case (benchmarks11 (s `cast` <Co:3>) LeftPart (:| y []) y eta)
                         `cast` <Co:11>
                    of
                    { (a1, s') ->
                    case a1 of wild3 {
                      Nothing -> jump go ys s';
                      Just ds1 -> (# wild3, s' #)
                    }
                    };
                  True -> jump go ys eta
                }
            }; } in
      jump go (keys_go [] s) w1

-- RHS size: {terms: 10, types: 22, coercions: 8, joins: 0/0}
benchmarks10
  :: AdjacencyMap Integer
     -> PartMap Integer -> Identity (Maybe [Integer], PartMap Integer)
benchmarks10
  = \ (w :: AdjacencyMap Integer) (w1 :: PartMap Integer) ->
      case $w$spartMonad2 w w1 of { (# ww1, ww2 #) ->
      (ww1, ww2) `cast` <Co:8>
      }

-- RHS size: {terms: 12, types: 25, coercions: 0, joins: 0/0}
benchmarks_$sdetectParts'2 :: AdjacencyMap Integer -> Bool
benchmarks_$sdetectParts'2
  = \ (x :: AdjacencyMap Integer) ->
      case $w$spartMonad2 x Tip of { (# ww1, ww2 #) ->
      case ww1 of {
        Nothing -> True;
        Just c -> False
      }
      }

Rec {
-- RHS size: {terms: 55, types: 59, coercions: 0, joins: 0/0}
$w$scropHeads1 :: Int -> [Int] -> NonEmpty Int -> (# Int, [Int] #)
$w$scropHeads1
  = \ (ww :: Int) (ww1 :: [Int]) (w :: NonEmpty Int) ->
      case ww1 of wild {
        [] ->
          (# case w of { :| a1 as -> a1 }, case w of { :| a1 as -> as } #);
        : ipv ipv1 ->
          case w of { :| ds ds1 ->
          case ds1 of wild2 {
            [] -> (# ww, wild #);
            : ipv2 ipv3 ->
              case ipv of wild3 { I# x ->
              case ipv2 of wild4 { I# y ->
              case ==# x y of {
                __DEFAULT ->
                  case ++_$s++ (reverse1 wild2 []) ww wild of { : ww3 ww4 ->
                  (# ww3, ww4 #)
                  };
                1# -> $w$scropHeads1 wild3 ipv1 (:| wild4 ipv3)
              }
              }
              }
          }
          }
      }
end Rec }

-- RHS size: {terms: 4, types: 17, coercions: 0, joins: 0/0}
lvl17
  :: Map Int (Part, NonEmpty Int)
     -> (Maybe [Int], Map Int (Part, NonEmpty Int))
lvl17 = \ (eta1 :: Map Int (Part, NonEmpty Int)) -> (Nothing, eta1)

Rec {
-- RHS size: {terms: 171, types: 231, coercions: 178, joins: 0/7}
$w$sdfs1
  :: AdjacencyMap Int
     -> Part
     -> NonEmpty Int
     -> Int#
     -> PartMap Int
     -> Identity (Maybe [Int], PartMap Int)
$w$sdfs1
  = \ (w :: AdjacencyMap Int)
      (w1 :: Part)
      (w3 :: NonEmpty Int)
      (ww :: Int#)
      (w4 :: PartMap Int) ->
      case $wpoly_go13 ww (w `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Int
            lvl25 = case w3 of { :| b bs -> b } } in
          let {
            lvl26 :: [Int]
            lvl26 = case w3 of { :| b bs -> bs } } in
          let {
            lvl27 :: [Int]
            lvl27 = : lvl25 lvl26 } in
          let {
            lvl28 :: NonEmpty Int
            lvl28
              = case reverse1 lvl27 [] of {
                  [] -> case cycle1 of wild2 { };
                  : a1 as -> :| a1 as
                } } in
          let {
            p :: Part
            p = case w1 of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          letrec {
            go3
              :: MaybeT (State (PartMap Int)) [Int]
                 -> Set Int -> MaybeT (State (PartMap Int)) [Int]
            go3
              = \ (z' :: MaybeT (State (PartMap Int)) [Int]) (ds :: Set Int) ->
                  case ds of {
                    Bin dt x1 l r ->
                      case x1 of ww1 { I# ww2 ->
                      go3
                        (let {
                           ys :: MaybeT (State (PartMap Int)) [Int]
                           ys = go3 z' r } in
                         (\ (eta :: PartMap Int) ->
                            case $wpoly_go13 ww2 eta of {
                              Nothing ->
                                case ($w$sdfs1 w p (:| ww1 lvl27) ww2 eta) `cast` <Co:11> of wild3
                                { (a1, s') ->
                                case a1 of {
                                  Nothing -> (ys `cast` <Co:19>) s';
                                  Just ds1 -> wild3 `cast` <Co:12>
                                }
                                };
                              Just pv ->
                                case pv of { (p1, lu) ->
                                case p1 of {
                                  LeftPart ->
                                    case p of {
                                      LeftPart -> (ys `cast` <Co:19>) eta;
                                      RightPart ->
                                        (Just
                                           (case reverse1
                                                   (: (case lu of { :| a1 as -> a1 })
                                                      (case lu of { :| a1 as -> as }))
                                                   []
                                            of {
                                              [] -> case cycle1 of wild7 { };
                                              : a1 as ->
                                                case $w$scropHeads1 a1 as lvl28 of
                                                { (# ww4, ww5 #) ->
                                                : ww4 ww5
                                                }
                                            }),
                                         eta)
                                        `cast` <Co:12>
                                    };
                                  RightPart ->
                                    case p of {
                                      LeftPart ->
                                        (Just
                                           (case reverse1
                                                   (: (case lu of { :| a1 as -> a1 })
                                                      (case lu of { :| a1 as -> as }))
                                                   []
                                            of {
                                              [] -> case cycle1 of wild7 { };
                                              : a1 as ->
                                                case $w$scropHeads1 a1 as lvl28 of
                                                { (# ww4, ww5 #) ->
                                                : ww4 ww5
                                                }
                                            }),
                                         eta)
                                        `cast` <Co:12>;
                                      RightPart -> (ys `cast` <Co:19>) eta
                                    }
                                }
                                }
                            })
                         `cast` <Co:17>)
                        l
                      };
                    Tip -> z'
                  }; } in
          ((go3 (lvl17 `cast` <Co:41>) x) `cast` <Co:14>)
            ($w$sgo8 ww (w1, w3) w4)
      }
end Rec }

-- RHS size: {terms: 53, types: 83, coercions: 14, joins: 1/2}
$w$spartMonad1
  :: AdjacencyMap Int
     -> PartMap Int -> (# Maybe [Int], PartMap Int #)
$w$spartMonad1
  = \ (w :: AdjacencyMap Int) (w1 :: PartMap Int) ->
      let {
        s :: Map Int (Set Int)
        s = symmetricClosure1 $fOrdInt w } in
      joinrec {
        go :: [Int] -> PartMap Int -> (# Maybe [Int], PartMap Int #)
        go (ds :: [Int]) (eta :: PartMap Int)
          = case ds of {
              [] -> (# Nothing, eta #);
              : y ys ->
                case y of ww { I# ww1 ->
                case $wpoly_go1 ww1 eta of {
                  False ->
                    case ($w$sdfs1 (s `cast` <Co:3>) LeftPart (:| ww []) ww1 eta)
                         `cast` <Co:11>
                    of
                    { (a1, s') ->
                    case a1 of wild3 {
                      Nothing -> jump go ys s';
                      Just ds1 -> (# wild3, s' #)
                    }
                    };
                  True -> jump go ys eta
                }
                }
            }; } in
      jump go (keys_go [] s) w1

-- RHS size: {terms: 10, types: 22, coercions: 8, joins: 0/0}
benchmarks9
  :: AdjacencyMap Int
     -> PartMap Int -> Identity (Maybe [Int], PartMap Int)
benchmarks9
  = \ (w :: AdjacencyMap Int) (w1 :: PartMap Int) ->
      case $w$spartMonad1 w w1 of { (# ww1, ww2 #) ->
      (ww1, ww2) `cast` <Co:8>
      }

-- RHS size: {terms: 12, types: 25, coercions: 0, joins: 0/0}
benchmarks_$sdetectParts'1 :: AdjacencyMap Int -> Bool
benchmarks_$sdetectParts'1
  = \ (x :: AdjacencyMap Int) ->
      case $w$spartMonad1 x Tip of { (# ww1, ww2 #) ->
      case ww1 of {
        Nothing -> True;
        Just c -> False
      }
      }

Rec {
-- RHS size: {terms: 66, types: 86, coercions: 0, joins: 1/1}
$w$scropHeads2
  :: Either Integer Integer
     -> [Either Integer Integer]
     -> NonEmpty (Either Integer Integer)
     -> [Either Integer Integer]
$w$scropHeads2
  = \ (ww :: Either Integer Integer)
      (ww1 :: [Either Integer Integer])
      (w :: NonEmpty (Either Integer Integer)) ->
      case ww1 of wild {
        [] -> toList w;
        : ipv ipv1 ->
          case w of { :| ds ds1 ->
          case ds1 of wild2 {
            [] -> : ww wild;
            : ipv2 ipv3 ->
              join {
                $j :: [Either Integer Integer]
                $j = ++_$s++ (reverse1 wild2 []) ww wild } in
              case ipv of wild3 {
                Left a1 ->
                  case ipv2 of wild4 {
                    Left b1 ->
                      case eqInteger# a1 b1 of {
                        __DEFAULT -> jump $j;
                        1# -> $w$scropHeads2 wild3 ipv1 (:| wild4 ipv3)
                      };
                    Right ipv4 -> jump $j
                  };
                Right a1 ->
                  case ipv2 of wild4 {
                    Left ipv4 -> jump $j;
                    Right b1 ->
                      case eqInteger# a1 b1 of {
                        __DEFAULT -> jump $j;
                        1# -> $w$scropHeads2 wild3 ipv1 (:| wild4 ipv3)
                      }
                  }
              }
          }
          }
      }
end Rec }

-- RHS size: {terms: 4, types: 29, coercions: 0, joins: 0/0}
lvl18
  :: Map
       (Either Integer Integer) (Part, NonEmpty (Either Integer Integer))
     -> (Maybe [Either Integer Integer],
         Map
           (Either Integer Integer) (Part, NonEmpty (Either Integer Integer)))
lvl18
  = \ (eta1
         :: Map
              (Either Integer Integer)
              (Part, NonEmpty (Either Integer Integer))) ->
      (Nothing, eta1)

Rec {
-- RHS size: {terms: 160, types: 387, coercions: 270, joins: 0/8}
benchmarks4
  :: AdjacencyMap (Either Integer Integer)
     -> Part
     -> NonEmpty (Either Integer Integer)
     -> Either Integer Integer
     -> PartMap (Either Integer Integer)
     -> Identity
          (Maybe [Either Integer Integer], PartMap (Either Integer Integer))
benchmarks4
  = \ (g :: AdjacencyMap (Either Integer Integer))
      (p :: Part)
      (l :: NonEmpty (Either Integer Integer))
      (v :: Either Integer Integer)
      (eta :: PartMap (Either Integer Integer)) ->
      case $slookup5 v (g `cast` <Co:4>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Either Integer Integer
            lvl25 = case l of { :| b bs -> b } } in
          let {
            lvl26 :: [Either Integer Integer]
            lvl26 = case l of { :| b bs -> bs } } in
          let {
            lvl27 :: [Either Integer Integer]
            lvl27 = : lvl25 lvl26 } in
          let {
            lvl28 :: NonEmpty (Either Integer Integer)
            lvl28
              = case reverse1 lvl27 [] of {
                  [] -> case cycle1 of wild2 { };
                  : a1 as -> :| a1 as
                } } in
          let {
            p1 :: Part
            p1
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          letrec {
            go3
              :: MaybeT
                   (State (PartMap (Either Integer Integer))) [Either Integer Integer]
                 -> Set (Either Integer Integer)
                 -> MaybeT
                      (State (PartMap (Either Integer Integer))) [Either Integer Integer]
            go3
              = \ (z'
                     :: MaybeT
                          (State (PartMap (Either Integer Integer)))
                          [Either Integer Integer])
                  (ds :: Set (Either Integer Integer)) ->
                  case ds of {
                    Bin dt x1 l1 r ->
                      let {
                        lvl29 :: NonEmpty (Either Integer Integer)
                        lvl29 = :| x1 lvl27 } in
                      go3
                        (let {
                           ys
                             :: MaybeT
                                  (State (PartMap (Either Integer Integer)))
                                  [Either Integer Integer]
                           ys = go3 z' r } in
                         (\ (eta1 :: PartMap (Either Integer Integer)) ->
                            case $slookup5 x1 eta1 of {
                              Nothing ->
                                case (benchmarks4 g p1 lvl29 x1 eta1) `cast` <Co:17> of wild3
                                { (a1, s') ->
                                case a1 of {
                                  Nothing -> (ys `cast` <Co:29>) s';
                                  Just ds1 -> wild3 `cast` <Co:18>
                                }
                                };
                              Just pv ->
                                case pv of { (p2, lu) ->
                                case p2 of {
                                  LeftPart ->
                                    case p1 of {
                                      LeftPart -> (ys `cast` <Co:29>) eta1;
                                      RightPart ->
                                        (Just
                                           (case reverse1
                                                   (: (case lu of { :| a1 as -> a1 })
                                                      (case lu of { :| a1 as -> as }))
                                                   []
                                            of {
                                              [] -> case cycle1 of wild7 { };
                                              : a1 as -> $w$scropHeads2 a1 as lvl28
                                            }),
                                         eta1)
                                        `cast` <Co:18>
                                    };
                                  RightPart ->
                                    case p1 of {
                                      LeftPart ->
                                        (Just
                                           (case reverse1
                                                   (: (case lu of { :| a1 as -> a1 })
                                                      (case lu of { :| a1 as -> as }))
                                                   []
                                            of {
                                              [] -> case cycle1 of wild7 { };
                                              : a1 as -> $w$scropHeads2 a1 as lvl28
                                            }),
                                         eta1)
                                        `cast` <Co:18>;
                                      RightPart -> (ys `cast` <Co:29>) eta1
                                    }
                                }
                                }
                            })
                         `cast` <Co:25>)
                        l1;
                    Tip -> z'
                  }; } in
          ((go3 (lvl18 `cast` <Co:61>) x) `cast` <Co:22>)
            ($sinsert_$sgo2 v (p, l) eta)
      }
end Rec }

-- RHS size: {terms: 50, types: 147, coercions: 22, joins: 1/2}
$w$spartMonad
  :: AdjacencyMap (Either Integer Integer)
     -> PartMap (Either Integer Integer)
     -> (# Maybe [Either Integer Integer],
           PartMap (Either Integer Integer) #)
$w$spartMonad
  = \ (w :: AdjacencyMap (Either Integer Integer))
      (w1 :: PartMap (Either Integer Integer)) ->
      let {
        s :: Map (Either Integer Integer) (Set (Either Integer Integer))
        s = symmetricClosure1 $s$fOrdEither1 w } in
      joinrec {
        go
          :: [Either Integer Integer]
             -> PartMap (Either Integer Integer)
             -> (# Maybe [Either Integer Integer],
                   PartMap (Either Integer Integer) #)
        go (ds :: [Either Integer Integer])
           (eta :: PartMap (Either Integer Integer))
          = case ds of {
              [] -> (# Nothing, eta #);
              : y ys ->
                case $smember5 y eta of {
                  False ->
                    case (benchmarks4 (s `cast` <Co:5>) LeftPart (:| y []) y eta)
                         `cast` <Co:17>
                    of
                    { (a1, s') ->
                    case a1 of wild3 {
                      Nothing -> jump go ys s';
                      Just ds1 -> (# wild3, s' #)
                    }
                    };
                  True -> jump go ys eta
                }
            }; } in
      jump go (keys_go [] s) w1

-- RHS size: {terms: 10, types: 38, coercions: 12, joins: 0/0}
benchmarks3
  :: AdjacencyMap (Either Integer Integer)
     -> PartMap (Either Integer Integer)
     -> Identity
          (Maybe [Either Integer Integer], PartMap (Either Integer Integer))
benchmarks3
  = \ (w :: AdjacencyMap (Either Integer Integer))
      (w1 :: PartMap (Either Integer Integer)) ->
      case $w$spartMonad w w1 of { (# ww1, ww2 #) ->
      (ww1, ww2) `cast` <Co:12>
      }

-- RHS size: {terms: 12, types: 43, coercions: 0, joins: 0/0}
benchmarks_$sdetectParts'
  :: AdjacencyMap (Either Integer Integer) -> Bool
benchmarks_$sdetectParts'
  = \ (x :: AdjacencyMap (Either Integer Integer)) ->
      case $w$spartMonad x Tip of { (# ww1, ww2 #) ->
      case ww1 of {
        Nothing -> True;
        Just c -> False
      }
      }

Rec {
-- RHS size: {terms: 54, types: 62, coercions: 0, joins: 0/0}
$wcropHeads
  :: forall a. Ord a => a -> [a] -> NonEmpty a -> (# a, [a] #)
$wcropHeads
  = \ (@ a) (w :: Ord a) (ww :: a) (ww1 :: [a]) (w1 :: NonEmpty a) ->
      case ww1 of wild {
        [] ->
          (# case w1 of { :| a1 as -> a1 }, case w1 of { :| a1 as -> as } #);
        : ipv ipv1 ->
          case w1 of { :| ds ds1 ->
          case ds1 of wild2 {
            [] -> (# ww, wild #);
            : ipv2 ipv3 ->
              case == ($p1Ord w) ipv ipv2 of {
                False ->
                  case ++_$s++ (reverse1 wild2 []) ww wild of { : ww3 ww4 ->
                  (# ww3, ww4 #)
                  };
                True -> $wcropHeads w ipv ipv1 (:| ipv2 ipv3)
              }
          }
          }
      }
end Rec }

-- RHS size: {terms: 5, types: 19, coercions: 0, joins: 0/0}
lvl19
  :: forall a.
     Map a (Part, NonEmpty a) -> (Maybe [a], Map a (Part, NonEmpty a))
lvl19
  = \ (@ a) (eta1 :: Map a (Part, NonEmpty a)) -> (Nothing, eta1)

Rec {
-- RHS size: {terms: 178, types: 242, coercions: 178, joins: 0/8}
partMonad1
  :: forall a.
     Ord a =>
     AdjacencyMap a
     -> Part
     -> NonEmpty a
     -> a
     -> PartMap a
     -> Identity (Maybe [a], PartMap a)
partMonad1
  = \ (@ a)
      ($dOrd :: Ord a)
      (g :: AdjacencyMap a)
      (p :: Part)
      (l :: NonEmpty a)
      (v :: a)
      (eta :: PartMap a) ->
      case lookup $dOrd v (g `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: a
            lvl25 = case l of { :| b bs -> b } } in
          let {
            lvl26 :: [a]
            lvl26 = case l of { :| b bs -> bs } } in
          let {
            lvl27 :: [a]
            lvl27 = : lvl25 lvl26 } in
          let {
            lvl28 :: NonEmpty a
            lvl28
              = case reverse1 lvl27 [] of {
                  [] -> case cycle1 of wild2 { };
                  : a1 as -> :| a1 as
                } } in
          let {
            p1 :: Part
            p1
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          letrec {
            go3
              :: MaybeT (State (PartMap a)) [a]
                 -> Set a -> MaybeT (State (PartMap a)) [a]
            go3
              = \ (z' :: MaybeT (State (PartMap a)) [a]) (ds :: Set a) ->
                  case ds of {
                    Bin dt x1 l1 r ->
                      let {
                        lvl29 :: NonEmpty a
                        lvl29 = :| x1 lvl27 } in
                      go3
                        (let {
                           ys :: MaybeT (State (PartMap a)) [a]
                           ys = go3 z' r } in
                         (\ (eta1 :: PartMap a) ->
                            case lookup $dOrd x1 eta1 of {
                              Nothing ->
                                case (partMonad1 $dOrd g p1 lvl29 x1 eta1) `cast` <Co:11> of wild3
                                { (a1, s') ->
                                case a1 of {
                                  Nothing -> (ys `cast` <Co:19>) s';
                                  Just ds1 -> wild3 `cast` <Co:12>
                                }
                                };
                              Just pv ->
                                case pv of { (p2, lu) ->
                                case p2 of {
                                  LeftPart ->
                                    case p1 of {
                                      LeftPart -> (ys `cast` <Co:19>) eta1;
                                      RightPart ->
                                        (Just
                                           (case reverse1
                                                   (: (case lu of { :| a1 as -> a1 })
                                                      (case lu of { :| a1 as -> as }))
                                                   []
                                            of {
                                              [] -> case cycle1 of wild7 { };
                                              : a1 as ->
                                                case $wcropHeads $dOrd a1 as lvl28 of
                                                { (# ww1, ww2 #) ->
                                                : ww1 ww2
                                                }
                                            }),
                                         eta1)
                                        `cast` <Co:12>
                                    };
                                  RightPart ->
                                    case p1 of {
                                      LeftPart ->
                                        (Just
                                           (case reverse1
                                                   (: (case lu of { :| a1 as -> a1 })
                                                      (case lu of { :| a1 as -> as }))
                                                   []
                                            of {
                                              [] -> case cycle1 of wild7 { };
                                              : a1 as ->
                                                case $wcropHeads $dOrd a1 as lvl28 of
                                                { (# ww1, ww2 #) ->
                                                : ww1 ww2
                                                }
                                            }),
                                         eta1)
                                        `cast` <Co:12>;
                                      RightPart -> (ys `cast` <Co:19>) eta1
                                    }
                                }
                                }
                            })
                         `cast` <Co:17>)
                        l1;
                    Tip -> z'
                  }; } in
          ((go3 (lvl19 `cast` <Co:41>) x) `cast` <Co:14>)
            (insert $dOrd v (p, l) eta)
      }
end Rec }

-- RHS size: {terms: 61, types: 107, coercions: 38, joins: 0/3}
partMonad :: forall a. Ord a => AdjacencyMap a -> PartMonad a
partMonad
  = \ (@ a) ($dOrd :: Ord a) (g :: AdjacencyMap a) ->
      let {
        s :: Map a (Set a)
        s = symmetricClosure1 $dOrd g } in
      let {
        w :: [a]
        w = keys_go [] s } in
      letrec {
        $wgo :: [a] -> PartMap a -> (# Maybe [a], PartMap a #)
        $wgo
          = \ (w1 :: [a]) (w3 :: PartMap a) ->
              case w1 of {
                [] -> (# Nothing, w3 #);
                : y ys ->
                  case member $dOrd y w3 of {
                    False ->
                      case (partMonad1 $dOrd (s `cast` <Co:3>) LeftPart (:| y []) y w3)
                           `cast` <Co:11>
                      of
                      { (a1, s') ->
                      case a1 of wild3 {
                        Nothing -> $wgo ys s';
                        Just ds -> (# wild3, s' #)
                      }
                      };
                    True -> $wgo ys w3
                  }
              }; } in
      (\ (w1 :: PartMap a) ->
         case $wgo w w1 of { (# ww1, ww2 #) -> (ww1, ww2) `cast` <Co:8> })
      `cast` <Co:16>

-- RHS size: {terms: 15, types: 28, coercions: 21, joins: 0/0}
detectParts' :: forall a. Ord a => AdjacencyMap a -> Bool
detectParts'
  = \ (@ a) ($dOrd :: Ord a) (x :: AdjacencyMap a) ->
      case (((partMonad $dOrd x) `cast` <Co:14>) Tip) `cast` <Co:7> of
      { (ds, m) ->
      case ds of {
        Nothing -> True;
        Just c -> False
      }
      }

Rec {
-- RHS size: {terms: 139, types: 178, coercions: 2, joins: 0/8}
benchmarks_$sdfs2
  :: Part
     -> NonEmpty Integer
     -> AdjacencyMap Integer
     -> PartMap Integer
     -> Integer
     -> Either [Integer] (PartMap Integer)
benchmarks_$sdfs2
  = \ (p :: Part)
      (l :: NonEmpty Integer)
      (g :: AdjacencyMap Integer)
      (m :: PartMap Integer)
      (v :: Integer) ->
      case $slookup3 v (g `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Part
            lvl25
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          let {
            lvl26 :: Integer
            lvl26 = case l of { :| b bs -> b } } in
          let {
            lvl27 :: [Integer]
            lvl27 = case l of { :| b bs -> bs } } in
          let {
            lvl28 :: [Integer]
            lvl28 = : lvl26 lvl27 } in
          let {
            lvl29 :: NonEmpty Integer
            lvl29
              = case reverse1 lvl28 [] of {
                  [] -> case cycle1 of wild2 { };
                  : a1 as -> :| a1 as
                } } in
          letrec {
            go3
              :: (PartMap Integer -> Either [Integer] (PartMap Integer))
                 -> Set Integer
                 -> PartMap Integer
                 -> Either [Integer] (PartMap Integer)
            go3
              = \ (z' :: PartMap Integer -> Either [Integer] (PartMap Integer))
                  (ds :: Set Integer) ->
                  case ds of {
                    Bin dt x1 l1 r ->
                      let {
                        lvl30 :: NonEmpty Integer
                        lvl30 = :| x1 lvl28 } in
                      go3
                        (let {
                           k :: PartMap Integer -> Either [Integer] (PartMap Integer)
                           k = go3 z' r } in
                         \ (z :: PartMap Integer) ->
                           case $slookup3 x1 z of {
                             Nothing ->
                               case benchmarks_$sdfs2 lvl25 lvl30 g z x1 of wild3 {
                                 Left l2 -> wild3;
                                 Right r1 -> k r1
                               };
                             Just ds1 ->
                               case ds1 of { (q, l') ->
                               case q of {
                                 LeftPart ->
                                   case p of {
                                     LeftPart ->
                                       Left
                                         (case lvl29 of { :| ww1 ww2 ->
                                          case $w$scropHeads ww1 ww2 (reverse l') of
                                          { (# ww4, ww5 #) ->
                                          : ww4 ww5
                                          }
                                          });
                                     RightPart -> k z
                                   };
                                 RightPart ->
                                   case p of {
                                     LeftPart -> k z;
                                     RightPart ->
                                       Left
                                         (case lvl29 of { :| ww1 ww2 ->
                                          case $w$scropHeads ww1 ww2 (reverse l') of
                                          { (# ww4, ww5 #) ->
                                          : ww4 ww5
                                          }
                                          })
                                   }
                               }
                               }
                           })
                        l1;
                    Tip -> z'
                  }; } in
          go3 Right x ($sinsert_$sgo1 v (p, l) m)
      }
end Rec }

-- RHS size: {terms: 42, types: 54, coercions: 3, joins: 1/2}
benchmarks_$sdetectParts''2 :: AdjacencyMap Integer -> Bool
benchmarks_$sdetectParts''2
  = \ (eta :: AdjacencyMap Integer) ->
      let {
        s :: Map Integer (Set Integer)
        s = symmetricClosure1 $fOrdInteger eta } in
      joinrec {
        go :: [Integer] -> PartMap Integer -> Bool
        go (ds :: [Integer]) (eta1 :: PartMap Integer)
          = case ds of {
              [] -> True;
              : y ys ->
                case $slookup3 y eta1 of {
                  Nothing ->
                    case benchmarks_$sdfs2 LeftPart (:| y []) (s `cast` <Co:3>) eta1 y
                    of {
                      Left l -> False;
                      Right r -> jump go ys r
                    };
                  Just ds1 -> jump go ys eta1
                }
            }; } in
      jump go (keys_go [] s) Tip

Rec {
-- RHS size: {terms: 140, types: 178, coercions: 2, joins: 0/7}
$w$sdfs
  :: Part
     -> NonEmpty Int
     -> AdjacencyMap Int
     -> PartMap Int
     -> Int#
     -> Either [Int] (PartMap Int)
$w$sdfs
  = \ (w :: Part)
      (w1 :: NonEmpty Int)
      (w3 :: AdjacencyMap Int)
      (w4 :: PartMap Int)
      (ww :: Int#) ->
      case $wpoly_go13 ww (w3 `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Part
            lvl25
              = case w of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          let {
            lvl26 :: Int
            lvl26 = case w1 of { :| b bs -> b } } in
          let {
            lvl27 :: [Int]
            lvl27 = case w1 of { :| b bs -> bs } } in
          let {
            lvl28 :: [Int]
            lvl28 = : lvl26 lvl27 } in
          let {
            lvl29 :: NonEmpty Int
            lvl29
              = case reverse1 lvl28 [] of {
                  [] -> case cycle1 of wild2 { };
                  : a1 as -> :| a1 as
                } } in
          letrec {
            go3
              :: (PartMap Int -> Either [Int] (PartMap Int))
                 -> Set Int -> PartMap Int -> Either [Int] (PartMap Int)
            go3
              = \ (z' :: PartMap Int -> Either [Int] (PartMap Int))
                  (ds :: Set Int) ->
                  case ds of {
                    Bin dt x1 l r ->
                      case x1 of ww1 { I# ww2 ->
                      go3
                        (let {
                           k :: PartMap Int -> Either [Int] (PartMap Int)
                           k = go3 z' r } in
                         \ (z :: PartMap Int) ->
                           case $wpoly_go13 ww2 z of {
                             Nothing ->
                               case $w$sdfs lvl25 (:| ww1 lvl28) w3 z ww2 of wild3 {
                                 Left l1 -> wild3;
                                 Right r1 -> k r1
                               };
                             Just ds1 ->
                               case ds1 of { (q, l') ->
                               case q of {
                                 LeftPart ->
                                   case w of {
                                     LeftPart ->
                                       Left
                                         (case lvl29 of { :| ww4 ww5 ->
                                          case $w$scropHeads1 ww4 ww5 (reverse l') of
                                          { (# ww7, ww8 #) ->
                                          : ww7 ww8
                                          }
                                          });
                                     RightPart -> k z
                                   };
                                 RightPart ->
                                   case w of {
                                     LeftPart -> k z;
                                     RightPart ->
                                       Left
                                         (case lvl29 of { :| ww4 ww5 ->
                                          case $w$scropHeads1 ww4 ww5 (reverse l') of
                                          { (# ww7, ww8 #) ->
                                          : ww7 ww8
                                          }
                                          })
                                   }
                               }
                               }
                           })
                        l
                      };
                    Tip -> z'
                  }; } in
          go3 Right x ($w$sgo8 ww (w, w1) w4)
      }
end Rec }

-- RHS size: {terms: 45, types: 56, coercions: 3, joins: 1/2}
benchmarks_$sdetectParts''1 :: AdjacencyMap Int -> Bool
benchmarks_$sdetectParts''1
  = \ (eta :: AdjacencyMap Int) ->
      let {
        s :: Map Int (Set Int)
        s = symmetricClosure1 $fOrdInt eta } in
      joinrec {
        go :: [Int] -> PartMap Int -> Bool
        go (ds :: [Int]) (eta1 :: PartMap Int)
          = case ds of {
              [] -> True;
              : y ys ->
                case y of ww { I# ww1 ->
                case $wpoly_go13 ww1 eta1 of {
                  Nothing ->
                    case $w$sdfs LeftPart (:| ww []) (s `cast` <Co:3>) eta1 ww1 of {
                      Left l -> False;
                      Right r -> jump go ys r
                    };
                  Just ds1 -> jump go ys eta1
                }
                }
            }; } in
      jump go (keys_go [] s) Tip

Rec {
-- RHS size: {terms: 129, types: 298, coercions: 4, joins: 0/8}
benchmarks_$sdfs
  :: Part
     -> NonEmpty (Either Integer Integer)
     -> AdjacencyMap (Either Integer Integer)
     -> PartMap (Either Integer Integer)
     -> Either Integer Integer
     -> Either
          [Either Integer Integer] (PartMap (Either Integer Integer))
benchmarks_$sdfs
  = \ (p :: Part)
      (l :: NonEmpty (Either Integer Integer))
      (g :: AdjacencyMap (Either Integer Integer))
      (m :: PartMap (Either Integer Integer))
      (v :: Either Integer Integer) ->
      case $slookup5 v (g `cast` <Co:4>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Part
            lvl25
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          let {
            lvl26 :: Either Integer Integer
            lvl26 = case l of { :| b bs -> b } } in
          let {
            lvl27 :: [Either Integer Integer]
            lvl27 = case l of { :| b bs -> bs } } in
          let {
            lvl28 :: [Either Integer Integer]
            lvl28 = : lvl26 lvl27 } in
          let {
            lvl29 :: NonEmpty (Either Integer Integer)
            lvl29
              = case reverse1 lvl28 [] of {
                  [] -> case cycle1 of wild2 { };
                  : a1 as -> :| a1 as
                } } in
          letrec {
            go3
              :: (PartMap (Either Integer Integer)
                  -> Either
                       [Either Integer Integer] (PartMap (Either Integer Integer)))
                 -> Set (Either Integer Integer)
                 -> PartMap (Either Integer Integer)
                 -> Either
                      [Either Integer Integer] (PartMap (Either Integer Integer))
            go3
              = \ (z'
                     :: PartMap (Either Integer Integer)
                        -> Either
                             [Either Integer Integer] (PartMap (Either Integer Integer)))
                  (ds :: Set (Either Integer Integer)) ->
                  case ds of {
                    Bin dt x1 l1 r ->
                      let {
                        lvl30 :: NonEmpty (Either Integer Integer)
                        lvl30 = :| x1 lvl28 } in
                      go3
                        (let {
                           k :: PartMap (Either Integer Integer)
                                -> Either
                                     [Either Integer Integer] (PartMap (Either Integer Integer))
                           k = go3 z' r } in
                         \ (z :: PartMap (Either Integer Integer)) ->
                           case $slookup5 x1 z of {
                             Nothing ->
                               case benchmarks_$sdfs lvl25 lvl30 g z x1 of wild3 {
                                 Left l2 -> wild3;
                                 Right r1 -> k r1
                               };
                             Just ds1 ->
                               case ds1 of { (q, l') ->
                               case q of {
                                 LeftPart ->
                                   case p of {
                                     LeftPart ->
                                       Left
                                         (case lvl29 of { :| ww1 ww2 ->
                                          $w$scropHeads2 ww1 ww2 (reverse l')
                                          });
                                     RightPart -> k z
                                   };
                                 RightPart ->
                                   case p of {
                                     LeftPart -> k z;
                                     RightPart ->
                                       Left
                                         (case lvl29 of { :| ww1 ww2 ->
                                          $w$scropHeads2 ww1 ww2 (reverse l')
                                          })
                                   }
                               }
                               }
                           })
                        l1;
                    Tip -> z'
                  }; } in
          go3 Right x ($sinsert_$sgo2 v (p, l) m)
      }
end Rec }

-- RHS size: {terms: 42, types: 104, coercions: 5, joins: 1/2}
benchmarks_$sdetectParts''
  :: AdjacencyMap (Either Integer Integer) -> Bool
benchmarks_$sdetectParts''
  = \ (eta :: AdjacencyMap (Either Integer Integer)) ->
      let {
        s :: Map (Either Integer Integer) (Set (Either Integer Integer))
        s = symmetricClosure1 $s$fOrdEither1 eta } in
      joinrec {
        go
          :: [Either Integer Integer]
             -> PartMap (Either Integer Integer) -> Bool
        go (ds :: [Either Integer Integer])
           (eta1 :: PartMap (Either Integer Integer))
          = case ds of {
              [] -> True;
              : y ys ->
                case $slookup5 y eta1 of {
                  Nothing ->
                    case benchmarks_$sdfs LeftPart (:| y []) (s `cast` <Co:5>) eta1 y
                    of {
                      Left l -> False;
                      Right r -> jump go ys r
                    };
                  Just ds1 -> jump go ys eta1
                }
            }; } in
      jump go (keys_go [] s) Tip

Rec {
-- RHS size: {terms: 135, types: 172, coercions: 2, joins: 1/9}
detectParts''_dfs
  :: forall a.
     Ord a =>
     Part
     -> NonEmpty a
     -> AdjacencyMap a
     -> PartMap a
     -> a
     -> Either [a] (PartMap a)
detectParts''_dfs
  = \ (@ a)
      ($dOrd :: Ord a)
      (p :: Part)
      (l :: NonEmpty a)
      (g :: AdjacencyMap a)
      (m :: PartMap a)
      (v :: a) ->
      case lookup $dOrd v (g `cast` <Co:2>) of {
        Nothing -> case fromJust1 of wild1 { };
        Just x ->
          let {
            lvl25 :: Part
            lvl25
              = case p of {
                  LeftPart -> RightPart;
                  RightPart -> LeftPart
                } } in
          let {
            lvl26 :: a
            lvl26 = case l of { :| b bs -> b } } in
          let {
            lvl27 :: [a]
            lvl27 = case l of { :| b bs -> bs } } in
          let {
            lvl28 :: [a]
            lvl28 = : lvl26 lvl27 } in
          let {
            lvl29 :: NonEmpty a
            lvl29
              = case reverse1 lvl28 [] of {
                  [] -> case cycle1 of wild2 { };
                  : a1 as -> :| a1 as
                } } in
          letrec {
            go3
              :: (PartMap a -> Either [a] (PartMap a))
                 -> Set a -> PartMap a -> Either [a] (PartMap a)
            go3
              = \ (z' :: PartMap a -> Either [a] (PartMap a)) (ds :: Set a) ->
                  case ds of {
                    Bin dt x1 l1 r ->
                      let {
                        lvl30 :: NonEmpty a
                        lvl30 = :| x1 lvl28 } in
                      go3
                        (let {
                           k :: PartMap a -> Either [a] (PartMap a)
                           k = go3 z' r } in
                         \ (z :: PartMap a) ->
                           case lookup $dOrd x1 z of {
                             Nothing ->
                               case detectParts''_dfs $dOrd lvl25 lvl30 g z x1 of wild3 {
                                 Left l2 -> wild3;
                                 Right r1 -> k r1
                               };
                             Just ds1 ->
                               case ds1 of { (q, l') ->
                               join {
                                 $j :: Either [a] (PartMap a)
                                 $j
                                   = Left
                                       (case lvl29 of { :| ww1 ww2 ->
                                        case $wcropHeads $dOrd ww1 ww2 (reverse l') of
                                        { (# ww4, ww5 #) ->
                                        : ww4 ww5
                                        }
                                        }) } in
                               case q of {
                                 LeftPart ->
                                   case p of {
                                     LeftPart -> jump $j;
                                     RightPart -> k z
                                   };
                                 RightPart ->
                                   case p of {
                                     LeftPart -> k z;
                                     RightPart -> jump $j
                                   }
                               }
                               }
                           })
                        l1;
                    Tip -> z'
                  }; } in
          go3 Right x (insert $dOrd v (p, l) m)
      }
end Rec }

-- RHS size: {terms: 46, types: 60, coercions: 3, joins: 1/2}
detectParts'' :: forall a. Ord a => AdjacencyMap a -> Bool
detectParts''
  = \ (@ a) ($dOrd :: Ord a) (eta :: AdjacencyMap a) ->
      let {
        s :: Map a (Set a)
        s = symmetricClosure1 $dOrd eta } in
      joinrec {
        go :: [a] -> PartMap a -> Bool
        go (ds :: [a]) (eta1 :: PartMap a)
          = case ds of {
              [] -> True;
              : y ys ->
                case lookup $dOrd y eta1 of {
                  Nothing ->
                    case detectParts''_dfs
                           $dOrd LeftPart (:| y []) (s `cast` <Co:3>) eta1 y
                    of {
                      Left l -> False;
                      Right r -> jump go ys r
                    };
                  Just ds1 -> jump go ys eta1
                }
            }; } in
      jump go (keys_go [] s) Tip

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
benchmarks8 :: Addr#
benchmarks8 = "Maybe"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
benchmarks7 :: [Char]
benchmarks7 = unpackCString# benchmarks8

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
benchmarks6 :: Addr#
benchmarks6 = "PartMonad"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
benchmarks5 :: [Char]
benchmarks5 = unpackCString# benchmarks6

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
benchmarks2 :: Addr#
benchmarks2 = "Either"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
benchmarks1 :: [Char]
benchmarks1 = unpackCString# benchmarks2

-- RHS size: {terms: 53, types: 40, coercions: 72, joins: 0/0}
$w$sbenchmarks2
  :: AdjacencyMap Integer -> (# Benchmark, [Benchmark] #)
$w$sbenchmarks2
  = \ (w :: AdjacencyMap Integer) ->
      (# Benchmark
           benchmarks7
           (Benchmarkable
              (rwhnf `cast` <Co:3>)
              (nf2 `cast` <Co:5>)
              (nf1 `cast` <Co:7>)
              ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                  ((nf' rwhnf benchmarks_$sdetectParts2 w eta) `cast` <Co:2>) eta1)
               `cast` <Co:7>)
              False),
         : (Benchmark
              benchmarks5
              (Benchmarkable
                 (rwhnf `cast` <Co:3>)
                 (nf2 `cast` <Co:5>)
                 (nf1 `cast` <Co:7>)
                 ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                     ((nf' rwhnf benchmarks_$sdetectParts'2 w eta) `cast` <Co:2>) eta1)
                  `cast` <Co:7>)
                 False))
           (: (Benchmark
                 benchmarks1
                 (Benchmarkable
                    (rwhnf `cast` <Co:3>)
                    (nf2 `cast` <Co:5>)
                    (nf1 `cast` <Co:7>)
                    ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                        ((nf' rwhnf benchmarks_$sdetectParts''2 w eta) `cast` <Co:2>) eta1)
                     `cast` <Co:7>)
                    False))
              []) #)

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main16 :: Integer
main16 = 1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main45 :: Integer
main45 = 1000000

-- RHS size: {terms: 27, types: 35, coercions: 9, joins: 0/0}
main44 :: AdjacencyMap Integer
main44
  = case enumDeltaToInteger1 main16 main45 of wild {
      [] -> Tip `cast` <Co:3>;
      : x ds ->
        case ds of wild1 {
          [] ->
            case x of dt { __DEFAULT ->
            (Bin 1# dt Tip Tip Tip) `cast` <Co:3>
            };
          : ipv ipv1 ->
            (path2 $fOrdInteger (map path1 (zip wild wild1))) `cast` <Co:3>
        }
    }

-- RHS size: {terms: 7, types: 10, coercions: 0, joins: 0/0}
main43 :: [Benchmark]
main43
  = case $w$sbenchmarks2 main44 of { (# ww1, ww2 #) -> : ww1 ww2 }

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
main42 :: Benchmark
main42 = BenchGroup main46 main43

-- RHS size: {terms: 53, types: 40, coercions: 72, joins: 0/0}
$w$sbenchmarks1 :: AdjacencyMap Int -> (# Benchmark, [Benchmark] #)
$w$sbenchmarks1
  = \ (w :: AdjacencyMap Int) ->
      (# Benchmark
           benchmarks7
           (Benchmarkable
              (rwhnf `cast` <Co:3>)
              (nf2 `cast` <Co:5>)
              (nf1 `cast` <Co:7>)
              ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                  ((nf' rwhnf benchmarks_$sdetectParts1 w eta) `cast` <Co:2>) eta1)
               `cast` <Co:7>)
              False),
         : (Benchmark
              benchmarks5
              (Benchmarkable
                 (rwhnf `cast` <Co:3>)
                 (nf2 `cast` <Co:5>)
                 (nf1 `cast` <Co:7>)
                 ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                     ((nf' rwhnf benchmarks_$sdetectParts'1 w eta) `cast` <Co:2>) eta1)
                  `cast` <Co:7>)
                 False))
           (: (Benchmark
                 benchmarks1
                 (Benchmarkable
                    (rwhnf `cast` <Co:3>)
                    (nf2 `cast` <Co:5>)
                    (nf1 `cast` <Co:7>)
                    ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                        ((nf' rwhnf benchmarks_$sdetectParts''1 w eta) `cast` <Co:2>) eta1)
                     `cast` <Co:7>)
                    False))
              []) #)

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main39 :: Map Int (Set Int)
main39 = $wbtree 20#

-- RHS size: {terms: 7, types: 10, coercions: 3, joins: 0/0}
main38 :: [Benchmark]
main38
  = case $w$sbenchmarks1 (main39 `cast` <Co:3>) of
    { (# ww1, ww2 #) ->
    : ww1 ww2
    }

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
main37 :: Benchmark
main37 = BenchGroup main40 main38

-- RHS size: {terms: 53, types: 48, coercions: 72, joins: 0/0}
$w$sbenchmarks
  :: AdjacencyMap (Either Integer Integer)
     -> (# Benchmark, [Benchmark] #)
$w$sbenchmarks
  = \ (w :: AdjacencyMap (Either Integer Integer)) ->
      (# Benchmark
           benchmarks7
           (Benchmarkable
              (rwhnf `cast` <Co:3>)
              (nf2 `cast` <Co:5>)
              (nf1 `cast` <Co:7>)
              ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                  ((nf' rwhnf benchmarks_$sdetectParts w eta) `cast` <Co:2>) eta1)
               `cast` <Co:7>)
              False),
         : (Benchmark
              benchmarks5
              (Benchmarkable
                 (rwhnf `cast` <Co:3>)
                 (nf2 `cast` <Co:5>)
                 (nf1 `cast` <Co:7>)
                 ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                     ((nf' rwhnf benchmarks_$sdetectParts' w eta) `cast` <Co:2>) eta1)
                  `cast` <Co:7>)
                 False))
           (: (Benchmark
                 benchmarks1
                 (Benchmarkable
                    (rwhnf `cast` <Co:3>)
                    (nf2 `cast` <Co:5>)
                    (nf1 `cast` <Co:7>)
                    ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                        ((nf' rwhnf benchmarks_$sdetectParts'' w eta) `cast` <Co:2>) eta1)
                     `cast` <Co:7>)
                    False))
              []) #)

-- RHS size: {terms: 64, types: 56, coercions: 72, joins: 0/3}
$wbenchmarks
  :: forall a.
     Ord a =>
     AdjacencyMap a -> (# Benchmark, [Benchmark] #)
$wbenchmarks
  = \ (@ a) (w :: Ord a) (w1 :: AdjacencyMap a) ->
      (# Benchmark
           benchmarks7
           (let {
              f :: AdjacencyMap a -> Bool
              f = detectParts w } in
            Benchmarkable
              (rwhnf `cast` <Co:3>)
              (nf2 `cast` <Co:5>)
              (nf1 `cast` <Co:7>)
              ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                  ((nf' rwhnf f w1 eta) `cast` <Co:2>) eta1)
               `cast` <Co:7>)
              False),
         : (Benchmark
              benchmarks5
              (let {
                 f :: AdjacencyMap a -> Bool
                 f = detectParts' w } in
               Benchmarkable
                 (rwhnf `cast` <Co:3>)
                 (nf2 `cast` <Co:5>)
                 (nf1 `cast` <Co:7>)
                 ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                     ((nf' rwhnf f w1 eta) `cast` <Co:2>) eta1)
                  `cast` <Co:7>)
                 False))
           (: (Benchmark
                 benchmarks1
                 (let {
                    f :: AdjacencyMap a -> Bool
                    f = detectParts'' w } in
                  Benchmarkable
                    (rwhnf `cast` <Co:3>)
                    (nf2 `cast` <Co:5>)
                    (nf1 `cast` <Co:7>)
                    ((\ _ (eta :: Int64) (eta1 :: State# RealWorld) ->
                        ((nf' rwhnf f w1 eta) `cast` <Co:2>) eta1)
                     `cast` <Co:7>)
                    False))
              []) #)

-- RHS size: {terms: 11, types: 17, coercions: 0, joins: 0/0}
benchmarks :: forall a. Ord a => AdjacencyMap a -> [Benchmark]
benchmarks
  = \ (@ a) (w :: Ord a) (w1 :: AdjacencyMap a) ->
      case $wbenchmarks w w1 of { (# ww1, ww2 #) -> : ww1 ww2 }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule4 :: Addr#
$trModule4 = "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule3 :: TrName
$trModule3 = TrNameS $trModule4

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule2 :: Addr#
$trModule2 = "Main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule1 :: TrName
$trModule1 = TrNameS $trModule2

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule :: Module
$trModule = Module $trModule3 $trModule1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tcPart2 :: Addr#
$tcPart2 = "Part"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tcPart1 :: TrName
$tcPart1 = TrNameS $tcPart2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tcPart :: TyCon
$tcPart
  = TyCon
      9633307036978254624##
      15530527461042983326##
      $trModule
      $tcPart1
      0#
      krep$*

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
$tc'LeftPart1 :: KindRep
$tc'LeftPart1 = KindRepTyConApp $tcPart []

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'LeftPart3 :: Addr#
$tc'LeftPart3 = "'LeftPart"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'LeftPart2 :: TrName
$tc'LeftPart2 = TrNameS $tc'LeftPart3

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'LeftPart :: TyCon
$tc'LeftPart
  = TyCon
      13043206929019312225##
      13913137502676550643##
      $trModule
      $tc'LeftPart2
      0#
      $tc'LeftPart1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$tc'RightPart2 :: Addr#
$tc'RightPart2 = "'RightPart"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$tc'RightPart1 :: TrName
$tc'RightPart1 = TrNameS $tc'RightPart2

-- RHS size: {terms: 7, types: 0, coercions: 0, joins: 0/0}
$tc'RightPart :: TyCon
$tc'RightPart
  = TyCon
      13019481427122163306##
      2592815072267558642##
      $trModule
      $tc'RightPart1
      0#
      $tc'LeftPart1

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl20 :: Addr#
lvl20 = "Main.hs:211:29-58|(left, _ : right)"#

-- RHS size: {terms: 3, types: 8, coercions: 0, joins: 0/0}
lvl21 :: forall a. ([a], [a])
lvl21 = \ (@ a) -> irrefutPatError lvl20

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
w2 :: Integer
w2 = 0

-- RHS size: {terms: 102, types: 114, coercions: 31, joins: 0/3}
lvl22
  :: forall a.
     a -> ([a] -> Rand StdGen [a]) -> [a] -> Rand StdGen [a]
lvl22
  = \ (@ a) (x :: a) (k :: [a] -> Rand StdGen [a]) (z :: [a]) ->
      let {
        w1 :: Integer
        w1
          = case $wlenAcc z 0# of ww2 { __DEFAULT ->
            smallInteger (-# ww2 1#)
            } } in
      (\ (s1 :: StdGen) ->
         let {
           ds :: (Int, StdGen)
           ds
             = case $wrandomIvalInteger $fRandomGenStdGen $fNumInt w2 w1 s1 of
               { (# ww3, ww4 #) ->
               (ww3, ww4)
               } } in
         let {
           ds1 :: Int
           ds1 = case ds of { (x1, v') -> x1 } } in
         ((k (: (!! z ds1)
                (case ds1 of { I# x1 ->
                 case <=# x1 0# of {
                   __DEFAULT ->
                     case z of {
                       [] -> case lvl21 of wild2 { };
                       : ipv ipv1 ->
                         case x1 of ds2 {
                           __DEFAULT ->
                             case splitAt_$s$wsplitAt' ipv1 (-# ds2 1#) of { (# ww1, ww2 #) ->
                             case ww2 of {
                               [] -> case lvl21 of wild3 { };
                               : ds3 right -> ++_$s++ (: x right) ipv ww1
                             }
                             };
                           1# ->
                             case ipv1 of {
                               [] -> case lvl21 of wild3 { };
                               : ds3 right -> ++_$s++ (: x right) ipv []
                             }
                         }
                     };
                   1# ->
                     case z of {
                       [] -> case lvl21 of wild2 { };
                       : ds2 right -> ++ [] (: x right)
                     }
                 }
                 })))
          `cast` <Co:14>)
           (case ds of { (x1, v') -> v' }))
      `cast` <Co:17>

Rec {
-- RHS size: {terms: 14, types: 16, coercions: 26, joins: 0/0}
shuffle1 :: forall a. [a] -> [a] -> Rand StdGen [a]
shuffle1
  = \ (@ a) (ds :: [a]) (eta :: [a]) ->
      case ds of {
        [] -> ((,) eta) `cast` <Co:26>;
        : y ys -> lvl22 y (shuffle1 ys) eta
      }
end Rec }

-- RHS size: {terms: 50, types: 35, coercions: 19, joins: 0/0}
shuffle :: forall a. Int -> [a] -> [a]
shuffle
  = \ (@ a) (ds :: Int) (ds1 :: [a]) ->
      case ds1 of {
        [] -> [];
        : ipv ipv1 ->
          case (((shuffle1 ipv1 (: ipv [])) `cast` <Co:14>)
                  (case ds of { I# x# ->
                   case $w$cdivMod1
                          (word2Int# (and# (int2Word# (narrow32Int# x#)) 2147483647##))
                          2147483562#
                   of
                   { (# ww5, ww6 #) ->
                   case ww6 of { I32# x#1 ->
                   case ww5 of { I32# ww3 ->
                   case modInt# ww3 2147483398# of wild3 { __DEFAULT ->
                   StdGen
                     (narrow32Int# (+# x#1 1#))
                     (narrow32Int# (+# (narrow32Int# wild3) 1#))
                   }
                   }
                   }
                   }
                   }))
               `cast` <Co:5>
          of
          { (a1, ds2) ->
          a1
          }
      }

Rec {
-- RHS size: {terms: 13, types: 13, coercions: 26, joins: 0/0}
main_go :: [Integer] -> [Integer] -> Rand StdGen [Integer]
main_go
  = \ (ds :: [Integer]) (eta :: [Integer]) ->
      case ds of {
        [] -> ((,) eta) `cast` <Co:26>;
        : y ys -> lvl22 y (main_go ys) eta
      }
end Rec }

-- RHS size: {terms: 26, types: 12, coercions: 0, joins: 0/0}
main14 :: StdGen
main14
  = case $w$cdivMod1 179# 2147483562# of { (# ww5, ww6 #) ->
    case ww6 of { I32# x# ->
    case ww5 of { I32# ww3 ->
    case modInt# ww3 2147483398# of wild1 { __DEFAULT ->
    StdGen
      (narrow32Int# (+# x# 1#))
      (narrow32Int# (+# (narrow32Int# wild1) 1#))
    }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main51 :: Integer
main51 = 2000

-- RHS size: {terms: 20, types: 17, coercions: 25, joins: 0/0}
main50 :: AdjacencyMap Integer
main50
  = case enumDeltaToInteger1 main16 main51 of {
      [] -> (clique1 $fOrdInteger []) `cast` <Co:3>;
      : ipv ipv1 ->
        case (((main_go ipv1 (: ipv [])) `cast` <Co:14>) main14)
             `cast` <Co:5>
        of
        { (a1, ds1) ->
        (clique1 $fOrdInteger a1) `cast` <Co:3>
        }
    }

-- RHS size: {terms: 7, types: 10, coercions: 0, joins: 0/0}
main49 :: [Benchmark]
main49
  = case $w$sbenchmarks2 main50 of { (# ww1, ww2 #) -> : ww1 ww2 }

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
main48 :: Benchmark
main48 = BenchGroup main52 main49

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
main15 :: Integer
main15 = 1500

-- RHS size: {terms: 25, types: 33, coercions: 19, joins: 0/0}
main13
  :: [Either Integer Integer]
     -> AdjacencyMap (Either Integer Integer)
main13
  = \ (karg :: [Either Integer Integer]) ->
      case enumDeltaToInteger1 main16 main15 of {
        [] -> biclique $s$fOrdEither1 [] karg;
        : ipv ipv1 ->
          case (((main_go ipv1 (: ipv [])) `cast` <Co:14>) main14)
               `cast` <Co:5>
          of
          { (a1, ds1) ->
          biclique $s$fOrdEither1 (map Left a1) karg
          }
      }

-- RHS size: {terms: 26, types: 12, coercions: 0, joins: 0/0}
main12 :: StdGen
main12
  = case $w$cdivMod1 239# 2147483562# of { (# ww5, ww6 #) ->
    case ww6 of { I32# x# ->
    case ww5 of { I32# ww3 ->
    case modInt# ww3 2147483398# of wild1 { __DEFAULT ->
    StdGen
      (narrow32Int# (+# x# 1#))
      (narrow32Int# (+# (narrow32Int# wild1) 1#))
    }
    }
    }
    }

-- RHS size: {terms: 20, types: 23, coercions: 19, joins: 0/0}
main11 :: AdjacencyMap (Either Integer Integer)
main11
  = case enumDeltaToInteger1 main16 main15 of {
      [] -> main13 [];
      : ipv ipv1 ->
        case (((main_go ipv1 (: ipv [])) `cast` <Co:14>) main12)
             `cast` <Co:5>
        of
        { (a1, ds1) ->
        main13 (map Right a1)
        }
    }

-- RHS size: {terms: 7, types: 10, coercions: 0, joins: 0/0}
main10 :: [Benchmark]
main10
  = case $w$sbenchmarks main11 of { (# ww1, ww2 #) -> : ww1 ww2 }

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
main9 :: Benchmark
main9 = BenchGroup main17 main10

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
main8 :: [Benchmark]
main8 = : main9 []

-- RHS size: {terms: 86, types: 55, coercions: 0, joins: 0/4}
$wcaterpillar :: Int# -> Map Int (Set Int)
$wcaterpillar
  = \ (ww :: Int#) ->
      let {
        y :: Int#
        y = -# ww 1# } in
      let {
        n :: [(Int, Set Int)]
        n = case ># 0# y of {
              __DEFAULT ->
                letrec {
                  go :: Int# -> [(Int, Set Int)]
                  go
                    = \ (x :: Int#) ->
                        : (I# (*# 2# x), Bin 1# (I# (+# (*# 2# x) 2#)) Tip Tip)
                          (case ==# x y of {
                             __DEFAULT -> go (+# x 1#);
                             1# -> []
                           }); } in
                go 0#;
              1# -> []
            } } in
      case ># 0# y of {
        __DEFAULT ->
          letrec {
            go :: Int# -> [(Int, Set Int)]
            go
              = \ (x :: Int#) ->
                  : (I# (*# 2# x), Bin 1# (I# (+# (*# 2# x) 1#)) Tip Tip)
                    (case ==# x y of {
                       __DEFAULT -> go (+# x 1#);
                       1# -> n
                     }); } in
          path2 $fOrdInt (go 0#);
        1# -> path2 $fOrdInt n
      }

-- RHS size: {terms: 6, types: 3, coercions: 0, joins: 0/0}
caterpillar1 :: Int -> Map Int (Set Int)
caterpillar1
  = \ (w :: Int) -> case w of { I# ww1 -> $wcaterpillar ww1 }

-- RHS size: {terms: 1, types: 0, coercions: 5, joins: 0/0}
caterpillar :: Int -> AdjacencyMap Int
caterpillar = caterpillar1 `cast` <Co:5>

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main26 :: Map Int (Set Int)
main26 = $wcaterpillar 500000#

-- RHS size: {terms: 7, types: 10, coercions: 3, joins: 0/0}
main25 :: [Benchmark]
main25
  = case $w$sbenchmarks1 (main26 `cast` <Co:3>) of
    { (# ww1, ww2 #) ->
    : ww1 ww2
    }

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
main24 :: Benchmark
main24 = BenchGroup main27 main25

-- RHS size: {terms: 144, types: 86, coercions: 0, joins: 0/11}
$wgrid :: Int# -> Map Int (Set Int)
$wgrid
  = \ (ww :: Int#) ->
      let {
        y :: Int#
        y = -# ww 1# } in
      let {
        y1 :: Int#
        y1 = -# ww 2# } in
      let {
        n :: [(Int, Set Int)]
        n = case ># 0# y1 of {
              __DEFAULT ->
                letrec {
                  go :: Int# -> [(Int, Set Int)]
                  go
                    = \ (x :: Int#) ->
                        let {
                          n1 :: [(Int, Set Int)]
                          n1
                            = case ==# x y1 of {
                                __DEFAULT -> go (+# x 1#);
                                1# -> []
                              } } in
                        case ># 0# y of {
                          __DEFAULT ->
                            let {
                              lvl25 :: Int#
                              lvl25 = *# x ww } in
                            letrec {
                              go1 :: Int# -> [(Int, Set Int)]
                              go1
                                = \ (x1 :: Int#) ->
                                    : (I# (+# lvl25 x1), Bin 1# (I# (+# (+# lvl25 x1) ww)) Tip Tip)
                                      (case ==# x1 y of {
                                         __DEFAULT -> go1 (+# x1 1#);
                                         1# -> n1
                                       }); } in
                            go1 0#;
                          1# -> n1
                        }; } in
                go 0#;
              1# -> []
            } } in
      case ># 0# y of {
        __DEFAULT ->
          letrec {
            go :: Int# -> [(Int, Set Int)]
            go
              = \ (x :: Int#) ->
                  let {
                    n1 :: [(Int, Set Int)]
                    n1
                      = case ==# x y of {
                          __DEFAULT -> go (+# x 1#);
                          1# -> n
                        } } in
                  case ># 0# y1 of {
                    __DEFAULT ->
                      let {
                        lvl25 :: Int#
                        lvl25 = *# x ww } in
                      letrec {
                        go1 :: Int# -> [(Int, Set Int)]
                        go1
                          = \ (x1 :: Int#) ->
                              : (I# (+# lvl25 x1), Bin 1# (I# (+# (+# lvl25 x1) 1#)) Tip Tip)
                                (case ==# x1 y1 of {
                                   __DEFAULT -> go1 (+# x1 1#);
                                   1# -> n1
                                 }); } in
                      go1 0#;
                    1# -> n1
                  }; } in
          path2 $fOrdInt (go 0#);
        1# -> path2 $fOrdInt n
      }

-- RHS size: {terms: 6, types: 3, coercions: 0, joins: 0/0}
grid1 :: Int -> Map Int (Set Int)
grid1 = \ (w :: Int) -> case w of { I# ww1 -> $wgrid ww1 }

-- RHS size: {terms: 1, types: 0, coercions: 5, joins: 0/0}
grid :: Int -> AdjacencyMap Int
grid = grid1 `cast` <Co:5>

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main21 :: Map Int (Set Int)
main21 = $wgrid 750#

-- RHS size: {terms: 7, types: 10, coercions: 3, joins: 0/0}
main20 :: [Benchmark]
main20
  = case $w$sbenchmarks1 (main21 `cast` <Co:3>) of
    { (# ww1, ww2 #) ->
    : ww1 ww2
    }

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
main19 :: Benchmark
main19 = BenchGroup main22 main20

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
main7 :: [Benchmark]
main7 = : main19 main8

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
main6 :: [Benchmark]
main6 = : main24 main7

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
lvl23 :: Addr#
lvl23 = "Main.hs:(192,9)-(194,50)|function pow3"#

-- RHS size: {terms: 2, types: 2, coercions: 0, joins: 0/0}
lvl24 :: Int
lvl24 = patError lvl23

Rec {
-- RHS size: {terms: 33, types: 6, coercions: 0, joins: 0/0}
$wpow3 :: Int# -> Int#
$wpow3
  = \ (ww :: Int#) ->
      case ww of ds {
        __DEFAULT ->
          case andI# ds 1# of {
            __DEFAULT -> case lvl24 of wild1 { };
            0# ->
              case $wpow3 (uncheckedIShiftRA# ds 1#) of ww1 { __DEFAULT ->
              *# ww1 ww1
              };
            1# -> case $wpow3 (-# ds 1#) of ww1 { __DEFAULT -> *# 3# ww1 }
          };
        0# -> 1#
      }
end Rec }

-- RHS size: {terms: 51, types: 30, coercions: 0, joins: 0/2}
$wsideEdges :: Int# -> Int -> Map Int (Set Int)
$wsideEdges
  = \ (ww :: Int#) (w :: Int) ->
      case $wpow3 ww of ww1 { __DEFAULT ->
      let {
        y :: Int#
        y = -# ww1 1# } in
      case ># 0# y of {
        __DEFAULT ->
          letrec {
            go :: Int# -> [(Int, Set Int)]
            go
              = \ (x :: Int#) ->
                  : (I# x,
                     case w of { I# y1 -> Bin 1# (I# (+# (*# 3# x) y1)) Tip Tip })
                    (case ==# x y of {
                       __DEFAULT -> go (+# x 1#);
                       1# -> []
                     }); } in
          path2 $fOrdInt (go 0#);
        1# -> btree2
      }
      }

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main34 :: Int
main34 = I# 1#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main33 :: Int
main33 = I# 2#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main32 :: Int
main32 = I# 3#

-- RHS size: {terms: 14, types: 5, coercions: 0, joins: 0/0}
$wttree :: Int# -> Map Int (Set Int)
$wttree
  = \ (ww :: Int#) ->
      $sunionWith
        $sunion
        ($sunionWith
           $sunion ($wsideEdges ww main34) ($wsideEdges ww main33))
        ($wsideEdges ww main32)

-- RHS size: {terms: 6, types: 3, coercions: 0, joins: 0/0}
ttree1 :: Int -> Map Int (Set Int)
ttree1 = \ (w :: Int) -> case w of { I# ww1 -> $wttree ww1 }

-- RHS size: {terms: 1, types: 0, coercions: 5, joins: 0/0}
ttree :: Int -> AdjacencyMap Int
ttree = ttree1 `cast` <Co:5>

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
main31 :: Map Int (Set Int)
main31 = $wttree 12#

-- RHS size: {terms: 7, types: 10, coercions: 3, joins: 0/0}
main30 :: [Benchmark]
main30
  = case $w$sbenchmarks1 (main31 `cast` <Co:3>) of
    { (# ww1, ww2 #) ->
    : ww1 ww2
    }

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
main29 :: Benchmark
main29 = BenchGroup main35 main30

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
main5 :: [Benchmark]
main5 = : main29 main6

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
main4 :: [Benchmark]
main4 = : main37 main5

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
main3 :: [Benchmark]
main3 = : main42 main4

-- RHS size: {terms: 3, types: 1, coercions: 0, joins: 0/0}
main2 :: [Benchmark]
main2 = : main48 main3

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
main1 :: State# RealWorld -> (# State# RealWorld, () #)
main1 = defaultMain2 defaultConfig main2

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
main :: IO ()
main = main1 `cast` <Co:3>

-- RHS size: {terms: 2, types: 1, coercions: 3, joins: 0/0}
main54 :: State# RealWorld -> (# State# RealWorld, () #)
main54 = runMainIO1 (main1 `cast` <Co:3>)

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
main :: IO ()
main = main54 `cast` <Co:3>


------ Local rules for imported ids --------
"SPEC detectParts @ Integer"
    forall ($dOrd :: Ord Integer).
      detectParts $dOrd
      = benchmarks_$sdetectParts2
"SPEC detectParts @ Int"
    forall ($dOrd :: Ord Int).
      detectParts $dOrd
      = benchmarks_$sdetectParts1
"SPEC detectParts @ (Either Integer Integer)"
    forall ($dOrd :: Ord (Either Integer Integer)).
      detectParts $dOrd
      = benchmarks_$sdetectParts
"SPEC detectParts' @ Integer"
    forall ($dOrd :: Ord Integer).
      detectParts' $dOrd
      = benchmarks_$sdetectParts'2
"SPEC detectParts' @ Int"
    forall ($dOrd :: Ord Int).
      detectParts' $dOrd
      = benchmarks_$sdetectParts'1
"SPEC detectParts' @ (Either Integer Integer)"
    forall ($dOrd :: Ord (Either Integer Integer)).
      detectParts' $dOrd
      = benchmarks_$sdetectParts'
"SPEC detectParts'' @ Integer"
    forall ($dOrd :: Ord Integer).
      detectParts'' $dOrd
      = benchmarks_$sdetectParts''2
"SPEC detectParts'' @ Int"
    forall ($dOrd :: Ord Int).
      detectParts'' $dOrd
      = benchmarks_$sdetectParts''1
"SPEC detectParts'' @ (Either Integer Integer)"
    forall ($dOrd :: Ord (Either Integer Integer)).
      detectParts'' $dOrd
      = benchmarks_$sdetectParts''
"SPEC/Main $fEqEither_$c/= @ Integer @ Integer"
    forall ($dEq1 :: Eq Integer) ($dEq :: Eq Integer).
      $fEqEither_$c/= $dEq $dEq1
      = $s$fEqEither_$s$fEqEither_$c/=1
"SPEC/Main $fEqEither @ Integer @ Integer"
    forall (v1 :: Eq Integer) (v :: Eq Integer).
      $fEqEither v v1
      = $s$fEqEither1
"SPEC/Main $fOrdEither @ Integer @ Integer"
    forall (v1 :: Ord Integer) (v :: Ord Integer).
      $fOrdEither v v1
      = $s$fOrdEither1
"SPEC/Main unionWith @ Int _"
    forall (@ a) ($dOrd :: Ord Int). unionWith $dOrd = $sunionWith
"SPEC/Main $wsplitS @ Int" [0]
    forall (w :: Ord Int). $wsplitS w = $s$wsplitS
"SPEC/Main union @ Int"
    forall ($dOrd :: Ord Int). union $dOrd = $sunion
