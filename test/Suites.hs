module Main where

import Test.Framework (defaultMain)

import Chap02.Suite
import Chap03.Suite
{-import Chap04.Suite-}
import Chap05.Suite
{-import Chap06.Suite-}
{-import Chap07.Suite-}
{-import Chap08.Suite-}
{-import Chap09.Suite-}
{-import Chap10.Suite-}
{-import Chap11.Suite-}

main :: IO ()
main = defaultMain [ --
                     chap02Suite
                   , chap03Suite
                   {-, chap04Suite-}
                   , chap05Suite
                   {-, chap06Suite-}
                   {-, chap07Suite-}
                   {-, chap08Suite-}
                   {-, chap09Suite-}
                   {-, chap10Suite-}
                   {-, chap11Suite-}
                   ]
