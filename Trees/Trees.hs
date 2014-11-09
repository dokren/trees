{-|
Module      : Trees.Trees
Description : Deriving tidy drawings of trees
Maintainer  : kren.domen@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Trees.Trees where

data Btree x 
	-- | Btree stands for Binary Tree which consists of a leaf Lf x labelled with a single element x.
	= Lf x 
	-- | Btree stands for Binary Tree which consists of a branch Br (Btree x) x (Btree x) consisting of two children and a label x.
	| Br (Btree x) x (Btree x)