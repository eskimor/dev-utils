{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GenerateModule.Templates where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

import           GenerateModule.Internal



internalContent :: Module -> Doc Text
internalContent Module{..}
  = vsep
      $  [ "{-|"
         , "Module      :" <+> _internalName
         , "Description : Types and internal function for \"" <> _fullName <> "\""
         , "Copyright   :" <+> _copyrightNotice
         , "-}"
         , "module" <+> _internalName <+> "where"
               , line <> line
               , "data" <+> _name
               , "  =" <+> _name <+> align (vsep ["{"
                 , "}"
                 ])
         ]

exposedContent :: Module -> Doc Text
exposedContent Module{..}
  = vsep
      $  [ "{-|"
         , "Module      :" <+> _fullName
         , "Description : Short description"
         , "Copyright   :" <+> _copyrightNotice
         , "Here is a longer description of this module, containing some"
         , "commentary with @some markup@."
         , "-}"
         , "module" <+> _fullName <+> align (vsep [ "( -- * Types"
                                                       , "," <+> _name <> "(..)"
                                                       , ") where"
                                                       ])

         , line <> line
         , "import" <+> _internalName
         , line <> line
         ]
