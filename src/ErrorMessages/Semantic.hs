module ErrorMessages.Semantic (generateErrMsg) where

import Utilities.Definitions

expected  = "Expected:      "
act  = "Actual:        "
loc  = "At location:   "
expr = "In expression: "

generateErrMsg :: Type -> Type -> ErrorMsg
generateErrMsg expT actT = expected ++ show expT ++ act ++ show actT
