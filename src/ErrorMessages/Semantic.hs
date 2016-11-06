module ErrorMessages.Semantic where

type ErrorMsg = String

exp  = "Expected:      "
act  = "Actual:        "
loc  = "At location:   "
expr = "In expression: "

generateErrMsg :: Type -> Type -> ErrorMsg
generateErrMsg expT actT = exp ++ show expT ++ act ++ show
