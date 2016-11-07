module ErrorMessages.Semantic (generateErrMsg) where

import Utilities.Definitions

msg1      = "Couldn't match type '"
msg2      = "' with '"
msg3      = "'\n"
expected  = "    Expected type: "
act       = "      Actual type: "
loc       = "      At location: "
expr      = "In the expression: "

generateErrMsg :: Type -> Type -> ErrorMsg
generateErrMsg expT actT
  = expected ++ show expT ++ act ++ show actT
