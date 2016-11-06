module ErrorMessages.Semantic where

type ErrorMsg = String

msg1 = "Couldn't match type '"
msg2 = "' with '"
msg3 = "'\n"
exp  = "    Expected type: "
act  = "      Actual type: "
loc  = "      At location: "
expr = "In the expression: "

generateErrMsg :: Type -> Type -> ErrorMsg
generateErrMsg expT actT = exp ++ show expT ++ act ++ show
