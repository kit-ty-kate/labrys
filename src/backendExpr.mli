type expr
type top_expr

val load :
  target:BackendValue.t ->
  ty:BackendType.t ->
  value:BackendValue.t ->
  expr
val call :
  target:BackendValue.t ->
  ty:BackendType.t ->
  f:BackendValue.t ->
  ty_param:BackendType.t ->
  param:BackendValue.t ->
  expr
val store :
  ty_value:BackendType.t ->
  value:BackendValue.t ->
  ty_target:BackendType.t ->
  target:BackendValue.t ->
  expr
val ret :
  ty:BackendType.t ->
  value:BackendValue.t ->
  expr

val define :
  ty:BackendType.t ->
  name:BackendValue.t ->
  ty_param:BackendType.t ->
  param:BackendValue.t ->
  expr list ->
  top_expr
val global :
  name:BackendValue.t ->
  ty:BackendType.t ->
  top_expr

val to_string : top_expr list -> string
