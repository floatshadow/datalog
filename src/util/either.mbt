///|
pub(all) enum Either[T, U] {
  Left(T)
  Right(U)
} derive(Show, Eq, Hash)
