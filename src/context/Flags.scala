package context

object Flags {
  val passByValue = 1
  val passByName = 2
  val passByText = 3
  var parameterPassing = passByName
  var useStaticScopeRule = true
}