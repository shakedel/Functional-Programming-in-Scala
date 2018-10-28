object List {
  def apply() = "{}"
  def apply(one: Int) = "{"+one+"}"
  def apply(one: Int, two: Int) = "{"+one+", "+two+"}"
}

println("List(): " +List())
println("List(1): "+List(1))
println("List(2,3): "+List(2,3))