
class Parent {
  def method: Unit = println("parent")
}
class Child extends Parent {
  override def method: Unit = println("child")
}

//------------------------------------

// demonstrating Covariant type
class Covariant[+T]
def methodCovariant[T](covariant: Covariant[T]): Unit = {
  println("Covariant Method")
}

def methodCovariantParent = methodCovariant[Parent]
methodCovariantParent(new Covariant[Parent])
methodCovariantParent(new Covariant[Child])

def methodCovariantChild = methodCovariant[Child]
methodCovariantChild(new Covariant[Child])
//methodCovariantChild(new Covariant[Parent])

//------------------------------------

// demonstrating Contravariant type
class Contravariant[-T]
def methodContravariant[T](contravariant: Contravariant[T]): Unit = {
  println("Covariant Method")
}

def methodContravariantParent = methodContravariant[Parent]
methodContravariantParent(new Contravariant[Parent])
//methodContravariantParent(new Contravariant[Child])

def methodContravariantChild = methodContravariant[Child]
methodContravariantChild(new Contravariant[Parent])
methodContravariantChild(new Contravariant[Child])

//------------------------------------

// demonstrating Invariant type
class Invariant[T]
def methodInvariant[T](invariant: Invariant[T]): Unit = {
  println("Invariant Method")
}

def methodInvariantParent = methodInvariant[Parent]
methodInvariantParent(new Invariant[Parent])
//methodInvariantParent(new Invariant[Child])

def methodInvariantChild = methodInvariant[Child]
//methodInvariantChild(new Invariant[Parent])
methodInvariantChild(new Invariant[Child])

//------------------------------------

def converterParentToChild(parent: Parent): Child = ???
def converterChildToParent(child: Child): Parent = ???

def funcParentToChild(method: Parent => Child): Unit = ???
funcParentToChild(converterParentToChild)
//funcParentToChild(converterChildToParent)

def funcChildToParent(method: Child => Parent): Unit = ???
funcChildToParent(converterChildToParent)
funcChildToParent(converterParentToChild)
