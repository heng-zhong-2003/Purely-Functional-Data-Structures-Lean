set_option autoImplicit false

universe u

namespace PFDS

class Stack (StackT : Type u -> Type u)where
  empty   : {a : Type u} -> StackT a
  isEmpty : {a : Type u} -> StackT a -> Bool
  cons    : {a : Type u} -> a -> StackT a -> StackT a
  head    : {a : Type u} -> StackT a -> Option a
  tail    : {a : Type u} -> StackT a -> Option (StackT a)

instance : Stack List where
  empty := []
  isEmpty := fun
    | List.nil => true
    | _        => false
  cons := List.cons
  head := fun
    | List.nil        => none
    | List.cons x _   => some x
  tail := fun
    | List.nil        => none
    | List.cons _ xs  => some xs

inductive CustomStack (a : Type u) where
  | nil : CustomStack a
  | cons : a -> CustomStack a -> CustomStack a
  deriving Repr

instance : Stack CustomStack where
  empty := CustomStack.nil
  isEmpty := fun
    | CustomStack.nil => true
    | _               => false
  cons := CustomStack.cons
  head := fun
    | CustomStack.nil        => none
    | CustomStack.cons x _   => some x
  tail := fun
    | CustomStack.nil        => none
    | CustomStack.cons _ xs  => some xs

def two_elem_stack {a : Type u} (S : Type u -> Type u) [Stack S] (x : a) : S a :=
  Stack.cons x (Stack.cons x Stack.empty)

#eval two_elem_stack List 1
#eval two_elem_stack CustomStack 2

end PFDS
