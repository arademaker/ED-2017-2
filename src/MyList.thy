theory MyList
imports Main 

begin

no_notation Nil ("[]") and Cons (infixr "#" 65) and append (infixr "@" 65)
hide_type list
hide_const rev

datatype 'a list = 
  Nil   ("[]") | 
  Cons 'a "'a list"  (infixr "#" 65)

(* defining two functions *)

primrec app :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" (infixr "@" 65)
where
  "[] @ ys  = ys" |
  "(x # xs) @ ys = x # (xs @ ys)"

term "app"
term "app []"

primrec rev :: "'a list \<Rightarrow> 'a list" 
where 
  "rev [] = []" |
  "rev (x # xs) = (rev xs) @ (x # [])"


(* testing the functions *)

term "rev (a # b # c # [])"
term "rev" 
value "rev (a # b # [])"
value "rev (1 # 2 # 3 # [])"
value "rev (a # b # c # [])"
value "rev (True # False # [])"
value "rev (rev (Cons True (Cons False Nil)))"

value "size []"
value "size (a # b # c # [])"

(* proofs about these functions *)

lemma app_nil [simp]: "xs @ [] = xs"
apply (induct xs)
apply (auto)
done

lemma app_assoc [simp] : "(xs @ ys) @ zs = xs @ (ys @ zs)"
apply (induct xs)
apply (auto)
done

lemma rev_app [simp]: "rev(xs @ ys) = (rev ys) @ (rev xs)" 
apply (induct xs)
apply (auto)
done

theorem rev_rev [simp]: "rev (rev xs) = xs"  
apply (induct xs)
apply (auto)
done

end