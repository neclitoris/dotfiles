syntax keyword haskellForall forall conceal cchar=∀
syntax match haskellLambda "\\\ze[[:alpha:][:space:]_([]" conceal cchar=λ
highlight clear Conceal
highlight! link Conceal Operator
