setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal expandtab
setlocal conceallevel=2

syntax keyword haskellForall forall conceal cchar=∀
syntax match haskellLambda "\\\ze[[:alpha:][:space:]_([]" conceal cchar=λ
highlight clear Conceal
highlight! link Conceal Operator

let b:delimitMate_matchpairs = "(:),[:],{:}"
" let hsoptions="qAsxerbhlw-iRtBQZNDC1a"
