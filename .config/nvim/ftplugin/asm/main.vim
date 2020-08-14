setlocal tabstop=8
setlocal softtabstop=8
setlocal shiftwidth=8
setlocal noexpandtab
let &l:listchars .= (empty(&l:listchars) ? "" : ",") . "tab:\ \ "
