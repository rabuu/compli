if exists("b:current_syntax")
	finish
end

syn keyword compliKeyword func let in
syn keyword compliDataType int bool
syn keyword compliConditional if then else
syn keyword compliBoolean true false
syn match compliNumber "-\=\<[0-9]*\>"
syn region compliComment start="//" end="\n"

hi def link compliKeyword Keyword
hi def link compliDataType Type
hi def link compliConditional Conditional
hi def link compliBoolean Boolean
hi def link compliNumber Number
hi def link compliComment Comment

hi compliDataType cterm=italic gui=italic

let b:current_syntax = "compli"
