if exists("b:current_syntax")
	finish
end

syn keyword compliKeyword func rec let in
syn keyword compliDataType int bool float
syn keyword compliConditional if then else
syn keyword compliBoolean true false
syn keyword compliOperator trace cast_int cast_float
syn match compliNumber "-\=\<[0-9]*\>"
syn match compliFloat "-\=\<[0-9]*\.[0-9]*\>"
syn region compliComment start="//" end="\n"

hi def link compliKeyword Keyword
hi def link compliDataType Type
hi def link compliConditional Conditional
hi def link compliBoolean Boolean
hi def link compliOperator Operator
hi def link compliNumber Number
hi def link compliFloat Float
hi def link compliComment Comment

let b:current_syntax = "compli"
