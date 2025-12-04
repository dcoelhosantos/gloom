" Vim syntax file
" Language: Gloom
" Maintainer: João
" Latest Revision: 2025

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword gloomKeyword fn return if else while for break continue
syn keyword gloomType Int Float String Bool
syn keyword gloomConstant true false null
syn keyword gloomStorageClass var const
syn keyword gloomStructure struct

" Comments
syn match gloomComment "//.*$"
syn region gloomBlockComment start="/\*" end="\*/"

" Strings
syn region gloomString start='"' end='"' skip='\\"'

" Numbers
syn match gloomNumber '\<\d\+\>'
syn match gloomFloat '\<\d\+\.\d\+\>'

" Operators
syn match gloomOperator "[-+*/%=<>!&|]"
syn match gloomOperator "->"
syn match gloomOperator "=="
syn match gloomOperator "!="
syn match gloomOperator "<="
syn match gloomOperator ">="
syn match gloomOperator "&&"
syn match gloomOperator "||"

" Functions
syn match gloomFunction '\<\w\+\>\s*('me=e-1

" Types (user-defined and built-in)
syn match gloomCustomType '\<[A-Z]\w*\>'

" Arrays
syn match gloomArray '\[\w\+\]'

" Highlight groups
hi def link gloomKeyword        Keyword
hi def link gloomType           Type
hi def link gloomConstant       Constant
hi def link gloomStorageClass   StorageClass
hi def link gloomStructure      Structure
hi def link gloomComment        Comment
hi def link gloomBlockComment   Comment
hi def link gloomString         String
hi def link gloomNumber         Number
hi def link gloomFloat          Float
hi def link gloomOperator       Operator
hi def link gloomFunction       Function
hi def link gloomCustomType     Type
hi def link gloomArray          Type

let b:current_syntax = "gloom"
