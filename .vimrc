" Enable hlint and GHC via Cabal
let g:ale_linters = {'haskell': ['hlint', 'my_cabal_ghc']}
" ... only
let g:ale_linters_explicit = 1
" Don't lint until I save
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_enter = 0
" Set up Alexis King options
call ale#Set('haskell_cabal_ghc_options', '-isrc -itest -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints -fno-code -v0')

function! GetCommand(buffer) abort
  return 'cabal exec -- ghc '
        \ . ale#Var(a:buffer, 'haskell_cabal_ghc_options')
        \ . ' %t'
endfunction

call ale#linter#Define('haskell', {
      \ 'name': 'my_cabal_ghc',
      \ 'output_stream': 'stderr',
      \ 'executable': 'cabal',
      \ 'command': function('GetCommand'),
      \ 'callback': 'ale#handlers#haskell#HandleGHCFormat',
      \})
