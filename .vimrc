set nocompatible
filetype off
filetype plugin indent off

set runtimepath+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'Shougo/vimproc'
Bundle 'Shougo/neocomplcache'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-speeddating'
Bundle 'ujihisa/neco-ghc'
Bundle 'ujihisa/neco-ruby'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'Lokaltog/vim-powerline'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
Bundle 'altercation/vim-colors-solarized'
Bundle 'dag/vim2hs'
Bundle 'kana/vim-filetype-haskell'
Bundle 'kien/ctrlp.vim'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'scrooloose/syntastic'
Bundle 'godlygeek/tabular'
Bundle 'othree/html5.vim'
Bundle 'tsaleh/vim-matchit'

filetype plugin indent on

syntax enable
set cursorline
set hidden
set nowrap
set tabstop=2
set shiftwidth=2
set expandtab
set ignorecase
set smartcase
set incsearch
set hlsearch
set list
set listchars=""
set listchars=tab:\ \
set listchars+=trail:.
set listchars+=extends:>
set listchars+=precedes:<

let g:acp_enableAtStartup = 0
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()

autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

let mapleader=","

let g:ctrlp_map="<Nop>"
let g:ctrlp_working_path_mode=2
nnoremap <leader>f :CtrlPMixed<CR>
nnoremap <leader>s :VimShell<CR>
nnoremap <space> :noh<CR>

au FileType haskell nnoremap <leader><space> :GhcModType<CR>
au FileType haskell nnoremap <space> :GhcModTypeClear<CR>:noh<CR>

au BufWritePost *.coffee silent CoffeeMake! -b | cwindow | redraw!

let g:syntastic_check_on_open=1

set laststatus=2
set encoding=utf-8
let g:Powerline_symbols = 'fancy'

set backupdir=~/.vim/backup
set directory=~/.vim/swap
