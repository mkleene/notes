set nocompatible              " be iMproved, required
filetype off                  " required
syntax on

set noexpandtab
set tabstop=8
set shiftwidth=4
set softtabstop=4
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

Plugin 'scrooloose/nerdtree'

Plugin 'derekwyatt/vim-scala'

" Plugin 'Valloric/YouCompleteMe' FIXME: use a vim with python 2.x support

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList          - list configured plugins
" :PluginInstall(!)    - install (update) plugins
" :PluginSearch(!) foo - search (or refresh cache first) for foo
" :PluginClean(!)      - confirm (or auto-approve) removal of unused plugins
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" Open nerdtree at startup and move the cursor to the other window 
autocmd VimEnter * NERDTree | wincmd p 

" close nerdtree if it's the only buffer around 
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

set path=$PWD/**
