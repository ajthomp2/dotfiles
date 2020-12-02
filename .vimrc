" Author: Alex Thompson
" Email: alex.thompson@gmail.com
" Edit Log:
"     * 12/18/19 - initial creation

set nocompatible " force vi/vim to run as vim

" disable arrow keys 
" inoremap for insert mode, noremap for other modes
noremap <Up> <Nop>
inoremap <Up> <Nop>
noremap <Down> <Nop>
inoremap <Down> <Nop>
noremap <Left> <Nop>
inoremap <Left> <Nop>
noremap <Right> <Nop>
inoremap <Right> <Nop>

" general
set encoding=utf-8
set autoread " detect when a file has changed
set history=1000 " change history to 1000
set tabpagemax=50 " max number of vim tabs/pages
set backspace=indent,eol,start " allow backspacing over autoindent, line breaks (line joins), and over start of insert
set nolazyredraw " don't redraw when executing macros
set ttyfast " faster redrawing
set formatoptions+=j " Delete comment character when joining commented lines
set completeopt-=preview "disable preview pane selecting option from autocomplete


" appearence
set textwidth=120
set number " show line numbers
set autoindent " automatically set indent of new line
set showcmd " show incomplete commands
set cmdheight=1 " command bar height
set showmatch " show matching braces

" tab control
set smarttab " tab respects tabstop, shiftwidth, and softtabstop
set tabstop=4 " the visible width of tabs
set softtabstop=4 " edit as if tabs are 4 characters wide
set shiftwidth=4 " number of spaces to use for indent and unindent
set noexpandtab

" search
set ignorecase " case insensitive search
set smartcase " case sensitive only if capital letter in search
set hlsearch " highlight search results
set incsearch " incremental search results

" error bells
set noerrorbells
set visualbell
set t_vb= " no beeping

" associate .h files with C
augroup project
  autocmd!
  autocmd BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
augroup END

" ensure vim-plug is installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" install plugins using vim-plug
call plug#begin('~/.vim/plugged')

" {

	Plug 'tpope/vim-commentary' " commenting/uncommenting lines
	Plug 'tpope/vim-surround' " updating/adding 'surrounding braces,quotes,etc
	Plug 'tpope/vim-repeat' " supports repeating command with plugins
	Plug 'tpope/vim-sleuth' " autodetect indent style (tab vs spaces)
	Plug 'sickill/vim-pasta' " context aware pasting

	" file browsing in vim
	Plug 'scrooloose/nerdtree'
	map <C-n> :NERDTreeToggle<CR>

	" autocomplete for vim
	Plug 'ycm-core/YouCompleteMe', { 'do': 'python3 install.py --clang-completer' } " '--clang-compiler' specifies that it was compiled to provide c family language support
	let g:ycm_global_ycm_extra_conf='~/.vim/.ycm_extra_conf.py' " provides the default compiler flags to enable autocomplete for source code
	let g:ycm_confirm_extra_conf=0 " disables confirming that the ycm extra conf is safe to load
" }

call plug#end()

