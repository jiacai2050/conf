if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
execute pathogen#infect()
syntax enable
syntax on
filetype plugin on
filetype plugin indent on

" {{{ https://github.com/scrooloose/syntastic
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
" }}} https://github.com/scrooloose/syntastic

if $MY_THEME == 'light'
   set bg=light
else
   set bg=dark
endif

set pastetoggle=<F2>
"不要vim模仿vi模式，建议设置，否则会有很多不兼容的问题
set nocompatible
set backspace=indent,eol,start

set number
set smartindent "智能缩进
set tabstop=4    "设置tab为4个空格
set shiftwidth=4 "设置缩进宽度为4
set softtabstop=4 "Backspace一次删除4格
set expandtab	"将tab制表符转换为空格,打印输出更好看

" 设置文件编码，支持中文
" 参考：http://edyfox.codecarver.org/html/vim_fileencodings_detection.html
set fileencodings=utf-8,gbk
set encoding=utf-8

" 按F8执行python文件
map <F8> :!/usr/bin/python %
" 按F9 格式化json数据
map <F9> :%!python -m json.tool

" 设置autocompletion 的颜色
highlight Pmenu term=reverse ctermbg=white ctermfg=black
highlight PmenuSel term=reverse ctermbg=green ctermfg=red

set hlsearch
set incsearch
set wildmenu
au BufNewFile,BufRead *.handlebars set filetype=html
au BufNewFile,BufRead *.ejs set filetype=html

" 一直显示状态栏
set laststatus=2
" 显示当前鼠标行列号
set ruler
" 高亮当前行
set cursorline
" 高亮当前列
"set cursorcolumn

"au BufWinLeave * mkview "退出文件时保存fold
"au BufWinEnter * silent loadview "打开文件时恢复fold

let mapleader = ","
" 折叠HTML标签
nnoremap <leader>f Vatzf


" 行末空格高亮
autocmd ColorScheme * highlight TrailWhitespace ctermbg=red guibg=red
highlight TrailWhitespace ctermbg=red guibg=red
match TrailWhitespace /\s\+$/

" 脚本文件自动添加 SheBang
augroup Shebang
    autocmd BufNewFile *.py 0put =\"#!/usr/bin/env python\<nl># coding: utf-8\<nl>\"|$
    autocmd BufNewFile *.sh 0put =\"#!/usr/bin/env bash\<nl>\"|$
    autocmd BufNewFile *.rb 0put =\"#!/usr/bin/env ruby\<nl>\"|$
augroup END
