"不要vim模仿vi模式，建议设置，否则会有很多不兼容的问题
set nocompatible    
set backspace=indent,eol,start
"打开高亮
syntax enable
syntax on   

"filetype plugin on 
"filetype plugin indent on
let g:pydiction_location = '/home/liujiacai/.vim/vimfiles/ftplugin/pydiction-1.2.1/complete-dict'

" 显示行号
set number
"启用鼠标
set mouse=v 

"set autoindent  "自动缩进，即每行的缩进与上一行相等
"set smartindent "智能缩进
set tabstop=4    "设置tab为4个空格
set shiftwidth=4 "设置缩进宽度为4	
set softtabstop=4 "Backspace一次删除4格
set expandtab	"将tab制表符转换为空格,打印输出更好看

" 设置文件编码，支持中文
set fileencodings=utf-8,gbk
set nobomb
" 按F12执行python文件
map <F12> :!/usr/bin/python %

set background=dark
" 设置autocompletion 的颜色
"highlight Pmenu term=reverse ctermbg=cyan ctermfg=red
"highlight PmenuSel term=reverse ctermbg=black ctermfg=green
" 搜索的字符串反白
set hlsearch

au BufNewFile,BufRead *.handlebars set filetype=html
au BufNewFile,BufRead *.ejs set filetype=html

" 一直显示状态栏
set laststatus=2
" 显示当前鼠标行列号
set ruler
"高亮当前行
set cursorline
"高亮当前列
set cursorcolumn
