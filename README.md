dotfiles
===========

> Good tools are prerequisite to the successful execution of a job.
> 工欲善其事，必先利其器。

## Submodule 更新方式

- https://stackoverflow.com/a/914090/2163429

```
# 修改 .gitmodules 后
git submodule sync

# 更新到最新 commit
git submodule update --init --recursive --remote

# 更新到指定 commit
git submodule update --init
```

## Emacs

``` shell
brew install mailutils libxml2
git clone https://gitee.com/mirrors/emacs.git

./autogen.sh
./configure --with-mailutils
export CPATH=`xcrun --show-sdk-path`/usr/include:`xcrun --show-sdk-path`/usr/include/libxml2
make
```

### Debug

使用 Emacs 过程中，如果遇到报错，可以通过下面配置打印出报错的堆栈
```
(toggle-debug-on-quit)
(toggle-debug-on-error)

(setq debug-on-message "free variable") ;; debug for specific error message
```

### Magit

通过源码编译 magit 时可能遇到

```
magit.texi:6: warning: unrecognized encoding name `UTF-8'.

# 解决方式
PATH=/usr/local/opt/texinfo/bin:$PATH make
```
参考：https://github.com/jwiegley/use-package/issues/609
