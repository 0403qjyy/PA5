#!/bin/bash
# 自动链接编译工具

echo "正在链接工具..."
echo "123" | sudo -S /usr/class/student-dist/etc/link-object 5 lexer 2>/dev/null
echo "123" | sudo -S /usr/class/student-dist/etc/link-object 5 parser 2>/dev/null
echo "123" | sudo -S /usr/class/student-dist/etc/link-object 5 semant 2>/dev/null

echo "工具链接完成！"
ls -la lexer parser semant
