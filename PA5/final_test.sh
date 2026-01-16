#!/bin/bash
# 使用已验证可用的文件测试代码生成器

echo "=== 测试代码生成器 ==="
echo "使用 ../PA3J/good.cl 文件..."

./lexer ../PA3J/good.cl 2>/dev/null | ./parser ../PA3J/good.cl 2>/dev/null | ./semant ../PA3J/good.cl 2>/dev/null | ./cgen -o final_test.s ../PA3J/good.cl 2>&1

if [ -f final_test.s ]; then
    echo ""
    echo "=== 成功！文件已生成 ==="
    echo "文件信息:"
    ls -lh final_test.s
    echo ""
    echo "=== 前50行内容 ==="
    head -50 final_test.s
    echo ""
    echo "=== 可以运行SPIM测试 ==="
    echo "命令: /usr/class/bin/spim -file final_test.s"
else
    echo "=== 失败：文件未生成 ==="
    exit 1
fi
