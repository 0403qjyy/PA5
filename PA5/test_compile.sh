#!/bin/bash
# 完整编译测试脚本

cd /usr/class/student-dist/assignments/PA5

echo "=== PA5 代码生成器编译测试 ==="
echo ""

echo "1. 检查关键文件内容"
echo "--- Expression_EXTRAS宏 ---"
grep -A 8 "^#define Expression_EXTRAS" cool-tree.handcode.h
echo ""

echo "--- Expression_SHARED_EXTRAS宏 ---"
grep -A 3 "^#define Expression_SHARED_EXTRAS" cool-tree.handcode.h
echo ""

echo "--- Environment前向声明 ---"
grep -B 2 -A 2 "^class Environment" cool-tree.handcode.h
echo ""

echo "2. 检查IsEmpty()方法"
echo "在Expression_EXTRAS中:"
grep "Expression_EXTRAS" cool-tree.handcode.h -A 8 | grep -i "isempty"
echo "在no_expr_class中:"
grep -A 10 "class no_expr_class" cool-tree.h | grep "IsEmpty"
echo ""

echo "3. 清理并编译"
make clean > /dev/null 2>&1
make cgen 2>&1 | tail -20
echo ""

if [ -f cgen ]; then
    echo "✅ 编译成功！"
    ls -lh cgen
else
    echo "❌ 编译失败"
    echo "错误详情:"
    make cgen 2>&1 | grep -i "error" | head -10
fi
