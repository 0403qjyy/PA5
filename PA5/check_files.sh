#!/bin/bash
# 提交前文件检查脚本

echo "=========================================="
echo "PA5 提交前文件检查"
echo "=========================================="
echo ""

cd /usr/class/student-dist/assignments/PA5

# 检查1: 文件是否存在
echo "1. 检查文件是否存在:"
FILES=("cgen.cc" "cgen.h" "cool-tree.h" "cool-tree.handcode.h" "example.cl")
for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "  ✅ $file"
    else
        echo "  ❌ $file (缺失)"
    fi
done
echo ""

# 检查2: cool-tree.handcode.h 关键内容
echo "2. 检查 cool-tree.handcode.h 关键内容:"
if grep -q "class Environment;" cool-tree.handcode.h; then
    echo "  ✅ 有 Environment 前向声明"
else
    echo "  ❌ 缺少 Environment 前向声明"
fi

if grep -q "virtual void code(ostream&, Environment)" cool-tree.handcode.h; then
    echo "  ✅ code() 方法签名正确（有 Environment 参数）"
else
    echo "  ❌ code() 方法签名错误（缺少 Environment 参数）"
fi

if grep -q "virtual bool IsEmpty()" cool-tree.handcode.h; then
    echo "  ✅ 有 IsEmpty() 方法"
else
    echo "  ❌ 缺少 IsEmpty() 方法"
fi
echo ""

# 检查3: cool-tree.h 中 no_expr_class
echo "3. 检查 cool-tree.h 中 no_expr_class:"
if grep -A 5 "class no_expr_class" cool-tree.h | grep -q "IsEmpty"; then
    echo "  ✅ no_expr_class 有 IsEmpty() 方法"
else
    echo "  ❌ no_expr_class 缺少 IsEmpty() 方法"
fi
echo ""

# 检查4: 编译测试
echo "4. 编译测试:"
make clean > /dev/null 2>&1
if make cgen 2>&1 | grep -q "error"; then
    echo "  ❌ 编译有错误"
    make cgen 2>&1 | grep -i "error" | head -5
else
    echo "  ✅ 编译成功"
fi
echo ""

echo "=========================================="
echo "检查完成"
echo "=========================================="
