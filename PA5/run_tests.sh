#!/bin/bash
# PA5 测试脚本 - 测试10个用例

cd /usr/class/student-dist/assignments/PA5

# 确保工具已链接
if [ ! -f lexer ] || [ ! -f parser ] || [ ! -f semant ]; then
    echo "正在链接工具..."
    echo "123" | sudo -S /usr/class/student-dist/etc/link-object 5 lexer 2>/dev/null
    echo "123" | sudo -S /usr/class/student-dist/etc/link-object 5 parser 2>/dev/null
    echo "123" | sudo -S /usr/class/student-dist/etc/link-object 5 semant 2>/dev/null
fi

echo "=========================================="
echo "PA5 代码生成器测试"
echo "=========================================="
echo ""

# 测试用例列表（从examples目录选择10个）
TEST_CASES=(
    "/usr/class/student-dist/examples/hello_world.cl"
    "/usr/class/student-dist/examples/complex.cl"
    "/usr/class/student-dist/examples/arith.cl"
    "/usr/class/student-dist/examples/list.cl"
    "/usr/class/student-dist/examples/io.cl"
    "/usr/class/student-dist/examples/cells.cl"
    "/usr/class/student-dist/examples/book_list.cl"
    "/usr/class/student-dist/examples/hairyscary.cl"
    "/usr/class/student-dist/examples/palindrome.cl"
    "/usr/class/student-dist/examples/new_complex.cl"
)

SUCCESS=0
FAILED=0

echo "开始测试..."
echo ""

for i in "${!TEST_CASES[@]}"; do
    TEST_FILE="${TEST_CASES[$i]}"
    TEST_NUM=$((i+1))
    OUTPUT_FILE="test${TEST_NUM}.s"
    TEST_NAME=$(basename "$TEST_FILE" .cl)
    
    echo "[测试 $TEST_NUM/10] $TEST_NAME"
    echo "  输入: $TEST_FILE"
    echo "  输出: $OUTPUT_FILE"
    
    # 运行代码生成器
    ./lexer "$TEST_FILE" 2>/dev/null | ./parser "$TEST_FILE" 2>/dev/null | ./semant "$TEST_FILE" 2>/dev/null | ./cgen -o "$OUTPUT_FILE" "$TEST_FILE" 2>&1 > /dev/null
    
    if [ -f "$OUTPUT_FILE" ]; then
        SIZE=$(ls -lh "$OUTPUT_FILE" | awk '{print $5}')
        echo "  ✅ 成功生成 $OUTPUT_FILE ($SIZE)"
        SUCCESS=$((SUCCESS+1))
    else
        echo "  ❌ 失败: 文件未生成"
        FAILED=$((FAILED+1))
    fi
    echo ""
done

echo "=========================================="
echo "测试结果汇总"
echo "=========================================="
echo "成功: $SUCCESS/10"
echo "失败: $FAILED/10"
echo ""

if [ $SUCCESS -eq 10 ]; then
    echo "🎉 所有测试通过！"
    exit 0
else
    echo "⚠️  部分测试失败"
    exit 1
fi

